import os
import json
import requests
import re
import sys
import base64
import time

# --------------------------------------------
# Environment
# --------------------------------------------
GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]             # repo workflow token
BIOC_ORG_TOKEN = os.environ.get("BIOC_ORG_TOKEN")     # org/team token
TEMP_BIOC_TOKEN = os.environ.get("TEMP_BIOC_TOKEN")
ORG_NAME = os.environ.get("ORG_NAME", "Bioconductor")
TEAM = os.environ["TEAM_SLUG"]
GIT_TARGET_ORG = os.environ["GIT_TARGET_ORG"]
REPO_FULL = os.environ["GITHUB_REPOSITORY"]
EVENT_PATH = os.environ["GITHUB_EVENT_PATH"]

OWNER, REPO = REPO_FULL.split("/")

HEADERS = {
    "Authorization": f"Bearer {GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json"
}

ORG_HEADERS = {
    "Authorization": f"Bearer {BIOC_ORG_TOKEN}",
    "Accept": "application/vnd.github+json"
} if BIOC_ORG_TOKEN else HEADERS

TEMP_BIOC_HEADERS = {
    "Authorization": f"Bearer {TEMP_BIOC_TOKEN}",
    "Accept": "application/vnd.github+json"
}

    
# --------------------------------------------
# Helper Functions
# --------------------------------------------
def remove_label(issue_number, label):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels/{label}"
    r = requests.delete(url, headers=HEADERS)
    if r.status_code in (200, 204, 404):
        print(f"[DEBUG] Label '{label}' removed from issue #{issue_number}")
    else:
        r.raise_for_status()


def post_comment(issue_number, body):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/comments"
    r = requests.post(url, headers=HEADERS, json={"body": body})
    if r.status_code >= 300:
        print(f"[WARN] Failed to post comment: {r.status_code} {r.text}")

def close_issue(issue_number):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}"
    r = requests.patch(url, headers=HEADERS, json={"state": "closed"})
    if r.status_code >= 300:
        print(f"[WARN] Failed to close issue: {r.status_code} {r.text}")

def extract_repo(issue_body):
    match = re.search(r"(?:https://github\.com/|git@github\.com:)([\w\-]+)/([\w\.\-]+)", issue_body)
    if not match:
        return None, None
    owner, repo = match.group(1), match.group(2)
    return owner, repo

def get_issue(issue_number):
    url = f"https://api.github.com/repos/{OWNER}/{REPO}/issues/{issue_number}"
    r = requests.get(url, headers=HEADERS, timeout=10)
    r.raise_for_status()
    return r.json()


def get_label_adder(owner, repo, issue_number, label_name, headers):
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/events"
    try:
        while url:
            resp = requests.get(url, headers=headers, timeout=10)
            resp.raise_for_status()
            events = resp.json()
            # iterate newest → oldest within this page
            for event in reversed(events):
                if event.get("event") == "labeled":
                    label = event.get("label", {}).get("name")
                    if label == label_name:
                        actor = event.get("actor", {})
                        return actor.get("login")
            # move to next page if present
            url = resp.links.get("next", {}).get("url")
    except requests.RequestException as e:
        print(f"[ERROR] Failed to fetch issue events for #{issue_number}: {e}")
        return None
    return None

# ----------------------------
# TEMPBIOC CLEANUP
# ----------------------------

def delete_temp_repo(repo_name):
    url = f"https://api.github.com/repos/{GIT_TARGET_ORG}/{repo_name}"
    r = requests.delete(url, headers=TEMP_BIOC_HEADERS)

    if r.status_code == 204:
        print(f"[INFO] Deleted repo {repo_name}")
    elif r.status_code == 404:
        print(f"[INFO] Repo {repo_name} does not exist")
    else:
        print(f"[WARN] Failed to delete repo: {r.status_code} {r.text}")


def remove_from_registry(repo_name):
    registry_repo = "tempbioc.r-universe.dev"

    url = f"https://api.github.com/repos/{GIT_TARGET_ORG}/{registry_repo}/contents/packages.json"
    r = requests.get(url, headers=TEMP_BIOC_HEADERS)

    if r.status_code != 200:
        print("[WARN] Could not fetch registry")
        return

    data = r.json()
    content = json.loads(base64.b64decode(data["content"]).decode())

    new_content = [x for x in content if x.get("package") != repo_name]

    if len(new_content) == len(content):
        print(f"[INFO] {repo_name} not found in registry")
        return

    updated = json.dumps(new_content, indent=2)

    r = requests.put(url, headers=TEMP_BIOC_HEADERS, json={
        "message": f"Remove {repo_name}",
        "content": base64.b64encode(updated.encode()).decode(),
        "sha": data["sha"]
    })
    if r.status_code >= 300:
        print(f"[WARN] Failed to update registry: {r.status_code} {r.text}")

    print(f"[INFO] Removed {repo_name} from registry")


def main():
    # --------------------------------------------
    # Grab event details
    # --------------------------------------------
    with open(EVENT_PATH) as f:
        event = json.load(f)

    if "issue" not in event or "comment" not in event:
        sys.exit(0)

    comment_body = event["comment"]["body"].strip().lower()
    actor = event.get("sender", {}).get("login")
    
    # Allow both "/confirm-decline-package" and "confirm-decline-pacakge"
    if comment_body not in ["/confirm-decline-package", "confirm-decline-package"]:
        # Ignore other comments
        sys.exit(0)

    issue_number = event["issue"]["number"]
    issue = get_issue(issue_number)
    issue_body = issue.get("body") or ""
    owner, repo = extract_repo(issue_body)
    
    # --------------------------------------------
    # Verify Confirm Delete by admin or assignee
    # --------------------------------------------
    admin_raw = os.environ.get("ADMIN_REVIEWERS", "")
    admin_list = [x.strip() for x in admin_raw.split(",") if x.strip()]
    assignees = [a["login"] for a in issue.get("assignees", [])]
    allowed_users = set(admin_list + assignees)
    # remove original label adder so two unique reviewers
    label_actor = get_label_adder(owner, repo, issue_number, "package declined", ORG_HEADERS)
    existing_labels = {l["name"] for l in issue.get("labels", [])}
    
    if label_actor:
        allowed_users.discard(label_actor)

    print(f"[DEBUG] Label added by: {label_actor} and Confirm Delete by: {actor}")
    if actor not in allowed_users:
        post_comment(issue_number, f"User '{actor}' is not allowed to confirm delete. Exiting.")
        sys.exit(0)

    if "package declined" not in existing_labels:
        post_comment(issue_number, "⚠️ Cannot confirm decline: 'package declined' label is no longer present.")
        sys.exit(0)
    # --------------------------------------------
    # extra repo, delete clone and from registry
    # --------------------------------------------
    if repo and repo.endswith(".git"):
        repo = repo[:-4]

    if repo:
        delete_temp_repo(repo)
        remove_from_registry(repo)
    else:
        print(f"[WARN] Could not extract repo from issue body: {issue_number} ")

    # --------------------------------------------
    # clean up labels
    # --------------------------------------------
    LABELS_TO_REMOVE = {
        "pre-review",
        "awaiting policy acceptance",
        "policies-accepted",
        "precheck-passed",
        "review in progress",
    }
    to_remove = LABELS_TO_REMOVE & existing_labels
    for lbl in to_remove:
        remove_label(issue_number, lbl)


    # --------------------------------------------
    # Post Comment and Close Issue
    # --------------------------------------------
    closing_comment = """🏁 Package Declined

This issue is being closed because at least two reviewers independently concluded that this submission is not a good fit for Bioconductor at this time.

The package has been removed from the submission registry and the temporary review environment.

Thank you for your submission and interest in Bioconductor.
"""
    post_comment(issue_number, closing_comment)
    time.sleep(3)
    close_issue(issue_number)

if __name__ == "__main__":
    main()
