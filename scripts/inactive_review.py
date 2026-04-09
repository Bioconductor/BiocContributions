import os
import json
import requests
import subprocess
import re
import sys
import base64

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
ISSUE_NUMBER = os.environ.get("ISSUE_NUMBER")
EVENT_PATH = os.environ["GITHUB_EVENT_PATH"]

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

def is_team_member(username):
    url = f"https://api.github.com/orgs/{ORG_NAME}/teams/{TEAM}/memberships/{username}"
    r = requests.get(url, headers=ORG_HEADERS)

    return r.status_code == 200 and r.json().get("state") == "active"


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


# ----------------------------
# Main Handler
# ----------------------------
def main():
    # --------------------------------------------
    # Grab event details
    # --------------------------------------------
    with open(EVENT_PATH) as f:
        event = json.load(f)

    # --------------------------------------------
    # extract data
    # --------------------------------------------

    issue = event["issue"]
    issue_number = issue["number"]
    issue_body = issue.get("body") or ""
    label_name = event["label"]["name"]
    actor = event.get("sender", {}).get("login")

    # --------------------------------------------
    # Only proceed if the label added is correct
    # --------------------------------------------

    if label_name != "inactive review":
        print(f"Label '{label_name}' is not 'inactive review', exiting.")
        sys.exit(0)

    # --------------------------------------------
    # Verify Label added by member of review team
    # --------------------------------------------
    if not is_team_member(actor):
        post_comment(issue_number, f"User '{actor}' is not allowed to make review inactive. Exiting.")
        remove_label(issue_number, "inactive review")
        sys.exit(0)

    # --------------------------------------------
    # extra repo, delete clone and from registry
    # --------------------------------------------
    owner, repo = extract_repo(issue_body)
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
    remove_label(issue_number, "pre-review")
    remove_label(issue_number, "awaiting policy acceptance")
    remove_label(issue_number, "policies-accepted")
    remove_label(issue_number, "precheck-passed")
    remove_label(issue_number, "review in progress")

    # --------------------------------------------
    # Post Comment and Close Issue
    # --------------------------------------------
    closing_comment = """🛑 Inactive Submission

This issue is being closed because there has been no progress for an extended period of time.

You are welcome to reopen the issue when you are able to actively participate in the review and submission process. Please keep in mind that acceptance requires a commitment to ongoing package maintenance.

Thank you for your interest in Bioconductor.
"""
    post_comment(issue_number, closing_comment)
    close_issue(issue_number)

if __name__ == "__main__":
    main()
