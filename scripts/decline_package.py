import os
import json
import requests
import sys
import random

# --------------------------------------------
# Environment
# --------------------------------------------
GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]             # repo workflow token
BIOC_ORG_TOKEN = os.environ.get("BIOC_ORG_TOKEN")     # org/team token
ORG_NAME = os.environ.get("ORG_NAME", "Bioconductor")
TEAM = os.environ["TEAM_SLUG"]
REPO_FULL = os.environ["GITHUB_REPOSITORY"]

event = {}
event_path = os.environ.get("GITHUB_EVENT_PATH")
if event_path and os.path.exists(event_path):
    try:
        with open(event_path) as f:
            event = json.load(f)
    except json.JSONDecodeError:
        event = {}


if "issue" not in event or "label" not in event:
    print("[ERROR] Invalid event payload: missing issue/label")
    sys.exit(1)


ISSUE_NUMBER = event["issue"]["number"]

# --------------------------------------------
# Only proceed if the label added is correct
# --------------------------------------------
label_name = event["label"]["name"]
if label_name != "package declined":
    print(f"Label '{label_name}' is not 'package declined', exiting.")
    exit(0)

# --------------------------------------------
# Headers
# --------------------------------------------
ORG_HEADERS = {
    "Authorization": f"Bearer {BIOC_ORG_TOKEN}",
    "Accept": "application/vnd.github+json"
} if BIOC_ORG_TOKEN else {"Authorization": f"Bearer {GITHUB_TOKEN}", "Accept": "application/vnd.github+json"}

REPO_HEADERS = {
    "Authorization": f"Bearer {GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json"
}

# --------------------------------------------
# Helper Functions
# --------------------------------------------
def add_label(issue_number, label):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels"
    r = requests.post(url, headers=REPO_HEADERS, json={"labels": [label]})
    r.raise_for_status()
    print(f"[DEBUG] Label '{label}' added to issue #{issue_number}")


def remove_label(issue_number, label):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels/{label}"
    r = requests.delete(url, headers=REPO_HEADERS)
    if r.status_code in (200, 204, 404):
        print(f"[DEBUG] Label '{label}' removed from issue #{issue_number}")
    else:
        r.raise_for_status()

# --------------------------------------------
# Retrieve all reviewers
# --------------------------------------------
r = requests.get(f"https://api.github.com/orgs/{ORG_NAME}/teams/{TEAM}/members", headers=ORG_HEADERS)
r.raise_for_status()
all_members = [m["login"] for m in r.json()]

# --------------------------------------------
# Check if label was added by a member of the team
# --------------------------------------------
sender = event.get("sender", {}).get("login")
if sender not in all_members:
    print(f"User '{sender}' is not allowed to decline reviews. Exiting.")
    try:
        remove_label(ISSUE_NUMBER, "package declined")
    except Exception as e:
        print(f"[WARN] Failed to remove 'package declined' label: {e}")   
    exit(0)

# --------------------------------------------
# Pick an Admin reviewer for secondary review
# --------------------------------------------
admin_raw = os.environ.get("ADMIN_REVIEWERS", "")
admin = [x.strip() for x in admin_raw.split(",") if x.strip()]

issue_url = f"https://api.github.com/repos/{REPO_FULL}/issues/{ISSUE_NUMBER}"

try:
    r = requests.get(issue_url, headers=REPO_HEADERS, timeout=10)
    r.raise_for_status()
    issue_data = r.json()
    current_assignees = [u["login"] for u in issue_data.get("assignees", [])]
except requests.RequestException as e:
    print(f"[WARN] Failed to fetch issue data: {e}")
    current_assignees = []

excluded = set(current_assignees + [sender, "github-actions[bot]"])
eligible_admin = [a for a in admin if a not in excluded]

if not eligible_admin:
    print("[WARN] No eligible admin reviewers available.")
    comment_url = f"https://api.github.com/repos/{REPO_FULL}/issues/{ISSUE_NUMBER}/comments"
    comment_body = {
        "body": (
            "⚠️ Your package has been declined by the first reviewer for reasons stated in the review.\n\n"
            "We are currently having issues automatically assigning a secondary reviewer to take another look. "
            "A reviewer will be assigned manually shortly. "
            "In the meantime, please feel free to leave any additional comments to assist in the review."
        )
    }
    try:
        r = requests.post(comment_url, headers=REPO_HEADERS, json=comment_body)
        r.raise_for_status()
        print(f"[INFO] Comment posted to issue #{ISSUE_NUMBER}")
    except requests.RequestException as e:
        print(f"[ERROR] Failed to post comment: {e}")

    sys.exit(1)

else:
    chosen_admin = random.choice(eligible_admin)
    print(f"[INFO] Selected admin reviewer: {chosen_admin}")


# --------------------------------------------
# Assign reviewer
# --------------------------------------------
assign_url = f"https://api.github.com/repos/{REPO_FULL}/issues/{ISSUE_NUMBER}/assignees"
try:
    r2 = requests.post(assign_url, headers=REPO_HEADERS, json={"assignees": [chosen_admin]})
    r2.raise_for_status()
    print(f"Reviewer {chosen_admin} assigned successfully.")
except requests.RequestException as e:
    print(f"[ERROR] Failed to assign reviewer: {e}")
    sys.exit(1)

# --------------------------------------------
# post comment
# --------------------------------------------
comment_url = f"https://api.github.com/repos/{REPO_FULL}/issues/{ISSUE_NUMBER}/comments"
comment_body = {
    "body": (
        f"👤 A secondary Reviewer @{chosen_admin} has been assigned. Please comment `/confirm-decline-package` to verify package decline.\n\n"
        "Your package has been declined by the first reviewer for reasons stated in the review. "
        "We have assigned a secondary reviewer to take a second look. "
        "In the meantime, please feel free to leave any additional comments to assist in the review."
    )
}
r3 = requests.post(comment_url, headers=REPO_HEADERS, json=comment_body)
try:
    r3.raise_for_status()
    print(f"Comment posted: Reviewer {chosen_admin} assigned.")
except requests.RequestException as e:
    print(f"[ERROR] Failed to post assignment comment: {e}")
