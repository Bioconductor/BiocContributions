import os
import json
import requests
import subprocess

# --------------------------------------------
# Environment
# --------------------------------------------
TOKEN = os.environ["GITHUB_TOKEN"]
#ORG_NAME = os.environ["ORG_NAME"]
# temporarily use testing team at tempbioc
ORG_NAME = "tempbioc"
TEAM = os.environ["TEAM_SLUG"]
REPO_FULL = os.environ["GITHUB_REPOSITORY"]
REVIEWER_STATE_FILE = os.environ["REVIEWER_STATE_PATH"]

with open(os.environ["GITHUB_EVENT_PATH"]) as f:
    event = json.load(f)

ISSUE_NUMBER = event["issue"]["number"]

# --------------------------------------------
# Only proceed if the label added is correct
# --------------------------------------------
label_name = event["label"]["name"]
if label_name != "assign reviewer":
    print(f"Label '{label_name}' is not 'assign reviewer', exiting.")
    exit(0)
   
TARGET_GITHUB_TOKEN = os.environ["TARGET_GITHUB_TOKEN"]
headers = {"Authorization": f"Bearer {TOKEN}", "Accept": "application/vnd.github+json"}
headersV2 = {"Authorization": f"Bearer {TARGET_GITHUB_TOKEN}", "Accept": "application/vnd.github+json"}

# --------------------------------------------
# Helper Functions 
# --------------------------------------------
def add_label(issue_number, label):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels"
    r = requests.post(url, headers=headers, json={"labels": [label]})
    r.raise_for_status()
    print(f"[DEBUG] Label '{label}' added to issue #{issue_number}")


def remove_label(issue_number, label):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels/{label}"
    r = requests.delete(url, headers=headers)
    if r.status_code in (200, 204, 404):
        print(f"[DEBUG] Label '{label}' removed from issue #{issue_number}")
    else:
        r.raise_for_status()


# --------------------------------------------
# Retrieve all reviewers
# --------------------------------------------

r = requests.get(f"https://api.github.com/orgs/{ORG_NAME}/teams/{TEAM}/members", headers=headersV2)
r.raise_for_status()
all_members = [m["login"] for m in r.json()]

# --------------------------------------------
# excluded reviewers (temporary leave or requested break)
# --------------------------------------------

excluded_raw = os.environ.get("EXCLUDED_REVIEWERS", "")
excluded = [x.strip() for x in excluded_raw.split(",") if x.strip()]
eligible_reviewers = [m for m in all_members if m not in excluded]

print(f"[DEBUG] all_members: {all_members}")
print(f"[DEBUG] excluded_members: {excluded}")
print(f"[DEBUG] eligible_reviewers: {eligible_reviewers}")

# --------------------------------------------
# check if label was added by a member of the team
# --------------------------------------------

sender = event.get("sender", {}).get("login")
if sender not in all_members:
    print(f"User '{sender}' is not allowed to assign reviewers. Exiting.")
    exit(0)

if not eligible_reviewers:
    print("No eligible reviewers left to assign.")
    exit(0)

# --------------------------------------------
# Checkout submissions branch where last_assignee.txt lives
# --------------------------------------------

subprocess.run(["git", "fetch", "origin", "submissions"], check=True)
subprocess.run(["git", "checkout", "-B", "submissions", "origin/submissions"], check=True)

last_assigned = None
if os.path.exists(REVIEWER_STATE_FILE):
    with open(REVIEWER_STATE_FILE) as f:
        last_assigned = f.read().strip()

print(f"[DEBUG] last_assigned: {last_assigned}")
        
# --------------------------------------------
# Determine and assign next reviewer
# --------------------------------------------

if last_assigned in eligible_reviewers:
    idx = eligible_reviewers.index(last_assigned)
    next_idx = (idx + 1) % len(eligible_reviewers)
else:
    next_idx = 0
reviewer = eligible_reviewers[next_idx]

print(f"Assigning reviewer: {reviewer}")

assign_url = f"https://api.github.com/repos/{REPO_FULL}/issues/{ISSUE_NUMBER}/assignees"
r2 = requests.post(assign_url, headers=headers, json={"assignees": [reviewer]})
r2.raise_for_status()
print(f"Reviewer {reviewer} assigned successfully.")

# --------------------------------------------
# Overwrite last_assignee.txt with new reviewer
#   and commit & push to submissions branch
# --------------------------------------------
os.makedirs(os.path.dirname(REVIEWER_STATE_FILE), exist_ok=True)
with open(REVIEWER_STATE_FILE, "w") as f:
    f.write(reviewer + "\n")

subprocess.run(['git', 'config', 'user.name', "github-actions[bot]"], check=True)
subprocess.run(['git', 'config', 'user.email', "github-actions[bot]@users.noreply.github.com"], check=True)
subprocess.run(['git', 'add', REVIEWER_STATE_FILE], check=True)
subprocess.run(['git', 'commit', '-m', f"Update last assigned reviewer for {TEAM}: {reviewer}"], check=False)
subprocess.run(['git', 'push', 'origin', 'submissions'], check=True)

# --------------------------------------------
# Update Labels and Post Comment
# --------------------------------------------

remove_label(ISSUE_NUMBER, "assign reviewer")

comment_url = f"https://api.github.com/repos/{REPO_FULL}/issues/{ISSUE_NUMBER}/comments"
comment_body = {"body": f"👤 Reviewer @{reviewer} has been assigned."}

r3 = requests.post(comment_url, headers=headers, json=comment_body)
try:
    r3.raise_for_status()
    print(f"Comment posted: Reviewer {reviewer} assigned.")
except requests.RequestException as e:
    print(f"[ERROR] Failed to post assignment comment: {e}")

