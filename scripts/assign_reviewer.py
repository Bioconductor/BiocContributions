import os
import json
import requests
import subprocess

# --------------------------------------------
# Environment
# --------------------------------------------
GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]             # repo workflow token
BIOC_ORG_TOKEN = os.environ.get("BIOC_ORG_TOKEN")     # org/team token
ORG_NAME = os.environ.get("ORG_NAME", "Bioconductor")
TEAM = os.environ["TEAM_SLUG"]
REPO_FULL = os.environ["GITHUB_REPOSITORY"]
REVIEWER_STATE_FILE = os.environ["REVIEWER_STATE_PATH"]

add_bot=False
event = {}
event_path = os.environ.get("GITHUB_EVENT_PATH")
if event_path and os.path.exists(event_path):
    try:
        with open(event_path) as f:
            event = json.load(f)
    except json.JSONDecodeError:
        event = {}

if "issue" not in event:
    issue_number = os.environ.get("ISSUE_NUMBER")
    if not issue_number:
        raise ValueError("ISSUE_NUMBER must be set when no valid event['issue'] exists")

    event = {
        "issue": {"number": int(issue_number)},
        "sender": {"login": "github-actions[bot]"},
        "label": {"name": "assign reviewer"}
    }
    add_bot = True

ISSUE_NUMBER = event["issue"]["number"]

# --------------------------------------------
# Only proceed if the label added is correct
# --------------------------------------------
label_name = event["label"]["name"]
if label_name != "assign reviewer":
    print(f"Label '{label_name}' is not 'assign reviewer', exiting.")
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

def get_current_branch():
    result = subprocess.run(
        ["git", "rev-parse", "--abbrev-ref", "HEAD"],
        capture_output=True, text=True
    )
    return result.stdout.strip() if result.returncode == 0 else None

# --------------------------------------------
# Retrieve all reviewers
# --------------------------------------------
r = requests.get(f"https://api.github.com/orgs/{ORG_NAME}/teams/{TEAM}/members", headers=ORG_HEADERS)
r.raise_for_status()
all_members = [m["login"] for m in r.json()]
if add_bot:
    all_members.append("github-actions[bot]")

# --------------------------------------------
# Excluded reviewers
# --------------------------------------------
excluded_raw = os.environ.get("EXCLUDED_REVIEWERS", "")
excluded = [x.strip() for x in excluded_raw.split(",") if x.strip()]
if add_bot:
    excluded.append("github-actions[bot]")
eligible_reviewers = [m for m in all_members if m not in excluded]

print(f"[DEBUG] all_members: {all_members}")
print(f"[DEBUG] excluded_members: {excluded}")
print(f"[DEBUG] eligible_reviewers: {eligible_reviewers}")

# --------------------------------------------
# Check if label was added by a member of the team
# --------------------------------------------
sender = event.get("sender", {}).get("login")
if sender not in all_members:
    print(f"User '{sender}' is not allowed to assign reviewers. Exiting.")
    exit(0)

if not eligible_reviewers:
    print("No eligible reviewers left to assign.")
    exit(0)

# --------------------------------------------
# Checkout submissions branch for last_assignee.txt
# --------------------------------------------
current_branch = get_current_branch()
subprocess.run(["git", "fetch", "origin", "submissions"], check=True)
subprocess.run(["git", "checkout", "-B", "submissions", "origin/submissions"], check=True)

last_assigned = None
if os.path.exists(REVIEWER_STATE_FILE):
    with open(REVIEWER_STATE_FILE) as f:
        last_assigned = f.read().strip()

print(f"[DEBUG] last_assigned: {last_assigned}")

# --------------------------------------------
# Determine next reviewer (round-robin)
# --------------------------------------------
if last_assigned in eligible_reviewers:
    idx = eligible_reviewers.index(last_assigned)
    next_idx = (idx + 1) % len(eligible_reviewers)
else:
    next_idx = 0
reviewer = eligible_reviewers[next_idx]

print(f"Assigning reviewer: {reviewer}")
# --------------------------------------------
# Remove existing reviewer if triggered
#   by label
# --------------------------------------------
if not add_bot:
    issue_url = f"https://api.github.com/repos/{REPO_FULL}/issues/{ISSUE_NUMBER}"
    r_issue = requests.get(issue_url, headers=REPO_HEADERS)
    r_issue.raise_for_status()
    current_assignees = [u["login"] for u in r_issue.json().get("assignees", [])]

    if current_assignees:
        print(f"[INFO] Removing existing assignees: {current_assignees}")
        r_remove = requests.delete(
            f"{issue_url}/assignees",
            headers=REPO_HEADERS,
            json={"assignees": current_assignees}
        )
        if r_remove.status_code not in (200, 204):
            print(f"[WARN] Failed to remove existing assignees (status {r_remove.status_code})")
        else:
            print("[INFO] Existing assignees cleared")    
# --------------------------------------------
# Assign reviewer
# --------------------------------------------
assign_url = f"https://api.github.com/repos/{REPO_FULL}/issues/{ISSUE_NUMBER}/assignees"
r2 = requests.post(assign_url, headers=REPO_HEADERS, json={"assignees": [reviewer]})
r2.raise_for_status()
print(f"Reviewer {reviewer} assigned successfully.")

# --------------------------------------------
# Update last_assignee.txt and commit
# --------------------------------------------
os.makedirs(os.path.dirname(REVIEWER_STATE_FILE), exist_ok=True)
with open(REVIEWER_STATE_FILE, "w") as f:
    f.write(reviewer + "\n")

subprocess.run(['git', 'config', 'user.name', "github-actions[bot]"], check=True)
subprocess.run(['git', 'config', 'user.email', "github-actions[bot]@users.noreply.github.com"], check=True)
subprocess.run(['git', 'add', REVIEWER_STATE_FILE], check=True)
subprocess.run(['git', 'commit', '-m', f"Update last assigned reviewer for {TEAM}: {reviewer}"], check=False)
subprocess.run(['git', 'push', 'origin', 'submissions'], check=True)

if current_branch:
    subprocess.run(["git", "checkout", current_branch], check=True)
# --------------------------------------------
# Remove label and post comment
# --------------------------------------------
remove_label(ISSUE_NUMBER, "assign reviewer")
remove_label(ISSUE_NUMBER, "pre-review")
add_label(ISSUE_NUMBER, "review in progress")

comment_url = f"https://api.github.com/repos/{REPO_FULL}/issues/{ISSUE_NUMBER}/comments"
comment_body = {"body": f"👤 Reviewer @{reviewer} has been assigned."}

r3 = requests.post(comment_url, headers=REPO_HEADERS, json=comment_body)
try:
    r3.raise_for_status()
    print(f"Comment posted: Reviewer {reviewer} assigned.")
except requests.RequestException as e:
    print(f"[ERROR] Failed to post assignment comment: {e}")
