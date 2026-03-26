import os
import csv
import requests
from datetime import datetime, timedelta
import subprocess
import re

# ----------------------------
# Environment
# ----------------------------
GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]
SUBMISSIONS_FILE = os.environ.get("SUBMISSIONS_PATH", "submissions/submitted_packages.csv")
RUNIVERSE_WORKFLOW = os.environ["RUNIVERSE_WORKFLOW"]
PACKAGE_NAME = os.environ.get("PACKAGE_NAME")
ISSUE_NUMBER = os.environ.get("ISSUE_NUMBER")

HEADERS = {
    "Authorization": f"Bearer {GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json"
}

# ----------------------------
# 10-minute cutoff
# ----------------------------
#cutoff_dt = datetime.utcnow() - timedelta(minutes=10)
# temporarily relax while debugging
cutoff_dt = datetime.utcnow() - timedelta(hours=4)
print(f"[DEBUG] Cutoff datetime: {cutoff_dt}")

# ----------------------------
# Helper Functions
# ----------------------------
def matches_package(text, pkg):
    text_lower = text.lower()
    pkg_lower = pkg.lower()

    pattern = r'(?<![a-z0-9])' + re.escape(pkg_lower) + r'(?![a-z0-9])'
    return re.search(pattern, text_lower) is not None

def parse_version(ver):
    try:
        x, y, z = [int(p) for p in ver.split(".")]
        return x, y, z
    except Exception:
        return None, None, None

def valid_z_bump(old, new):
    if not old:
        return True  # first run
    old_x, old_y, old_z = parse_version(old)
    new_x, new_y, new_z = parse_version(new)
    if old_x is None or new_x is None:
        return False
    return old_x == new_x and old_y == new_y and new_z > old_z

def run_git_command(args):
    result = subprocess.run(args, capture_output=True, text=True)
    return result.returncode == 0


def get_current_branch():
    result = subprocess.run(
        ["git", "rev-parse", "--abbrev-ref", "HEAD"],
        capture_output=True,
        text=True
    )
    if result.returncode == 0:
        return result.stdout.strip()
    return None

# ----------------------------
# Get version from DESCRIPTION
# ----------------------------
def get_version_from_description(owner, repo, sha):
    url = f"https://raw.githubusercontent.com/{owner}/{repo}/{sha}/DESCRIPTION"
    try:
        resp = requests.get(url, timeout=10)
        resp.raise_for_status()
    except requests.RequestException as e:
        print(f"[WARN] Could not fetch DESCRIPTION for {owner}/{repo}@{sha}: {e}")
        return None

    for line in resp.text.splitlines():
        if line.startswith("Version:"):
            return line.split("Version:")[1].strip()
    return None

# ----------------------------
# Fetch latest workflow runs (all packages)
# ----------------------------
def get_recent_workflow_runs():
    parts = RUNIVERSE_WORKFLOW.split("/")
    owner = parts[3]
    repo = parts[4]
    workflow_file = parts[-1]  

    url = f"https://api.github.com/repos/{owner}/{repo}/actions/workflows/{workflow_file}/runs"

    params = {
        "event": "push",
        "status": "completed",
        "per_page": 100
    }

    print(f"[DEBUG] Fetching workflow runs: {url}")

    try:
        resp = requests.get(url, headers=HEADERS, params=params, timeout=10)
        resp.raise_for_status()
    except requests.RequestException as e:
        print(f"[WARN] Failed to fetch workflow runs: {e}")
        return []

    data = resp.json()
    all_runs = data.get("workflow_runs", [])

    print(f"[DEBUG] Total runs returned: {len(all_runs)}")

    if not all_runs:
        return []

    print(f"[DEBUG] Most recent run created_at: {all_runs[0]['created_at']}")

    recent_runs = []

    for run in all_runs:
        run_time = datetime.strptime(run["created_at"], "%Y-%m-%dT%H:%M:%SZ")

        print(f"[DEBUG] Comparing run_time={run_time} vs cutoff_dt={cutoff_dt}")

        if run_time >= cutoff_dt:
            print(f"[DEBUG] KEEP: {run.get('name')} ({run['created_at']})")
            recent_runs.append(run)
        else:
            print(f"[DEBUG] SKIP (too old): {run.get('name')} ({run['created_at']})")

    print(f"[DEBUG] Runs after cutoff filter: {len(recent_runs)}")

    return recent_runs

# ----------------------------
# Load CSV submissions
# ----------------------------
csv_rows = {}
current_branch = get_current_branch()
run_git_command(["git", "fetch", "origin", "submissions"])
run_git_command(["git", "checkout", "-B", "submissions", "origin/submissions"])

if os.path.exists(SUBMISSIONS_FILE):
    with open(SUBMISSIONS_FILE, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            row.setdefault("last_sha", "")
            row.setdefault("last_version", "")
            csv_rows[row["package_name"]] = row

print(f"[DEBUG] SUBMISSIONS_FILE path: {SUBMISSIONS_FILE}")
print(f"[DEBUG] CSV rows loaded: {len(csv_rows)}")
print(f"[DEBUG] CSV package names: {list(csv_rows.keys())}")

if current_branch:
    run_git_command(["git", "checkout", current_branch])
    
# ----------------------------
# Map package -> latest run
# ----------------------------
recent_runs = get_recent_workflow_runs()

latest_run_per_package = {}
remaining_pkgs = set(csv_rows.keys())

for run in recent_runs:
    run_name = run.get("name", "") or ""
    display_title = run.get("display_title", "") or ""
    text = run_name + " " + display_title
    print(f"[DEBUG] Matching run text: '{text}'")
    print(f"[DEBUG] Remaining packages: {remaining_pkgs}")

    if PACKAGE_NAME and PACKAGE_NAME.lower() not in text.lower():
        continue

    sha = run.get("head_sha")
    if not sha:
        continue

    for pkg in list(remaining_pkgs):
        if matches_package(text, pkg):
            print(f"[DEBUG] MATCH FOUND: {pkg} in '{text}'")
            latest_run_per_package[pkg] = {
                "sha": sha,
                "run_url": run["html_url"]
            }
            remaining_pkgs.remove(pkg)

    if not remaining_pkgs:
        break

# ----------------------------
# Process only packages that had a run
# ----------------------------
updated_rows = []
changes_made = False

GITHUB_REPOSITORY = os.environ["GITHUB_REPOSITORY"]
queue_owner, queue_repo = GITHUB_REPOSITORY.split("/")

for pkg, row in csv_rows.items():
    run_info = latest_run_per_package.get(pkg)

    if not run_info:
        updated_rows.append(row)
        continue

    sha = run_info["sha"]
    run_url = run_info["run_url"]
    last_sha = row.get("last_sha", "")
    last_version = row.get("last_version", "")

    try:
        owner, repo = row["repo_full"].split("/")
    except Exception:
        updated_rows.append(row)
        continue

    if sha == last_sha:
        updated_rows.append(row)
        continue

    version = get_version_from_description(owner, repo, sha)
    if not version:
        updated_rows.append(row)
        continue

    if valid_z_bump(last_version, version):

        print(f"[INFO] {pkg}: New build detected {last_version} -> {version}")
        row["last_sha"] = sha
        row["last_version"] = version
        changes_made = True

        issue_num = row.get("issue_number")
        if issue_num:
            url = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_num}/comments"
            try:
                resp = requests.post(url, headers=HEADERS, json={
                    "body": f"✅ New build detected for {pkg}, version {version}.\n"
                            f"🔗 Detailed run: {run_url}\n"
                            f"📊 Check summary table: https://tempbioc.r-universe.dev/{pkg}$checktable"
                }, timeout=10)
                resp.raise_for_status()
            except requests.RequestException as e:
                print(f"[ERROR] Failed to post success comment for {pkg}: {e}")
    else:
        print(f"[WARN] {pkg}: Version bump invalid ({last_version} -> {version})")
        issue_num = row.get("issue_number")
        if issue_num:
            url = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_num}/comments"
            try:
                resp = requests.post(url, headers=HEADERS, json={
                    "body": f"⚠️ Build detected for {pkg} with invalid version bump ({last_version} -> {version}). "
                            f"Only z should increase; please correct version.\n"
                }, timeout=10)
                resp.raise_for_status()
            except requests.RequestException as e:
                print(f"[ERROR] Failed to post warning comment for {pkg}: {e}")

    updated_rows.append(row)

# ----------------------------
# updated CSV if needed
# ----------------------------
# ----------------------------
# Commit updated CSV if needed
# ----------------------------
if changes_made:
    current_branch = get_current_branch()
    actor = os.environ.get("GITHUB_ACTOR", "github-actions[bot]")

    # --- switch to submissions branch ---
    run_git_command(["git", "fetch", "origin", "submissions"])
    run_git_command(["git", "checkout", "-B", "submissions", "origin/submissions"])

    # --- ensure directory exists ---
    os.makedirs(os.path.dirname(SUBMISSIONS_FILE), exist_ok=True)

    # --- write updated CSV ---
    with open(SUBMISSIONS_FILE, "w", newline="") as f:
        fieldnames = ["package_name","repo_full","submitter","issue_number","last_sha","last_version"]
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(updated_rows)

    # --- commit + push ---
    run_git_command(["git", "config", "user.name", actor])
    run_git_command(["git", "config", "user.email", f"{actor}@users.noreply.github.com"])
    run_git_command(["git", "add", SUBMISSIONS_FILE])

    committed = run_git_command(["git", "commit", "-m", "Update build SHAs and versions"])

    if committed:
        run_git_command(["git", "push", "origin", "submissions"])

    # --- return to original branch ---
    if current_branch:
        run_git_command(["git", "checkout", current_branch])

    print("[INFO] CSV updated and pushed to submissions branch")

else:
    print("[INFO] No updates detected")
