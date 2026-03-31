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
BIOC_ORG_TOKEN = os.environ.get("BIOC_ORG_TOKEN")
TEMP_BIOC_TOKEN = os.environ.get("TEMP_BIOC_TOKEN")

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
# GitHub Label Helpers
# ----------------------------

# Mapping of Status to Valid Label 
STATUS_LABELS = {
    "OK": "Build OK",
    "WARNING": "Build Warning",
    "ERROR": "Build Error",
    "UNKNOWN": "Build Unknown"
}

def get_queue_owner_repo():
    repo = os.environ.get("GITHUB_REPOSITORY", "")
    if "/" in repo:
        return repo.split("/")
    return None, None

def update_labels(issue_number, status_list, headers=None):

    if headers is None:
        headers = HEADERS

    queue_owner, queue_repo = get_queue_owner_repo()
    if not queue_owner or not queue_repo:
        print("[WARN] Cannot determine queue_owner/queue_repo from environment")
        return

    # Map status_list to canonical labels
    desired_labels = [STATUS_LABELS[s] for s in status_list if s in STATUS_LABELS]

    url = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_number}"
    try:
        resp = requests.get(url, headers=headers, timeout=10)
        resp.raise_for_status()
    except requests.RequestException as e:
        print(f"[ERROR] Failed to fetch issue #{issue_number}: {e}")
        return

    issue_data = resp.json()
    current_labels = [lbl["name"] for lbl in issue_data.get("labels", [])]

    to_add = [lbl for lbl in desired_labels if lbl not in current_labels]
    to_remove = [lbl for lbl in current_labels if lbl in STATUS_LABELS.values() and lbl not in desired_labels]

    if to_add:
        url_add = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_number}/labels"
        try:
            resp = requests.post(url_add, headers=headers, json={"labels": to_add}, timeout=10)
            resp.raise_for_status()
            print(f"[INFO] Added labels {to_add} to issue #{issue_number}")
        except requests.RequestException as e:
            print(f"[ERROR] Failed to add labels to issue #{issue_number}: {e}")

    for lbl in to_remove:
        url_remove = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_number}/labels/{lbl}"
        try:
            resp = requests.delete(url_remove, headers=headers, timeout=10)
            if resp.status_code in (200, 204):
                print(f"[INFO] Removed label {lbl} from issue #{issue_number}")
            else:
                print(f"[WARN] Could not remove label {lbl} (status {resp.status_code})")
        except requests.RequestException as e:
            print(f"[ERROR] Failed to remove label {lbl} from issue #{issue_number}: {e}")


# ----------------------------
# Get version from DESCRIPTION
# ----------------------------
def get_version_from_description(pkg, branch="devel"):
    url = f"https://raw.githubusercontent.com/tempbioc/{pkg}/{branch}/DESCRIPTION"
    try:
        resp = requests.get(url, timeout=10)
        resp.raise_for_status()
    except requests.RequestException as e:
        print(f"[WARN] Could not fetch DESCRIPTION for {pkg}@{branch}: {e}")
        return None

    for line in resp.text.splitlines():
        if line.startswith("Version:"):
            return line.split("Version:")[1].strip()
    return None


# ------------------------------
# Parse R-Universe Package API
#   For Build Results
# ------------------------------
def parse_runiverse_build(pkg):
    url = f"https://tempbioc.r-universe.dev/api/packages/{pkg}"

    try:
        resp = requests.get(url, headers=TEMP_BIOC_HEADERS, timeout=10)
        if resp.status_code == 404:
            return {
                "status": ["ERROR"],
                "message": f"❌ Package `{pkg}` not available in R-universe (likely build failure)"
            }
        resp.raise_for_status()
        data = resp.json()
    except requests.RequestException as e:
        print(f"[WARN] API fetch failed for {pkg}: {e}")
        return {
            "status": ["UNKNOWN"],
            "message": f"⚠️ Could not fetch R-universe data for `{pkg}`"
        }

    build_url = data.get("_buildurl")

    # HARD FAILURE
    failure_msg = data.get("_failure")
    if failure_msg:
        fail_build_url = failure_msg.get("buildurl") or build_url
        table = (
            "| Platform | R | Status | URL |\n"
            "|----------|---|--------|------|\n"
            f"| ❌ build | — | ❌ BUILD FAILED | "
            f"{f'[run]({fail_build_url})' if fail_build_url else ''} |"
        )
        return {
            "status": ["ERROR"],
            "message": (
                f"🚨 R-universe build failed for `{pkg}` "
                f"(no check results available)\n\n{table}"
            )
        }

    # PARSE JOBS
    jobs = data.get("_jobs", [])
    rows = []

    for job in jobs:
        if not isinstance(job, dict) or "check" not in job:
            continue

        status_str = str(job.get("check", "UNKNOWN")).upper()
        if status_str == "OK":
            status = "✅ OK"
        elif status_str == "WARNING":
            status = "⚠️ WARNING"
        elif status_str == "ERROR":
            status = "❌ ERROR"
        else:
            status = "❓ UNKNOWN"

        rows.append({
            "platform": job.get("config"),
            "r": job.get("r"),
            "status": status,
            "job_id": job.get("job") or job.get("artifact"),
        })

    # NO JOBS
    if not rows:
        table = (
            "| Platform | R | Status | URL |\n"
            "|----------|---|--------|------|\n"
            "| ❓ unknown | — | ❓ NO DATA | — |"
        )
        return {
            "status": ["UNKNOWN"],
            "message": f"⚠️ No check results available for `{pkg}`\n\n{table}"
        }

    # BUILD TABLE
    header = "| Platform | R | Status | URL |\n|----------|---|--------|------|\n"
    lines = []
    unique_statuses = set()

    for r in sorted(rows, key=lambda x: (str(x["platform"]), str(x["r"]))):
        if "❌" in r["status"]:
            unique_statuses.add("ERROR")
        elif "⚠️" in r["status"]:
            unique_statuses.add("WARNING")
        elif r["status"] == "❓ UNKNOWN":
            unique_statuses.add("UNKNOWN")
        else:
            unique_statuses.add("OK")

        job_url = f"{build_url}/job/{r['job_id']}" if build_url and r["job_id"] else None
        link = f"[run]({job_url})" if job_url else ""

        lines.append(f"| {r['platform']} | {r['r']} | {r['status']} | {link} |")

    table = header + "\n".join(lines)
    return {
        "status": sorted(unique_statuses),  
        "message": f"📊 R-universe check results for `{pkg}`\n\n{table}"
    }

# ----------------------------
# Fetch latest workflow runs
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
        resp = requests.get(url, headers=TEMP_BIOC_HEADERS, params=params, timeout=10)
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
            row.setdefault("last_valid_version", "")
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

# -------------------------------------
# Process only packages that had a run
# -------------------------------------
updated_rows = []
changes_made = False

GITHUB_REPOSITORY = os.environ["GITHUB_REPOSITORY"]
queue_owner, queue_repo = GITHUB_REPOSITORY.split("/")

for pkg, row in csv_rows.items():
    run_info = latest_run_per_package.get(pkg)

    if not run_info:
        updated_rows.append(row)
        continue

    run_url = run_info["run_url"]
    last_sha = row.get("last_sha", "")
    last_version = row.get("last_version", "")
    last_valid_version = row.get("last_valid_version", "")
    
    version = get_version_from_description(pkg)
    if not version:
        updated_rows.append(row)
        continue

    ru = parse_runiverse_build(pkg)
    issue_num = row.get("issue_number")
    if issue_num:
        update_labels(issue_number, ru['status'], headers=HEADERS)
    
    # First build: last_sha is empty
    first_build = (not last_sha)

    if first_build:
        print(f"[INFO] {pkg}: First build detected, version {version}")
        row["last_sha"] = run_info["sha"]
        changes_made = True

        issue_num = row.get("issue_number")
        if issue_num:
            url = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_num}/comments"
            try:
                requests.post(url, headers=HEADERS, json={
                    "body": f"✅ First build detected for {pkg}, version {version}.\n"
                            f"🔗 Detailed run: {run_url}\n"
                            f"🌐 R-universe package page: https://tempbioc.r-universe.dev/{pkg}#checktable\n\n"
                            f"{ru['message']}"
                }, timeout=10)
            except requests.RequestException as e:
                print(f"[ERROR] Failed to post first-build comment for {pkg}: {e}")

        updated_rows.append(row)
        continue

    # Version matches last_version
    if version == last_version:
        if last_sha != run_info["sha"]:
            issue_num = row.get("issue_number")
            if issue_num:
                url = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_num}/comments"
                try:
                    requests.post(url, headers=HEADERS, json={
                        "body": f"⚠️ A new commit was detected for {pkg}, but the package version ({version}) was not updated.\n"
                                f"Please increment the z component (x.99.z).\n"
                                f"🔗  Detailed run: {run_url}"
                    }, timeout=10)
                except requests.RequestException as e:
                    print(f"[ERROR] Failed to post no-version-bump warning for {pkg}: {e}")

            row["last_sha"] = run_info["sha"]
            changes_made = True

        updated_rows.append(row)
        continue

    # SHA matches last SHA
    if last_sha == run_info["sha"]:
        updated_rows.append(row)
        continue

    # Valid z bump
    if valid_z_bump(last_valid_version, version):
        print(f"[INFO] {pkg}: New build detected {last_valid_version} -> {version}")
        row["last_sha"] = run_info["sha"]
        row["last_version"] = version
        row["last_valid_version"] = version
        changes_made = True

        issue_num = row.get("issue_number")
        if issue_num:
            url = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_num}/comments"
            try:
                resp = requests.post(url, headers=HEADERS, json={
                    "body": f"✅ New build detected for {pkg}, version {version}.\n"
                            f"🔗 Detailed run: {run_url}\n"
                            f"🌐 R-universe package page: https://tempbioc.r-universe.dev/{pkg}#checktable\n\n"
                            f"{ru['message']}"
                }, timeout=10)
                resp.raise_for_status()
            except requests.RequestException as e:
                print(f"[ERROR] Failed to post success comment for {pkg}: {e}")
    else:
        # Invalid bump
        if version != last_version:
            print(f"[WARN] {pkg}: Version bump invalid ({last_valid_version} -> {version})")
            row["last_version"] = version
            changes_made = True
            issue_num = row.get("issue_number")
            if issue_num:
                url = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_num}/comments"
                try:
                    resp = requests.post(url, headers=HEADERS, json={
                        "body": f"⚠️ Build detected for {pkg} with invalid version bump ({last_version} -> {version}). "
                                f"Only z should increase; please correct version.\n"
                                f"Reports not posted but can be accessed directly at https://tempbioc.r-universe.dev/builds"
                       }, timeout=10)
                    resp.raise_for_status()
                except requests.RequestException as e:
                    print(f"[ERROR] Failed to post warning comment for {pkg}: {e}")

    updated_rows.append(row)
    
    #   TODO:
    #   Assign Reviewer if no ERROR and not assigned
    #

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
        fieldnames = ["package_name","repo_full","submitter","issue_number","last_sha","last_version","last_valid_version"]
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
