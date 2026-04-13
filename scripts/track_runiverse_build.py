import os
import csv
import requests
from datetime import datetime, timedelta
import subprocess
import re
import sys
import yaml

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
ORG_NAME = os.environ.get("ORG_NAME", "Bioconductor")
TEAM = os.environ["TEAM_SLUG"]
REPO_FULL = os.environ["GITHUB_REPOSITORY"]
REVIEWER_STATE_FILE = os.environ["REVIEWER_STATE_PATH"]

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
    "NOTE": "Build Note",
    "WARNING": "Build Warning",
    "ERROR": "Build Error",
    "UNKNOWN": "Build Unknown"
}

def get_queue_owner_repo():
    repo = os.environ.get("GITHUB_REPOSITORY", "")
    if "/" in repo:
        return repo.split("/")
    return None, None

def update_labels(issue_number, status_list, issue_data, headers=None):

    if headers is None:
        headers = HEADERS

    queue_owner, queue_repo = get_queue_owner_repo()
    if not queue_owner or not queue_repo:
        print("[WARN] Cannot determine queue_owner/queue_repo from environment")
        return

    # Map status_list to canonical labels
    desired_labels = [STATUS_LABELS[s] for s in status_list if s in STATUS_LABELS]

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

            
def add_label(issue_number, label, headers=None):
    if headers is None:
        headers = HEADERS
    queue_owner, queue_repo = get_queue_owner_repo()
    if not queue_owner or not queue_repo:
        print("[WARN] Cannot determine queue_owner/queue_repo from environment")
        return
    url = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_number}/labels"
    try:
        resp = requests.post(url, headers=headers, json={"labels": [label]}, timeout=10)
        resp.raise_for_status()
        print(f"[INFO] Label '{label}' added to issue #{issue_number}")
    except requests.RequestException as e:
        print(f"[ERROR] Failed to add label '{label}' to issue #{issue_number}: {e}")


def remove_label(issue_number, label, headers=None):
    if headers is None:
        headers = HEADERS

    queue_owner, queue_repo = get_queue_owner_repo()
    if not queue_owner or not queue_repo:
        print("[WARN] Cannot determine queue_owner/queue_repo from environment")
        return

    url = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_number}/labels/{label}"
    try:
        resp = requests.delete(url, headers=headers, timeout=10)
        if resp.status_code in (200, 204, 404):
            print(f"[INFO] Label '{label}' removed from issue #{issue_number}")
        else:
            print(f"[WARN] Could not remove label '{label}' (status {resp.status_code})")
    except requests.RequestException as e:
        print(f"[ERROR] Failed to remove label '{label}' from issue #{issue_number}: {e}")


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

# ----------------------------
# Assign Reviewer
# ----------------------------
def assign_reviewer(issue_number):
    env = os.environ.copy()
    env["ISSUE_NUMBER"] = str(issue_number)
    env["REVIEWER_STATE_PATH"] = REVIEWER_STATE_FILE
    try:
        result = subprocess.run([sys.executable, "scripts/assign_reviewer.py"],check=False,env=env,capture_output=True,text=True)
        print(f"[INFO] assign_reviewer.py completed for issue #{issue_number}")
    except subprocess.CalledProcessError as e:
        print(f"[ERROR] assign_reviewer.py failed for issue #{issue_number}: {e}")

    print("----- Child stdout -----")
    print(result.stdout)
    print("----- Child stderr -----")
    print(result.stderr)


# ------------------------------
# Parse R-Universe Package API
#   For Build Results
# ------------------------------
def parse_runiverse_build(pkg):
    # -----------------------------
    # Platform policy
    # -----------------------------
    platforms_ok = ["source"]
    platforms_warnings = ["bioccheck", "linux", "macos", "windows"]
    ALWAYS_KEEP = ["source", "bioc-check"]
    url = f"https://tempbioc.r-universe.dev/api/packages/{pkg}"

    build_clean = True

    bioc_r_version = None
    try:
        cfg_url = "https://bioconductor.org/config.yaml"
        cfg_resp = requests.get(cfg_url, timeout=30)
        cfg_resp.raise_for_status()

        config = yaml.safe_load(cfg_resp.text)
        single_pkg = config.get("single_package_builder", {})
        bioc_r_version = single_pkg.get("r_version")

        print(f"[DEBUG] Bioconductor R version: {bioc_r_version}")

    except Exception as e:
        print(f"[WARN] Could not fetch Bioconductor config, skipping R filtering: {e}")

    try:
        resp = requests.get(url, headers=TEMP_BIOC_HEADERS, timeout=10)

        if resp.status_code == 404:
            return {
                "status": ["ERROR"],
                "message": f"❌ Package `{pkg}` not available in R-universe (likely build failure)",
                "_build_clean": False
            }

        resp.raise_for_status()
        data = resp.json()

    except requests.RequestException as e:
        return {
            "status": ["UNKNOWN"],
            "message": f"⚠️ Could not fetch R-universe data for `{pkg}`",
            "_build_clean": False
        }

    build_url = data.get("_buildurl")

    # -----------------------------
    # HARD FAILURE CASE 
    # -----------------------------
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
            ),
            "_build_clean": False
        }

    jobs = data.get("_jobs", [])


    # -----------------------------
    # HELPER FUNCTIONS
    # -----------------------------
    def matches_r_version(job_r):
        if not bioc_r_version:
            return True
        if not job_r:
            return False
        return str(job_r).startswith(str(bioc_r_version))

    def keep_platform(platform, job_r):
        p = (platform or "").lower()
        # ALWAYS KEEP CORE
        if any(k in p for k in ALWAYS_KEEP):
            return True
        # R filtering applies only if config exists
        if bioc_r_version:
            return matches_r_version(job_r)
        return True

    def match_platform(platform, key):
        return key in (platform or "").lower()

    # -----------------------------
    # FILTER JOBS
    # -----------------------------
    filtered = []

    for job in jobs:
        if not isinstance(job, dict):
            continue

        platform = str(job.get("config"))
        job_r = job.get("r")

        if keep_platform(platform, job_r):
            filtered.append(job)

    if not filtered:
        return {
            "status": ["UNKNOWN"],
            "message": f"⚠️ No filtered check results available for `{pkg}`",
            "_build_clean": False
        }

    rows = []
    unique_statuses = set()

    for job in filtered:
        platform = str(job.get("config"))
        rver = job.get("r")
        status_str = str(job.get("check", "UNKNOWN")).upper()

        if status_str == "OK":
            status = "✅ OK"
        elif status_str == "NOTE":
            status = "ℹ️ NOTE"
        elif status_str == "WARNING":
            status = "⚠️ WARNING"
        elif status_str == "ERROR":
            status = "❌ ERROR"
        else:
            status = "❓ UNKNOWN"

        plat_lower = platform.lower()

        if any(match_platform(plat_lower, p) for p in platforms_ok):
            # strict
            if status_str not in ["OK", "NOTE"]:
                build_clean = False
                print(f"[FAIL] {pkg} {platform} expected OK/NOTE got {status_str}")
        elif any(match_platform(plat_lower, p) for p in platforms_warnings):
            # lenient
            if status_str not in ["OK", "NOTE", "WARNING"]:
                build_clean = False
                print(f"[FAIL] {pkg} {platform} expected OK/NOTE/WARNING got {status_str}")
        else:
            if status_str != "OK":
                build_clean = False
                print(f"[FAIL] {pkg} {platform} unexpected platform rule got {status_str}")

        if status == "❌ ERROR":
            unique_statuses.add("ERROR")
        elif status == "⚠️ WARNING":
            unique_statuses.add("WARNING")
        elif status == "ℹ️ NOTE":
            unique_statuses.add("NOTE")
        elif status == "❓ UNKNOWN":
            unique_statuses.add("UNKNOWN")
        else:
            unique_statuses.add("OK")

        job_url = f"{build_url}/job/{job.get('job') or job.get('artifact')}" if build_url else None
        link = f"[run]({job_url})" if job_url else ""

        rows.append({
            "platform": platform,
            "r": rver,
            "status": status,
            "job_id": job.get("job") or job.get("artifact"),
            "link": link
        })

    # -----------------------------
    # BUILD RESULT FILTERED TABLE 
    # -----------------------------
    header = "| Platform | R | Status | URL |\n|----------|---|--------|------|\n"

    def platform_priority(p):
        p = (p or "").lower()
        if "source" in p:
             return 0
        if "bioc-check" in p or "bioccheck" in p:
            return 1
        return 2

    lines = []
    for r in sorted(rows, key=lambda x: (platform_priority(x["platform"]), x["platform"], str(x["r"]))):
        lines.append(
            f"| {r['platform']} | {r['r']} | {r['status']} | {r['link']} |"
        )

    table = header + "\n".join(lines)

    return {
        "status": sorted(unique_statuses),
        "message": f"📊 R-universe check results for `{pkg}`\n\n{table}",
        "_build_clean": build_clean
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
    temp_repo_url = f"https://github.com/tempbioc/{pkg}"

    version = get_version_from_description(pkg)
    if not version:
        updated_rows.append(row)
        continue

    ru = parse_runiverse_build(pkg)
    issue_num = row.get("issue_number")
    issue_data = None
    if issue_num:
        url = f"https://api.github.com/repos/{queue_owner}/{queue_repo}/issues/{issue_num}"
        try:
            resp = requests.get(url, headers=HEADERS, timeout=10)
            resp.raise_for_status()
            issue_data = resp.json()
        except requests.RequestException as e:
            print(f"[ERROR] Failed to fetch issue #{issue_num}: {e}")

    if issue_data:
        update_labels(issue_num, ru['status'], issue_data, headers=HEADERS)

    clean_build = ru['_build_clean']
    print(f"[DEBUG] Processing package: {pkg}")
    print(f"[DEBUG] issue_num: {issue_num}")
    print(f"[DEBUG] ru['_build_clean']: {ru['_build_clean']}")
    if issue_data:
        assignees = issue_data.get("assignees", [])
        if not assignees:
            if clean_build and issue_num:
                try:
                    assign_reviewer(issue_num)
                except Exception as e:
                    print(f"[ERROR] Failed to assign reviewer: {e}")
            else:
                print("[INFO] No Assignee but Not Clean Build")
        else:
            print("[INFO] Already assigned:", [u["login"] for u in assignees])
            current_labels = [lbl["name"] for lbl in issue_data.get("labels",[])]
            if "review in progress" not in current_labels:
                add_label(issue_num, "review in progress", headers=HEADERS)
            if "pre-review" in current_labels:
                remove_label(issue_num, "pre-review", headers=HEADERS)
    else:
        print("[INFO] No issue data")
        
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
                            f"⚙️ Detailed run: {run_url}\n"
                            f"📦 Bioconductor staging repository: {temp_repo_url}\n"
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
                                f"📦 Bioconductor staging repository: {temp_repo_url}\n"
                                f"⚙️ Detailed run: {run_url}"
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
                            f"⚙️ Detailed run: {run_url}\n"
                            f"📦 Bioconductor staging repository: {temp_repo_url}\n"
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
                                f"📦 Bioconductor staging repository: {temp_repo_url}\n"
                                f"Reports not posted but can be accessed directly at https://tempbioc.r-universe.dev/builds"
                       }, timeout=10)
                    resp.raise_for_status()
                except requests.RequestException as e:
                    print(f"[ERROR] Failed to post warning comment for {pkg}: {e}")

    updated_rows.append(row)
   
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
