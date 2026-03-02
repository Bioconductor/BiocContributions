import os
import re
import sys
import json
import base64
import csv
import subprocess
import requests

# ----------------------------
# Environment & Config
# ----------------------------

GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]
EVENT_PATH = os.environ["GITHUB_EVENT_PATH"]
REPO_FULL = os.environ["GITHUB_REPOSITORY"]
SUBMISSIONS_FILE = os.environ.get("SUBMISSIONS_PATH", "submissions/submissions.csv")

HEADERS = {
    "Authorization": f"Bearer {GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json"
}

MAX_FILE_SIZE = 5 * 1024 * 1024  # 5MB


# ----------------------------
# Utility
# ----------------------------

def run_git_command(args):
    """Run git command safely and return True if success."""
    result = subprocess.run(args, capture_output=True, text=True)
    return result.returncode == 0


# ----------------------------
# GitHub API Helpers
# ----------------------------

def github_get(url):
    r = requests.get(url, headers=HEADERS)
    if r.status_code != 200:
        return None
    return r.json()


def github_post(url, payload):
    return requests.post(url, headers=HEADERS, json=payload)


def post_comment(body):
    with open(EVENT_PATH) as f:
        event = json.load(f)

    issue_number = event["issue"]["number"]
    owner, repo = REPO_FULL.split("/")

    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/comments"
    github_post(url, {"body": body})


def close_issue():
    with open(EVENT_PATH) as f:
        event = json.load(f)

    issue_number = event["issue"]["number"]
    owner, repo = REPO_FULL.split("/")

    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}"
    requests.patch(url, headers=HEADERS, json={"state": "closed"})


def add_label(label):
    with open(EVENT_PATH) as f:
        event = json.load(f)

    issue_number = event["issue"]["number"]
    owner, repo = REPO_FULL.split("/")

    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels"
    requests.post(url, headers=HEADERS, json={"labels": [label]})


# ----------------------------
# Submission Table Helpers
# ----------------------------

def load_submissions():
    submissions = []

    if not os.path.exists(SUBMISSIONS_FILE):
        return submissions

    with open(SUBMISSIONS_FILE, newline="") as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            submissions.append(row)

    return submissions


def check_duplicate(package_name):
    submissions = load_submissions()

    for row in submissions:
        if row["package_name"].strip().lower() == package_name.strip().lower():
            return (
                True,
                f"Package '{package_name}' has already been submitted by "
                f"{row['submitter']} (repo: {row['repo_full']}, issue #{row['issue_number']})."
            )

    return False, ""


def record_submission(package_name):
    with open(EVENT_PATH) as f:
        event = json.load(f)

    submitter = event["issue"]["user"]["login"]
    issue_number = event["issue"]["number"]
    repo_full = f"{event['repository']['owner']['login']}/{event['repository']['name']}"

    os.makedirs(os.path.dirname(SUBMISSIONS_FILE), exist_ok=True)
    file_exists = os.path.exists(SUBMISSIONS_FILE)

    with open(SUBMISSIONS_FILE, "a", newline="") as csvfile:
        fieldnames = ["package_name", "repo_full", "submitter", "issue_number"]
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        if not file_exists:
            writer.writeheader()

        writer.writerow({
            "package_name": package_name,
            "repo_full": repo_full,
            "submitter": submitter,
            "issue_number": issue_number
        })

    actor = os.environ.get("GITHUB_ACTOR", "github-actions[bot]")

    run_git_command(["git", "-C", "submissions", "config", "user.name", actor])
    run_git_command(["git", "-C", "submissions", "config", "user.email",
                     f"{actor}@users.noreply.github.com"])

    run_git_command(["git", "-C", "submissions", "add", "submissions.csv"])

    commit_message = (
        f"Record submission of {package_name} by {submitter} "
        f"(issue #{issue_number})"
    )

    committed = run_git_command([
        "git", "-C", "submissions",
        "commit", "-m", commit_message
    ])

    if committed:
        run_git_command(["git", "-C", "submissions", "push", "origin", "submissions"])


# ----------------------------
# Validation Logic
# ----------------------------

def main():
    failures = []

    with open(EVENT_PATH) as f:
        event = json.load(f)

    issue_body = event["issue"]["body"] or ""

    match = re.search(r"https://github\.com/([\w\-]+)/([\w\.\-]+)", issue_body)
    if not match:
        failures.append("No valid GitHub repository URL found in issue body.")
        finalize(failures)
        return

    owner, repo = match.group(1), match.group(2)

    repo_data = github_get(f"https://api.github.com/repos/{owner}/{repo}")
    if not repo_data:
        failures.append("Repository does not exist or is not accessible.")
        finalize(failures)
        return

    if repo_data.get("private"):
        failures.append("Repository must be public.")

    default_branch = repo_data.get("default_branch")

    description_file = github_get(
        f"https://api.github.com/repos/{owner}/{repo}/contents/DESCRIPTION"
    )

    if not description_file:
        failures.append("DESCRIPTION file not found at repository root.")

    vignette_dir = github_get(
        f"https://api.github.com/repos/{owner}/{repo}/contents/vignettes"
    )

    if not vignette_dir or vignette_dir.get("type") != "dir":
        failures.append("vignettes/ directory not found.")

    package_name = None

    if description_file:
        try:
            description_text = base64.b64decode(
                description_file["content"]
            ).decode("utf-8")

            pkg_match = re.search(r"^Package:\s*(.+)$", description_text, re.MULTILINE)
            version_match = re.search(r"^Version:\s*(.+)$", description_text, re.MULTILINE)

            if not pkg_match:
                failures.append("DESCRIPTION missing 'Package:' field.")

            if not version_match:
                failures.append("DESCRIPTION missing 'Version:' field.")

            if pkg_match:
                package_name = pkg_match.group(1).strip()
                if package_name != repo:
                    failures.append(
                        f"Package name '{package_name}' does not match repository name '{repo}'."
                    )

            if version_match:
                version = version_match.group(1).strip()
                parts = version.split(".")
                if len(parts) != 3 or parts[1] != "99" or not all(p.isdigit() for p in parts):
                    failures.append("Version must be in format x.99.z")

            remotes_match = re.search(r"^Remotes:\s*(.+)$", description_text, re.MULTILINE)
            if remotes_match:
                failures.append("DESCRIPTION contains a 'Remotes:' field. All dependencies must be on CRAN or Bioconductor; Remotes are not allowed.")

        except Exception:
            failures.append("Unable to decode or parse DESCRIPTION file.")

    if package_name:
        is_dup, dup_msg = check_duplicate(package_name)
        if is_dup:
            failures.append(dup_msg)
            finalize(failures)
            return

    if default_branch:
        tree = github_get(
            f"https://api.github.com/repos/{owner}/{repo}/git/trees/{default_branch}?recursive=1"
        )

        if tree and "tree" in tree:
            for item in tree["tree"]:
                if item.get("type") == "blob" and item.get("size", 0) > MAX_FILE_SIZE:
                    failures.append(f"File '{item['path']}' exceeds 5MB limit.")
        else:
            failures.append("Unable to retrieve repository file tree.")

    finalize(failures, package_name)


# ----------------------------
# Finalization
# ----------------------------

def finalize(failures, package_name=None):
    if failures:
        message = "## ❌ Bioconductor Precheck Failed\n\n"
        message += "The following pre-checks did not pass:\n\n"
        for f in failures:
            message += f"- {f}\n"
        message += "\nPlease address these issues and open a new submission issue."

        post_comment(message)
        close_issue()
        sys.exit(1)

    else:
        if package_name:
            record_submission(package_name)

        message = "## ✅ Bioconductor Precheck Passed\n\n"
        message += (
            "All automated structural checks passed.\n\n"
            "**Next Step:** Please accept the Bioconductor submission policies.\n\n"
            "Comment exactly:\n\n"
            "`/accept-policies`"
        )

        post_comment(message)
        add_label("precheck-passed")
        add_label("awaiting policy acceptance")
        sys.exit(0)


if __name__ == "__main__":
    main()
