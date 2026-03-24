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
SUBMISSIONS_FILE = os.environ.get("SUBMISSIONS_PATH", "submissions/submitted_packages.csv")

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

    
def has_label(label_name):
    with open(EVENT_PATH) as f:
        event = json.load(f)
    labels = event["issue"].get("labels", [])
    return any(label["name"] == label_name for label in labels)


# ----------------------------
# Submission Table Helpers
# ----------------------------

def get_current_branch():
    """Return the current git branch name, or None if it cannot be determined."""
    result = subprocess.run(["git", "rev-parse", "--abbrev-ref", "HEAD"], capture_output=True, text=True)
    if result.returncode == 0:
        return result.stdout.strip()
    return None


def load_submissions():
    """Load submissions from the CSV on the submissions branch."""
    submissions = []

    submissions_csv = os.environ.get("SUBMISSIONS_PATH", "submissions/submitted_packages.csv")
    current_branch = get_current_branch()

    # Fetch & checkout submissions branch
    run_git_command(["git", "fetch", "origin", "submissions"])
    run_git_command(["git", "checkout", "-B", "submissions", "origin/submissions"])

    # Read CSV if it exists
    if os.path.exists(submissions_csv):
        with open(submissions_csv, newline="") as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                row.setdefault("last_sha", "")
                row.setdefault("last_version", "")
                submissions.append(row)

    # Return to original branch
    if current_branch:
        run_git_command(["git", "checkout", current_branch])

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
    """
    Append a new submission to the submissions branch CSV.
    Ensures the branch exists, appends safely, commits, pushes, and returns to the original branch.
    """

    with open(EVENT_PATH) as f:
        event = json.load(f)

    submitter = event["issue"]["user"]["login"]
    issue_number = event["issue"]["number"]
    repo_full = f"{event['repository']['owner']['login']}/{event['repository']['name']}"

    submissions_csv = os.environ.get("SUBMISSIONS_PATH", "submissions/submitted_packages.csv")
    actor = os.environ.get("GITHUB_ACTOR", "github-actions[bot]")
    current_branch = get_current_branch()

    # --- 1. Fetch and checkout submissions branch ---
    run_git_command(["git", "fetch", "origin", "submissions"])
    run_git_command(["git", "checkout", "-B", "submissions", "origin/submissions"])

    # --- 2. Ensure directory exists ---
    os.makedirs(os.path.dirname(submissions_csv), exist_ok=True)

    # --- 3. Append row to CSV ---
    file_exists = os.path.exists(submissions_csv)
    with open(submissions_csv, "a", newline="") as csvfile:
        fieldnames = ["package_name", "repo_full", "submitter", "issue_number", "last_sha", "last_version"]
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

        if not file_exists:
            writer.writeheader()

        writer.writerow({
            "package_name": package_name,
            "repo_full": repo_full,
            "submitter": submitter,
            "issue_number": issue_number,
            "last_sha": "",
            "last_version": ""
        })

    # --- 4. Configure Git user ---
    run_git_command(["git", "config", "user.name", actor])
    run_git_command(["git", "config", "user.email", f"{actor}@users.noreply.github.com"])

    # --- 5. Commit and push ---
    run_git_command(["git", "add", submissions_csv])
    commit_message = f"Record submission of {package_name} by {submitter} (issue #{issue_number})"
    committed = run_git_command(["git", "commit", "-m", commit_message])

    if committed:
        run_git_command(["git", "push", "origin", "submissions"])

    # --- 6. Return to original branch ---
    if current_branch:
        run_git_command(["git", "checkout", current_branch])

        
# ----------------------------
# Large File Check Helper/Overrider
# ----------------------------

def is_team_member(org, team_slug, username):
    """
    Temporary override for local development.
    Always returns True so team restrictions are bypassed.
    url = f"https://api.github.com/orgs/{org}/teams/{team_slug}/memberships/{username}"
    r = requests.get(url, headers=HEADERS)
    return r.status_code == 200
    """
    return True


def allow_large_file_override():
    """
    Returns True if the issue has the allow-large-files label
    """
    with open(EVENT_PATH) as f:
        event = json.load(f)

    labels = event["issue"].get("labels", [])
    # For local dev, is_team_member always returns True
    return any(label["name"] == "allow-large-files" for label in labels)


# ----------------------------
# Git LFS Check
# ----------------------------

def check_git_lfs(owner, repo):
    """
    Checks if the repository uses Git LFS by inspecting the .gitattributes file.
    Returns a list of failure messages if Git LFS usage is detected or if the file cannot be read.
    """
    failures = []

    # Fetch .gitattributes from repo
    gitattributes = github_get(f"https://api.github.com/repos/{owner}/{repo}/contents/.gitattributes")

    if gitattributes and gitattributes.get("content"):
        try:
            # Decode base64 content
            attr_text = base64.b64decode(gitattributes["content"]).decode("utf-8")

            if "filter=lfs" in attr_text:
                failures.append(
                    "Git LFS usage detected in `.gitattributes`. "
                    "Bioconductor does NOT allow Git LFS. "
                    "Please remove LFS tracking and rewrite repository history."
                )
        except Exception:
            failures.append("Unable to parse `.gitattributes` file.")

    return failures


# ----------------------------
# Validation
# ----------------------------

def main():
    failures = []

    with open(EVENT_PATH) as f:
        event = json.load(f)

    action = event.get("action")
    issue_body = event["issue"]["body"] or ""

    org = "Bioconductor"
    team_slug = "packagereviewers"

    actor = event["sender"]["login"]

    # ----------------------------
    # Reopen restriction
    # ----------------------------
    if action == "reopened":
        if not is_team_member(org, team_slug, actor):
            close_issue()
            post_comment(
                "⚠️ Only members of the PackageReview team can reopen this issue. Issue closed again."
            )
            sys.exit(1)

    # ----------------------------
    # Extract GitHub URL
    # ----------------------------
    match = re.search(r"(?:https://github\.com/|git@github\.com:)([\w\-]+)/([\w\.\-]+)", issue_body)
    if not match:
        failures.append("No valid GitHub repository URL found in issue body.")
        finalize(failures)
        return

    owner, repo = match.group(1), match.group(2)

    if repo.endswith(".git"):
        repo = repo[:-4]

    # ----------------------------
    # Check URL accessible
    # ----------------------------
    repo_data = github_get(f"https://api.github.com/repos/{owner}/{repo}")
    if not repo_data:
        failures.append("Repository does not exist or is not accessible.")
        finalize(failures)
        return

    if repo_data.get("private"):
        failures.append("Repository must be public.")

    default_branch = repo_data.get("default_branch")

    # ----------------------------
    # Check DESCRIPTION and vignettes exists
    # ----------------------------
    description_file = github_get(
        f"https://api.github.com/repos/{owner}/{repo}/contents/DESCRIPTION"
    )

    if not description_file:
        failures.append("DESCRIPTION file not found at repository root.")

    vignette_dir = github_get(
        f"https://api.github.com/repos/{owner}/{repo}/contents/vignettes"
    )

    if not vignette_dir or not isinstance(vignette_dir, list):
        failures.append("vignettes/ directory not found.")

    package_name = None

    # ----------------------------
    # DESCRIPTION file checks
    # ----------------------------
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

    # ----------------------------
    # Large File Check
    # ----------------------------
    if default_branch:
        tree = github_get(
            f"https://api.github.com/repos/{owner}/{repo}/git/trees/{default_branch}?recursive=1"
        )

        allow_large_files = allow_large_file_override()

        if tree and "tree" in tree:
            for item in tree["tree"]:
                if item.get("type") == "blob" and item.get("size", 0) > MAX_FILE_SIZE:
                    if not allow_large_files:
                        failures.append(f"File '{item['path']}' exceeds 5MB limit.")
                    else:
                        print(f"[INFO] Large file exception applied: {item['path']}")
        else:
            failures.append("Unable to retrieve repository file tree.")

    # ----------------------------
    # Git LFS check
    # ----------------------------
    failures.extend(check_git_lfs(owner, repo))

    # ----------------------------
    # Duplicate Submission Check
    # ----------------------------
    is_reopened = action == "reopened"
    precheck_passed = has_label("precheck-passed")
    skip_duplicates = is_reopened and precheck_passed
    if package_name and not skip_duplicates:
        is_dup, dup_msg = check_duplicate(package_name)
        if is_dup:
            add_label("duplicate")
            failures.append(dup_msg)
            finalize(failures, package_name, skip_duplicates)
            return

    # ----------------------------
    # Finalization
    # ----------------------------
    finalize(failures, package_name, skip_duplicates)


# ----------------------------
# Finalization
# ----------------------------

def finalize(failures, package_name=None, skip_duplicates=False):
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
        if package_name and not skip_duplicates:
            record_submission(package_name)

        message = "## ✅ Bioconductor Precheck Passed\n\n"
        message += (
           "**Next Step:** Please accept the Bioconductor submission policies:\n\n"
        )
        points = [
            "I understand that by submitting my package to _Bioconductor_, "
            "the package source and all review commentary are visible to the general public.",

            "I have read the _Bioconductor_ [Package Submission](https://contributions.bioconductor.org/submission-overview.html) instructions. "
            "My package is consistent with the _Bioconductor_ [Package Guidelines](https://contributions.bioconductor.org/develop-overview.html).",

            "I understand Bioconductor [Package Naming Policy](https://contributions.bioconductor.org/bioconductor-package-submissions.html#naming) "
            "and acknowledge Bioconductor may retain use of package name.",

            "I understand that a minimum requirement for package acceptance "
            "is to pass R CMD check and R CMD BiocCheck with no ERROR or WARNINGS. "
            "Passing these checks does not result in automatic acceptance. "
            "The package will then undergo a formal review and recommendations for "
            "acceptance regarding other Bioconductor standards will be addressed.",

            "I am committed to the long-term maintenance of my package. "
            "This includes monitoring the [support site](https://support.bioconductor.org) for issues that users may have, "
            "subscribing to the [bioc-devel](https://stat.ethz.ch/mailman/listinfo/bioc-devel) mailing list to stay aware of developments in the _Bioconductor_ community, "
            "responding promptly to requests for updates from the Core team in response to changes in _R_ or underlying software.",

            "I understand it is my responsibility to maintain a valid, active "
            "maintainer email in the DESCRIPTION of my package. "
            "This email should allow emails from noreply@bioconductor.org and BBS-noreply@bioconductor.org "
            "to allow automatic notifications from the Bioconductor team concerning my package.",

            "I am familiar with the [Bioconductor code of conduct](https://bioconductor.org/about/code-of-conduct/) and agree to abide by it.",

            "I am familiar with the essential aspects of _Bioconductor_ software management, including:\n"
            "    - The 'devel' branch for new packages and features.\n"
            "    - The stable 'release' branch, made available every six months, for bug fixes."
        ]

        for p in points:
            message += f"- {p}\n\n"

        message += (
            "For questions/help about the submission process, including questions about "
            "the output of the automatic reports generated by the SPB (Single Package Builder), "
            "please use the bioc-devel mailing list or #package-submission channel of our [Community Zulip](https://community-bioc.zulipchat.com/join/4k2tpsy7h6zjbaduydwm2n56/).\n\n"
            "To Accept Policies Comment Exactly (all lowercase!):\n\n"
            "`/accept-policies`"
        )
        
        if not has_label("precheck-passed"):
            add_label("precheck-passed")
        if not has_label("policies-accepted"):
            post_comment(message)
            add_label("awaiting policy acceptance")
        sys.exit(0)


if __name__ == "__main__":
    main()
