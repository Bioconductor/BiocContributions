import os
import json
import sys
import subprocess
import requests
import re
import shutil

# ----------------------------
# Environment / Tokens
# ----------------------------
GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]
TARGET_GITHUB_TOKEN = os.environ["TARGET_GITHUB_TOKEN"]
GITHUB_EVENT_PATH = os.environ.get("GITHUB_EVENT_PATH")
GITHUB_REPOSITORY = os.environ["GITHUB_REPOSITORY"]
GIT_TARGET_ORG = os.environ["GIT_TARGET_ORG"]
ISSUE_NUMBER = os.environ.get("ISSUE_NUMBER")  # mandatory for manual trigger

OWNER, REPO = GITHUB_REPOSITORY.split("/")

HEADERS = {
    "Authorization": f"Bearer {GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json"
}

TARGET_HEADERS = {
    "Authorization": f"Bearer {TARGET_GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json"
}

# ----------------------------
# Load event / fetch issue if needed
# ----------------------------
EVENT = {}
if GITHUB_EVENT_PATH and os.path.exists(GITHUB_EVENT_PATH):
    with open(GITHUB_EVENT_PATH) as f:
        EVENT = json.load(f)

# Determine issue number
if "issue" in EVENT:
    ISSUE_NUMBER = EVENT["issue"]["number"]

if not ISSUE_NUMBER:
    raise ValueError("ISSUE_NUMBER is not defined. Provide it as input for manual workflow run.")

# Fetch issue body and submitter if manual trigger
if "issue" in EVENT:
    issue_body = EVENT["issue"]["body"]
    original_submitter = EVENT["issue"]["user"]["login"]
else:
    # Manual trigger: fetch issue via API
    url = f"https://api.github.com/repos/{OWNER}/{REPO}/issues/{ISSUE_NUMBER}"
    r = requests.get(url, headers=HEADERS)
    if r.status_code != 200:
        raise RuntimeError(f"Failed to fetch issue #{ISSUE_NUMBER}: {r.text}")
    issue_data = r.json()
    issue_body = issue_data["body"]
    original_submitter = issue_data["user"]["login"]

# ----------------------------
# GitHub helpers
# ----------------------------
def post_comment(body):
    url = f"https://api.github.com/repos/{OWNER}/{REPO}/issues/{ISSUE_NUMBER}/comments"
    requests.post(url, headers=HEADERS, json={"body": body})

def add_label(label):
    url = f"https://api.github.com/repos/{OWNER}/{REPO}/issues/{ISSUE_NUMBER}/labels"
    requests.post(url, headers=HEADERS, json={"labels": [label]})

def remove_label(label):
    url = f"https://api.github.com/repos/{OWNER}/{REPO}/issues/{ISSUE_NUMBER}/labels/{label}"
    requests.delete(url, headers=HEADERS)

# ----------------------------
# GitHub repo management
# ----------------------------
def create_target_repo(repo_name):
    url = f"https://api.github.com/orgs/{GIT_TARGET_ORG}/repos"
    data = {"name": repo_name, "private": False, "auto_init": False}
    r = requests.post(url, headers=TARGET_HEADERS, json=data)
    if r.status_code not in [201, 422]:
        post_comment(f"❌ Failed to create repo: {r.text}")
        sys.exit(1)

def set_default_branch(repo_name, branch="devel"):
    url = f"https://api.github.com/repos/{GIT_TARGET_ORG}/{repo_name}"
    r = requests.patch(url, headers=TARGET_HEADERS, json={"default_branch": branch})
    if r.status_code != 200:
        post_comment(f"⚠️ Could not set default branch to {branch}: {r.text}")

def add_collaborator(repo_name, username, permission="write"):
    url = f"https://api.github.com/repos/{GIT_TARGET_ORG}/{repo_name}/collaborators/{username}"
    data = {"permission": permission}
    r = requests.put(url, headers=TARGET_HEADERS, json=data)
    if r.status_code not in [201, 204]:
        post_comment(f"⚠️ Failed to add @{username}: {r.text}")
    else:
        post_comment(f"✅ Added @{username} as collaborator with {permission} access")

# ----------------------------
# Clone & push
# ----------------------------
def clone_and_push():
    match = re.search(r"(?:https://github\.com/|git@github\.com:)([\w\-]+)/([\w\.\-]+)(?:\.git)?", issue_body)
    if not match:
        post_comment("❌ No valid GitHub repo URL found in issue body.")
        sys.exit(1)

    source_owner, source_repo = match.groups()
    source_repo = source_repo.rstrip(".git")
    source_url = f"https://x-access-token:{GITHUB_TOKEN}@github.com/{source_owner}/{source_repo}.git"
    target_url = f"https://x-access-token:{TARGET_GITHUB_TOKEN}@github.com/{GIT_TARGET_ORG}/{source_repo}.git"

    # Fetch source default branch
    repo_api_url = f"https://api.github.com/repos/{source_owner}/{source_repo}"
    r = requests.get(repo_api_url, headers=HEADERS)
    if r.status_code != 200:
        post_comment(f"❌ Failed to fetch repo info for {source_owner}/{source_repo}")
        sys.exit(1)
    source_default_branch = r.json().get("default_branch", "main")

    # Check if target repo exists
    target_repo_api_url = f"https://api.github.com/repos/{GIT_TARGET_ORG}/{source_repo}"
    r_target = requests.get(target_repo_api_url, headers=TARGET_HEADERS)

    if r_target.status_code == 404:
        # Repo does not exist — create it
        create_target_repo(source_repo)
        repo_empty = True
    elif r_target.status_code == 200:
        # Repo exists — check if it is empty
        r_contents = requests.get(f"{target_repo_api_url}/contents", headers=TARGET_HEADERS)
        if r_contents.status_code == 200 and len(r_contents.json()) > 0:
            post_comment(f"ℹ️ Target repository **{GIT_TARGET_ORG}/{source_repo}** already exists and is not empty. Skipping clone/push.")
            return f"{GIT_TARGET_ORG}/{source_repo}"
        repo_empty = True  # Repo exists but empty
    else:
        post_comment(f"❌ Failed to check target repo: {r_target.text}")
        sys.exit(1)

    # Only wrap the git commands in try/except
    try:
        # Full clone (no --depth) to avoid shallow history issues
        subprocess.run(["git", "clone", source_url], check=True)
        os.chdir(source_repo)

        # Configure Git user
        subprocess.run(["git", "config", "user.name", "Bioconductor Bot"], check=True)
        subprocess.run(["git", "config", "user.email", "bot@bioconductor.org"], check=True)

        # Rename branch to devel
        subprocess.run(["git", "branch", "-M", "devel"], check=True)

        # Point remote to target repo
        subprocess.run(["git", "remote", "remove", "origin"], check=True)
        subprocess.run(["git", "remote", "add", "origin", target_url], check=True)

        # Push only if repo is empty
        if repo_empty:
            subprocess.run(["git", "push", "-u", "origin", "devel"], check=True)

        # Clean up
        os.chdir("..")
        shutil.rmtree(source_repo)

        # Set default branch on GitHub
        set_default_branch(source_repo, "devel")

        post_comment(f"✅ Cloned **{source_owner}/{source_repo}** → **{GIT_TARGET_ORG}/{source_repo}** (branch: `devel`)")

    except subprocess.CalledProcessError as e:
        post_comment(f"❌ Git operation failed: {e}")
        sys.exit(1)

    return f"{GIT_TARGET_ORG}/{source_repo}"


# ----------------------------
# Update packages.json
# ----------------------------
def update_registry(repo_path):
    registry_repo = "tempbioc.r-universe.dev"
    repo_url = f"https://github.com/{repo_path}"
    package_name = repo_path.split("/")[-1]
    clone_url = f"https://x-access-token:{TARGET_GITHUB_TOKEN}@github.com/{GIT_TARGET_ORG}/{registry_repo}.git"

    try:
        subprocess.run(["git", "clone", clone_url], check=True)
        os.chdir(registry_repo)
        packages_file = "packages.json"
        if os.path.exists(packages_file):
            with open(packages_file) as f:
                data = json.load(f)
        else:
            data = []

        if not isinstance(data, list):
            post_comment("❌ packages.json must be a list.")
            sys.exit(1)

        already_exists = any(entry.get("package") == package_name or entry.get("url") == repo_url
                             for entry in data if isinstance(entry, dict))
 
        if already_exists:
            post_comment(f"ℹ️ Already registered: **{package_name}**")
        else:
            # Append new package at the end to preserve existing order
            data.append({"package": package_name, "url": repo_url})
            with open(packages_file, "w") as f:
                json.dump(data, f, indent=2)

            # Stage changes
            subprocess.run(["git", "add", packages_file], check=True)

            # Commit and push if there are changes
            if subprocess.run(["git", "diff", "--cached", "--quiet"]).returncode != 0:
                subprocess.run(["git", "config", "user.name", "Bioconductor Bot"], check=True)
                subprocess.run(["git", "config", "user.email", "bot@bioconductor.org"], check=True)
                subprocess.run(["git", "commit", "-m", f"Add {package_name}"], check=True)
                subprocess.run(["git", "push"], check=True)
                post_comment(f"✅ Added **{package_name}** to registry.")

        # Clean up
        os.chdir("..")
        shutil.rmtree(registry_repo)

# ----------------------------
# Main
# ----------------------------
if __name__ == "__main__":
    add_label("pre-review")
    repo_path = clone_and_push()
    update_registry(repo_path)
