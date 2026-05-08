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
BIOC_STAGING_TOKEN = os.environ["BIOC_STAGING_TOKEN"]
GITHUB_EVENT_PATH = os.environ.get("GITHUB_EVENT_PATH")
GITHUB_REPOSITORY = os.environ["GITHUB_REPOSITORY"]
BIOC_STAGING_ORG = os.environ["BIOC_STAGING_ORG"]
ISSUE_NUMBER = os.environ.get("ISSUE_NUMBER")
SPB_RUNIVERSE = os.environ["SPB_RUNIVERSE"]

OWNER, REPO = GITHUB_REPOSITORY.split("/")

HEADERS = {
    "Authorization": f"Bearer {GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json"
}

BIOC_STAGING_HEADERS = {
    "Authorization": f"Bearer {BIOC_STAGING_TOKEN}",
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
    url = f"https://api.github.com/orgs/{BIOC_STAGING_ORG}/repos"
    data = {"name": repo_name, "private": False, "auto_init": False}
    r = requests.post(url, headers=BIOC_STAGING_HEADERS, json=data)
    if r.status_code not in [201, 422]:
        print(f"❌ Failed to create repo: {r.status_code} {r.text}")
        post_comment("❌ Failed to create repo: ask admins for assistance")
        sys.exit(1)

def set_default_branch(repo_name, branch="devel"):
    url = f"https://api.github.com/repos/{BIOC_STAGING_ORG}/{repo_name}"
    r = requests.patch(url, headers=BIOC_STAGING_HEADERS, json={"default_branch": branch})
    if r.status_code != 200:
        post_comment("⚠ Failed to set default branch: ask admins for assistance")
        print(f"⚠️ Could not set default branch to {branch}: {r.status_code} {r.text}")

def add_collaborator(repo_name, username, permission="write"):
    url = f"https://api.github.com/repos/{BIOC_STAGING_ORG}/{repo_name}/collaborators/{username}"
    data = {"permission": permission}
    r = requests.put(url, headers=BIOC_STAGING_HEADERS, json=data)
    if r.status_code not in [201, 204]:
        print(f"⚠️ Failed to add @{username}: {r.status_code} {r.text}")
        post_comment(f"⚠ Failed adding @{username} as collaborator: ask admins for assistance")
    else:
        print(f"✅ Added @{username} as collaborator with {permission} access")


# ----------------------------
#  Current tempbioc is Free not Team
#  Cannot implement rulesets org wide
#  Implement per repo
#      protect devel
#      no force pushes except for admins
# ----------------------------

def protect_devel(repo_name):
    url = f"https://api.github.com/repos/{BIOC_STAGING_ORG}/{repo_name}/branches/devel/protection"

    data = {
        "required_status_checks": None,
        "enforce_admins": False,
        "required_pull_request_reviews": None,
        "restrictions": None,
        "allow_force_pushes": False,
        "allow_deletions": False
    }

    r = requests.put(url, headers=BIOC_STAGING_HEADERS, json=data)
    if r.status_code not in [200]:
        print(f"⚠️ Failed to protect devel: {r.status_code} {r.text}")
    else:
        print("✅ devel protection applied")

       
# ----------------------------
# Clone & push
# ----------------------------
def clone_and_push():
    msg = ""
    match = re.search(r"(?:https://github\.com/|git@github\.com:)([\w\-]+)/([\w\.\-]+)(?:\.git)?", issue_body)
    if not match:
        print("❌ No valid GitHub repo URL found in issue body.")
        post_comment(f"❌  No valid GitHub repo found: ask admins for assistance")
        sys.exit(1)

    source_owner, source_repo = match.groups()
    source_repo = source_repo.removesuffix(".git")
    source_url = f"https://x-access-token:{GITHUB_TOKEN}@github.com/{source_owner}/{source_repo}.git"
    target_url = f"https://x-access-token:{BIOC_STAGING_TOKEN}@github.com/{BIOC_STAGING_ORG}/{source_repo}.git"

    # Fetch source default branch
    repo_api_url = f"https://api.github.com/repos/{source_owner}/{source_repo}"
    r = requests.get(repo_api_url, headers=HEADERS)
    if r.status_code != 200:
        print(f"❌ Failed to fetch repo info for {source_owner}/{source_repo}")
        post_comment(f"❌  Failed to fetch repo info for {source_owner}/{source_repo} : ask admins for assistance")
        sys.exit(1)
    source_default_branch = r.json().get("default_branch", "main")

    # Check if target repo exists and is empty
    target_repo_api_url = f"https://api.github.com/repos/{BIOC_STAGING_ORG}/{source_repo}"
    r_target = requests.get(target_repo_api_url, headers=BIOC_STAGING_HEADERS)
    if r_target.status_code == 404:
        create_target_repo(source_repo)
        repo_empty = True
    elif r_target.status_code == 200:
        r_contents = requests.get(f"{target_repo_api_url}/contents", headers=BIOC_STAGING_HEADERS)
        if r_contents.status_code == 200 and len(r_contents.json()) > 0:
            msg = (f"ℹ️ Target repository **{BIOC_STAGING_ORG}/{source_repo}** already exists and is not empty.\n"
                   "  - Cloning is skipped to avoid overwriting existing content.\n")
            add_collaborator(source_repo, original_submitter, permission="write")
            return f"{BIOC_STAGING_ORG}/{source_repo}", msg
        repo_empty = True
    else:
        print(f"❌ Failed to check target repo: {r_target.text}")
        sys.exit(1)

    try:
        subprocess.run(["git", "clone", source_url], check=True)
        os.chdir(source_repo)
        subprocess.run(["git", "config", "user.name", "Bioconductor Bot"], check=True)
        subprocess.run(["git", "config", "user.email", "bot@bioconductor.org"], check=True)
        subprocess.run(["git", "branch", "-M", "devel"], check=True)
        subprocess.run(["git", "remote", "remove", "origin"], check=True)
        subprocess.run(["git", "remote", "add", "origin", target_url], check=True)

        if repo_empty:
            subprocess.run(["git", "push", "-u", "origin", "devel"], check=True)

        os.chdir("..")
        shutil.rmtree(source_repo)

        set_default_branch(source_repo, "devel")
        protect_devel(source_repo)
       
        msg = f"✅ Cloned **{source_owner}/{source_repo}** → **{BIOC_STAGING_ORG}/{source_repo}** (branch: `devel`)"

        add_collaborator(source_repo, original_submitter, permission="write")
        
    except subprocess.CalledProcessError as e:
        print(f"❌ Git operation failed: {e}")
        post_comment(f"❌  Failed to clone : ask admins for assistance")
        sys.exit(1)

    return f"{BIOC_STAGING_ORG}/{source_repo}", msg

# ----------------------------
# Update packages.json
# ----------------------------
def update_registry(repo_path):
    msg = ""
    registry_repo = f"{SPB_RUNIVERSE}.r-universe.dev"
    repo_url = f"https://github.com/{repo_path}"
    package_name = repo_path.split("/")[-1]
    clone_url = f"https://x-access-token:{BIOC_STAGING_TOKEN}@github.com/{BIOC_STAGING_ORG}/{registry_repo}.git"

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
            print("❌ packages.json must be a list.")
            post_comment(f"❌  Failed to add to runiverse registry : ask admins for assistance")
            sys.exit(1)

        already_exists = any(entry.get("package") == package_name or entry.get("url") == repo_url
                             for entry in data if isinstance(entry, dict))

        if already_exists:
            msg = f"ℹ️ Already registered: **{package_name}**"
        else:
            data.append({"package": package_name, "url": repo_url})
            with open(packages_file, "w") as f:
                json.dump(data, f, indent=2)

            subprocess.run(["git", "add", packages_file], check=True)

            if subprocess.run(["git", "diff", "--cached", "--quiet"]).returncode != 0:
                subprocess.run(["git", "config", "user.name", "Bioconductor Bot"], check=True)
                subprocess.run(["git", "config", "user.email", "bot@bioconductor.org"], check=True)
                subprocess.run(["git", "commit", "-m", f"Add {package_name}"], check=True)
                subprocess.run(["git", "push"], check=True)
                msg = f"✅ Added **{package_name}** to registry."

        os.chdir("..")
        shutil.rmtree(registry_repo)
        return msg
    
    except subprocess.CalledProcessError as e:
        print(f"❌ Failed to update registry: {e}")
        post_comment(f"❌  Failed to add to runiverse registry : ask admins for assistance")
        sys.exit(1)
 
# ----------------------------
# Main
# ----------------------------
if __name__ == "__main__":
    add_label("pre-review")
    repo_path, clone_msg = clone_and_push()
    registry_msg = update_registry(repo_path)

    full_message = f"""
{clone_msg}

{registry_msg}

Your package is cloned to the Bioconductor new submission source repository and r-universe for testing. 

We have added you as a collaborator at https://github.com/{BIOC_STAGING_ORG}/{repo_path.split('/')[-1]}
You will need to accept the github invitation to push future changes.
**Task:** Please accept collaborator access now.

If you want to push command line updates, you need to update your remotes:
```
  git remote add {SPB_RUNIVERSE} git@github.com:{BIOC_STAGING_ORG}/{repo_path.split('/')[-1]}
  git push {SPB_RUNIVERSE} devel
```

Bioconductor uses **devel** as its default branch.
If you use a different branch (example: main) map branches when pushing:
``` 
  git push {SPB_RUNIVERSE} main:devel
```

You will receive your build report shortly.  Builds may take an hour or two to
    register based on github action scheduler.
If you do not receive a build report within 24 hours please ask for assistance.

Reminders:

  - All packages should be free of Errors and Warnings.
  - Any Errors or Warnings must be fixed or justified; a reviewer will not be assigned until this is complete.
  - Once a reviewer is assigned, justify any remaining Errors, Warnings, or Notes
  - Address reviewer comments with a point-by-point response.
"""

    post_comment(full_message)
