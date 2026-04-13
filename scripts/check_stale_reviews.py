import os
import json
import requests
import base64
import re
# --------------------------------------------
# Environment
# --------------------------------------------
GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]             # repo workflow token
BIOC_ORG_TOKEN = os.environ.get("BIOC_ORG_TOKEN")     # org/team token
TEMP_BIOC_TOKEN = os.environ.get("TEMP_BIOC_TOKEN")
GIT_TARGET_ORG = os.environ["GIT_TARGET_ORG"]
REPO_FULL = os.environ["GITHUB_REPOSITORY"]
EVENT_PATH = os.environ["GITHUB_EVENT_PATH"]
OWNER, REPO = REPO_FULL.split("/")

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


# --------------------------------------------
# GitHub API: open issues
# --------------------------------------------
def get_open_issues():
    issues = []
    page = 1
    while True:
        url = f"https://api.github.com/repos/{OWNER}/{REPO}/issues"
        r = requests.get(
            url,
            headers=HEADERS,
            params={
                "state": "open",
                "per_page": 100,
                "page": page
            },
            timeout=10
        )
        r.raise_for_status()
        batch = r.json()
        if not batch:
            break
        issues.extend(batch)
        page += 1
    return issues


# --------------------------------------------
# Canonical package extraction 
# --------------------------------------------
def extract_package(issue, failures=None):
    if failures is None:
        failures = []
    issue_body = issue.get("body") or ""
    match = re.search(
        r"(?:https://github\.com/|git@github\.com:)([\w\-]+)/([\w\.\-]+)",
        issue_body
    )
    if not match:
        failures.append(
            f"Issue #{issue.get('number')} missing valid GitHub repo URL"
        )
        return None
    owner, repo = match.group(1), match.group(2)
    if repo.endswith(".git"):
        repo = repo[:-4]
    return repo


# --------------------------------------------
# Registry
# --------------------------------------------
def get_registry_packages():
    registry_repo = "tempbioc.r-universe.dev"
    url = f"https://api.github.com/repos/{GIT_TARGET_ORG}/{registry_repo}/contents/packages.json"
    r = requests.get(url, headers=TEMP_BIOC_HEADERS, timeout=10)
    r.raise_for_status()
    data = r.json()
    content = json.loads(base64.b64decode(data["content"]).decode())
    return {x["package"] for x in content if "package" in x}


# --------------------------------------------
# Main
# --------------------------------------------
def main():

    print("[INFO] Fetching open issues...")
    issues = get_open_issues()

    open_packages = set()
    failures = []

    for issue in issues:
        pkg = extract_package(issue, failures)
        if pkg:
            open_packages.add(pkg.lower())

    print(f"[INFO] Open packages found: {len(open_packages)}")

    if failures:
        print("\n[WARN] Issues with missing package URLs:")
        for f in failures:
            print(" -", f)

    print("\n[INFO] Fetching registry...")
    registry_packages = get_registry_packages()

    print(f"[INFO] Registry packages: {len(registry_packages)}")

    # Map lowercase → canonical registry name
    registry_map = {p.lower(): p for p in registry_packages}

    stale_packages = [
        registry_map[pkg_lower]
        for pkg_lower in registry_map
        if pkg_lower not in open_packages
    ]

    print("\n================ STALE PACKAGES ================\n")

    for pkg in sorted(stale_packages):
        print(pkg)

    print("\n===============================================\n")
    print(f"[INFO] Total stale packages: {len(stale_packages)}")


    ## Should this auto clean up registry and cloned repos
    
if __name__ == "__main__":
    main()
