"""Helpers for identifying a submission from its issue.

The repository URL in the issue body says where the submitted code lives.
The package name comes from the `Package:` field of the DESCRIPTION file in
that repository; the repository name may differ (e.g. `foo-R`, `foo-py`).
validate.py records that name in the submissions CSV (on the `submissions`
branch) keyed by issue number, so later steps look it up there first and
only read the submitter's DESCRIPTION if the issue is not recorded.
Everything on the Bioconductor side (staging repo, r-universe registry,
gitolite, git.bioconductor.org, hosting repo, manifest, DOI) is named after
the package, not the repository.

No environment access at import.
"""
import base64
import csv
import functools
import io
import os
import re
import subprocess

import requests

DEFAULT_SUBMISSIONS_PATH = "submissions/submitted_packages.csv"

GITHUB_URL_RE = re.compile(r"(?:https://github\.com/|git@github\.com:)([\w\-]+)/([\w\.\-]+)")


def extract_repo(issue_body):
    """Return (owner, repo) of the first GitHub URL in the issue body, or (None, None)."""
    match = GITHUB_URL_RE.search(issue_body or "")
    if not match:
        return None, None
    owner, repo = match.group(1), match.group(2)
    return owner, repo.removesuffix(".git")


def parse_package_field(desc_text):
    match = re.search(r"^Package:\s*(\S+)\s*$", desc_text or "", re.MULTILINE)
    return match.group(1) if match else None


def fetch_package_name(owner, repo, headers=None, ref=None):
    """Read the `Package:` field from DESCRIPTION at the root of owner/repo.

    Returns None if the file cannot be fetched or has no Package field.
    """
    url = f"https://api.github.com/repos/{owner}/{repo}/contents/DESCRIPTION"
    params = {"ref": ref} if ref else None
    try:
        r = requests.get(url, headers=headers, params=params, timeout=20)
    except requests.RequestException as e:
        print(f"[WARN] Could not fetch DESCRIPTION from {owner}/{repo}: {e}")
        return None
    if r.status_code != 200:
        print(f"[WARN] Could not fetch DESCRIPTION from {owner}/{repo}: HTTP {r.status_code}")
        return None
    try:
        text = base64.b64decode(r.json()["content"]).decode("utf-8")
    except Exception as e:
        print(f"[WARN] Could not decode DESCRIPTION from {owner}/{repo}: {e}")
        return None
    return parse_package_field(text)


@functools.lru_cache(maxsize=None)
def _submissions_by_issue(csv_path):
    """Map issue number (str) -> package_name from the submissions CSV.

    Reads origin/submissions with `git show`, so the working tree and current
    branch are left alone. Returns {} if the branch or file is unavailable.
    """
    subprocess.run(["git", "fetch", "-q", "origin", "submissions"], capture_output=True)
    r = subprocess.run(
        ["git", "show", f"origin/submissions:{csv_path}"],
        capture_output=True, text=True,
    )
    if r.returncode != 0:
        print(f"[WARN] Could not read {csv_path} from origin/submissions: {r.stderr.strip()}")
        return {}
    mapping = {}
    for row in csv.DictReader(io.StringIO(r.stdout)):
        issue = (row.get("issue_number") or "").strip()
        pkg = (row.get("package_name") or "").strip()
        if issue and pkg:
            mapping[issue] = pkg
    return mapping


def package_from_submissions(issue_number, csv_path=None):
    """Package name recorded by validate.py for this issue, or None."""
    if issue_number is None:
        return None
    csv_path = csv_path or os.environ.get("SUBMISSIONS_PATH", DEFAULT_SUBMISSIONS_PATH)
    return _submissions_by_issue(csv_path).get(str(issue_number))


def package_from_issue(issue_body, headers=None, issue_number=None, fallback_to_repo=True):
    """Return (owner, repo, package) for a submission issue.

    `package` is resolved in order from:
      1. the submissions CSV entry for `issue_number` (recorded at precheck),
      2. the `Package:` field of the submitted repository's DESCRIPTION,
      3. the repository name, with a warning, if `fallback_to_repo` is True
         (correct for submissions whose repo name matches the package).
    Any element may be None if it cannot be determined.
    """
    owner, repo = extract_repo(issue_body)
    package = package_from_submissions(issue_number)
    if package:
        return owner, repo, package
    if not owner or not repo:
        return owner, repo, None
    package = fetch_package_name(owner, repo, headers=headers)
    if not package and fallback_to_repo:
        print(f"[WARN] Using repository name '{repo}' as the package name")
        package = repo
    return owner, repo, package
