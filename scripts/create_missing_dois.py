"""Create DataCite DOIs for accepted packages that do not have one.

Run daily (and on demand) by .github/workflows/create-dois.yml.

For each closed issue carrying an acceptance label:
  1. get the package name from the issue body
  2. skip it if it already has the 'doi-created' label (production runs only)
  3. ask DataCite whether the DOI exists (DataCite is the record of what has a DOI)
  3. if missing, check the acceptance label was added by a review team member
  4. read the package DESCRIPTION from the public hosting repo, get authors
  5. create the DOI

Failures add the 'doi-error' label and one comment per issue; the label is
removed when a later run succeeds. The job exits non-zero if anything failed.
"""
import os
import sys
from datetime import datetime, timezone

import requests

from bioc_doi import create_doi, doi_exists, doi_name
from description_utils import parse_description

GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]
BIOC_ORG_TOKEN = os.environ.get("BIOC_ORG_TOKEN") or GITHUB_TOKEN
REPO_FULL = os.environ["GITHUB_REPOSITORY"]
ORG_NAME = os.environ.get("ORG_NAME", "Bioconductor")
TEAM = os.environ["TEAM_SLUG"]
HOSTING_ORG = os.environ.get("BIOC_PKG_HOSTING_ORG", "bioconductor-source")
HOSTING_TOKEN = os.environ.get("BIOC_PKG_HOSTING_TOKEN")
ACCEPT_LABELS = [
    x.strip() for x in os.environ.get("ACCEPT_LABELS", "package accepted").split(",") if x.strip()
]
# Optional (YYYY-MM-DD). When empty, all closed accepted issues are checked.
SINCE = (os.environ.get("DOI_SINCE") or "").strip()
TESTING = os.environ.get("DOI_PRODUCTION", "false").lower() != "true"
DRY_RUN = os.environ.get("DRY_RUN", "false").lower() == "true"
ONLY_PACKAGE = (os.environ.get("ONLY_PACKAGE") or "").strip()
ERROR_LABEL = "doi-error"
DONE_LABEL = "doi-created"  # production only; removing it forces a re-check

GH = {"Authorization": f"Bearer {GITHUB_TOKEN}", "Accept": "application/vnd.github+json"}
ORG = {"Authorization": f"Bearer {BIOC_ORG_TOKEN}", "Accept": "application/vnd.github+json"}


def gh_paginate(url, headers, params=None):
    params = dict(params or {}, per_page=100)
    while url:
        r = requests.get(url, headers=headers, params=params, timeout=30)
        r.raise_for_status()
        yield from r.json()
        url = r.links.get("next", {}).get("url")
        params = None  # the next link already carries the query string


def closed_accepted_issues():
    owner, repo = REPO_FULL.split("/")
    since_dt = datetime.fromisoformat(SINCE).replace(tzinfo=timezone.utc) if SINCE else None
    found = {}
    for label in ACCEPT_LABELS:
        url = f"https://api.github.com/repos/{owner}/{repo}/issues"
        params = {"state": "closed", "labels": label}
        if since_dt:
            params["since"] = since_dt.isoformat()
        for issue in gh_paginate(url, GH, params):
            if "pull_request" in issue:
                continue
            closed_at = issue.get("closed_at")
            if not closed_at:
                continue
            closed = datetime.fromisoformat(closed_at.replace("Z", "+00:00"))
            if since_dt is None or closed >= since_dt:
                found[issue["number"]] = issue
    return [found[n] for n in sorted(found)]


def accepted_by(issue_number):
    """Login of whoever most recently added an acceptance label, or None."""
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/events"
    actor = None
    for ev in gh_paginate(url, GH):
        if ev.get("event") == "labeled" and ev.get("label", {}).get("name") in ACCEPT_LABELS:
            actor = (ev.get("actor") or {}).get("login")
    return actor


def is_team_member(username):
    url = f"https://api.github.com/orgs/{ORG_NAME}/teams/{TEAM}/memberships/{username}"
    r = requests.get(url, headers=ORG, timeout=20)
    return r.status_code == 200 and r.json().get("state") == "active"


def package_from_issue(issue):
    import re
    m = re.search(
        r"(?:https://github\.com/|git@github\.com:)([\w\-]+)/([\w\.\-]+)", issue.get("body") or ""
    )
    if not m:
        return None
    repo = m.group(2)
    return repo[:-4] if repo.endswith(".git") else repo


def fetch_description(pkg):
    url = f"https://raw.githubusercontent.com/{HOSTING_ORG}/{pkg}/devel/DESCRIPTION"
    headers = {"Authorization": f"Bearer {HOSTING_TOKEN}"} if HOSTING_TOKEN else {}
    r = requests.get(url, headers=headers, timeout=20)
    if r.status_code != 200:
        raise RuntimeError(f"could not read DESCRIPTION from {HOSTING_ORG}/{pkg}: HTTP {r.status_code}")
    return r.text


def issue_api(number, suffix=""):
    owner, repo = REPO_FULL.split("/")
    return f"https://api.github.com/repos/{owner}/{repo}/issues/{number}{suffix}"


def mark_error(issue, message):
    labels = {l["name"] for l in issue.get("labels", [])}
    if ERROR_LABEL in labels:
        return  # already reported; do not repeat the comment every day
    requests.post(issue_api(issue["number"], "/labels"), headers=GH, json={"labels": [ERROR_LABEL]}, timeout=20)
    requests.post(
        issue_api(issue["number"], "/comments"), headers=GH, timeout=20,
        json={"body": f"DOI creation failed for this package. It will be retried daily.\nPlease contact the Bioconductor Core Team if not resolved after a week."},
    )


def mark_done(issue):
    labels = {l["name"] for l in issue.get("labels", [])}
    if DONE_LABEL not in labels:
        requests.post(issue_api(issue["number"], "/labels"), headers=GH, json={"labels": [DONE_LABEL]}, timeout=20)


def clear_error(issue):
    if ERROR_LABEL in {l["name"] for l in issue.get("labels", [])}:
        requests.delete(issue_api(issue["number"], f"/labels/{ERROR_LABEL}"), headers=GH, timeout=20)


def process(issue):
    """Returns (status, detail). status: exists, created, skipped, would-create, error."""
    if not TESTING and DONE_LABEL in {l["name"] for l in issue.get("labels", [])}:
        return "skipped", f"already has '{DONE_LABEL}' label"
    pkg = package_from_issue(issue)
    if not pkg:
        raise RuntimeError("could not find a repository URL in the issue body")
    if ONLY_PACKAGE and pkg != ONLY_PACKAGE:
        return "skipped", "not the requested package"

    if doi_exists(pkg, TESTING):
        return "exists", doi_name(pkg, TESTING)

    actor = accepted_by(issue["number"])
    if not actor or not is_team_member(actor):
        return "skipped", f"acceptance label not added by an active team member"

    desc = parse_description(fetch_description(pkg))
    authors = desc.get("Authors") or [issue["user"]["login"]]
    pubyear = datetime.fromisoformat(issue["closed_at"].replace("Z", "+00:00")).year

    if DRY_RUN:
        return "would-create", f"{doi_name(pkg, TESTING)} authors={authors}"
    ok, msg = create_doi(pkg, authors, pubyear, testing=TESTING)
    if not ok:
        raise RuntimeError(msg)
    return "created", msg


def main():
    mode = "TESTING" if TESTING else "PRODUCTION"
    print(f"[INFO] DOI mode: {mode}")
    print(f"[INFO] dry run: {DRY_RUN}")
    print(f"[INFO] closed since {SINCE or 'the beginning'}")
    print(f"[INFO] labels {ACCEPT_LABELS}")
    issues = closed_accepted_issues()
    print(f"[INFO] {len(issues)} closed accepted issues to check")

    rows, failures = [], 0
    for issue in issues:
        n = issue["number"]
        try:
            status, detail = process(issue)
            if status in ("created", "exists") and not DRY_RUN:
                clear_error(issue)
                if not TESTING:
                    mark_done(issue)
        except Exception as e:
            status, detail = "error", str(e)
            failures += 1
            if not DRY_RUN:
                try:
                    mark_error(issue, detail)
                except Exception as e2:
                    print(f"[WARN] could not report error on #{n}: {e2}")
        print(f"[{status.upper()}] #{n}: {detail}")
        rows.append((n, status, detail))

    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a") as f:
            f.write(f"### DOI run ({mode}{', dry run' if DRY_RUN else ''})\n\n| Issue | Status | Detail |\n|---|---|---|\n")
            for n, s, d in rows:
                f.write(f"| #{n} | {s} | {d.replace('|', '/')[:200]} |\n")

    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
