import os
import json
import requests
from requests.auth import HTTPBasicAuth
import subprocess
import re
import sys
import base64
import time
import sqlite3
from collections import Counter
import shutil
from datetime import datetime


# --------------------------------------------
# Environment
# --------------------------------------------
GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]             # repo workflow token
BIOC_ORG_TOKEN = os.environ.get("BIOC_ORG_TOKEN")     # org/team token
TEMP_BIOC_TOKEN = os.environ.get("TEMP_BIOC_TOKEN")
ORG_NAME = os.environ.get("ORG_NAME", "Bioconductor")
TEAM = os.environ["TEAM_SLUG"]
GIT_TARGET_ORG = os.environ["GIT_TARGET_ORG"]
REPO_FULL = os.environ["GITHUB_REPOSITORY"]
ISSUE_NUMBER = os.environ.get("ISSUE_NUMBER")
EVENT_PATH = os.environ["GITHUB_EVENT_PATH"]

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


def bioc_credentials_auth():
    user = os.environ.get("BIOC_CREDENTIALS_USER")
    password = os.environ.get("BIOC_CREDENTIALS_PASSWORD")

    if not user or not password:
        raise RuntimeError(
            "Missing BiocCredentials environment variables: "
            "BIOC_CREDENTIALS_USER and/or BIOC_CREDENTIALS_PASSWORD"
        )

    return HTTPBasicAuth(user, password)


BIOC_CREDENTIALS_URL="https://git.bioconductor.org/BiocCredentials/api/biocusers/"

# --------------------------------------------
# Helper Functions Issues / Labels / Org
# --------------------------------------------
def remove_label(issue_number, label):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels/{label}"
    r = requests.delete(url, headers=HEADERS)
    if r.status_code in (200, 204, 404):
        print(f"[DEBUG] Label '{label}' removed from issue #{issue_number}")
    else:
        r.raise_for_status()

def get_issue_labels(issue_number):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels"
    r = requests.get(url, headers=HEADERS)
    r.raise_for_status()
    return {label["name"] for label in r.json()}


def post_comment(issue_number, body):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/comments"
    r = requests.post(url, headers=HEADERS, json={"body": body})
    if r.status_code >= 300:
        print(f"[WARN] Failed to post comment: {r.status_code} {r.text}")

def close_issue(issue_number):
    owner, repo = REPO_FULL.split("/")
    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}"
    r = requests.patch(url, headers=HEADERS, json={"state": "closed"})
    if r.status_code >= 300:
        print(f"[WARN] Failed to close issue: {r.status_code} {r.text}")

def is_team_member(username):
    url = f"https://api.github.com/orgs/{ORG_NAME}/teams/{TEAM}/memberships/{username}"
    r = requests.get(url, headers=ORG_HEADERS)

    return r.status_code == 200 and r.json().get("state") == "active"

# --------------------------------------------
# Helper Functions Parsing and Extracting
# --------------------------------------------
def extract_repo(issue_body):
    match = re.search(r"(?:https://github\.com/|git@github\.com:)([\w\-]+)/([\w\.\-]+)", issue_body)
    if not match:
        return None, None
    owner, repo = match.group(1), match.group(2)
    return owner, repo


def get_description_file(owner, repo):
    url = f"https://raw.githubusercontent.com/{owner}/{repo}/devel/DESCRIPTION"
    try:
        r = requests.get(url, headers=TEMP_BIOC_HEADERS, timeout=10)
        r.raise_for_status()
        return r.text
    except requests.RequestException as e:
        print(f"[ERROR] Failed to fetch DESCRIPTION for {repo}@devel: {e}")
        return None


def parse_dcf(desc_text):
    fields = {}
    current_key = None
    for line in desc_text.splitlines():
        if not line.strip():
            continue

        # New field
        if re.match(r"^[A-Za-z0-9@-]+:", line):
            key, value = line.split(":", 1)
            current_key = key.strip()
            fields[current_key] = value.strip()
        # Continuation line
        else:
            if current_key:
                fields[current_key] += " " + line.strip()
    return {k.lower(): v for k, v in fields.items()}


def extract_all_emails(text):
    if not text:
        return []
    return re.findall(r"[\w\.-]+@[\w\.-]+\.\w+", text)


def extract_maintainer_names(text):
    if not text:
        return []
	
    cleaned = re.sub(r"<[^>]+>", "", text)
    parts = [p.strip() for p in cleaned.split(",") if p.strip()]
    return parts



def parse_authors(authors_r=None, author_field=None):
    def extract_given(expr):
        if not expr:
            return ""
        parts = re.findall(r'"([^"]+)"', expr)
        if parts:
            return " ".join(parts)
        return expr.strip('"').strip()

    def parse_person_block(block):
        # ----------------------------
        # named form
        # ----------------------------
        given_match = re.search(
            r'given\s*=\s*(c\([^)]+\)|"[^"]+")',
            block
        )
        family_match = re.search(
            r'family\s*=\s*"([^"]+)"',
            block
        )
        if family_match:
            given = extract_given(given_match.group(1)) if given_match else ""
            return f"{given} {family_match.group(1)}".strip()
        # ----------------------------
        # positional form
        # person("First", "Last", ...)
        # ----------------------------
        positional = re.findall(r'"([^"]+)"', block)
        if len(positional) >= 2:
            return f"{positional[0]} {positional[1]}".strip()
        return None

    def clean_author_text(text):
        if not text:
            return None
        text = re.sub(r"<[^>]+>", "", text)
        text = re.sub(r"\[[^\]]+\]", "", text)
        text = re.sub(r"\s+", " ", text).strip()
        parts = [p.strip() for p in text.split(",") if p.strip()]
        return ", ".join(parts) if parts else None

    # ----------------------------
    # Authors@R
    # ----------------------------
    if authors_r:
        person_blocks = extract_person_blocks(authors_r)
        names = []
        for block in person_blocks:
            name = parse_person_block(block)
            if name:
                names.append(name)
        if names:
            return ", ".join(names)

    # ----------------------------
    # fallback Author field
    # ----------------------------
    if author_field:
        return clean_author_text(author_field)

    return None


def extract_cre_emails(authors_r):
    if not authors_r:
        return []

    cre_emails = []
    person_blocks = extract_person_blocks(authors_r)
    for block in person_blocks:
        # ----------------------------
        # detect role (order-independent)
        # ----------------------------
        role_match = re.search(
            r'role\s*=\s*(c\([^)]+\)|"[^"]+")',
            block
        )

        is_cre = False

        if role_match:
            role_text = role_match.group(1)
            is_cre = '"cre"' in role_text

        if not is_cre:
            continue

        # ----------------------------
        # extract email
        # ----------------------------
        email_match = re.search(r'email\s*=\s*"([^"]+)"', block)
        if email_match:
            cre_emails.append(email_match.group(1))

    return cre_emails


def extract_person_blocks(text):
    blocks = []
    i = 0
    while True:
        start = text.find("person(", i)
        if start == -1:
            break

        depth = 0
        end = None
        for j in range(start, len(text)):
            if text[j] == "(":
                depth += 1
            elif text[j] == ")":
                depth -= 1
                if depth == 0:
                    end = j
                    break

        if end is None:
            break
        blocks.append(text[start:end])
        i = end + 1

    return blocks


def parse_description(desc_text):
    f = parse_dcf(desc_text)

    # ----------------------------
    # biocViews
    # ----------------------------
    bioc_views = []
    if "biocviews" in f:
        bioc_views = [x.strip() for x in f["biocviews"].split(",") if x.strip()]

    # ----------------------------
    # BiocType
    # ----------------------------
    bioc_type = f.get("bioctype")

    # --------------------------------------------------
    # MAINTAINERS
    # --------------------------------------------------
    emails = set()
    authors_r = f.get("authors@r")
    
    if authors_r:
        emails.update(extract_cre_emails(authors_r))

    maintainer_field = f.get("maintainer")
    if maintainer_field:
        emails.update(extract_all_emails(maintainer_field))

    maintainer_email = sorted(emails) if emails else None

    # ----------------------------
    # AUTHORS
    # ----------------------------
    authors = parse_authors(
        authors_r=authors_r,
        author_field=f.get("author")
    )

    # ----------------------------
    # Merge maintainer names into Authors (names only)
    # ----------------------------
    maintainer_names = extract_maintainer_names(f.get("maintainer"))
    if authors:
        author_list = [a.strip() for a in authors.split(",") if a.strip()]
    else:
        author_list = []

    for m in maintainer_names:
        if m not in author_list:
            author_list.append(m)

    authors = ", ".join(author_list) if author_list else None

    return {
        "biocViews": bioc_views,
        "BiocType": bioc_type,
        "MaintainerEmail": maintainer_email,
        "Authors": authors
    }


# --------------------------------------------
# biocViews Helpers
# --------------------------------------------
def download_biocviews_sqlite(dest_path):
    owner = "Bioconductor"
    repo = "biocViews"
    file_path = "inst/extdata/biocViewsVocab.sqlite"
    url = f"https://api.github.com/repos/{owner}/{repo}/contents/{file_path}"
    r = requests.get(url, headers=ORG_HEADERS, timeout=30)

    r.raise_for_status()
    data = r.json()
    content = base64.b64decode(data["content"])
    with open(dest_path, "wb") as f:
        f.write(content)
    print(f"[INFO] Downloaded biocViews SQLite → {dest_path}")


def load_biocviews_vocab(sqlite_path):
    conn = sqlite3.connect(sqlite_path)
    cur = conn.cursor()
    cur.execute("SELECT edgeFrom, edgeTo FROM biocViews")
    rows = cur.fetchall()
    conn.close()

    parent_map = {}
    for parent, child in rows:
        if parent and child:
            parent_map[child] = parent
    ROOTS = {
        "Software",
        "ExperimentData",
        "AnnotationData",
        "Workflow",
        "Book"
    }

    def resolve_root(term):
        seen = set()
        while term and term not in ROOTS:
            if term in seen:
                return None
            seen.add(term)
            term = parent_map.get(term)
        return term if term in ROOTS else None

    term_to_category = {}
    for term in parent_map.keys():
        root = resolve_root(term)
        if root:
            term_to_category[term.lower()] = root
    print(f"[INFO] Loaded {len(term_to_category)} biocViews mappings")
    return term_to_category


def determine_package_type(metadata, term_to_category):
    bioc_type = metadata.get("BiocType")
    bioc_views = metadata.get("biocViews", [])

    # ----------------------------
    # BiocType override 
    # ----------------------------
    if bioc_type:
        bt = bioc_type.strip().lower()
        if bt in {"software"}:
            return "Software"
        if bt in {"experimentdata", "experiment"}:
            return "ExperimentData"
        if bt in {"annotationdata", "annotation"}:
            return "AnnotationData"
        if bt in {"workflow"}:
            return "Workflow"
        if bt in {"book"}:
            return "Book"
        print(f"[WARN] Unknown BiocType '{bioc_type}', falling back to biocViews")

    # ----------------------------
    # Map biocViews
    # ----------------------------
    categories = []
    for term in bioc_views:
        if not term:
            continue
        cat = term_to_category.get(term.lower())
        if cat:
            categories.append(cat)
        else:
            print(f"[WARN] Unknown biocViews term: {term}")

    # ----------------------------
    # Fallback is Software
    # ----------------------------
    if not categories:
        print("[WARN] No valid biocViews terms found → defaulting to Software")
        return "Software"

    # ----------------------------
    # Determine from biocViews
    # ----------------------------
    counts = Counter(categories)
    most_common = counts.most_common()
    top_count = most_common[0][1]
    top = [c for c, v in most_common if v == top_count]
    if len(top) > 1:
        print(f"[WARN] Tie in classification: {top} → defaulting to Software")
        return "Software"
    return top[0]

# ----------------------------
# BiocCredentials Helpers
# ----------------------------
def normalize_bioc_response(resp_json):
    if resp_json is None:
        return None
    if isinstance(resp_json, str):
        return resp_json.strip() or None
    if isinstance(resp_json, dict):
        return resp_json.get("github_id") or resp_json.get("id")
    return None


def bioc_credentials_lookup(github_id, maintainer_email, auth):
    github_id_result = None
    email_id_results = []
    headers = {"Accept": "application/json"}

    # --------------------------------------------
    # GitHub ID lookup
    # --------------------------------------------
    if github_id:
        url = f"{BIOC_CREDENTIALS_URL}query_by_github/{github_id}/"
        try:
            r = requests.get(url, auth=auth, headers=headers, timeout=10)
            if r.ok:
                try:
                    data = r.json()
                    github_id_result = normalize_bioc_response(data)
                except ValueError:
                    print(f"[WARN] Invalid JSON from GitHub lookup: {r.text}")
        except requests.RequestException as e:
            print(f"[WARN] GitHub credential lookup failed: {e}")

    # --------------------------------------------
    # Normalize maintainer_email → list
    # --------------------------------------------
    if maintainer_email:
        if isinstance(maintainer_email, str):
            maintainer_emails = [maintainer_email]
        else:
            maintainer_emails = list(maintainer_email)
    else:
        maintainer_emails = []

    # --------------------------------------------
    # Email lookups 
    # --------------------------------------------
    for email in maintainer_emails:
        url = f"{BIOC_CREDENTIALS_URL}query_by_email/{email}/"

        try:
            r = requests.get(url, auth=auth, headers=headers, timeout=10)

            if not r.ok:
                print(f"[WARN] Email lookup failed ({email}) HTTP {r.status_code}")
                email_id_results.append(None)
                continue

            try:
                data = r.json()
                email_id = normalize_bioc_response(data)

                if email_id:
                    print(f"[INFO] email {email} → {email_id}")
                    email_id_results.append(email_id)
                else:
                    print(f"[INFO] email {email} → NOT FOUND")
                    email_id_results.append(None)

            except ValueError:
                print(f"[WARN] Invalid JSON from email lookup ({email}): {r.text}")
                email_id_results.append(None)

        except requests.RequestException as e:
            print(f"[WARN] Email credential lookup failed ({email}): {e}")
            email_id_results.append(None)

    # --------------------------------------------
    # Conflict logic 
    # --------------------------------------------
    conflict = False
    valid_email_ids = [x for x in email_id_results if x is not None]

    if github_id_result and valid_email_ids:
        if github_id_result not in valid_email_ids:
            conflict = True

    if len(valid_email_ids) > 1:
        if len(set(valid_email_ids)) > 1:
            conflict = True

    if conflict:
        print(
            "[WARN] BiocCredentials potential mismatch:",
            f"github_id={github_id_result}, email_ids={email_id_results}"
        )

    return {
        "github_id": github_id_result,
        "email_id": email_id_results,
        "conflict": conflict
    }



def create_bioccredentials(username, email, auth):

    url = f"{BIOC_CREDENTIALS_URL}query_users/{email}/{username}"
    headers = {"Accept": "application/json"}
    try:
        print(f"[INFO] Creating BiocCredentials account for {username} ({email})")
        r = requests.get(url, auth=auth, headers=headers, timeout=10)

        if r.status_code == 200:
            try:
                data = r.json()
                print(f"[INFO] BiocCredentials response: {data}")
                return data
            except ValueError:
                print(f"[WARN] Non-JSON response: {r.text}")
                return r.text
        elif r.status_code == 400:
            print(f"[WARN] Bad request when creating BiocCredentials: {r.text}")
        elif r.status_code == 403:
            print(f"[WARN] Unauthorized to create BiocCredentials")
        else:
            print(f"[WARN] Unexpected response ({r.status_code}): {r.text}")
    except requests.RequestException as e:
        print(f"[ERROR] Failed to create BiocCredentials: {e}")
    return None


def identity_resolution(submitter, res, metadata, auth):
    github_id = res.get("github_id")
    email_ids = res.get("email_id") or []
    valid_email_ids = [x for x in email_ids if x]

    warnings = []
    primary_ids = set()

    # Single or No Matches on Email
    if len(valid_email_ids) <= 1:
        email_id = valid_email_ids[0] if valid_email_ids else None

        # if github_id found and a single email_id found
        if github_id and email_id:

            if github_id == email_id:
                primary_ids.add(github_id)
            else:
                warnings.append(
                    f"[WARN] Credential mismatch: github_id={github_id}, email_id={email_id}"
                )
                primary_ids.update([github_id, email_id])
        # if github_id found but no email_id
        elif github_id and not email_id:
            primary_ids.add(github_id)
            warnings.append(
                f"[WARN] Maintainer email not found but submitter has credentials: github_id={github_id}"
            )
        # if email found but no github_id
        elif email_id and not github_id:
            primary_ids.add(email_id)
            warnings.append(
                f"[WARN] Maintainer email Credentials found with no github id. Consider adding to account or mismatch"
            )
            primary_ids.add(submitter)
        else:
            primary_ids.add(submitter)
            warnings.append(
                "[WARN] No BiocCredentials identity found. Creating BiocCredentials Account"
            )
            # Creating BiocCredentials Account
            maintainer_emails = metadata.get("MaintainerEmail") or []
            if isinstance(maintainer_emails, str):
                maintainer_emails = [maintainer_emails]
            email = maintainer_emails[0] if maintainer_emails else None
            if email:
                create_bioccredentials(submitter, email, auth)
            else:
                warnings.append(
                    "[WARN] No Email found cannot create BiocCredential Account"
                )
    # Multiple Maintainer Emails Found
    else:
        # Add all valid maintainer ids
        warnings.append(
            f"[WARN] Multiple maintainers detected: {valid_email_ids}"
        )
        primary_ids.update(valid_email_ids)

        # github_id found
        if github_id:
            primary_ids.add(github_id)
            warnings.append(
                f"[WARN] Id found in BiocCredentials: {github_id}, as well as multiple maintainers: {valid_email_ids}"
            )
        else:
            warnings.append(
                f"[WARN] Github id not found in BiocCredentials, but valid maintainer emails exist: {valid_email_ids}"
            )
            primary_ids.add(submitter)

    return {
        "primary_ids": sorted(primary_ids),
        "warnings": warnings,
    }


# ----------------------------
# Gitolite Helpers
# ----------------------------


def _git_env():
    env = os.environ.copy()
    env["GIT_SSH_COMMAND"] = "ssh -o IdentitiesOnly=yes"
    return env    

def clone_gitolite_admin(dest_dir):
    repo_url = os.environ.get("GITOLITE_ADMIN_REPO")
    if not repo_url:
        raise RuntimeError("GITOLITE_ADMIN_REPO is not set")
    if os.path.exists(dest_dir):
        if not dest_dir.startswith("/tmp/"):
            raise RuntimeError(f"Refusing to delete unsafe path: {dest_dir}")
        print(f"[INFO] Removing existing gitolite-admin clone at {dest_dir}")
        shutil.rmtree(dest_dir)
    print("[INFO] Cloning gitolite-admin...")
    subprocess.run(
        ["git", "clone", repo_url, dest_dir],
        check=True,
        env=_git_env()
    )


def build_gitolite_entry(repository, id_str, hook_suffix):
    return [
        f"repo packages/{repository}",
        f"    RW devel = {id_str}",
        "#    RW RELEASE_3_12 = {id_str}",
        f"    option hook.pre-receive = pre-receive-hook-{hook_suffix}",
        ""
    ]


def update_packages_conf(ids, repository, pkg_type, gitolite_dir):
    conf_path = os.path.join(gitolite_dir, "conf", "packages.conf")
    if not os.path.exists(conf_path):
        raise RuntimeError("packages.conf not found")

    type_map = {
        "Software": "software",
        "ExperimentData": "data-experiment",
        "AnnotationData": "data-annotation",
        "Workflow": "workflows",
        "Book": "books"
    }

    type_str = type_map.get(pkg_type, "software")
    hook_suffix = "software" if type_str == "software" else "dataexp-workflow"
    id_str = " ".join(ids)

    print(f"[INFO] Adding gitolite entry for {repository}")

    with open(conf_path, "r") as f:
        lines = f.read().splitlines()
    while lines and lines[-1].strip() == "":
        lines.pop()
    if lines:
        lines.append("")
    lines.extend(build_gitolite_entry(repository, id_str, hook_suffix))
    with open(conf_path, "w") as f:
        f.write("\n".join(lines) + "\n")

    return True

def commit_and_push_gitolite(gitolite_dir, repository, dry_run=False):

    subprocess.run(["git", "checkout", "master"], check=True, cwd=gitolite_dir, env=_git_env())
    subprocess.run(["git", "pull", "origin", "master"], check=True, cwd=gitolite_dir, env=_git_env())
    subprocess.run(["git", "config", "user.name", "bioc-ci"], check=True, cwd=gitolite_dir, env=_git_env())
    subprocess.run(["git", "config", "user.email", "bioconductorcoreteam@gmail.com"], check=True, cwd=gitolite_dir, env=_git_env())

    changed = subprocess.run(
        ["git", "diff", "--quiet", "conf/packages.conf"],
        cwd=gitolite_dir,
        env=_git_env()
    )
    
    has_changes = changed.returncode != 0
    if has_changes:
        subprocess.run(["git", "add", "conf/packages.conf"], check=True, cwd=gitolite_dir, env=_git_env())
        subprocess.run(["git", "commit", "-m", f"Add package {repository}"], check=True, cwd=gitolite_dir, env=_git_env())

        if not dry_run:
            subprocess.run(["git", "push", "origin", "master"], check=True, cwd=gitolite_dir, env=_git_env())
            print("[INFO] Gitolite config pushed")
        else:
            print("[DRY RUN] Skipping push")
    else:
        print("[INFO] No gitolite changes detected — skipping commit")
	

def configure_gitolite(ids, repository, pkg_type, dry_run=False):
    gitolite_dir = "/tmp/gitolite-admin"
    clone_gitolite_admin(gitolite_dir)
    update_packages_conf(ids, repository, pkg_type, gitolite_dir)
    commit_and_push_gitolite(gitolite_dir, repository, dry_run)


# -----------------------------------------------
# CLONING TO Git@git.bioconductor.org Helpers
# -----------------------------------------------

def clone_github_repo(owner, repo, dest):
    url = f"https://github.com/{owner}/{repo}.git"
    if os.path.exists(dest):
        if not dest.startswith("/tmp/"):
            raise RuntimeError(f"Refusing to delete unsafe path: {dest}")
        shutil.rmtree(dest)
    subprocess.run(
        ["git", "clone", "--single-branch", "--branch", "devel", url, dest],
        check=True,
        env=_git_env()
    )

def set_bioc_remote(repo_dir, repo_name):
    bioc_url = f"git@git.bioconductor.org:packages/{repo_name}.git"
    subprocess.run(
        ["git", "remote", "remove", "origin"],
        cwd=repo_dir,
        check=False
    )
    subprocess.run(
        ["git", "remote", "add", "origin", bioc_url],
        cwd=repo_dir,
        check=True
    )
    
def ensure_devel_branch(repo_dir):
    subprocess.run(
        ["git", "checkout", "-B", "devel"],
        cwd=repo_dir,
        check=True
    )


def push_to_bioc(repo_dir, dry_run=False):
    if dry_run:
        print("[DRY RUN] skipping push")
        return

    subprocess.run(
        ["git", "push", "-u", "origin", "devel"],
        cwd=repo_dir,
        check=True,
        env=_git_env()
    )

def transfer_to_git_bioc(owner, repo, dry_run=False):
    tmp_clone = f"/tmp/{repo}"
    clone_github_repo(owner, repo, tmp_clone)
    ensure_devel_branch(tmp_clone)
    set_bioc_remote(tmp_clone, repo)
    push_to_bioc(tmp_clone, dry_run=dry_run)



# ----------------------------
# Add to Manifest Helpers
# ----------------------------



def clone_manifest_repo(dest):
    repo_url = os.environ.get("MANIFEST_REPO")  # git@git.bioconductor.org:admin/manifest
    if not repo_url:
        raise RuntimeError("MANIFEST_REPO is not set")
    manifest_dir = dest
    if os.path.exists(manifest_dir):
        if not manifest_dir.startswith("/tmp/"):
            raise RuntimeError(f"Refusing to delete unsafe path: {manifest_dir}")
        shutil.rmtree(manifest_dir)
    print("[INFO] Cloning manifest repo...")
    subprocess.run(
        ["git", "clone", repo_url, manifest_dir],
        check=True,
        env=_git_env()
    )
    return manifest_dir


def manifest_file_for_type(pkg_type):
    return {
        "Software": "software.txt",
        "ExperimentData": "data-experiment.txt",
        "AnnotationData": "data-annotation.txt",
        "Workflow": "workflows.txt",
        "Book": "books.txt"
    }.get(pkg_type, "software.txt")


def manifest_has_package(path, package):
    if not os.path.exists(path):
        return False
    needle = f"Package: {package}"
    with open(path) as f:
        return any(line.strip() == needle for line in f)


def add_package_to_manifest(manifest_dir, pkg_type, package):
    file_name = manifest_file_for_type(pkg_type)
    path = os.path.join(manifest_dir, file_name)
    os.makedirs(manifest_dir, exist_ok=True)
    if os.path.exists(path):
        with open(path, "r") as f:
            lines = f.read().splitlines()
    else:
        lines = ["## Blank lines between all entries"]
    while lines and lines[-1].strip() == "":
        lines.pop()
    if manifest_has_package(path, package):
        print(f"[INFO] {package} already in {file_name}")
        return False
    if lines and lines[-1].strip() != "":
        lines.append("")
    lines.append(f"Package: {package}")
    lines.append("")  # maintain spacing invariant
    with open(path, "w") as f:
        f.write("\n".join(lines) + "\n")
    print(f"[INFO] Added {package} to {file_name}")
    return True

def commit_and_push_manifest(manifest_dir, package, pkg_type, dry_run=False):
    subprocess.run(
        ["git", "config", "user.name", "bioc-ci"],
        cwd=manifest_dir,
        check=True
    )
    subprocess.run(
        ["git", "config", "user.email", "bioconductorcoreteam@gmail.com"],
        cwd=manifest_dir,
        check=True
    )
    file_name = manifest_file_for_type(pkg_type)
    subprocess.run(["git", "add", file_name], cwd=manifest_dir, check=True)
    diff = subprocess.run(
        ["git", "diff", "--cached", "--quiet"],
        cwd=manifest_dir
    )
    if diff.returncode == 0:
        print("[INFO] No manifest changes")
        return
    subprocess.run(
        ["git", "commit", "-m", f"Add package {package} to manifest"],
        cwd=manifest_dir,
        check=True
    )
    if dry_run:
        print("[DRY RUN] skipping manifest push")
        return
    subprocess.run(
        ["git", "push", "origin", "devel"],
        cwd=manifest_dir,
        check=True,
        env=_git_env()
    )
    print("[INFO] Manifest updated")


def update_manifest(pkg_type, repo, dry_run=False):
    dest_dir = "/tmp/manifest"
    manifest_dir = clone_manifest_repo(dest_dir)
    changed = add_package_to_manifest(manifest_dir, pkg_type, repo)
    if not changed:
        return
    commit_and_push_manifest(manifest_dir, repo, pkg_type, dry_run=dry_run)


# ----------------------------
# DOI Helper
# ----------------------------

def normalize_authors(authors):
    if not authors:
        return []

    if isinstance(authors, list):
        return [a.strip() for a in authors if a.strip()]

    # split on comma OR semicolon, but not inside parentheses etc.
    return [a.strip() for a in re.split(r"[;,]", authors) if a.strip()]

def generate_bioc_pkg_doi(pkg, authors, pubyear=None, event="publish", testing=True):

    if event not in {"hide", "register", "publish"}:
        raise ValueError("event must be 'hide', 'register', or 'publish'")

    if pubyear is None:
        pubyear = datetime.utcnow().year

    if testing:
        bioc_prefix = "10.82962"
        base_url = "https://api.test.datacite.org/dois"
        DATACITE_USERNAME = os.environ.get("DATACITE_TESTING_USERNAME")
        DATACITE_PASSWORD = os.environ.get("DATACITE_TESTING_PASSWORD")
    else:
        bioc_prefix = "10.18129"
        base_url = "https://api.datacite.org/dois"
        DATACITE_USERNAME = os.environ.get("DATACITE_USERNAME")
        DATACITE_PASSWORD = os.environ.get("DATACITE_PASSWORD")

    bioc_namespace = "B9.bioc"
    pkg_doi = f"{bioc_prefix}/{bioc_namespace}.{pkg}"

    authors_list = normalize_authors(authors)

    payload = {
        "data": {
            "id": f"https://doi.org/{pkg_doi}",
            "doi": pkg_doi.upper(),
            "attributes": {
                "doi": pkg_doi,
                "event": event,
                "prefix": bioc_prefix,
                "suffix": f"{bioc_namespace}.{pkg}",
                "creators": [{"name": a} for a in authors_list],
                "titles": [{"title": pkg}],
                "url": f"https://bioconductor.org/packages/{pkg}",
                "publisher": "Bioconductor",
                "publicationYear": pubyear,
                "types": {
                    "resourceTypeGeneral": "Software"
                }
            }
        }
    }

    auth = base64.b64encode(
        f"{DATACITE_USERNAME}:{DATACITE_PASSWORD}".encode()
    ).decode()

    headers = {
        "Authorization": f"Basic {auth}",
        "Content-Type": "application/vnd.api+json",
        "Accept": "application/vnd.api+json"
    }

    r = requests.post(base_url, headers=headers, json=payload, timeout=20)

    if 200 <= r.status_code < 300:
        return True
    try:
        error_body = r.json()
    except Exception:
        error_body = r.text

    print(f"DOI creation failed: HTTP {r.status_code}\nResponse: {error_body}")
    return False


# ----------------------------
# TEMPBIOC CLEANUP
# ----------------------------

def delete_temp_repo(repo_name):
    url = f"https://api.github.com/repos/{GIT_TARGET_ORG}/{repo_name}"
    r = requests.delete(url, headers=TEMP_BIOC_HEADERS)

    if r.status_code == 204:
        print(f"[INFO] Deleted repo {repo_name}")
    elif r.status_code == 404:
        print(f"[INFO] Repo {repo_name} does not exist")
    else:
        print(f"[WARN] Failed to delete repo: {r.status_code} {r.text}")


def remove_from_registry(repo_name):
    registry_repo = "tempbioc.r-universe.dev"

    url = f"https://api.github.com/repos/{GIT_TARGET_ORG}/{registry_repo}/contents/packages.json"
    r = requests.get(url, headers=TEMP_BIOC_HEADERS)

    if r.status_code != 200:
        print("[WARN] Could not fetch registry")
        return

    data = r.json()
    content = json.loads(base64.b64decode(data["content"]).decode())

    new_content = [x for x in content if x.get("package") != repo_name]

    if len(new_content) == len(content):
        print(f"[INFO] {repo_name} not found in registry")
        return

    updated = json.dumps(new_content, indent=2)

    r = requests.put(url, headers=TEMP_BIOC_HEADERS, json={
        "message": f"Remove {repo_name}",
        "content": base64.b64encode(updated.encode()).decode(),
        "sha": data["sha"]
    })
    if r.status_code >= 300:
        print(f"[WARN] Failed to update registry: {r.status_code} {r.text}")

    print(f"[INFO] Removed {repo_name} from registry")



# ----------------------------
# Main Handler
# ----------------------------
def main():
    # --------------------------------------------
    # Grab event details
    # --------------------------------------------
    with open(EVENT_PATH) as f:
        event = json.load(f)

    # --------------------------------------------
    # extract data
    # --------------------------------------------

    issue = event["issue"]
    issue_number = issue["number"]
    issue_body = issue.get("body") or ""
    label_name = event["label"]["name"]
    actor = event.get("sender", {}).get("login")
    submitter = issue.get("user", {}).get("login")

    # --------------------------------------------
    # Only proceed if the label added is correct
    # --------------------------------------------

    if label_name != "package accepted":
        print(f"Label '{label_name}' is not 'package accepted', exiting.")
        sys.exit(0)

    # --------------------------------------------
    # Verify Label added by member of review team
    # --------------------------------------------
    if not is_team_member(actor):
        post_comment(issue_number, f"User '{actor}' is not allowed to accept package. Exiting.")
        remove_label(issue_number, "package accepted")
        sys.exit(0)

    # --------------- -----------------------------
    # extra repo
    # --------------------------------------------
    owner, repo = extract_repo(issue_body)
    if repo and repo.endswith(".git"):
        repo = repo[:-4]

    if not owner or not repo:
        print("[ERROR] Could not extract repository from issue body")
        sys.exit(1)

    # --------------------------------------------
    # package type based on bioctype / biocViews
    # --------------------------------------------
    desc_text = get_description_file(owner, repo)
    if not desc_text:
        print("[ERROR] DESCRIPTION file not found — cannot proceed")
        sys.exit(1)
    sqlite_path = "/tmp/biocViews.sqlite"
    download_biocviews_sqlite(sqlite_path)
    term_to_category = load_biocviews_vocab(sqlite_path)
    metadata = parse_description(desc_text)
    pkg_type = determine_package_type(metadata, term_to_category)
    print(f"[INFO] Package type: {pkg_type}")    

    # --------------------------------------------
    # If needed create BiocCredentials account
    #   Return id(s) to use in gitolite config
    # --------------------------------------------
    auth = bioc_credentials_auth()
    res = bioc_credentials_lookup(
        github_id=submitter,
        maintainer_email=metadata.get('MaintainerEmail'),
        auth=auth
    )
    evaluate_ids = identity_resolution(submitter, res, metadata, auth)

    warnings = evaluate_ids.get("warnings", [])
    if warnings:
        print("\n".join(warnings))

    valid_ids = evaluate_ids.get('primary_ids') or [submitter]

    pipeline_success = True

    # --------------------------------------------
    # Add to gitolite configuration with ids
    #   and correct hook based on package type
    # --------------------------------------------

    try:
        configure_gitolite(valid_ids, repo, pkg_type, dry_run=False)
    except Exception as e:
        print(f"[ERROR] Gitolite configuration failed: {e}")
        pipeline_success = False

    if not pipeline_success:
        closing_comment="""ERROR:
There was an ERROR adding the package to the official repository configurations
We appreciate your patience as we investigate
"""
        post_comment(issue_number, closing_comment)
        sys.exit(1)

    # --------------------------------------------
    # Clone to git.bioconductor.org
    # --------------------------------------------
    try:
        transfer_to_git_bioc(owner, repo, dry_run=False)
    except Exception as e:
        print(f"[ERROR] Cloning to git.bioconductor.org failed: {e}")
        pipeline_success = False

    if not pipeline_success:
        closing_comment="""ERROR:
There was an ERROR cloning the package to the official repository
We appreciate your patience as we investigate
"""
        post_comment(issue_number, closing_comment)
        sys.exit(1)
	
    # --------------------------------------------
    # Add to manifest based on package type
    # --------------------------------------------

    try:
        update_manifest(pkg_type, repo, dry_run=False)
    except Exception as e:
        print(f"[ERROR] Adding to Manifest failed: {e}")
        closing_comment="""ERROR:
There was an ERROR adding the package to the manifest
We appreciate your patience as we investigate
"""
        post_comment(issue_number, closing_comment)

    # --------------------------------------------
    # Create DOI
    # --------------------------------------------

    try:
        doi = generate_bioc_pkg_doi(
            pkg=repo,
            authors=metadata.get("Authors") or [submitter],
            pubyear=datetime.utcnow().year,
            testing=True
        )
    except Exception as e:
        print(f"[ERROR] DOI Failure: {e}")
        doi = False

    if doi:
        print("[INFO] DOI Created")
    else:
        print("[WARN] DOI creation failed")

    # --------------------------------------------
    # delete clone and from registry for SPB
    #   only if gitolite and clone successful
    #   manifest and DOI do not need to be successful
    #     those can be manually fixed
    #     while package still building on daily
    # --------------------------------------------
    if repo:
        if pipeline_success:
            delete_temp_repo(repo)
            remove_from_registry(repo)
        else:
            print("[WARN] Either gitolite configuration or cloning failed.")
            print("[Skip] Skipping removal from SPB for debugging.")
    else:
        print(f"[WARN] Could not extract repo from issue body: {issue_number} ")

    # --------------------------------------------
    # clean up labels
    #    policies should be accepted so no need to check
    #      awaiting policy acceptance
    # keep policies-accepted on accepted packages
    #    ensures that maintainer new policies when officially accepted
    # --------------------------------------------
    LABELS_TO_REMOVE = {
        "Build Error",
        "Build Note",
        "Build OK",
        "Build Unknown",
        "Build Warning",
        "pre-review",
        "precheck-passed",
        "review in progress",
    }
    existing_labels = get_issue_labels(issue_number)
    to_remove = LABELS_TO_REMOVE & existing_labels
    for lbl in to_remove:
        remove_label(issue_number, lbl)


    # --------------------------------------------
    # Post Comment and Close Issue
    # --------------------------------------------
    closing_comment = f"""🏁 Package Accepted!

Congratulations! Your package has been accepted to Bioconductor

Your package has been removed from the new submission location and registry. 
It has been added to the official Bioconductor devel system. 

If you want to push command line updates, you need to update your remotes:
```
  git remote remove tempbioc
  git remote add bioc git@git.bioconductor.org:packages/{repo}
  git push bioc devel
```

Bioconductor still uses **devel** as its default branch.
If you use a different branch (example: main) map branches when pushing:
``` 
  git push bioc main:devel
```
Other useful documentation for pushing to git.bioconductor.org may be found
[here](http://contributions.bioconductor.org/git-version-control.html#new-package-workflow)

Currently we use ssh-keys for git.bioconductor.org access. To manage your keys
and future access, activate your [BiocCredentials
Account](https://git.bioconductor.org/BiocCredentials)

Reminder:
Packages must remain ERROR free to avoid deprecation and removal. It is your
responsibility to check for ERRORs and to fix in a timely manner. The maintainer
email in the DESCRIPTION should remain active for future communications and
notifications. 

Thank you for participating in the review process.
Welcome to Bioconductor.
"""

    post_comment(issue_number, closing_comment)
    time.sleep(3)
    close_issue(issue_number)

if __name__ == "__main__":
    main()
