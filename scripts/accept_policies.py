import os
import json
import sys
import requests

GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]
EVENT_PATH = os.environ["GITHUB_EVENT_PATH"]
REPO_FULL = os.environ["GITHUB_REPOSITORY"]

HEADERS = {
    "Authorization": f"Bearer {GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json"
}


def github_post(url, payload):
    return requests.post(url, headers=HEADERS, json=payload)


def post_comment(body):
    with open(EVENT_PATH) as f:
        event = json.load(f)

    issue_number = event["issue"]["number"]
    owner, repo = REPO_FULL.split("/")

    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/comments"
    github_post(url, {"body": body})


def add_label(label):
    with open(EVENT_PATH) as f:
        event = json.load(f)

    issue_number = event["issue"]["number"]
    owner, repo = REPO_FULL.split("/")

    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels"
    github_post(url, {"labels": [label]})


def remove_label(label):
    """Remove a GitHub label from the issue."""
    with open(EVENT_PATH) as f:
        event = json.load(f)

    issue_number = event["issue"]["number"]
    owner, repo = REPO_FULL.split("/")

    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels/{label}"
    requests.delete(url, headers=HEADERS)


def main():
    with open(EVENT_PATH) as f:
        event = json.load(f)

    comment_body = event["comment"]["body"].strip().lower()
    commenter = event["comment"]["user"]["login"]
    issue_author = event["issue"]["user"]["login"]

    # Allow both "/accept-policies" and "accept-policies"
    if comment_body not in ["/accept-policies", "accept-policies"]:
        # Ignore other comments
        sys.exit(0)

    if commenter != issue_author:
        post_comment("Only the original submitter can accept the Bioconductor policies.")
        sys.exit(1)

    # Add label to indicate policies accepted
    add_label("policies-accepted")
    add_label("pre-review")
    remove_label("precheck-passed") 
    remove_label("awaiting policy acceptance")  

    post_comment(
        "✅ Bioconductor policies accepted.\n\n"
        "Your submission will now proceed to the build and check process.\n"
        "When build/check is clean a reviewer will be assigned for indepth review."
    )


if __name__ == "__main__":
    main()
