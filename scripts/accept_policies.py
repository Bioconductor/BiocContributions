import os
import json
import sys
import requests

GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]
EVENT_PATH = os.environ["GITHUB_EVENT_PATH"]
REPO_FULL = os.environ["GITHUB_REPOSITORY"]
AUTO_ACCEPT = os.environ.get("AUTO_ACCEPT") == "true"

HEADERS = {
    "Authorization": f"Bearer {GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json"
}


def github_post(url, payload):
    return requests.post(url, headers=HEADERS, json=payload)


def post_comment(issue_number, body):
    with open(EVENT_PATH) as f:
        event = json.load(f)

    #issue_number = event["issue"]["number"]
    owner, repo = REPO_FULL.split("/")

    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/comments"
    github_post(url, {"body": body})


def add_label(issue_number, label):
    with open(EVENT_PATH) as f:
        event = json.load(f)

    #issue_number = event["issue"]["number"]
    owner, repo = REPO_FULL.split("/")

    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels"
    github_post(url, {"labels": [label]})


def remove_label(issue_number, label):
    """Remove a GitHub label from the issue."""
    with open(EVENT_PATH) as f:
        event = json.load(f)

    #issue_number = event["issue"]["number"]
    owner, repo = REPO_FULL.split("/")

    url = f"https://api.github.com/repos/{owner}/{repo}/issues/{issue_number}/labels/{label}"
    requests.delete(url, headers=HEADERS)


def main():
    with open(EVENT_PATH) as f:
        event = json.load(f)

    if "issue" in event:
        comment_body = event["comment"]["body"].strip().lower()
        commenter = event["comment"]["user"]["login"]
        issue_author = event["issue"]["user"]["login"]
        issue_number = event["issue"]["number"]
    elif AUTO_ACCEPT:
        issue_number = int(os.environ.get("ISSUE_NUMBER", 1))
        comment_body = "/accept-policies"
        commenter = issue_author = event.get("sender", {}).get("login")
    else:
        sys.exit(0)
   
    # Allow both "/accept-policies" and "accept-policies"
    if comment_body not in ["/accept-policies", "accept-policies"]:
        # Ignore other comments
        sys.exit(0)

    if commenter != issue_author:
        post_comment(issue_number, "Only the original submitter can accept the Bioconductor policies.")
        sys.exit(1)

    # Add label to indicate policies accepted
    add_label(issue_number, "policies-accepted")
    remove_label(issue_number, "awaiting policy acceptance")  

    post_comment(issue_number,
        "✅ Bioconductor policies accepted.\n\n"
        "Your submission will now proceed to the build and check process.\n"
        "When build/check is clean a reviewer will be assigned for indepth review."
    )


if __name__ == "__main__":
    main()
