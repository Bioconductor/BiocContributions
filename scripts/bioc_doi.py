"""DataCite DOI helpers for Bioconductor packages.

No environment access at import time. Credentials are read when a call is made.

DOI pattern: <prefix>/B9.bioc.<package>
  production prefix: 10.18129  (api.datacite.org)
  testing prefix:    10.82962  (api.test.datacite.org)

A findable DOI cannot be deleted, so callers should default to testing=True and
only use production when explicitly asked.
"""
import os
import re
import requests

NAMESPACE = "B9.bioc"

_CONFIG = {
    True: {
        "prefix": "10.82962",
        "base_url": "https://api.test.datacite.org",
        "user_env": "DATACITE_TESTING_USERNAME",
        "pass_env": "DATACITE_TESTING_PASSWORD",
    },
    False: {
        "prefix": "10.18129",
        "base_url": "https://api.datacite.org",
        "user_env": "DATACITE_USERNAME",
        "pass_env": "DATACITE_PASSWORD",
    },
}


def doi_name(pkg, testing=True):
    return f"{_CONFIG[testing]['prefix']}/{NAMESPACE}.{pkg}"


def doi_url(pkg, testing=True):
    return f"{_CONFIG[testing]['base_url']}/dois/{doi_name(pkg, testing)}"


def _auth(testing):
    cfg = _CONFIG[testing]
    user = os.environ.get(cfg["user_env"])
    password = os.environ.get(cfg["pass_env"])
    if not user or not password:
        raise RuntimeError(
            "Missing DataCite credentials"
        )
    return (user, password)


def normalize_authors(authors):
    if not authors:
        return []
    if isinstance(authors, list):
        return [a.strip() for a in authors if a and a.strip()]
    return [a.strip() for a in re.split(r"[;,]", authors) if a.strip()]


def doi_exists(pkg, testing=True):
    """True if DataCite already has a DOI for the package, False if not.

    Raises on any other response so a DataCite outage is never read as
    'no DOI'. Reading a DOI needs no credentials.
    """
    r = requests.get(
        doi_url(pkg, testing),
        headers={"Accept": "application/vnd.api+json"},
        timeout=20,
    )
    if r.status_code == 200:
        return True
    if r.status_code == 404:
        return False
    raise RuntimeError(f"DataCite lookup failed: HTTP {r.status_code} {r.text[:300]}")


def build_payload(pkg, authors, pubyear, testing=True, event="publish"):
    cfg = _CONFIG[testing]
    authors_list = normalize_authors(authors)
    if not authors_list:
        raise ValueError(f"No authors available for {pkg}; DataCite requires creators")
    return {
        "data": {
            "type": "dois",  # required by the DataCite JSON:API; was missing before
            "attributes": {
                "doi": doi_name(pkg, testing),
                "event": event,
                "creators": [{"name": a} for a in authors_list],
                "titles": [{"title": pkg}],
                "url": f"https://bioconductor.org/packages/{pkg}",
                "publisher": "Bioconductor",
                "publicationYear": int(pubyear),
                "types": {"resourceTypeGeneral": "Software"},
            },
        }
    }


def create_doi(pkg, authors, pubyear, testing=True, event="publish"):
    """Create a DOI. Returns (ok, message). Never raises on an HTTP error."""
    if event not in {"hide", "register", "publish"}:
        raise ValueError("event must be 'hide', 'register', or 'publish'")
    payload = build_payload(pkg, authors, pubyear, testing, event)
    r = requests.post(
        f"{_CONFIG[testing]['base_url']}/dois",
        auth=_auth(testing),
        headers={
            "Content-Type": "application/vnd.api+json",
            "Accept": "application/vnd.api+json",
        },
        json=payload,
        timeout=30,
    )
    if 200 <= r.status_code < 300:
        return True, f"created {doi_name(pkg, testing)}"
    return False, f"HTTP {r.status_code}: {r.text[:500]}"
