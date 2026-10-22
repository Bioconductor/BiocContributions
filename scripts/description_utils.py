"""Helpers for parsing a Bioconductor package DESCRIPTION file.

Shared by accept_package.py and create_missing_dois.py. No environment access at import.
"""
import re


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
