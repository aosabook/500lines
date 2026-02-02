from collections import Counter
from email import message_from_binary_file
from pathlib import Path
from typing import Iterable, List, Tuple
import importlib.util
import re

WORD_RE = re.compile(r"[A-Za-z0-9']+")


def _tokenize(text: str) -> List[str]:
    return [token.lower() for token in WORD_RE.findall(text) if len(token) > 2]


def _summarize_subjects(subjects: Iterable[str], limit: int = 8) -> List[Tuple[str, int]]:
    counter: Counter[str] = Counter()
    for subject in subjects:
        counter.update(_tokenize(subject))
    return counter.most_common(limit)


def outlook_available() -> bool:
    return importlib.util.find_spec("win32com.client") is not None


def analyze_recent_outlook_emails(limit: int = 25) -> str:
    if not outlook_available():
        raise RuntimeError("Outlook MAPI is not available. Use .eml files instead.")

    import win32com.client

    outlook = win32com.client.Dispatch("Outlook.Application").GetNamespace("MAPI")
    inbox = outlook.GetDefaultFolder(6)
    messages = inbox.Items
    messages.Sort("[ReceivedTime]", True)

    senders: List[str] = []
    subjects: List[str] = []
    collected = 0

    for message in messages:
        if collected >= limit:
            break
        try:
            senders.append(str(message.SenderName))
            subjects.append(str(message.Subject))
            collected += 1
        except AttributeError:
            continue

    return _format_email_summary(collected, senders, subjects)


def analyze_eml_file(file_path: str) -> str:
    path = Path(file_path)
    with path.open("rb") as handle:
        message = message_from_binary_file(handle)

    sender = message.get("From", "(unknown)")
    subject = message.get("Subject", "(no subject)")

    summary = [
        "EML analysis:",
        f"From: {sender}",
        f"Subject: {subject}",
    ]

    tokens = _tokenize(subject)
    if tokens:
        summary.append("Subject keywords: " + ", ".join(tokens))

    return "\n".join(summary)


def _format_email_summary(count: int, senders: List[str], subjects: List[str]) -> str:
    lines = [
        f"Emails analyzed: {count}",
        "",
        "Top senders:",
    ]

    for sender, total in Counter(senders).most_common(8):
        lines.append(f"- {sender}: {total}")

    lines.extend(["", "Top subject keywords:"])
    for keyword, total in _summarize_subjects(subjects):
        lines.append(f"- {keyword}: {total}")

    return "\n".join(lines)
