# Ni Agent2

Ni Agent2 is a lightweight desktop AI-style assistant built with Python. It focuses on three productivity workflows:

- Generate PowerPoint decks from a structured outline.
- Analyze Excel/CSV data (shape, schema, missing values, summary stats).
- Analyze Outlook email data (via Outlook MAPI or `.eml` files).

## Features

### PPT Generator
Provide a deck title and a slide outline (one slide per line) to generate a `.pptx` file.

### Excel Analyzer
Load `.xlsx` or `.csv` files and see schema, missing counts, and summary statistics.

### Outlook Analyzer
- **Windows + Outlook installed**: read recent emails from Inbox using MAPI.
- **Cross-platform**: analyze exported `.eml` files.

## Setup

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## Run

```bash
python -m ni_agent2.app
```

## Outline Format

Each line represents one slide:

```
Slide Title | bullet 1; bullet 2; bullet 3
```

Example:

```
Project Update | milestones complete; budget on track; risks: vendor delay
Next Steps | finalize QA; prepare demo; launch plan
```

## Notes

- Outlook MAPI access is only available on Windows with Outlook installed.
- `.eml` analysis works on all platforms.
