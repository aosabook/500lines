from pathlib import Path
from typing import Tuple

import pandas as pd


def load_dataframe(file_path: str) -> Tuple[pd.DataFrame, str]:
    path = Path(file_path)
    if path.suffix.lower() == ".csv":
        return pd.read_csv(path), "csv"
    return pd.read_excel(path), "excel"


def analyze_excel(file_path: str) -> str:
    dataframe, source_type = load_dataframe(file_path)
    rows, columns = dataframe.shape

    lines = [
        f"Source type: {source_type}",
        f"Rows: {rows}",
        f"Columns: {columns}",
        "",
        "Column schema:",
    ]

    for column in dataframe.columns:
        lines.append(f"- {column}: {dataframe[column].dtype}")

    lines.extend(["", "Missing values:"])
    missing = dataframe.isna().sum()
    for column, count in missing.items():
        lines.append(f"- {column}: {count}")

    lines.extend(["", "Summary statistics:"])
    summary = dataframe.describe(include="all").fillna("")
    lines.append(summary.to_string())

    return "\n".join(lines)
