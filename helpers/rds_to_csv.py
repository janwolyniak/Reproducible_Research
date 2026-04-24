from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Convert RDS files into CSV files."
    )
    parser.add_argument(
        "input_path",
        type=Path,
        help="Path to an .rds file or a directory containing .rds files.",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="Optional output file or directory. Defaults next to the input data.",
    )
    parser.add_argument(
        "--encoding",
        default="utf-8",
        help="CSV encoding. Defaults to utf-8.",
    )
    return parser


def load_rds(path: Path) -> pd.DataFrame:
    try:
        import pyreadr
    except ImportError as exc:
        raise SystemExit(
            "pyreadr is required to read .rds files. Install it with `pip install pyreadr`."
        ) from exc

    result = pyreadr.read_r(path)
    if not result:
        raise ValueError(f"No objects found in {path}.")

    frame = next(iter(result.values()))
    if not isinstance(frame, pd.DataFrame):
        raise TypeError(
            f"{path} does not contain a data frame and cannot be exported to CSV."
        )
    return frame


def resolve_output_path(input_file: Path, output: Path | None) -> Path:
    if output is None:
        return input_file.with_suffix(".csv")
    if output.suffix.lower() == ".csv":
        return output
    return output / f"{input_file.stem}.csv"


def convert_file(input_file: Path, output: Path | None, encoding: str) -> Path:
    frame = load_rds(input_file)
    output_file = resolve_output_path(input_file, output)
    output_file.parent.mkdir(parents=True, exist_ok=True)
    frame.to_csv(output_file, index=False, encoding=encoding)
    return output_file


def iter_rds_files(input_path: Path) -> list[Path]:
    if input_path.is_file():
        if input_path.suffix.lower() != ".rds":
            raise ValueError(f"{input_path} is not an .rds file.")
        return [input_path]

    if input_path.is_dir():
        files = sorted(input_path.glob("*.rds"))
        if not files:
            raise ValueError(f"No .rds files found in {input_path}.")
        return files

    raise FileNotFoundError(f"Input path does not exist: {input_path}")


def main() -> None:
    parser = build_parser()
    args = parser.parse_args()

    input_path = args.input_path.resolve()
    output = args.output.resolve() if args.output else None
    files = iter_rds_files(input_path)

    if len(files) > 1 and output and output.suffix.lower() == ".csv":
        raise SystemExit("A CSV file output path can only be used with a single input file.")

    for file_path in files:
        output_file = convert_file(file_path, output, args.encoding)
        print(f"Converted {file_path} -> {output_file}")


if __name__ == "__main__":
    main()
