#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

"""Parse FIT data from a Garmin Xero chronograph and convert it to JSON."""
import argparse
import json
import pathlib
import typing as T

import garmin_fit_sdk


def convert_mm_to_ft(meas_in_mm: int) -> float:
    """Convert a measurement in mm into feet."""
    return meas_in_mm / 25.6 / 12


def parse_fit(inpath: pathlib.Path) -> T.List[float]:
    """Parse the FIT data file for a session, returning a sorted list of
    velocity measurements.
    """
    stream = garmin_fit_sdk.Stream.from_file(inpath)
    decoder = garmin_fit_sdk.Decoder(stream)
    messages, _ = decoder.read(convert_datetimes_to_dates=False)
    # TODO: Use the actual message name/fields once Garmin updates the SDK
    return [
        convert_mm_to_ft(message[0])
        for message in sorted(messages["388"], key=lambda m: m[1])
    ]


def create_org_table(data: T.List[float]) -> str:
    """Create an orgmode table summarizing the given shot data."""
    result = """| shot  | speed     |
|-------+-----------|
|   avg |           |
| stdev |           |
"""
    result += "\n".join(
        f"| {i + 1:>5} | {speed:>9.1f} |" for i, speed in enumerate(data)
    )
    result += "\n#+TBLFM: @2$2=vmean(@4$2..@>$2)::@3$2=vsdev(@4$2..@>$2)"
    return result


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "input", type=pathlib.Path, help="Path to the FIT file to parse"
    )
    parser.add_argument(
        "--output",
        "-o",
        type=pathlib.Path,
        help="Path to the output file to write; defaults to stdout.",
    )
    formats = parser.add_mutually_exclusive_group()
    formats.add_argument(
        "--json", "-j", action="store_true", help="Write JSON-formatted data (default)"
    )
    formats.add_argument(
        "--org", "-O", action="store_true", help="Write Org-formatted data"
    )
    args = parser.parse_args()
    data = parse_fit(args.input)

    if args.org:
        encoded = create_org_table(data)
    else:
        encoded = json.dumps(data)

    if args.output:
        args.output.write_text(encoded)
    else:
        print(encoded)


if __name__ == "__main__":
    main()
