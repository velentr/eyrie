#!/usr/bin/env python3
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


def _main() -> None:
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
    args = parser.parse_args()
    data = parse_fit(args.input)
    encoded = json.dumps(data)
    if args.output:
        args.output.write_text(encoded)
    else:
        print(encoded)


if __name__ == "__main__":
    _main()
