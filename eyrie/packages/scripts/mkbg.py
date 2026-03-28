#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2026 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

"""Symlink background photos."""

import argparse
import itertools
import pathlib
import typing as T

from PIL import Image


def get_exif_rating(image: pathlib.Path) -> int:
    """Get the photo rating from the EXIF metadata."""
    with Image.open(image) as im:
        return int(
            im.getxmp()
            .get("xmpmeta", {})
            .get("RDF", {})
            .get("Description", {})
            .get("Rating", "0")
        )


def highly_rated_images(
    *,
    input_dir: pathlib.Path,
    min_rating: T.Optional[int],
) -> T.Set[pathlib.Path]:
    """
    Get the set of images that need to be symlinked from a directory.
    """
    return {
        image
        for image in itertools.chain(
            input_dir.glob("*.jpg"), input_dir.glob("*.JPG")
        )
        if get_exif_rating(image) >= min_rating
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "input",
        type=pathlib.Path,
        help="input directory containing .jpg files to convert",
    )
    parser.add_argument(
        "--output",
        "-o",
        type=pathlib.Path,
        default=pathlib.Path.home() / "backgrounds",
        help="output directory to store the symlinks",
    )
    parser.add_argument(
        "--rating",
        "-r",
        type=int,
        default=3,
        help="only link images with at least this rating in the EXIF data",
    )
    args = parser.parse_args()

    args.output.mkdir(parents=True, exist_ok=True)
    for image in highly_rated_images(
        input_dir=args.input, min_rating=args.rating
    ):
        output = args.output / image.name
        if not output.is_symlink():
            output.symlink_to(image)


if __name__ == "__main__":
    main()
