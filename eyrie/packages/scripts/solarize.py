#!/usr/bin/env python3

# SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

"""Convert JPEG images to use the solarized color scheme."""

import argparse
import itertools
import os
import pathlib
import typing as T

from PIL import Image
from rich.progress import Progress

SOLARIZED_COLORS = [
    (0x00, 0x2B, 0x36),  # base03
    (0x07, 0x36, 0x42),  # base02
    (0x58, 0x6E, 0x75),  # base01
    (0x65, 0x7B, 0x83),  # base00
    (0x83, 0x94, 0x96),  # base0
    (0x93, 0xA1, 0xA1),  # base1
    (0xEE, 0xE8, 0xD5),  # base2
    (0xFD, 0xF6, 0xE3),  # base3
    (0xB5, 0x89, 0x00),  # yellow
    (0xCB, 0x4B, 0x16),  # orange
    (0xDC, 0x32, 0x2F),  # red
    (0xD3, 0x36, 0x82),  # magenta
    (0x6C, 0x71, 0xC4),  # violet
    (0x26, 0x8B, 0xD2),  # blue
    (0x2A, 0xA1, 0x98),  # cyan
    (0x85, 0x99, 0x00),  # green
]


def solarize(px: T.Tuple[int, int, int]) -> T.Tuple[int, int, int]:
    (r, g, b) = px
    distance = 255**2 * 3  # note that distance will never be this high
    result = (0, 0, 0)

    for (r_s, g_s, b_s) in SOLARIZED_COLORS:
        this_distance = (r - r_s) ** 2 + (g - g_s) ** 2 + (b - b_s) ** 2
        if this_distance < distance:
            distance = this_distance
            result = (r_s, g_s, b_s)

    return result


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


def get_unconverted_images(
    *,
    input_dir: pathlib.Path,
    output_dir: pathlib.Path,
    min_rating: T.Optional[int] = None,
) -> T.Set[pathlib.Path]:
    """Get the set of images that need to be converted from a directory.

    This gets all *.JPG and *.jpg images in the input directory that do not
    already exist in the output directory and (optionally) have the given
    minimum rating.
    """
    result = set()

    for image in itertools.chain(
        input_dir.glob("*.jpg"), input_dir.glob("*.JPG")
    ):
        if (output_dir / image.name).exists():
            continue
        if min_rating is not None:
            rating = get_exif_rating(image)
            if rating < min_rating:
                continue
        result.add(image)

    return result


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
        required=True,
        help="output directory to store the resulting images",
    )
    parser.add_argument(
        "--rating",
        "-r",
        type=int,
        help="only convert images with at least this rating in the EXIF data",
    )
    args = parser.parse_args()

    images_to_convert = get_unconverted_images(
        input_dir=args.input, output_dir=args.output, min_rating=args.rating
    )
    args.output.mkdir(parents=True, exist_ok=True)

    with Progress() as progress:
        overall_progress = progress.add_task(
            "converting...", total=len(images_to_convert)
        )
        image_progress = progress.add_task("...")
        for image in images_to_convert:
            with Image.open(image) as im:
                (width, height) = im.size
                px = im.load()
                progress.reset(
                    image_progress,
                    description=os.path.basename(image.name),
                    total=width * height,
                )
                for (x, y) in itertools.product(range(width), range(height)):
                    px[x, y] = solarize(px[x, y])
                    progress.advance(image_progress)

                im.save(args.output / image.name)
                progress.advance(overall_progress)


if __name__ == "__main__":
    main()
