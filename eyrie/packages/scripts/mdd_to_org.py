#!/usr/bin/env python3
"""
Convert the Mammal Diversity Database to an org-mode TODO list.
"""

import argparse
import csv
import pathlib


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "input", type=pathlib.Path, help="Path to the input file to process"
    )
    args = parser.parse_args()

    with open(args.input) as csvfile:
        reader = csv.reader(csvfile)
        cur_family = ""
        print("#+title: Mammals")
        print("#+author: Brian Kubisiak")
        print()
        print("* Mammals [0%]")
        for row in reader:
            common_name = row[3]
            other_names = row[4]
            family = row[14]
            extinct = row[42] == "1"
            # domesticated = row[43] == "1"
            if extinct:
                continue
            if family == "family":
                continue
            if family != cur_family:
                cur_family = family
                print(f"** {cur_family} [0%]")
            print(f"*** TODO {common_name}")
            if other_names and other_names.strip():
                print("Also known as:")
                for other_name in other_names.strip().split("|"):
                    if other_name.strip():
                        print(f"- {other_name.strip()}")


if __name__ == "__main__":
    main()
