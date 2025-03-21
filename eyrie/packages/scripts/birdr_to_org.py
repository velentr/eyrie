#!/usr/bin/env python3
"""
Convert a birdr database to an org-mode TODO list.
"""

import pathlib
import sqlite3
import textwrap

SELECT_ALL_SPECIES = """
SELECT species.id, species.name, category.name
FROM (species JOIN category ON species.category_id = category.id)
ORDER BY category.name;
"""

SELECT_SIGHTINGS = """
SELECT year, month, day, location, notes
FROM sightings
WHERE species_id = ?;
"""

SELECT_CHECKLISTS = """
SELECT checklist.name
FROM species_checklist JOIN checklist
  ON species_checklist.checklist_id = checklist.id
WHERE species_checklist.species_id = ?;
"""

CHECKLIST_TAGS = {
    "American Upland Game": "game",
    "Channel Islands National Park": "chnis_np",
    "Costa Rica (Cocos Island)": "cocos",
    "Costa Rica": "costarica",
    "Death Valley National Park": "dv_np",
    "Denali National Park": "dena_np",
    "Galapagos Islands (Endemic)": "gal_end",
    "Glacier National Park": "glac_np",
    "Montana": "mt",
    "New World Vultures": "nw_vult",
    "North American Diurnal Raptors": "na_rapt",
    "North American Pelicans": "na_peli",
    "Penguins": "peng",
    "Pinnacles National Park": "pinn_np",
    "State Birds": "states",
}


def main() -> None:
    db = sqlite3.connect(pathlib.Path.home() / ".local/share/birdr/birds.db")

    print("#+title: Birds")
    print("#+author: Brian Kubisiak")

    for name, tag in CHECKLIST_TAGS.items():
        print(f"#+lifelist_{tag}: {name}")

    print()
    print("* Birds [%]")

    species_cur = db.cursor()
    cur_category = ""
    for id, name, category in species_cur.execute(SELECT_ALL_SPECIES):
        if category != cur_category:
            print(f"** {category} [%]")
            cur_category = category

        sightings = db.cursor().execute(SELECT_SIGHTINGS, (id,)).fetchall()
        todo = "TODO" if len(sightings) == 0 else "DONE"

        checklists = sorted(
            set(
                CHECKLIST_TAGS[checklist[0]]
                for checklist in db.cursor().execute(SELECT_CHECKLISTS, (id,))
            )
        )
        header = f"*** {todo} {name}"
        print(header, end="")
        if checklists:
            tags = ":" + ":".join(checklists) + ":"
            spaces = 77 - len(header) - len(tags)
            print((spaces * " ") + tags)
        else:
            print()

        for year, month, day, location, notes in sightings:
            text = textwrap.wrap(f"{year}/{month}/{day} {location}. {notes}")
            for line in text:
                print(line)
            print()


if __name__ == "__main__":
    main()
