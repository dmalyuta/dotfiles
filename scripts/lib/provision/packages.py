#!/usr/bin/python3 -I
"""Print the names in one of packages.toml's lists, for this machine, on one line.

Usage: packages.py FILE KEY TABLE=FLAG...
       packages.py FILE held

  FILE   packages.toml, which says what its tables and keys are.
  KEY    The list to print: dnf, flatpak, services or groups.
  TABLE  One of the file's tables, with FLAG empty if it doesn't apply to this machine.

Prints the KEY lists of the tables that apply, in the file's order. Fails, saying where, if the
file has a table that isn't among the arguments, a key that isn't a list's, or a list of anything
but names: all are mistakes in the file that would otherwise go unnoticed. With held, prints the
[held] table instead, as NAME=VERSION pairs, failing if a version isn't one like 3.8.0.

Run by choose_packages and read_held_packages in packages.sh.
"""

import re
import sys
import tomllib

KEYS = ("dnf", "flatpak", "services", "groups")


def main():
    path, key, *flags = sys.argv[1:]
    applies = dict(flag.split("=", 1) for flag in flags)
    try:
        with open(path, "rb") as f:
            tables = tomllib.load(f)
    except (OSError, tomllib.TOMLDecodeError) as error:
        sys.exit(f"{path}: {error}")

    # Not a machine's table: the versions packages are held at, on every machine.
    held = tables.pop("held", {})
    if key == "held":
        if not isinstance(held, dict) or not all(
            isinstance(version, str) and re.fullmatch(r"[0-9]+(\.[0-9]+)*", version)
            for version in held.values()
        ):
            sys.exit(f'{path}: [held] must map package names to versions like "3.8.0"')
        print(" ".join(f"{name}={version}" for name, version in held.items()))
        return

    names, mistakes = [], []
    for table, lists in tables.items():
        if table not in applies or not isinstance(lists, dict):
            mistakes.append(f"[{table}] is not one of the tables: {', '.join(applies)}")
            continue
        for name, values in lists.items():
            if name not in KEYS:
                mistakes.append(f"[{table}] {name} is not one of the lists: {', '.join(KEYS)}")
            elif not (isinstance(values, list) and all(isinstance(v, str) for v in values)):
                mistakes.append(f"[{table}] {name} must be a list of names in quotes")
            elif name == key and applies[table]:
                names += values
    if mistakes:
        sys.exit("\n".join(f"{path}: {mistake}" for mistake in mistakes))
    print(" ".join(names))


if __name__ == "__main__":
    main()
