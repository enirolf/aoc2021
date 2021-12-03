#!/usr/bin/env python3

import os
import click
from jinja2 import Template


@click.command()
@click.argument('day', type=int)
def generate_day(day):
    """
    Generate all files necessary for a new day in the Advent of Code.
    """
    workdir = os.path.dirname(os.path.realpath(__file__))

    if os.path.isdir(f"{workdir}/day{day}"):
        print(f"A directory for day {day} already exists!")
        return

    os.mkdir(f"{workdir}/day{day}")

    template_name = "template.hs.j2"

    with open(f"{workdir}/{template_name}") as template_file:
        template = Template(template_file.read())

        with open(f"{workdir}/day{day}/Day{day}.hs", "w") as puzzle_file:
            puzzle_file.write(template.render(day=day))

    open(f"{workdir}/day{day}/small.txt", "a").close()
    open(f"{workdir}/day{day}/large.txt", "a").close()

    print(f"Created puzzle files for day {day}!")


if __name__ == '__main__':
    generate_day()
