#!/usr/bin/env python
# gnome-python/pygobject/examples/option.py

from gobject.option import OptionGroup, OptionParser, make_option

group = OptionGroup("example", "OptionGroup Example", "Shows all example options",
    option_list = [
        make_option("--example",
                    action="store_true",
                    dest="example",
                    help="An example option."),
    ])

parser = OptionParser("NAMES ...",
    description="A simple gobject.option example.",
    option_list = [
        make_option("--file", "-f",
                    type="filename",
                    action="store",
                    dest="file",
                    help="A filename option"),
        # ...
    ])

parser.add_option_group(group)

parser.parse_args()

print "group: example ", group.values.example
print "parser: file", parser.values.file
