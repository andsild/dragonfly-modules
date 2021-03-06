"""
Command-module for **Chrome** editor
=======================================

This module offers various commands for Chrome
, as well as integration with the Vimium plugin
to allow clicking of links via Telephony character names.

"""

#---------------------------------------------------------------------------

from dragonfly import AppContext,Grammar, Dictation, IntegerRef, Text, Choice, Key, Function, MappingRule

#chrome_context = AppContext(executable="chrome")

from utility.program_manager import goto_chrome

mapping = {
    # Press escape to the blur focus on any input fields
    "undo tab": Key("shift:down/3, c-t, shift:up/3"),
    'new window': Key('c-n'),
    "new tab": Key("c-t"),
    "next tab [<n>]": Key('c-pgdown:%(n)d'),
    'pre tab [<n>]': Key('c-pgup:%(n)d'),
    'first tab': Key('c-1'),
    'second tab': Key('c-2'),
    'third tab': Key('c-3'),
    'fourth tab': Key('c-4'),
    'fifth tab': Key('c-5'),
    'sixth tab': Key('c-6'),
    'seventh tab': Key('c-7'),
    'eighth tab': Key('c-8'),
    'last tab': Key('c-9'),
    'bar': Key('c-l'),
    'inspector': Key('cs-i'),
    'forward [<n>]': Key('a-right:%(n)d'),
    'find next': Key('c-g'),
    'find pre': Key('cs-g'),
    #'find <text>': Key('c-f') + Text('%(text)s\n'),
    'copy url': Key('c-l, c-c'),
    'goto top': Key('g, g'),
    'goto bottom': Key('G'),
    'reload page': Key('c-r'),

    'search youtube': Function(lambda: goto_chrome("youtube.com")),
}

class ChromeRule(MappingRule):
    mapping = mapping
    extras = [
        Dictation('text'),
        IntegerRef('n', 1, 999),
        IntegerRef('n2', 1, 999),
    ]
    defaults = {
        "n": 1,
    }

rules = ChromeRule()
