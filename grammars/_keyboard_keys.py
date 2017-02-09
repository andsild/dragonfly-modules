""" This file handles voicebindings for single keyboard keys.
"""
from dragonfly import Key, Function, Text, MappingRule, Choice, IntegerRef
from utility.lettermap import Phonetic_Letter_map, Shorttalk_Letters
from dragonfly.actions.typeables import typeables
from dragonfly.actions.keyboard import keyboard

if 'semicolon' not in typeables:
    typeables["semicolon"] = keyboard.get_typeable(char=';')

release = Key("shift:up, ctrl:up, alt:up")

modifierMap = {
    "alt": "alt",
    "control": "ctrl",
   "shift": "shift",
    "super": "win",
    "[(hold|press)] alt": Key("alt:down/3"),
    "release alt": Key("alt:up"),
    "[(hold|press)] shift": Key("shift:down/3"),
    "release shift": Key("shift:up"),
    "[(hold|press)] control": Key("ctrl:down/3"),
    "release control": Key("ctrl:up"),
    "win key": release + Key("win/3"),
    "release [all]": release,
}

specialCharMap = {
    "(bar|vertical bar|pipe)": "|",
    "(dash|minus|hyphen)": "-",
    "(dot|period)": ".",
    "comma": ",",
    "backslash": "\\",
    "underscore": "_",
    "(star|asterisk)": "*",
    "colon": ":",
    "(semicolon|semi-colon)": ";",
    "at": "@",
    "[double] quote": '"',
    "single quote": "'",
    "hash": "#",
    "dollar": "$",
    "percent": "%",
    # "and": "&",
    "slash": "/",
    "equal": "=",
    "plus": "+",
    "space": " ",
    "tilde": "~",
    "laip": "(",
    "rye": ")",
    "lace": "{",
    "race": "}",
    "lack": "[",
    "rack": "]",
    "colon": ":",
    "sink": ";",
    "think": ";",
    "Sim": ":",
    "sing": "'",
    "quest": "?",
    "bang": "!",
    "luke": "<",
    "look": "<",
    "ruke": ">",
    "rook": ">",
}

grammarItems = {
    "<letters>": Text("%(letters)s"),
    "<char>": Text("%(char)s"),
    # Shorthand multiple characters.
    "double <char>": Text("%(char)s%(char)s"),
    "triple <char>": Text("%(char)s%(char)s%(char)s"),

    # navigation keys
    "up [<n>]": Key("up:%(n)d"),
    "up [<n>] slow": Key("up/15:%(n)d"),
    "down [<n>]": Key("down:%(n)d"),
    "down [<n>] slow": Key("down/15:%(n)d"),
    "left [<n>]": Key("left:%(n)d"),
    "left [<n>] slow": Key("left/15:%(n)d"),
    "right [<n>]": Key("right:%(n)d"),
    "right [<n>] slow": Key("right/15:%(n)d"),
    "page up [<n>]": Key("pgup:%(n)d"),
    "page down [<n>]": Key("pgup:%(n)d"),
    "north [<n>]": Key("pgup:%(n)d"),
    "page down [<n>]": Key("pgdown:%(n)d"),
    "page next [<n>]": Key("ctrl:down, pgdown:%(n)d, ctrl:up"),
    "page prev [<n>]": Key("ctrl:down, pgup:%(n)d, ctrl:up"),

    # f1 to f12
    'F one':    'f1',
    'F two':    'f2',
    'F three':  'f3',
    'F four':   'f4',
    'F five':   'f5',
    'F six':    'f6',
    'F seven':  'f7',
    'F eight':  'f8',
    'F nine':   'f9',
    'F ten':    'f10',
    'F eleven': 'f11',
    'F twelve': 'f12',

    # other
    "tab [<n>]": Key("tab:%(n)d"),
    "ace [<n>]": release + Key("space:%(n)d"),# no need to bind space, most engines has thid default
    "delete [<n>]": Key("del/3:%(n)d"),
    "backspace [<n>]": release + Key("backspace:%(n)d"),
    "chook [<n>]": release + Key("backspace:%(n)d"),
    "slap [<n>]": release + Key("enter:%(n)d"),
}

for voice_command,key in Shorttalk_Letters.iteritems():
    grammarItems[voice_command] = Key(key)

class KeyboardKeyRule(MappingRule):
    mapping = grammarItems
    extras = [
        IntegerRef("n", 1, 99),
        Choice("char", specialCharMap),
        Choice("letters", Phonetic_Letter_map),
    ]
    defaults = {
        "n": 1,
    }

rules = KeyboardKeyRule()