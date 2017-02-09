from dragonfly import Text, Function, Config, Section, MappingRule, Item, Choice, Key, Dictation
from utility.format import (
    format_text_logic,
    FormatTypes as ft,
)

def format_text(text, format_type_list=None):
    result = format_text_logic(str(text), format_type_list)
    Text("%(text)s").execute({"text": result})

grammarItems = {
    # Format dictated words.  See the formatMap for all available types.
    # Ex: "camel case my new variable" -> "myNewVariable"
    # Ex: "snake case my new variable" -> "my_new_variable"
    # Ex: "uppercase squash my new hyphen variable" -> "MYNEW-VARIABLE"
    "<format_type_list> <text>": Function(format_text),

    # For writing words that would otherwise be characters or commands.
    # Ex: "period", tab", "left", "right", "home".
    "say <reservedWord>": Text("%(reservedWord)s"),
}

formatMap = {
    "(sentence|sense|since) case": [ft.sentenceCase],
    "camel":                       [ft.camelCase],
    "pascal":                        [ft.pascalCase],
    "super title":                        [ft.pascalCase],
    "snake":                       [ft.snakeCase],
    "uppercase":                   [ft.upperCase],
    "dwarf":                   [ft.lowerCase],
    "mumble case":                   [ft.lowerCase],
    "squash":                      [ft.squash],
    "wash":                      [ft.squash],
    "lowercase squash":            [ft.squash, ft.lowerCase],
    "uppercase squash":            [ft.squash, ft.upperCase],
    "squash lowercase":            [ft.squash, ft.lowerCase],
    "squash uppercase":            [ft.squash, ft.upperCase],
    "dashify":                     [ft.dashify],
    "lowercase dashify":           [ft.dashify, ft.lowerCase],
    "uppercase dashify":           [ft.dashify, ft.upperCase],
    "dashify lowercase":           [ft.dashify, ft.lowerCase],
    "dashify uppercase":           [ft.dashify, ft.upperCase],
    "dotify":                      [ft.dotify],
    "lowercase dotify":            [ft.dotify, ft.lowerCase],
    "uppercase dotify":            [ft.dotify, ft.upperCase],
    "dotify lowercase":            [ft.dotify, ft.lowerCase],
    "dotify uppercase":            [ft.dotify, ft.upperCase],
    "say":                         [ft.spokenForm],
    "spay":                         [ft.spokenFormWithSpace],
    "environment variable":        [ft.snakeCase, ft.upperCase],
}

# For use with "say"-command.  Words that are commands in the generic edit
# grammar were treated as separate commands and could not be written with the
# "say"-command.  This overrides that behavior.
# Other words that won't work for one reason or another, can also be added to
# this list.
reservedWord = {
    "up": "up",
    "down": "down",
    "left": "left",
    "right": "right",
    "home": "home",
    "end": "end",
    "space": "space",
    "tab": "tab",
    "backspace": "backspace",
    "delete": "delete",
    "enter": "enter",
    "paste": "paste",
    "copy": "copy",
    "cut": "cut",
    "release": "release",
    "page up": "page up",
    "page down": "page down",
    "say": "say",
    "select": "select",
    "select all": "select all",
    "uppercase": "uppercase",
    "lowercase": "lowercase",
    "expand": "expand",
    "squash": "squash",
    "dash": "dash",
    "underscore": "underscore",
    "dot": "dot",
    "period": "period",
    "minus": "minus",
    "semi-colon": "semi-colon",
    "hyphen": "hyphen",
    "triple": "triple"
}

class FormattingRule(MappingRule):
    mapping = grammarItems
    extras = [
        Choice("format_type_list", formatMap),
        Choice("reservedWord", reservedWord),
        Dictation("text"),
        ]
    defaults = {
        "n": 1,
        "ntimes": 1,
    }

rules = FormattingRule()
