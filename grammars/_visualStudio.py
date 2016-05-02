import re
from dragonfly import (
    Dictation,
    Grammar,
    Key,
    MappingRule,
    Function,
    IntegerRef,
    Text,
    Choice,
    AppContext,
)


from _generic_edit import pressKeyMap, letters, letterMap
from _vim import range_insert_symbol

#visual_studio_context = AppContext(executable="devenv", title="Microsoft visual studio")
#grammar = Grammar('visualstudio', context=visual_studio_context)

basics_mapping = {
    'fuzzy':   Key("ctrl:down/3, comma, ctrl:up/3"),
    'run tests': Key("ctrl:down/3, r, ctrl:up/3, t"),
    'add a new file': Key("shift:down/3, c-a, shift:up/50, a-n"),
    'go [to] definition': Key("f12"),
    'find <text>': Key("c-f") + Function(range_insert_symbol),
}
class Basics(MappingRule):
    mapping = basics_mapping
    extras = [
        Dictation('text'),
        IntegerRef('n', 0, 999),
    ]
    defaults = {
        "n": 1,  # Default repeat count.
    }

rules = Basics()