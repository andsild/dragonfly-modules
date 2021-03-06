from dragonfly import (
    Dictation,
    Key,
    MappingRule,
    Function,
    IntegerRef,
)

from utility.substitute_phrase import translate_spokenform_to_queryform

#visual_studio_context = AppContext(executable="devenv", title="Microsoft visual studio")
#grammar = Grammar('visualstudio', context=visual_studio_context)
basics_mapping = {
    'solution Explorer': Key("c-w,s"),
    'fuzzy':   Key("ctrl:down/3, comma, ctrl:up/3"),
    'run tests': Key("ctrl:down/3, r, ctrl:up/3, t"),
    'add a new file': Key("shift:down/3, c-a, shift:up/50, a-n"),
    'go [to] definition': Key("f12"),
    'find <text>': Key("c-f") + Function(translate_spokenform_to_queryform),
    'find project <text>': Key("cs-f") + Function(translate_spokenform_to_queryform),
    'debug main': Key("f5"),
    'format': Key("ctrl:down, k, d, ctrl:up"),
    'run main': Key("ctrl:down/3, f5, ctrl:up/3"),
    'fly away [<n>]': Key("ctrl:down/3, tab:%(n)d") + Key("ctrl:up/3"),
    'rename [this]': Key("ctrl:down/3, r, ctrl:up/3, r"),
    'complete [this]': Key("ctrl:down/3, space, ctrl:up/3"),
    'open solution': Key("shift:down, c-o, shift:up"),
    'cursor back': Key("ctrl:down, hyphen, ctrl:up"),
}

class Basics(MappingRule):
    mapping = basics_mapping
    extras = [Dictation('text'),
        IntegerRef('n', 0, 999),]
    defaults = {
        "n": 1,  # Default repeat count.
    }

rules = Basics()