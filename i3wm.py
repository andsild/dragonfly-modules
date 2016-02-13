# Created for aenea using libraries from the Dictation Toolbox
# https://github.com/dictation-toolbox/dragonfly-scripts
#
# Commands for interacting with the i3 window manager
#
# Author: Tony Grosinger
#
# Licensed under LGPL

import aenea
import aenea.configuration
from aenea.lax import Key, Text
from aenea import IntegerRef, Dictation
import dragonfly
try:
    import aenea.communications
except ImportError:
    print 'Unable to import Aenea client-side modules.'
    raise


def get_grammar(context, config):
    if "i3-mod-key" not in config:
        print("Missing required 'i3-mod-key' in config file")
        return None

    mod_key = config["i3-mod-key"]
    mod_char = "a"
    if mod_key == "ctrl":
        mod_char = "c"
    elif mod_key == "alt":
        mod_char = "a"
    elif mod_key == "win":
        mod_char = "w"
    else:
        print("Invalid value specified for 'i3-mod-key' in config file")
        return None

    class Mapping(dragonfly.MappingRule):
        mapping = aenea.configuration.make_grammar_commands('i3wm', {
            '(works|workspace) <n>': Key(mod_char + "-%(n)d"),
            'lock screen': Key(mod_char + "-l"),
            '(win|window) next one': Key("a-tab"),
            '(win|window) next two': Key("a-tab, a-tab"),
            '(win|window) next three': Key("a-tab, a-tab, a-tab"),
            '(win|window) next four': Key("a-tab, a-tab, a-tab, a-tab"),
            '(win|window) previous one': Key("s-a-tab"),
            '(win|window) previous two': Key("s-a-tab, s-a-tab"),
            '(win|window) previous three': Key("s-a-tab, s-a-tab, s-a-tab"),
            '(win|window) previous four': Key("s-a-tab, s-a-tab, s-a-tab, s-a-tab"),
            '(win|window) left': Key(mod_char + "-left"),
            '(win|window) right': Key(mod_char + "-right"),
            '(win|window) up': Key(mod_char + "-up"),
            '(win|window) stacking': Key(mod_char + "-s"),
            '(win|window) default': Key(mod_char + "-d"),

            '(win|window) open terminal': Key(mod_char + "-enter"),

            '(win|window) (kill|close)': Key(mod_char + "s-c"),
            '(win|window) launch': Key(mod_char + "-p"),

            '(win|window) chrome': Key(mod_char + "-c"),
            '(win|window) terminal': Key(mod_char + "-t"),
            '(win|window) document': Key(mod_char + "-d"),
        })
        extras = [
            IntegerRef('n', 1, 99),
            Dictation('text'),
            dragonfly.IntegerRef('appnum', 1, 99),
        ]


    i3wm_grammar = dragonfly.Grammar('i3wm', context=context)
    i3wm_grammar.add_rule(Mapping(mod_char))
    return i3wm_grammar
