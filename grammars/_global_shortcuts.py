""" Keybindings that should be accesible from every application
"""
from dragonfly import MappingRule, Key

grammarItems = {
    "next music" : Key("alt:down, pgdown, alt:up"), # I use streamkeys for this (and play all my music in chrome)
    "prev music" : Key("alt:down, pgup, alt:up"),# I use streamkeys for this (and play all my music in chrome)
    "pause music" : Key("alt:down, home, alt:up"), # I use streamkeys for this (and play all my music in chrome)
    "play music" : Key("alt:down, home, alt:up"), # I use streamkeys for this (and  play all my music in chrome)
    "go fullscreen" : Key("f11"),
}

class GlobalShortcuts(MappingRule):
    mapping = grammarItems
    extras = [
        ]
    defaults = {
        "n": 1,
        "ntimes": 1,
    }

rules = GlobalShortcuts()