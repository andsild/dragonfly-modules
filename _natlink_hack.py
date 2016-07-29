"""
To load grammars, natlink has a file core\natlinkmain.py
That file uses reflection and many if-sentences, making it hard to trace bugs from reloading.
Most importantly, it doesn't load directories.

This files finds all modules that I want to load (from grammars import ...)
It also adds a reload feature, so we can break out of errors from natlink 
  (example bug: natlink reloads a file but not the global variables, giving a lot of "None" errors)

The downside is that you can no longer (per default) reload grammar files simply by turning mic on and off (we do, however, have the voice command below)
"""
from dragonfly import (Grammar, MappingRule, Function)
import sys
import reloader
from dragonflymodules.config import GRAMMAR_IMPORTS

from types import ModuleType
import os
import sys

# pre-emptive import of the above modules for reload() extension to work the
# first time
for import_name in GRAMMAR_IMPORTS:
    getattr(__import__(import_name, fromlist=["rules"]), "rules")

grammar = Grammar("to rule them all")

def reload_grammars():
    unload()
    global grammar
    grammar = Grammar("to rule them all")

    # reload module and re-add the rules imported from that module
    global GRAMMAR_IMPORTS
    for import_name in GRAMMAR_IMPORTS:
        reloader.reload(sys.modules[import_name])
        import_rule = getattr(__import__(import_name, fromlist=["rules"]), "rules")
        grammar.add_rule(import_rule)

    grammar.add_rule(get_reloader_rules()) # for the "reload grammar module" code in get_reloader_rules
    grammar.load()

    print "reloaded all modules"

def unload():
    """ dawd
    """
    global grammar
    grammar.unload()
    grammar = None

def get_reloader_rules():
    return MappingRule(name = 'Reloader rules',
        mapping = {
        'reload grammar module': Function(reload_grammars),
        })

reload_grammars()
