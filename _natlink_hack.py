"""
To load grammars, natlink has a file core\natlinkmain.py
That file uses reflection and many if-statements, making it hard to trace bugs from reloading.
Most importantly, it doesn't load directories.

This files lets you define grammar-rules whereever you want (also outside of this directory :) )
we define their relative path in dragonflymodules.config, and they automatically get imported by this file

It also adds a reload feature, so we can break out of errors from natlink 
  (example bug: natlink reloads a file but not the global variables, giving a lot of "None" errors)

The downsides is that:
    #1 you can no longer (per default) reload grammar files simply by turning mic on and off (we do, however, have the voice command below)
    #2 We can no longer have context-specific grammars (e.g. grammars that are only enabled when chrome is)
        (I hope to fix #2 sometime in the future, although PRs are very welcome)
"""
from dragonfly import (Grammar, MappingRule, Function)
import sys
import reloader
from dragonflymodules.config import GRAMMAR_IMPORTS

from types import ModuleType
import os
import sys
import datetime

# pre-emptive import of the above modules for reload() extension to work the
# first time
for import_name in GRAMMAR_IMPORTS:
    getattr(__import__(import_name, fromlist=["rules"]), "rules")

grammar = Grammar("to rule them all")

def reload_grammars():
    unload()
    global grammar
    grammar = Grammar("to rule them all")

    now = datetime.datetime.now()
    print "begun reloading at %s:%s" % (now.hour, now.minute)

    # reload module and re-add the rules imported from that module
    global GRAMMAR_IMPORTS
    for import_name in GRAMMAR_IMPORTS:
        try:
            reloader.reload(sys.modules[import_name])
            import_rule = getattr(__import__(import_name, fromlist=["rules"]), "rules")
            grammar.add_rule(import_rule)
            print "Loaded module %s successfully" % import_name
        except RuntimeError as runtime_error:
            "There was an error in file %s" % import_name
            print runtime_error, '\n', '\n'
        except NameError as nameerror:
            "Forgot something in file %s?" % import_name
            print nameerror, '\n', '\n'

    grammar.add_rule(get_reloader_rules()) # for the "reload grammar module" code in get_reloader_rules
    grammar.load()

    print "reloaded all modules"

def unload():
    """ Needs to be here for natlink
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
