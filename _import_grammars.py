"""
To load grammars, natlink has a file core\natlinkmain.py
That file uses reflection and many if-statements, making it hard to trace bugs from reloading.
Most importantly, it doesn't load directories.

This files lets you define grammar-rules whereever you want (also outside of this directory :) )

It also adds a reload feature, so we can break out of errors from natlink 
  (example bug: natlink reloads a file but not the global variables, giving a lot of "None" errors)

The downsides is that:
    #1 you can no longer (per default) reload grammar files simply by turning mic on and off (we do, however, have the voice command below)
    #2 We can no longer have context-specific grammars (e.g. grammars that are only enabled when chrome is)
        (I hope to fix #2 sometime in the future, although PRs are very welcome)
"""
from dragonfly import (Grammar, MappingRule, Function)
import __builtin__
from IPython.lib import deepreload
import sys
import datetime

# see also __init__.py in that grammars directory: you need to define __all__
from grammars import * 
from grammars import __all__ as grammarFiles
grammarFiles = map(lambda s: "grammars." + s, grammarFiles)

grammar = Grammar("to rule them all")

def load_module(module_name):
    if len(module_name) > 8 and module_name[-8:] == "__init__": return
    module = sys.modules[module_name]

    try:
        print "Loaded module %s ... " % module_name,
        deepreload.reload(module, exclude=('sys', 'os.path', 'builtins', '__main__', 'numpy', 'numpy._globals', 'dragonfly', 'natlink', 'os', 'glob', 're', 'subprocess', 'posixpath', 'stat', 'ctypes', 'time', 'win32con', 'dragonfly.actions', 'win32api', 'copy_reg', 'types'))

        import_rule = getattr(__import__(module_name, fromlist=["rules"]), "rules")
        grammar.add_rule(import_rule)
        print "successfully"
    except AttributeError as ae:
        print "successfully"
    except RuntimeError as runtime_error:
        "There was an error in file %s" % module_name
        print runtime_error, '\n', '\n'
    except NameError as nameerror:
        "Forgot something in file %s?" % module_name
        print nameerror, '\n', '\n'

def reload_grammars():
    global grammar
    unload()
    assert grammar is None
    grammar = Grammar("to rule them all")

    now = datetime.datetime.now()
    print "begun reloading at %02d:%02d" % (now.hour, now.minute)

    map(load_module, grammarFiles)

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