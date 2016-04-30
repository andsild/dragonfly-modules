"""
To load grammars, natlink has a file core\natlinkmain.py
That file uses reflection and many if-sentences, making it hard to trace bugs from reloading.
Also, it doesn't load directories.

This files finds all modules that I want to load (from grammars import ...)
It also adds a reload feature, so we can break out of errors from natlink 
  (example bug: natlink reloads a file but not the global variables, giving a lot of "None" errors)
"""
from dragonfly import (Grammar, AppContext, MappingRule, Key, Text,
                       Dictation, Integer, Function)
import sys
import reloader


import grammars
from grammars._programming import rules as programming_rules

grammar = Grammar("to rule them all")

def reload_grammars():
	unload()

	reloader.reload(grammars._programming)
	from grammars._programming import rules as programming_rules

	global grammar
	grammar = Grammar("to rule them all")

	grammar.add_rule(programming_rules)
	grammar.add_rule(get_reloader_rules())
	grammar.load()

	print "reloaded all modules"

def unload():
	global grammar
	grammar.unload()
	grammar = None

def get_reloader_rules():
	""" Keep in mind that turning microphone on and off in DNS/Natlink will also reload modules
	    Use the following MappingRule to reload in case of bugs.
	"""
	return MappingRule(
		name = 'Reloader rules',
		mapping = {
		'reload grammar module': Function(reload_grammars),
		})

reload_grammars()
