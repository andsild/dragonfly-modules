from dragonfly import (Grammar, AppContext, MappingRule, Key, Text,
                       Dictation, Integer, Function)
from aenea.strict import *

grammar = Grammar("programming mode")
rules = MappingRule(
    name = 'Programming rules',
    mapping = {
        'integer': Text('int '),
        '(death|deaf)': Text('def '),
        'also': Text(' && '),
        'or': Text(' || '),
    })


grammar.add_rule(rules)
grammar.load()

def unload():
    global grammar
    if grammar: grammar.unload()
    grammar = None
