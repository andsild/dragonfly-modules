from dragonfly import (Grammar, AppContext, MappingRule, Key, Text,
                       Dictation, Integer, Function)

rules = MappingRule(
    name = 'Programming rules',
    mapping = {
        'integer': Text('int '),
        'struct': Text('class '),
        '(death|deaf)': Text('def '),
        'also': Text(' && '),
        'or': Text(' || '),
        'pass': Text('pass'),
        'is equal': Text('== '),
        'equals': Text('='),
        'left arrow': Text('<-'),
        'right arrow': Text('->'),

        'graph' : Text('graph'), # (why is this word so hard for DNS? lol)
        'node': Text('node'),
    })
