from dragonfly import (Grammar, AppContext, MappingRule, Key, Text,
                       Dictation, Integer, Function)

rules = MappingRule(
    name = 'Programming rules',
    mapping = {
        'integer': Text('int '),
        '(death|deaf)': Text('def '),
        'also': Text(' && '),
        'or': Text(' || '),
        'pass': Text('pass'),
        'is equal': Text('== '),
        'equals': Text(' = '),
    })
