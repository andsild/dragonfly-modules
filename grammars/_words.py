from dragonfly import (Grammar, AppContext, MappingRule, Key, Text,
                       Dictation, Integer, Function)

rules = MappingRule(
    name = 'Common words and phrases',
    mapping = {
        'graph' : Text('graph'), # (why is this word so hard for DNS? lol)
        'node': Text('node'),
        'jim hub': Text('github'),
    })