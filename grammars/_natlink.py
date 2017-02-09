from dragonfly import MappingRule, Function
from natlink import setMicState

def cancel_and_sleep():
    print("Dictation canceled, going to sleep. Say \"wake up\" to restart")
    setMicState("sleeping")

grammarItems = {
    "(go to sleep|cancel and sleep)": Function(cancel_and_sleep),  
    }

class NatlinkRule(MappingRule):
    mapping = grammarItems
    extras = [
    ]

rules = NatlinkRule()