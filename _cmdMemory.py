# inspired/taken from https://github.com/t4ngo/dragonfly-modules/blob/master/command-modules/_cmdmemory.py
from dragonfly import PlaybackHistory, MappingRule, DictList, DictListRef, Config, Section, Item, Function, IntegerRef, Dictation, Grammar, Key

from utility.cmdHistory import get_backspaces_for_commands

config                       = Config("command memory")
config.lang                  = Section("Language section")
config.lang.playback_last    = Item("(playback | repeat) [last] [<n>] (commands | command | recognitions) [<count> times]")
config.lang.recall           = Item("recall everything")
config.lang.clear_command_history = Item("clear command history")
config.lang.backspace_commands = Item("undo [<n>] command")
config.load()

# Dictionary for storing memories.
memories     = DictList("memories")
memories_ref = DictListRef("memory", memories)

playback_history = PlaybackHistory()
try:
    playback_history.register()
except Exception, e:
    print "Warning, failed to register playback_history: %s" % e

def playback_last(n, count):
    # Retrieve playback-action from recognition observer.
    if playback_history and playback_history.complete: 
        playback_history.pop()      # Remove playback recognition itself.
    action = playback_history[-n:]  # Retrieve last *n* recognitions.
    action.speed = 10               # Playback at 10x original speed.

    # Playback action.
    import time; time.sleep(1)
    playback_history.unregister()   # Don't record playback.
    try:
        for index in range(count):  # Playback *count* times.
            action.execute()        # Perform playback.
    finally:
        playback_history.register() # Continue recording after playback.

def print_history():
    o = playback_history
    print ("(%s) %s: %s\n" % (id(o), "playback_history", "recall") + "\n".join(("    - %r" % (item,)) for item in o))

def clear_command_history():
    while playback_history:
        playback_history.pop()

def backspaces_for_commands():
    for n in range(get_backspaces_for_commands(playback_history)):
        Key("backspace").execute()


class PlaybackRule(MappingRule):
    mapping  = {  # Spoken form   ->  ->  ->  action
                config.lang.playback_last:    Function(playback_last),
                config.lang.recall:           Function(print_history),
                config.lang.clear_command_history: Function(clear_command_history),
                config.lang.backspace_commands: Function(backspaces_for_commands),
               }
    extras   = [
                IntegerRef("n", 1, 100),      # *n* designates the number
                                              #  of recent recognitions.
                IntegerRef("count", 1, 100),  # *count* designates how
                                              #  many times to repeat
                                              #  playback.
                Dictation("name"),            # *name* is used when
                                              #   Naming new memories.
                memories_ref,                 # This is the list of
                                              #  already-remembered
                                              #  memories; its name is
                                              #  *memory*.

               ]
    defaults = {
                "n": 1,
                "count": 1,
               }

grammar = Grammar("command memory")     # Create this module's grammar.
grammar.add_rule(PlaybackRule())        # Add the top-level rule.
grammar.load()                          # Load the grammar.

# Unload function which will be called at unload time.
def unload():
    playback_history.unregister()
    global grammar
    if grammar: grammar.unload()
    grammar = None