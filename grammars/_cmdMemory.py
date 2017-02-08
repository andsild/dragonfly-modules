from dragonfly import PlaybackHistory, MappingRule

# Dictionary for storing memories.
memories     = DictList("memories")
memories_ref = DictListRef("memory", memories)

playback_history = PlaybackHistory()
try:
    playback_history.register()
except Exception, e:
    print "Warning, failed to register playback_history: %s" % e

record_history = PlaybackHistory()
record_name = None

config                       = Config("command memory")
config.lang                  = Section("Language section")
config.lang.playback_last    = Item("(playback | repeat) [last] [<n>] (commands | command | recognitions) [<count> times]")
config.lang.recall           = Item("recall everything")
config.load()

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
    o =record_history
    print ("(%s) %s: %s\n" % (id(o), "record_history", "recall") + "\n".join(("    - %r" % (item,)) for item in o))


class PlaybackRule(MappingRule):
    mapping  = {  # Spoken form   ->  ->  ->  action
                config.lang.playback_last:    Function(playback_last),
                config.lang.recall:           Function(print_history),
               }
    extras   = [
                memories_ref,                 # This is the list of
                                              #  already-remembered
                                              #  memories; its name is
                                              #  *memory*.

               ]
    defaults = {
                "n": 1,
                "count": 1,
               }

rules = PlaybackRule()