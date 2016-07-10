# Created for aenea using libraries from the Dictation Toolbox
# https://github.com/dictation-toolbox/dragonfly-scripts
#
# Commands for interacting with terminal and desktop environment
#
# Author: Tony Grosinger
# Modified by: Anders Sildnes
#
# Licensed under LGPL

from dragonfly import Grammar, Key, Text, AppContext, MappingRule

#terminal_context = AppContext(executable="cmd")
#grammar = Grammar('terminal', context=terminal_context)

terminal_mapping = {
    # Terminal commands
    # dir is hard to say and recognize. Use something else
    'deer home':                    Text("cd") + Key("enter"),
    'deer up':                      Text("cd ..") + Key("enter"),
    'deer list':                    Text("ls") + Key("enter"),
    'deer list all':                Text("ls -lha") + Key("enter"),
    'deer list details':            Text("ls -lh") + Key("enter"),
    'deer into':                    Text("cd "),
    'grep process':                 Text("pgrep "),

    'emerge':                       Text("sudo dnf install "),
    'scratch that':                 Key("c-w"),
    'delete line':                  Key("c-u"),
    'editor':                       Text("$EDITOR "),

    '(terminal|term) clear':        Key("ctrl:down, l, ctrl:up"),
    '(terminal|term) (close|exit)': Key("c-c") + Key("enter"),
    '(terminal|term) (kill)':       Key("c-d") + Key("enter"),

    'revision status':              Text("git status ") + Key("enter"),
    'revision push':                Text("git push ") + Key("enter"),
    'revision add all':             Text("git add -A ") + Key("enter"),
    'revision commit':              Text("git commit -m "),
    'revision fetch':               Text("git fetch ") + Key("tab"),
    'revision merge':               Text("git merge ") + Key("tab"),
    'revision difference':          Text("git diff "),
    'revision log':                 Text("git log ") + Key("enter"),
    'revision pull':                Text("git pull ") + Key("enter"),

    # Common words
    '(pseudo|sudo|pseudo-)':        Text("sudo "),
    '(pseudo|sudo|pseudo-) that':   Text("sudo !! ") + Key("enter"),

    # I recommend installing fzf
    'history':                      Key("c-r"),
    'open':                         Text("xdg-open "),
    'last parameter':               Key("escape, dot"),
    'view top':                     Text("htop") + Key("enter"),

    'redo last [command]':          Key("c-u, up, enter"),
}


class Mapping(MappingRule):
    mapping = terminal_mapping
    extras = []

rules = Mapping()