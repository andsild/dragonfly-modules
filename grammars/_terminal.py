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
    'deer home':                    Text("cdls") + Key("enter"),
    'deer up':                      Text("cdls ..") + Key("enter"),
    'deer list':                    Text("ls") + Key("enter"),
    'deer list all':                Text("ls -lha") + Key("enter"),
    'deer list details':            Text("ls -lh") + Key("enter"),
    'deer into':                    Text("cdls "),
    'grep process':                 Text("pgrep "),

    'emerge':                       Text("sudo dnf install "),
    'scratch that':                 Key("c-w"),
    'delete line':                  Key("c-u"),
    'editor':                       Text("$EDITOR "),

    'history': Key("ctrl:down, r, ctrl:up"),

    'kill command': Key("ctrl:down, c, ctrl:up"),
    'kill window': Key("ctrl:down, d, ctrl:up"),

    '(terminal|term) clear':        Key("ctrl:down, l, ctrl:up"),

    'jim':              Text("git "),
    'jim status':              Text("git status ") + Key("enter"),
    'jim push':                Text("git push ") + Key("enter"),
    'jim add all':             Text("git add -A ") + Key("enter"),
    'jim commit':              Text("git commit") + Key("enter"),
    'jim fetch':               Text("git fetch ") + Key("tab"),
    'jim merge':               Text("git merge ") + Key("tab"),
    'jim difference':          Text("git diff ") + Key("enter"),
    'jim log':                 Text("git log ") + Key("enter"),
    'jim pull':                Text("git pull ") + Key("enter"),
    'jim diff':                Text("git diff ") + Key("enter"),
    'jim clone':                Text("git clone ") + Key("enter"),
    'jim clone paste':                Text("git clone ") + Key("shift:down, insert, shift:up, enter"),

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