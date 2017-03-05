from dragonfly import Grammar, Key, Text, AppContext, MappingRule, IntegerRef

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

    #'scratch that':                 Key("c-w"),
    #'delete line':                  Key("c-u"),
    'crude':                  Key("c-u"),
    'editor':                       Text("$EDITOR "),

    'history': Key("ctrl:down, r, ctrl:up"),

    'kill command': Key("c-c"),
    'kill window': Key("c-d"),
    'interrupt': Key('c-c'),
    '(term|terminal) exit': Key('c-d'),


    'pip install': Text("pip install "), # pip is hard to recognize, so need custom command set
    'pip search': Text("pip search "),

    '(terminal|term) clear':        Key("c-l"),

    'jim':              Text("git "),
    'jim status':              Text("git status") + Key("enter"),
    'jim push':                Text("git push") + Key("enter"),
    'jim force push':                Text("git push -f") + Key("enter"),
    'jim add':             Text("git add "),
    'jim add all':             Text("git add -A") + Key("enter"),
    'jim commit':              Text("git commit") + Key("enter"),
    'jim change <n> commit':        Text("git rebase -i HEAD~[<n>]") + Key("enter"),
    'jim change <n> commit':        Text("git rebase -i HEAD~[<n>]") + Key("enter"),
    'jim rabbit continue':    Text("git rebase --continue") + Key("enter"),
    'jim rabbit abort':    Text("git rebase --abort") + Key("enter"),
    'jim amend':              Text("git commit --amend") + Key("enter"),
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

    'history':                      Key("c-r"),
    'open':                         Text("xdg-open "),
    'last parameter':               Key("escape, dot"),
    'view top':                     Text("htop") + Key("enter"),

    'redo command':          Key("c-u, up, enter"),
}


class Mapping(MappingRule):
    mapping = terminal_mapping
    extras = [  
        IntegerRef("n", 1, 1000),]
    defaults = {
        "n": 1,
    }

rules = Mapping()