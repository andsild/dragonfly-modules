""" Keyboard bindings like ctrl+home, copy/paste typing in smilies, etc
"""
from dragonfly import (
    MappingRule,
    Text,
    Key,
    Function,
    Dictation,
    Window,
    Config,
    Section,
    Item,
    IntegerRef,
    AppContext,
)

release = Key("shift:up, ctrl:up, alt:up")

def copy_command():
    # Add Command Prompt, putty, ...?
    context = AppContext(executable="console")
    window = Window.get_foreground()
    if context.matches(window.executable, window.title, window.handle):
        return
    release.execute()
    Key("c-c/3").execute()

def paste_command():
    # Add Command Prompt, putty, ...?
    context = AppContext(executable="console")
    window = Window.get_foreground()
    if context.matches(window.executable, window.title, window.handle):
        return
    release.execute()
    Key("c-v/3").execute()

def goto_page(n):
    Key("ctrl:down, home").execute()
    for _ in range(1, n):
        Key("pgdown").execute()
    Key("ctrl:up").execute()

grammarItems = {
        "go page [<n>]": Function(goto_page),
        "south [<n>]": Key("pgdown:%(n)d"),
        "left <n> (word|words)": Key("c-left/3:%(n)d/10"),
        "right <n> (word|words)": Key("c-right/3:%(n)d/10"),
        "doc home": Key("c-home/3"),
        "ghin buffer": Key("c-home/3"),
        "ghin end": Key("c-end/3"),
        "bum <n>": Function(lambda n: Text(str(n)).execute()),
        "fly fly": Key("alt:down, tab, alt:up"),
        "fly fly fly": Key("alt:down, tab, tab, alt:up"),
        "doc end": Key("c-end/3"),
        "ex buffer": Key("c-end/3"),
        "delete [this] line": Key("home, s-end, del"),  
        "application key": release + Key("apps/3"),
        "paste [that]": Function(paste_command),
        "copy [that]": Function(copy_command),
        "cut [that]": release + Key("c-x/3"),
        "select all": release + Key("c-a/3"),
        "double escape": Key("escape, escape"),  # Exiting menus.
        # Punctuation and separation characters, for quick editing.
        "cam ": Key("comma, space"),
        "sad smilie": Key("colon, lparen"),
        "slash smilie": Key("colon, slash"),
        "pee smilie": Key("colon, p"),
        "(delete|remove) rest": Key("s-end, del"),
}

grammarCfg = Config("multi edit")
grammarCfg.cmd = Section("Language section")
grammarCfg.cmd.map = Item(grammarItems,
    namespace={
        "Key": Key,
        "Text": Text,
    })

class KeystrokeRule(MappingRule):
    mapping = grammarCfg.cmd.map
    extras = [
        IntegerRef("n", 1, 100),
    ]
    defaults = {
        "n": 1,
        "ntimes": 1,
    }


rules = KeystrokeRule()
