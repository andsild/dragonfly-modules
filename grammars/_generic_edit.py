# Based on the work done by the creators of the Dictation Toolbox
# https://github.com/dictation-toolbox/dragonfly-scripts
#
# and _multiedit-en.py found at:
# http://dragonfly-modules.googlecode.com/svn/trunk/command-modules/documentation/mod-_multiedit.html
#
# Modifications by: Tony Grosinger
#
# Licensed under LGPL


from natlink import setMicState
from dragonfly import (
    MappingRule,
    Text,
    Key,
    Function,
    Dictation,
    Choice,
    Window,
    Config,
    Section,
    Item,
    IntegerRef,
    Alternative,
    RuleRef,
    Repetition,
    CompoundRule,
    AppContext,
)


from dragonfly.actions.keyboard import keyboard
from dragonfly.actions.typeables import typeables
if 'semicolon' not in typeables:
    typeables["semicolon"] = keyboard.get_typeable(char=';')

from utility._format import (
    format_text_logic,
    FormatTypes as ft,
)
from utility.lettermap import *

def format_text(text, format_type_list=None):
    result = format_text_logic(str(text), format_type_list)
    Text("%(text)s").execute({"text": result})


release = Key("shift:up, ctrl:up, alt:up")


def cancel_and_sleep(text=None, text2=None):
    """Used to cancel an ongoing dictation and puts microphone to sleep.

    This method notifies the user that the dictation was in fact canceled,
     a message in the Natlink feedback window.
    Then the the microphone is put to sleep.
    Example:
    "'random mumbling go to sleep'" => Microphone sleep.

    """
    print("* Dictation canceled. Going to sleep. *")
    setMicState("sleeping")


# For repeating of characters.
specialCharMap = {
    "(bar|vertical bar|pipe)": "|",
    "(dash|minus|hyphen)": "-",
    "(dot|period)": ".",
    "comma": ",",
    "backslash": "\\",
    "underscore": "_",
    "(star|asterisk)": "*",
    "colon": ":",
    "(semicolon|semi-colon)": ";",
    "at": "@",
    "[double] quote": '"',
    "single quote": "'",
    "hash": "#",
    "dollar": "$",
    "percent": "%",
    # "and": "&",
    "slash": "/",
    "equal": "=",
    "plus": "+",
    "space": " ",
    "tilde": "~",
}

# Modifiers for the press-command.
modifierMap = {
    "alt": "a",
    "control": "c",
    "shift": "s",
    "super": "w",
}

# Modifiers for the press-command, if only the modifier is pressed.
singleModifierMap = {
    "alt": "alt",
    "control": "ctrl",
   "shift": "shift",
    "super": "win",
}

numberMap = {
    "zero":  "0",
    "one":   "1",
    "two":   "2",
    "three": "3",
    "four":  "4",
    "five":  "5",
    "six":   "6",
    "seven": "7",
    "eight": "8",
    "nine":  "9",
}

controlKeyMap = {
    "left":                "left",
    "right":               "right",
    "up":                  "up",
    "down":                "down",
    "page up":             "pgup",
    "page down":           "pgdown",
    "home":                "home",
    "end":                 "end",
    "space":               "space",
    "(enter|return|slap)": "enter",
    "escape":              "escape",
    "tab":                 "tab"
}

# F1 to F12.
functionKeyMap = {
    'F one':    'f1',
    'F two':    'f2',
    'F three':  'f3',
    'F four':   'f4',
    'F five':   'f5',
    'F six':    'f6',
    'F seven':  'f7',
    'F eight':  'f8',
    'F nine':   'f9',
    'F ten':    'f10',
    'F eleven': 'f11',
    'F twelve': 'f12',
}

pressKeyMap = {}
pressKeyMap.update(Phonetic_Letter_map)
pressKeyMap.update(numberMap)
pressKeyMap.update(controlKeyMap)
pressKeyMap.update(functionKeyMap)


formatMap = {
    "(sentence|sense|since) case": [ft.sentenceCase],
    "camel":                       [ft.camelCase],
    "pass":                        [ft.pascalCase],
    "snake":                       [ft.snakeCase],
    "uppercase":                   [ft.upperCase],
    "dwarf":                   [ft.lowerCase],
    "mumble case":                   [ft.lowerCase],
    "squash":                      [ft.squash],
    "wash":                      [ft.squash],
    "lowercase squash":            [ft.squash, ft.lowerCase],
    "uppercase squash":            [ft.squash, ft.upperCase],
    "squash lowercase":            [ft.squash, ft.lowerCase],
    "squash uppercase":            [ft.squash, ft.upperCase],
    "dashify":                     [ft.dashify],
    "lowercase dashify":           [ft.dashify, ft.lowerCase],
    "uppercase dashify":           [ft.dashify, ft.upperCase],
    "dashify lowercase":           [ft.dashify, ft.lowerCase],
    "dashify uppercase":           [ft.dashify, ft.upperCase],
    "dotify":                      [ft.dotify],
    "lowercase dotify":            [ft.dotify, ft.lowerCase],
    "uppercase dotify":            [ft.dotify, ft.upperCase],
    "dotify lowercase":            [ft.dotify, ft.lowerCase],
    "dotify uppercase":            [ft.dotify, ft.upperCase],
    "say":                         [ft.spokenForm],
    "environment variable":        [ft.snakeCase, ft.upperCase],
}


abbreviationMap = {
    "administrator": "admin",
    "administrators": "admins",
    "application": "app",
    "applications": "apps",
    "argument": "arg",
    "arguments": "args",
    "attribute": "attr",
    "attributes": "attrs",
    "(authenticate|authentication)": "auth",
    "binary": "bin",
    "button": "btn",
    "class": "cls",
    "command": "cmd",
    "(config|configuration)": "cfg",
    "context": "ctx",
    "control": "ctrl",
    "database": "db",
    "(define|definition)": "def",
    "description": "desc",
    "(develop|development)": "dev",
    "(dictionary|dictation)": "dict",
    "(direction|directory)": "dir",
    "dynamic": "dyn",
    "example": "ex",
    "execute": "exec",
    "exception": "exc",
    "expression": "exp",
    "(extension|extend)": "ext",
    "function": "func",
    "framework": "fw",
    "(initialize|initializer)": "init",
    "instance": "inst",
    "integer": "int",
    "iterate": "iter",
    "java archive": "jar",
    "javascript": "js",
    "keyword": "kw",
    "keyword arguments": "kwargs",
    "language": "lng",
    "library": "lib",
    "length": "len",
    "maximum": "max",
    "mininum": "min",
    "number": "num",
    "object": "obj",
    "okay": "ok",
    "package": "pkg",
    "parameter": "param",
    "parameters": "params",
    "pixel": "px",
    "position": "pos",
    "point": "pt",
    "previous": "prev",
    "property": "prop",
    "python": "py",
    "query string": "qs",
    "reference": "ref",
    "references": "refs",
    "(represent|representation)": "repr",
    "regular (expression|expressions)": "regex",
    "request": "req",
    "revision": "rev",
    "ruby": "rb",
    "session aidee": "sid",  # "session id" didn't work for some reason.
    "source": "src",
    "(special|specify|specific|specification)": "spec",
    "standard": "std",
    "standard in": "stdin",
    "standard out": "stdout",
    "string": "str",
    "(synchronize|synchronous)": "sync",
    "system": "sys",
    "utility": "util",
    "utilities": "utils",
    "temporary": "tmp",
    "text": "txt",
    "value": "val",
    "window": "win",
}

# For use with "say"-command. Words that are commands in the generic edit
# grammar were treated as separate commands and could not be written with the
# "say"-command. This overrides that behavior.
# Other words that won't work for one reason or another, can also be added to
# this list.
reservedWord = {
    "up": "up",
    "down": "down",
    "left": "left",
    "right": "right",
    "home": "home",
    "end": "end",
    "space": "space",
    "tab": "tab",
    "backspace": "backspace",
    "delete": "delete",
    "enter": "enter",
    "paste": "paste",
    "copy": "copy",
    "cut": "cut",
    "undo": "undo",
    "release": "release",
    "page up": "page up",
    "page down": "page down",
    "say": "say",
    "select": "select",
    "select all": "select all",
    "short": "abbreviate",
    "uppercase": "uppercase",
    "lowercase": "lowercase",
    "expand": "expand",
    "squash": "squash",
    "dash": "dash",
    "underscore": "underscore",
    "dot": "dot",
    "period": "period",
    "minus": "minus",
    "semi-colon": "semi-colon",
    "hyphen": "hyphen",
    "triple": "triple"
}


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



def translate_to_number(n):
    Text(str(n)).execute()

grammarItems = {
        # Navigation keys.
        "up [<n>]": Key("up:%(n)d"),
        "up [<n>] slow": Key("up/15:%(n)d"),
        "down [<n>]": Key("down:%(n)d"),
        "down [<n>] slow": Key("down/15:%(n)d"),
        "left [<n>]": Key("left:%(n)d"),
        "left [<n>] slow": Key("left/15:%(n)d"),
        "right [<n>]": Key("right:%(n)d"),
        "right [<n>] slow": Key("right/15:%(n)d"),
        "page up [<n>]": Key("pgup:%(n)d"),
        "north [<n>]": Key("pgup:%(n)d"),
        "page down [<n>]": Key("pgdown:%(n)d"),
        "south [<n>]": Key("pgdown:%(n)d"),
        "up <n> (page|pages)": Key("pgup:%(n)d"),
        "down <n> (page|pages)": Key("pgdown:%(n)d"),
        "left <n> (word|words)": Key("c-left/3:%(n)d/10"),
        "right <n> (word|words)": Key("c-right/3:%(n)d/10"),
        "home": Key("home"),
        "(luke|look)": Key("lessthan"),
        "(ruke|rook)": Key("greaterthan"),
        "end": Key("end"),
        "doc home": Key("c-home/3"),
        "ghin buffer": Key("c-home/3"),
        "ghin end": Key("c-end/3"),
        "bum <n>": Function(translate_to_number),
        "doc end": Key("c-end/3"),
        "ex buffer": Key("c-end/3"),
        "quest": Key("question"),
        "bang": Key("exclamation"),
        "space": release + Key("space"),
        "(space|ace) [<n>]": release + Key("space:%(n)d"),
        "enter [<n>]": release + Key("enter:%(n)d"),
        "tab [<n>]": Key("tab:%(n)d"),
        "delete [<n>]": Key("del/3:%(n)d"),
        "delete [this] line": Key("home, s-end, del"),  # @IgnorePep8
        "backspace [<n>]": release + Key("backspace:%(n)d"),
        "chook [<n>]": release + Key("backspace:%(n)d"),
        "application key": release + Key("apps/3"),
        "win key": release + Key("win/3"),
        "paste [that]": Function(paste_command),
        "copy [that]": Function(copy_command),
        "cut [that]": release + Key("c-x/3"),
        "select all": release + Key("c-a/3"),
        "[(hold|press)] alt": Key("alt:down/3"),
        "release alt": Key("alt:up"),
        "[(hold|press)] shift": Key("shift:down/3"),
        "release shift": Key("shift:up"),
        "[(hold|press)] control": Key("ctrl:down/3"),
        "release control": Key("ctrl:up"),
        "release [all]": release,
        "(sink|think)": Key("semicolon"),
        # Closures.
        "angle brackets": Key("langle, rangle, left/3"),
        "brackets": Key("lbracket, rbracket, left/3"),
        "braces": Key("lbrace, rbrace, left/3"),
        "parens": Key("lparen, rparen, left/3"),
        "laip": Key("lparen"),
        "rye": Key("rparen"),
        "lace": Key("lbrace"),
        "race": Key("rbrace"),
        "lack": Key("lbracket"),
        "rack": Key("rbracket"),
        "backticks": Key("backtick:2, left"),
        "sing": Key("squote"),
        # Shorthand multiple characters.
        "double <char>": Text("%(char)s%(char)s"),
        "triple <char>": Text("%(char)s%(char)s%(char)s"),
        "double escape": Key("escape, escape"),  # Exiting menus.
        # Punctuation and separation characters, for quick editing.
        "colon [<n>]": Key("colon/2:%(n)d"),
        "Col. [<n>]": Key("colon/2:%(n)d"),
        "semi-colon [<n>]": Key("semicolon/2:%(n)d"),
        "comma [<n>]": Key("comma/2:%(n)d"),
        "cam ": Key("comma, space"),
        "(dot|period) [<n>]": Key("dot/2:%(n)d"),
        "(dash|hyphen|minus) [<n>]": Key("hyphen/2:%(n)d"),
        "underscore [<n>]": Key("underscore/2:%(n)d"),
        "<letters>": Text("%(letters)s"),
        "<char>": Text("%(char)s"),
        # Format dictated words. See the formatMap for all available types.
        # Ex: "camel case my new variable" -> "myNewVariable"
        # Ex: "snake case my new variable" -> "my_new_variable"
        # Ex: "uppercase squash my new hyphen variable" -> "MYNEW-VARIABLE"
        "<format_type_list> <text>": Function(format_text),
        # For writing words that would otherwise be characters or commands.
        # Ex: "period", tab", "left", "right", "home".
        "say <reservedWord>": Text("%(reservedWord)s"),
        # Abbreviate words commonly used in programming.
        # Ex: arguments -> args, parameters -> params.
        "short <short>": Text("%(short)s"),
        # Text corrections.
        "(delete|remove) rest": Key("s-end, del"),
        # Microphone sleep/cancel started dictation.
        "[<text>] (go to sleep|cancel and sleep) [<text2>]": Function(cancel_and_sleep),  # @IgnorePep8

}

voiceLetters = {}

for voice_command,key in Shorttalk_Letters.iteritems():
    voiceLetters[voice_command] = Key(key)


grammarItems.update(voiceLetters)

grammarCfg = Config("multi edit")
grammarCfg.cmd = Section("Language section")
grammarCfg.cmd.map = Item(
    grammarItems,
    namespace={
        "Key": Key,
        "Text": Text,
    }
)



class KeystrokeRule(MappingRule):
    exported = False
    mapping = grammarCfg.cmd.map
    extras = [
        IntegerRef("n", 1, 100),
        IntegerRef("ntimes", 1, 100),
        Dictation("text"),
        Dictation("text2"),
        Choice("char", specialCharMap),
        Choice("letters", Phonetic_Letter_map),
        Choice("modifier1", modifierMap),
        Choice("modifier2", modifierMap),
        Choice("modifierSingle", singleModifierMap),
        Choice("pressKey", pressKeyMap),
        Choice("format_type_list", formatMap),
        Choice("short", abbreviationMap),
        Choice("reservedWord", reservedWord),
    ]
    defaults = {
        "n": 1,
        "ntimes": 1,
    }


alternatives = []
alternatives.append(RuleRef(rule=KeystrokeRule()))
single_action = Alternative(alternatives)


sequence = Repetition(single_action, min=1, max=16, name="sequence")


class RepeatRule(CompoundRule):
    # Here we define this rule's spoken-form and special elements.
    spec = "<sequence> [[[and] repeat [that]] <n> times]"
    extras = [
        sequence,  
        IntegerRef("n", 1, 1000), 
    ]
    defaults = {
        "n": 1,  # Default repeat count.
        "ntimes": 1,  # Default repeat count.
    }

    def _process_recognition(self, node, extras):  # @UnusedVariable
        sequence = extras["sequence"]  # A sequence of actions.
        count = extras["n"]  # An integer repeat count.
        for i in range(count):  # @UnusedVariable
            for action in sequence:
                action.execute()

rules = RepeatRule()
