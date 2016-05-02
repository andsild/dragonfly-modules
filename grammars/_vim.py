# Created for aenea using libraries from the Dictation Toolbox
# https://github.com/dictation-toolbox/dragonfly-scripts
#
# Commands for interacting with Vim
#
# Author: Tony Grosinger
# modified by: Anders Sildnes # # Licensed under LGPL 
import re
from dragonfly import (
    Dictation,
    Grammar,
    Key,
    MappingRule,
    Function,
    IntegerRef,
    Text,
    Choice,
    AppContext,
)

from _generic_edit import pressKeyMap, letters, letterMap
IS_WINDOWS = True

vim_context = AppContext(executable="devenv", title="Microsoft visual studio")
grammar = Grammar('vim', context=vim_context)

surroundCharsMap = {
    'quotes': '"',
    'parens': "(",
    'brackets': "[",
    'braces': "{",
}

symbolMap = {
    'dollar': 'dollar',
    'comma': ',',
    'period': 'dot',
    'laip': 'lparen',
    'lace': 'lbrace',
    'lack': 'lbracket',

    'race': 'rbrace',
    'rack': 'rbracket',
    'rye': 'rparen',
    'colon': 'colon',

    'sink': 'semi-colon',
    'quote': 'quote',
    #FIXME: two words are hard to parse (need singular for current logic)
    # 'single quote': 'single quote',
    'sing': 'squote',
    'equals': 'equal',
    'space': 'space',
}

repls = {
             'quote': 'dquote',
             'sing': 'squote',
             "colon": "colon",
             "lap": 'lparen',
        }
# When running DNS in windows, DNS will automatically replace some symbols
# e.g. when saying "underscore", it will input "_"
windows_special_cases = {
    '_': 'underscore'
}
repls.update(letters)
repls.update(letterMap)
repls.update(symbolMap)
if IS_WINDOWS:
    repls.update(windows_special_cases)

key = 'escape,'
if not IS_WINDOWS:
    key = 'c-backslash, c-n'

goto_normal_mode_keys = key
goto_normal_mode = Key(key)

def range_insert_symbol_logic(text):
    input_text = str(text).split()
    boolBig = "big" == input_text[0]
    if boolBig:
        input_text = input_text[1:]
    lenInput = len(input_text)
    returnWord = ""
    for ind,word in enumerate(input_text):
        word = word.lower()
        foundLetter = False
        for key,val in repls.iteritems():
            chars_to_remove = ['|', '(', ')']
            rx = '[' + re.escape(''.join(chars_to_remove)) + ']'
            newkey = re.sub(rx, ' ', key).split()
            if word in newkey:
                if boolBig:
                    returnWord += val.upper()
                else:
                    returnWord += val.lower()
                foundLetter = True
                break
        if not foundLetter:
            for letter in word:
                returnWord += letter
            if ind < lenInput-1:
                returnWord += ",space"
        returnWord += ','
    return returnWord[:-1]

def range_insert_symbol(text):
    for words in range_insert_symbol_logic(text).split(','):
        if words == "space":
            Key(words).execute()
        else:
            Key(','.join(words)).execute()


def goto_line_up(n):
    Key('m, squote').execute()
    for c in str(n):
        Key(c).execute()
    Key("k").execute()

def goto_line_absolute(n):
    for c in str(n):
        Key(c).execute()
    Key("G").execute()


def goto_line(n):
    Key('m, squote').execute()
    just_goto_line(n)

def just_goto_line(n):
    for c in str(n):
        Key(c).execute()
    Key("j").execute()


def lineJuggle(n1, n2, operation, linePrefix):
    upper_line=min(n1,n2)
    min_line=max(n1,n2)
    if upper_line==0:
        upper_line="."
    else:
        upper_line=linePrefix+str(upper_line)
    if min_line==0:
        min_line="."
    else:
        min_line=linePrefix+str(min_line)

    goto_normal_mode.execute()
    start=":"
    if not IS_WINDOWS:
        start= start + "silent "
    Text(start + str(min_line) + operation).execute()
    Key("enter").execute()


def yank_lines(n, n2):
    lineJuggle(n, n2, "y", "+")

def yank_lines_up(n, n2):
    lineJuggle(n, n2, "y", "-")

def delete_lines(n, n2):
    lineJuggle(n, n2, "d", "+")

def delete_lines_up(n, n2):
    lineJuggle(n, n2, "d", "-")

def delete_line(n):
    lineJuggle(n,n, "d", "+")
def delete_line_zero():
    lineJuggle(0,0, "d", "+")
def delete_line_up(n):
    lineJuggle(n,n, "d", "-")
def yank_line(n):
    lineJuggle(n,n, "y", "+")
def yank_line_zero():
    lineJuggle(0,0, "y", "+")
def yank_line_up(n):
    lineJuggle(n,n, "y", "-")


basics_mapping = {
    'vim': Text("vim"),

    # Moving between splits
    'split-side': Key(goto_normal_mode_keys + "semicolon, v"),
    'split-down': Key(goto_normal_mode_keys + "semicolon, s"),
    'split-close': Key(goto_normal_mode_keys + "Z, Q"),
    'open [in] split': Key("s"),
    'open [in] tab': Key(goto_normal_mode_keys + "semicolon, t"),

    'visual [mode]': Key("v"),
    'extend [<n>]': Key("rbrace:%(n)d"),
    'withdraw': Key("lbrace:%(n)d"),
    'visual block': Key("c-v"),
    'inner block': Key("i, b"),
    'paragraph': Key("a, p"),
    'visual line': Key("s-v"),
    'comment': Key("g, c, c"),

    'surround word [with] <surroundChar>': Key("s, a, W") + Text("%(surroundChar)s"),
    'stern <surroundChar>': Key("s, a, W") + Text("%(surroundChar)s"),

    # Moving viewport
    'set number': Key(goto_normal_mode_keys + "comma, dot"), 
    'screen center': Key(goto_normal_mode_keys + "z, dot, i"),
    'screen top': Key(goto_normal_mode_keys + "z, t, i"),
    'screen bottom': Key(goto_normal_mode_keys + "z, b, i"),

    # Append to line
    'noop <n>': goto_normal_mode + Function(goto_line) + Key("o"),
    'nope <n>': goto_normal_mode + Function(goto_line) + Key("A"),

    'prepend': Key(goto_normal_mode_keys + "I"),
    'insert': Key(goto_normal_mode_keys + "i"),
    'insert below': Key(goto_normal_mode_keys + "o"),
    'insert above': Key(goto_normal_mode_keys + "O"),
    'undo': Key(goto_normal_mode_keys + "u"),
    'read': Key(goto_normal_mode_keys + "c-r"),
    'scratch': Key(goto_normal_mode_keys + "u"),
    'escape': Key("escape"),
    'filename': Key(goto_normal_mode_keys + "c-g"),
    'save': Key(goto_normal_mode_keys + "colon, w, enter"),
    'save and quit': Key(goto_normal_mode_keys + "colon, w, q, enter"),
    'quit all': Key(goto_normal_mode_keys + "colon, q, a, enter"),
    'discard': Key(goto_normal_mode_keys + "colon, q, exclamation"),
    '(vim|vic) tab <n>': Key(goto_normal_mode_keys + "comma, %(n)d"),
    'comma': Key("comma"),
    '(rook|Brook|rock)': Key("right, colon, space"),

    'quick (prev|previous)': Key("lbracket, q"),
    'quick next': Key(goto_normal_mode_keys + "rbracket, q"),
    'location (prev|previous)': Key(goto_normal_mode_keys + "lbracket, l"),
    'location next': Key(goto_normal_mode_keys + "rbracket, l"),

    # Finding text
    #'find <text>': Key(goto_normal_mode_keys + "slash") + Text("%(text)s"),
    'jump <text>': Key(goto_normal_mode_keys + "slash, backslash, c") + Function(range_insert_symbol),
    'next': Key(goto_normal_mode_keys + "n"),
    'prev|previous': Key(goto_normal_mode_keys + "N"),
    'clear search': Key(goto_normal_mode_keys + "colon, n, o, h, enter"),
    'change [the] big word': Key(goto_normal_mode_keys + "c, a, W"),
    'change [the] word': Key(goto_normal_mode_keys + "c, a, w"),
    '(Sea|See) world': Key(goto_normal_mode_keys + "c, a, w"),
    'change inner block': Key(goto_normal_mode_keys + "c, i, b"),
    'yank inner block': Key(goto_normal_mode_keys + "y, i, b"),
    '(del|delete) inner block': Key(goto_normal_mode_keys + "d, i, b"),
    '(pseudo|sudo|pseudo-) save': goto_normal_mode + Text(":w !sudo tee > /dev/null %%") + Key("enter"),
    'remove [the] word': Key(goto_normal_mode_keys + "d, a, w"),
    'remove [the] big word': Key(goto_normal_mode_keys + "d, a, W"),
    'change [the] word': Key(goto_normal_mode_keys + "c, a, W"),
    'gargle': Key(goto_normal_mode_keys + "D"),
    'behind [<n>]': Key(goto_normal_mode_keys + "e:%(n)d"),
    'ass [<n>]': Key(goto_normal_mode_keys + "E:%(n)d"),

    # Character operations
    'dart': Key("x"),
    'dart <n>': Key("x:%(n)d"),
    'replace letter': Key("r"),
    'replace mode': Key("R"),
    'change case': Key(goto_normal_mode_keys + "right, s-backtick"),

    # Window movement
    'window': Key("c-w"),
    'remove [the] buffer': Key(goto_normal_mode_keys + "semicolon, d"),

    # Word operations
    'sword <n>': Key(goto_normal_mode_keys + "%(n)d, w"),
    'forward <n>': Key(goto_normal_mode_keys + "%(n)d, w"),
    'backward <n>': Key(goto_normal_mode_keys + "%(n)d, b"),
    'start': goto_normal_mode + Text("^"),
    'finish': goto_normal_mode + Text("$"),
    'quick run': goto_normal_mode + Key("comma, r"),

    'command mode': goto_normal_mode + Key("colon"),

    # Line operations
    'dine': goto_normal_mode + Function(delete_line_zero),
    'dine <n>': goto_normal_mode + Function(delete_line),
    'dine up <n>': goto_normal_mode + Function(delete_line_up),
    'dine <n> (thru|through|to) <n2>': goto_normal_mode + Function(delete_lines),
    'dine up <n> (thru|through|to) <n2>': goto_normal_mode + Function(delete_lines_up),
    'yank ': goto_normal_mode + Function(yank_line_zero),
    'yank [down] <n>': goto_normal_mode + Function(yank_line),
    'yank up <n>': goto_normal_mode + Function(yank_line_up),
    'yank <n> (thru|through|to) <n2>': goto_normal_mode + Function(yank_lines),
    'yank up <n> (thru|through|to) <n2>': goto_normal_mode + Function(yank_lines_up),

    'select until <text>': Key(goto_normal_mode_keys + "v, t") + Function(range_insert_symbol),
    'select including <text>': Key(goto_normal_mode_keys + "v, f") + Function(range_insert_symbol),
    'dell until <text>': Key(goto_normal_mode_keys + "d, t") + Function(range_insert_symbol),
    'dell including <text>': Key(goto_normal_mode_keys + "d, f") + Function(range_insert_symbol),
    '(see|sea) until <text>': Key(goto_normal_mode_keys + "c, t") + Function(range_insert_symbol),
    '(see|sea) including <text>': Key(goto_normal_mode_keys + "c, f") + Function(range_insert_symbol),

    # Fancy operations
    'clay': Key(goto_normal_mode_keys + "c, i, b"),

    # Movement
    'up <n> (lines|line)': Key("%(n)d, k"),
    'down <n> (lines|line)': Key("%(n)d, j"),
    'go [down] [to] [line] <n>': goto_normal_mode + Function(goto_line),
    'go up [to] [line] <n>': goto_normal_mode + Function(goto_line_up),
    'go absolute to [line] <n>': goto_normal_mode + Function(goto_line_absolute),
    'matching': Key(goto_normal_mode_keys + "percent"),
    'rash': Key(goto_normal_mode_keys + "down, s-a"),
    'back': Key(goto_normal_mode_keys + "c-o"),

    'next tab': Key("g, t"),
    'previous tab': Key("g, s-t"),
    'new tab': goto_normal_mode + Text(":tabe | FZFMru") + Key("enter"),

    # Plug-ins
    'explorer': Key(goto_normal_mode_keys + "colon") + Text("VimFilerExplorer") + Key("enter"),
    }


class VimRule(MappingRule):
    mapping = basics_mapping
    extras = [
        Dictation('text'),
        IntegerRef('n', 0, 999),
        IntegerRef('n2', 0, 999),
        Choice("surroundChar", surroundCharsMap),
    ]
    defaults = {
        "n": 1,  # Default repeat count.
    }

rules = VimRule()
#grammar.add_rule(Basics())
#grammar.load()


#def unload():
    #global grammar
    #if grammar:
        #grammar.unload()
    #grammar = None
