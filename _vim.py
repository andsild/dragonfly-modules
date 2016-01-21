# Created for aenea using libraries from the Dictation Toolbox
# https://github.com/dictation-toolbox/dragonfly-scripts
#
# Commands for interacting with Vim
#
# Author: Tony Grosinger
#
# Licensed under LGPL

import aenea
import aenea.configuration
from aenea.lax import Key, Function
from aenea import (
    Dictation,
    IntegerRef,
    Text,
    Choice
)
import dragonfly

from _generic_edit import pressKeyMap

vim_context = aenea.ProxyCustomAppContext(executable="gnome-terminal")
grammar = dragonfly.Grammar('vim', context=vim_context)

surroundCharsMap = {
    'quotes': '"',
    'parens': "(",
    'brackets': "[",
    'braces': "{",
}

def goto_line_up(n):
    Key('m, squote').execute()
    for c in str(n):
        Key(c).execute()
    Key("k").execute()

def goto_line_absolute():
    for c in str(n):
        Key(c).execute()
    Key("G").execute()


def goto_line(n):
    Key('m, squote').execute()
    for c in str(n):
        Key(c).execute()
    Key("j").execute()


def yank_lines(n, n2):
    goto_line(n)
    Key("V").execute()
    goto_line(n2)
    Key("y").execute()

def yank_lines_up(n, n2):
    upper_line=max(n,n2)
    min_line=min(n,n2)
    goto_line_up(upper_line)
    Key("V").execute()
    goto_line(min_line)
    Key("y").execute()

def delete_lines(n, n2):
    goto_line(n)
    Key("V").execute()
    goto_line(n2)
    Key("d").execute()

def delete_lines_up(n, n2):
    upper_line=max(n,n2)
    min_line=min(n,n2)
    goto_line_up(upper_line)
    Key("V").execute()
    goto_line(min_line)
    Key("d").execute()

goto_normal_mode_keys = 'c-backslash, c-n, '
goto_normal_mode = Key('c-backslash, c-n')


basics_mapping = aenea.configuration.make_grammar_commands('vim', {
    'vim': Text("vim"),

    # Moving between splits
    'split-side': Key(goto_normal_mode_keys + "semicolon, v"),
    'split-down': Key(goto_normal_mode_keys + "semicolon, s"),
    'split-close': Key(goto_normal_mode_keys + "Z, Q"),
    'open [in] split': Key("s"),
    'open [in] tab': Key(goto_normal_mode_keys + "semicolon, t"),

    'visual [mode]': Key("v"),
    'visual block': Key("c-v"),
    'inner block': Key("i, b"),
    'paragraph': Key("a, p"),
    'visual line': Key("s-v"),
    'comment': Key(goto_normal_mode_keys + "g, c, c"),

    # Moving viewport
    'set number': Key(goto_normal_mode_keys + "comma, period"), 
    'screen center': Key(goto_normal_mode_keys + "z, period, i"),
    'screen top': Key(goto_normal_mode_keys + "z, t, i"),
    'screen bottom': Key(goto_normal_mode_keys + "z, b, i"),

    # Append to line
    'noop <n>': goto_normal_mode + Function(goto_line) + Key("A, enter"),
    'noop': Key(goto_normal_mode_keys + "A, enter"),
    'nope': Key(goto_normal_mode_keys + "A"),
    'nope <n>': goto_normal_mode + Function(goto_line) + Key("A"),

    'prepend': Key(goto_normal_mode_keys + "I"),
    'insert': Key(goto_normal_mode_keys + "i"),
    'insert below': Key(goto_normal_mode_keys + "o"),
    'insert above': Key(goto_normal_mode_keys + "O"),
    'undo': Key(goto_normal_mode_keys + "u"),
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

    # Finding text
    'find <text>': Key(goto_normal_mode_keys + "slash") + Text("%(text)s"),
    'jump <text>': Key(goto_normal_mode_keys + "f") + Text("%(text)s"),
    'next': Key(goto_normal_mode_keys + "n"),
    'prev|previous': Key(goto_normal_mode_keys + "N"),
    'clear search': Key(goto_normal_mode_keys + "colon, n, o, h, enter"),
    'change [the] big word': Key(goto_normal_mode_keys + "c, a, W"),
    'change [the] word': Key(goto_normal_mode_keys + "c, a, w"),
    'change inner block': Key(goto_normal_mode_keys + "c, i, b"),
    'yank inner block': Key(goto_normal_mode_keys + "y, i, b"),
    '(del|delete) inner block': Key(goto_normal_mode_keys + "y, i, b"),
    '(pseudo|sudo|pseudo-) save': goto_normal_mode + Text(":w !sudo tee > /dev/null %%") + Key("enter"),
    'remove [the] word': Key(goto_normal_mode_keys + "d, a, w"),
    'remove [the] big word': Key(goto_normal_mode_keys + "d, a, W"),
    'change [the] word': Key(goto_normal_mode_keys + "c, a, W"),
    'gargle': Key(goto_normal_mode_keys + "D"),
    'choose': Key("semicolon"),
    'behind <n>': Key(goto_normal_mode_keys + "e:%(n)d"),
    'ass <n>': Key(goto_normal_mode_keys + "E:%(n)d"),

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
    'forward':  Key(goto_normal_mode_keys + "w"),
    'forward <n>': Key(goto_normal_mode_keys + "%(n)d, w"),
    'backward': Key(goto_normal_mode_keys + "b"),
    'backward <n>': Key(goto_normal_mode_keys + "%(n)d, b"),
    'start': goto_normal_mode + Text("^"),
    'finish': goto_normal_mode + Text("$"),

    # Line operations
    'dine': Key(goto_normal_mode_keys + "d:2"),
    'dine <n>': goto_normal_mode + Function(goto_line) + Key("d:2"),
    'dine up <n>': goto_normal_mode + Function(goto_line_up) + Key("d:2, c-o"),
    'dine <n> (thru|through|to) <n2>': goto_normal_mode + Function(delete_lines),
    'dine up <n> (thru|through|to) <n2>': goto_normal_mode + Function(delete_lines_up),
    'yank': goto_normal_mode + Key("y:2"),
    'yank [down] <n>': goto_normal_mode + Function(goto_line) + Key("y:2, c-o"),
    'yank up <n>': goto_normal_mode + Function(goto_line_up) + Key("y:2, c-o"),
    'yank <n> (thru|through|to) <n2>': goto_normal_mode + Function(yank_lines) + Key("c-o"),
    'yank up <n> (thru|through|to) <n2>': goto_normal_mode + Function(yank_lines_up) + Key("c-o"),

    'select until <pressKey>': Key(goto_normal_mode_keys + "v, t") + Text("%(pressKey)s"),
    'select including <pressKey>': Key(goto_normal_mode_keys + "v, f") + Text("%(pressKey)s"),
    'dell until <pressKey>': Key(goto_normal_mode_keys + "d, t") + Text("%(pressKey)s"),
    'dell including <pressKey>': Key(goto_normal_mode_keys + "d, f") + Text("%(pressKey)s"),

    # Fancy operations
    'clay': Key(goto_normal_mode_keys + "c, i, dqoute"),
    'yip': Key(goto_normal_mode_keys + "right, y, i, lparen"),
    'yib': Key(goto_normal_mode_keys + "right, y, i lbrace"),

    # Copy and Paste
    'extract': Key("x"),
    'glue': Key(goto_normal_mode_keys + 'p'),

    # Movement
    'up <n> (lines|line)': Key("%(n)d, k"),
    'down <n> (lines|line)': Key("%(n)d, j"),
    'go [down] [to] [line] <n>': goto_normal_mode + Function(goto_line),
    'go up [to] [line] <n>': goto_normal_mode + Function(goto_line_up),
    'go absolute to [line] <n>': goto_normal_mode + Function(goto_line_absolute),
    'matching': Key(goto_normal_mode_keys + "percent"),
    'rash': Key(goto_normal_mode_keys + "down, s-a"),
    'back': Key(goto_normal_mode_keys + "c-o"),

    'tab next': Key("g, t"),
    'tab previous': Key("g, s-t"),
    'tab new': goto_normal_mode + Text(":tabe | FZFMru") + Key("enter"),

    # Plug-ins
    'explorer': Key(goto_normal_mode_keys + "colon") + Text("VimFilerExplorer") + Key("enter"),
    })


class Basics(dragonfly.MappingRule):
    mapping = basics_mapping
    extras = [
        Dictation('text'),
        IntegerRef('n', 1, 999),
        IntegerRef('n2', 1, 999),
        Choice("pressKey", pressKeyMap),
        Choice("surroundChar", surroundCharsMap),
    ]

grammar.add_rule(Basics())
grammar.load()


def unload():
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
