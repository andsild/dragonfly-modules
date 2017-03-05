# Created for aenea using libraries from the Dictation Toolbox
# https://github.com/dictation-toolbox/dragonfly-scripts
#
# Commands for interacting with Vim
#
# Author: Tony Grosinger
# modified by: Anders Sildnes # # Licensed under LGPL 

from utility.vim_logic import lineJuggle_logic

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

from utility.substitute_phrase import translate_spokenform_to_queryform
IS_WINDOWS = True

#vim_context = AppContext(executable="devenv", title="Microsoft visual studio")
#grammar = Grammar('vim', conteimport __builtin__
#context=vim_context)

surroundCharsMap = {
    'quotes': '"',
    'parens': "(",
    'brackets': "[",
    'braces': "{",
}

key = 'escape,'
if not IS_WINDOWS:
    key = 'c-backslash, c-n'

goto_normal_mode_keys = key
goto_normal_mode = Key(key)

def goto_line_absolute(n):
    for c in str(n):
        Key(c).execute()
    Key("G").execute()

def just_goto_line(n, dir):
    Key('m, squote').execute()
    if n <= 0:
        return
    for c in str(n):
        Key(c).execute()
    Key(dir).execute()

def lineJuggle(n1, n2, operation, linePrefix):
    goto_normal_mode.execute()
    Text(lineJuggle_logic(n1, n2, operation, linePrefix)).execute()
    Key("enter").execute()

basics_mapping = {
    'vim': Text("vim"),

    # Moving between splits
#    'split-side': Key(goto_normal_mode_keys + "semicolon, v"),
#    'split-down': Key(goto_normal_mode_keys + "semicolon, s"),
    'split-close': Key(goto_normal_mode_keys + "Z, Q"),
    'open [in] split': Key("s"),
#    'open [in] tab': Key(goto_normal_mode_keys + "semicolon, t"),

    'visual block': Key("c-v"),
    'inner block': Key("i, b"),
    'paragraph': Key("a, p"),
    'visual line': Key("s-v"),

    'comment': Key("g, c, c"),
    'surround word [with] <surroundChar>': Key("s, a, W") + Text("%(surroundChar)s"),
    'stern <surroundChar>': Key("s, a, W") + Text("%(surroundChar)s"),

    # Moving viewport
    'set number': Key(goto_normal_mode_keys + "comma, dot"), 
    'screen center': Key(goto_normal_mode_keys + "z, dot"),
    'screen top': Key(goto_normal_mode_keys + "z, t"),
    'screen bottom': Key(goto_normal_mode_keys + "z, b"),

    # Append to line
    'noop [<n0>]': goto_normal_mode + Function(lambda n0: just_goto_line(n0, 'j')) + Key("o"),
    'nope [<n0>]': goto_normal_mode + Function(lambda n0: just_goto_line(n0, 'j')) + Key("A"),
    'noop up [<n0>]': goto_normal_mode + Function(lambda n0: just_goto_line(n0, 'k')) + Key("o"),
    'nope up [<n0>]': goto_normal_mode + Function(lambda n0: just_goto_line(n0, 'k')) + Key("A"),

    'prepend': Key(goto_normal_mode_keys + "I"),
    'insert below': Key(goto_normal_mode_keys + "o"),
    'insert above': Key(goto_normal_mode_keys + "O"),
    'regret': Key(goto_normal_mode_keys + "u"),
    'read': Key(goto_normal_mode_keys + "c-r"),
    'escape': Key("escape"),
    'filename': Key(goto_normal_mode_keys + "c-g"),
    'save and quit': Key(goto_normal_mode_keys + "colon, w, q, enter"),
    'save': Key(goto_normal_mode_keys + "colon, w, enter"),
    'quit all': Key(goto_normal_mode_keys + "colon, q, a, enter"),
    'discard': Key(goto_normal_mode_keys + "colon, q, exclamation"),
    '(vim|vic) tab <n>': Key(goto_normal_mode_keys + "comma, %(n)d"),
    'comma': Key("comma"),
    '(rook|Brook|rock)': Key("right, colon, space"),
    'ghin front': Key(goto_normal_mode_keys + "zero, w"),
    'ghin back': Key(goto_normal_mode_keys + "dollar"),
    'auto format': Key(goto_normal_mode_keys + "colon, A, u, t, o, f, o, r, m, a, t, enter"),

    'quick (prev|previous)': Key("lbracket, q"),
    'quick next': Key(goto_normal_mode_keys + "rbracket, q"),
    'location (prev|previous)': Key(goto_normal_mode_keys + "lbracket, l"),
    'location next': Key(goto_normal_mode_keys + "rbracket, l"),
    

    # Finding text
    #'find <text>': Key(goto_normal_mode_keys + "slash") + Text("%(text)s"),
    'jump <text>': Key("escape, slash") + Function(translate_spokenform_to_queryform),
    'next': Key(goto_normal_mode_keys + "n"),
    'prev|previous': Key(goto_normal_mode_keys + "N"),
    'clear search': Key(goto_normal_mode_keys + "colon, n, o, h, enter"),
    'change [the] big word': Key(goto_normal_mode_keys + "c, a, W"),
    'change [the] word': Key(goto_normal_mode_keys + "c, a, w"),
    '(Sea|See) world': Key(goto_normal_mode_keys + "c, a, w"),
    '(Sea|See) inner block': Key(goto_normal_mode_keys + "c, i, b"),
    '(Sea|See) inner quote': Key(goto_normal_mode_keys + "c, i, quote"),
    '(Sea|See) inner sing': Key(goto_normal_mode_keys + "c, i, squote"),
    'dine inner block': Key(goto_normal_mode_keys + "d, i, b"),
    'dine inner quote': Key(goto_normal_mode_keys + "d, i, quote"),
    'dine inner sing': Key(goto_normal_mode_keys + "d, i, squote"),
    '(Sea|see) inner quote': Key(goto_normal_mode_keys + "c, i, quote"),
    'yank inner block': Key(goto_normal_mode_keys + "y, i, b"),
    'yank line': Key(goto_normal_mode_keys + "y, y"),
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
    'change case': Key(goto_normal_mode_keys + "s-backtick"),

    # Window movement
    'window': Key("c-w"),
    #'remove [the] buffer': Key(goto_normal_mode_keys + "semicolon, d"),

    # Word operations
    'sword <n>': Key(goto_normal_mode_keys + "%(n)d, w"),
    'forward <n>': Key(goto_normal_mode_keys + "%(n)d, w"),
    'backward <n>': Key(goto_normal_mode_keys + "%(n)d, b"),
    'start': goto_normal_mode + Text("^"),
    'finish': goto_normal_mode + Text("$"),
    'quick run': goto_normal_mode + Key("comma, r"),

    'command mode': goto_normal_mode + Key("colon"),

    # Line operations
    'dine': goto_normal_mode + Function(lambda: lineJuggle(0,0,"d", "+")),
    'dine <n>': goto_normal_mode + Function(lambda n: lineJuggle(n,n,"d","+")),
    'dine up <n>': goto_normal_mode + Function(lambda n: lineJuggle(n, n, "d", "-")),
    'dine <n> (thru|through|to) <n2>': goto_normal_mode + Function(lambda n1,n2: lineJuggle(n1,n2,"d","+")),
    'dine up <n> (thru|through|to) <n2>': goto_normal_mode + Function(lambda n1,n2: lineJuggle(n1,n2,"d","-")),
    'yank ': goto_normal_mode + Function(lambda: lineJuggle(0,0,"y", "+")),
    'yank [down] <n>': goto_normal_mode + Function(lambda n: lineJuggle(n,n,"d","+")),
    'yank up <n>': goto_normal_mode + Function(lambda n: lineJuggle(n,n,"d","-")),
    'yank <n> (thru|through|to) <n2>': goto_normal_mode + Function(lambda n1,n2: lineJuggle(n,n2,"y","+")),
    'yank up <n> (thru|through|to) <n2>': goto_normal_mode + Function(lambda n1,n2: lineJuggle(n,n2,"y","-")),

    'select until <text>': Key(goto_normal_mode_keys + "v, t, slash") + Function(translate_spokenform_to_queryform),
    'select including <text>': Key(goto_normal_mode_keys + "v, f, slash") + Function(translate_spokenform_to_queryform),
    'dine until <text>': Key(goto_normal_mode_keys + "d, t") + Function(translate_spokenform_to_queryform),
    'dine including <text>': Key(goto_normal_mode_keys + "d, f") + Function(translate_spokenform_to_queryform),
    '(see|sea) until <text>': Key(goto_normal_mode_keys + "c, t") + Function(translate_spokenform_to_queryform),
    '(see|sea) including <text>': Key(goto_normal_mode_keys + "c, f") + Function(translate_spokenform_to_queryform),

    # The zs and ze denote pattern start and pattern end
    # I use this to move the cursor right after the character we are looking for
    # e.g. "next block" looks for a lparen, so I move to the character after it
    'next block': goto_normal_mode + Text("?(\\zs[^ ]") + Key("enter, N, N"),
    'pre block': goto_normal_mode + Text("?(\\zs[^ ]") + Key("enter"),
    'next quote': goto_normal_mode + Text("?\\v('|\")\\zs.\\ze.{-\\}('|\")") + Key("enter, N, N"),
    'pre quote': goto_normal_mode + Text("?\\v('|\")\\zs.\\ze.{-\\}('|\")") + Key("enter"),



    # Fancy operations
    'clay': Key(goto_normal_mode_keys + "c, i, b"),

    # Movement
    'go [down] [to] [line] <n>': goto_normal_mode + Function(lambda n: just_goto_line(n, 'j')),
    'go up [to] [line] <n>': goto_normal_mode + Function(lambda n: just_goto_line(n, 'k')),
    'go absolute to [line] <n>': goto_normal_mode + Function(goto_line_absolute),
    'matching': Key(goto_normal_mode_keys + "percent"),
    'rash': Key(goto_normal_mode_keys + "down, s-a"),
    'back': Key(goto_normal_mode_keys + "c-o"),

    'next tab': Key("g, t"),
    'previous tab': Key("g, s-t"),
    'new tab': goto_normal_mode + Text(":tabe | FZFMru") + Key("enter"),

    # Plug-ins
    'file explorer': Key(goto_normal_mode_keys + "colon") + Text("VimFilerExplorer") + Key("enter"),
    }


class VimRule(MappingRule):
    mapping = basics_mapping
    extras = [
        Dictation('text'),
        IntegerRef('n0', 0, 999),
        IntegerRef('n', 0, 999),
        IntegerRef('n2', 0, 999),
        Choice("surroundChar", surroundCharsMap),
    ]
    defaults = {
        "n0": 0,  # Default repeat count.
        "n": 1,  # Default repeat count.
    }

rules = VimRule()