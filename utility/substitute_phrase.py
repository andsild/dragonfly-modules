from dragonfly import Key
from utility.text_translate import translate_spokenform_to_queryform_logic, Symbol_map

Symbol_to_dragonfly = {
    '(': 'lparen',
    ')': 'rparen',
    '[': 'lbracket',
    ')': 'rbracket',
    '{': 'lbrace',
    '}': 'rbrace',
    '|': 'pipe',
}

def translate_spokenform_to_queryform(text):
    dragonfly_parsable_text = translate_spokenform_to_queryform_logic(text).split(',')
    print dragonfly_parsable_text

    # since we will be popping off words
    dragonfly_parsable_text = dragonfly_parsable_text[::-1]


    while dragonfly_parsable_text:
        words = dragonfly_parsable_text.pop()
        if words in Symbol_map.itervalues():
            Key(words).execute()
            print "doingA: ", words
        elif words in Symbol_to_dragonfly:
            # Symbols are for whatever reason followed by 'space' and \\<symbolname>.
            # pop those off..
            Key(Symbol_to_dragonfly[words]).execute()
            print "doingB: ", Symbol_to_dragonfly[words]

            dragonfly_parsable_text.pop() 
            dragonfly_parsable_text.pop()
        # special case for words like "colon" and "asterisk" that dragonfly prepends with "\\"
        elif len(words) > 1 and words[0] == '\\':
            print "doingC: ", words[1:]
            Key(words[1:]).execute()
        else:
            print "doingD: ", ','.join(words)
            Key(','.join(words)).execute()
