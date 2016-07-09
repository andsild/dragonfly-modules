""" Convenient to have in a separate file since it uses dragonfly import
    (importing dragonfly causes a lot of commotion)
"""
from dragonfly import Key
from utility.text_translate import range_insert_symbol_logic, Symbol_map

def range_insert_symbol(text):
    dragonfly_parsable_text = range_insert_symbol_logic(text).split(',')
    print dragonfly_parsable_text
    for words in dragonfly_parsable_text:
        if words in Symbol_map.itervalues():
            Key(words).execute()
        # special case for words like "colon" and "asterisk" that someone mysteriously modifies
        elif len(words) > 1 and words[1] == "\\":
            Key(words[2:]).execute()
        else:
            Key(','.join(words)).execute()
