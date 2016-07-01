import re
from utility.lettermap import Letters, Letter_map
from subprocess import Popen, PIPE, STDOUT
import os


subRepoName="TextToNumber"
currentPath = os.getcwd()
exeName = os.path.join(currentPath, subRepoName, "dist", "build",  subRepoName, subRepoName + ".exe")


Symbol_map = {
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
    "colon": "colon",
}

# When running DNS in windows, DNS will automatically replace some symbols
# e.g. when saying "underscore", it will input "_"
windows_special_cases = {
    '_': 'underscore'
}

Symbol_map.update(Letters)
Symbol_map.update(Letter_map)
Symbol_map.update(Symbol_map)
IS_WINDOWS = True
if IS_WINDOWS:
    Symbol_map.update(windows_special_cases)

def range_insert_symbol_logic(text):
    input_text = str(text).split()
    boolBig = "big" == input_text[0]
    if boolBig:
        input_text = input_text[1:]
    if input_text[0] == "the" and len(input_text) > 1: 
        # DNS will often append "the" to words (because it is a linguistic model)
        input_text = input_text[1:]
    if input_text[0] == "and" and input_text[1] == "the": 
        # DNS corner-case (searching for the word "the")
        input_text = input_text[1:]
    lenInput = len(input_text)
    returnWord = ""
    for ind,word in enumerate(input_text):
        word = word.lower()
        foundLetter = False
        # this inner for loop is just for special symbols
        for key,val in Symbol_map.iteritems():
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

def translate_numbers(text):
    p = Popen([exeName], stdout=PIPE, stdin=PIPE, stderr=STDOUT)    
    stdout = p.communicate(input=bytearray(text))[0]
    return str(stdout.decode().rstrip())
