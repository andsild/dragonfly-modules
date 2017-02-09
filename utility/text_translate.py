import sys
import re
from utility.lettermap import Shorttalk_Letters, Phonetic_Letter_map
from subprocess import Popen, PIPE, STDOUT
import os
from dragonflymodules.config import default_install_dir

subRepoName="TextToNumber"

full_path_for_this_file = os.path.realpath(__file__).split(os.sep)
full_path_for_this_github_repository = full_path_for_this_file[0] + os.path.join(os.path.sep, *full_path_for_this_file[1:full_path_for_this_file.index('dragonfly-modules')+1])
exeName = os.path.join(full_path_for_this_github_repository, "optional_dependencies", subRepoName, subRepoName)
if hasattr(sys, 'getwindowsversion'):
    exeName = exeName + ".exe"

if not os.path.isfile(exeName):
    print("warning: '%s' not found; some, few commands may not work" % exeName)

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
    "underscore": "underscore",
}

# When running DNS in windows, DNS will automatically replace some symbols
# e.g. when saying "underscore", it will input "_"
windows_special_cases = {
    '_': 'underscore'
}

Symbol_map.update(Shorttalk_Letters)
Symbol_map.update(Phonetic_Letter_map)
IS_WINDOWS = True
if IS_WINDOWS:
    Symbol_map.update(windows_special_cases)


""" sometimes I need to forward text to my own functions.
e.g. I might want to say "jump pooch hotel ork" to do a search (ctrl+f) for "pho"
to avoid having to say "jump" first to launch the ctrl+f, and then do the letters,
or to avoid hard-binding "jump" to beeing just ctrl+f.
This function does internal "translation" that can immediately fetch the keys for e.g. letters and symbols
without having to register this as multiple dragonfly commands.
i.e. using this function I just need one voice command to do "jump pooch hotel ork"
without this function, it would be four commands "jump", then "pooch", then "hotel", then "ork"
"""
#TODO: fix casings and formats
def translate_spokenform_to_queryform_logic(text):
    translated_text = translate_numbers(str(text))
    input_text = str(translated_text).split()
    boolBig = "big" == input_text[0]
    if boolBig:
        input_text = input_text[1:]
    if input_text[0] == "the" and len(input_text) > 1: 
        # DNS will often append "the" to words (because it uses a linguistic model)
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

    if ':%' in returnWord:
        returnWord = re.sub(r':%\(\w+\)[dD]', '', returnWord)
    return returnWord[:-1]

def translate_numbers(text):
    if not os.path.isfile(exeName):
        print("Tried to execute %s, but it was not found, ignoring..." % exeName)
        return text
    p = Popen([exeName], stdout=PIPE, stdin=PIPE, stderr=STDOUT)    
    stdout = p.communicate(input=bytearray(text))[0]
    return str(stdout.decode().rstrip())
