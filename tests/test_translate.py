import unittest2
import logging
import sys
import os
import sure

from utility.substitute_phrase import range_insert_symbol
from utility.text_translate import range_insert_symbol_logic, translate_numbers

class TranslatorTest(unittest2.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_translate_word_to_number(self):
        pass


    def test_range_insert_logic(self):
        def assert_input_equal_output(intext, outtext):
            res = range_insert_symbol_logic(intext)
            if ':' in res:
                res = res[0:res.index(':')] # remove ':' part (for repetitions)
            outtext.should.be.equal(res)
        assert_input_equal_output("pooch", "p") # phonetic
        assert_input_equal_output("hello", "hello") # word
        assert_input_equal_output("the main", "main") # trim "the"
        #FIXME: tricky scenario: how to avoid/acquire two words?  e.g.  "single
                                           #quote"?
        assert_input_equal_output("quote", "quote") 
        assert_input_equal_output('(', '(')  # special symbols
        assert_input_equal_output("_", "underscore") # windows DNS auto replace
        assert_input_equal_output("alpha", "a") # phonetic
        assert_input_equal_output("words in", "words,space,in") # phonetic
        assert_input_equal_output("faye", "f") # shorttalk
        assert_input_equal_output("big faye", "F") # capital
        assert_input_equal_output("tangible", "tangible") # word that is substring of phonetic key

    def test_translate_numbers(self):
        res = translate_numbers("one bear against two hundred and fifty 6 tigers") 
        res.should.be.equal("1 bear against 256 tigers".rstrip().lstrip())
