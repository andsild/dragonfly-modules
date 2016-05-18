import unittest2
import logging
import sys
import os
import sure

from utility.text_translate import range_insert_symbol_logic
from utility.translate_to_number import translate_word_to_number

class TranslatorTest(unittest2.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_translate_word_to_number(self):
        pass

    def test_range_insert(self):
        def assert_input_equal_output(intext, outtext):
            res = range_insert_symbol_logic(intext)
            mystr = ''.join([x for x in res])
            outtext.should.be.equal(res)
        assert_input_equal_output("hello", "hello") # word
        assert_input_equal_output("the main", "main") # trim "the"
        assert_input_equal_output("hundred", "100") # trim "the"
        #FIXME: tricky scenario: how to avoid/acquire two words?  e.g.  "single
                                           #quote"?
        assert_input_equal_output("quote", "quote") 
        assert_input_equal_output("_", "underscore") # windows DNS auto replace
        assert_input_equal_output("alpha", "a") # phonetic
        assert_input_equal_output("words in", "words,space,in") # phonetic
        assert_input_equal_output("faye", "f") # shorttalk
        assert_input_equal_output("big faye", "F") # capital
        assert_input_equal_output("tangible", "tangible") # word that is substring of phonetic key

    def test_translate_to_number(self):
        def assert_input_equal_output(intext, outtext):
            res = translate_word_to_number(intext)
            outtext.should.be.equal(res)

        assert_input_equal_output("one", "1") # normal case
        assert_input_equal_output("Two", "2")  # capital letter
        assert_input_equal_output("fifty", "50")  # word translate
        assert_input_equal_output("hundred", "100")  # word translate (100 and above are special cases)
        assert_input_equal_output("one hundred and thirty four", "134")  # all words
        assert_input_equal_output("200 and thirty four", "234")  # all words
        assert_input_equal_output("two hundred and 34", "234")  # with mix
        assert_input_equal_output("3 hundred and thirty 4", "334") # more complex mix

        assert_input_equal_output("", "") # empty case
        assert_input_equal_output("hello one hundred and 9", "109") # empty case
