import unittest2
import logging
import sys
import os
import sure

from grammars._vim import range_insert_symbol_logic, lineJuggle_logic

class VimTest(unittest2.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_range_insert(self):
        testInOut("hello", "hello") # word
        testInOut("the main", "main") # trim "the"
        #FIXME: tricky scenario: how to avoid/acquire two words? e.g. "single quote"?
        testInOut("quote", "quote") 
        testInOut("_", "underscore") # windows DNS auto replace
        testInOut("alpha", "a") # phonetic
        testInOut("words in", "words,space,in") # phonetic
        testInOut("faye", "f") # shorttalk
        testInOut("big faye", "F") # capital
        testInOut("tangible", "tangible") # word that is substring of phonetic key

    def test_linejuggle(self):
        res = lineJuggle_logic(0, 1, "d", "+")
        res.should.be.equal(":.,+1d")

        res = lineJuggle_logic(3, 4, "d", "+")
        res.should.be.equal(":+3,+4d")

    def test_linejuggle_up(self):
        res = lineJuggle_logic(0, 1, "d", "-")
        res.should.be.equal(":-1,.d")


def testInOut(intext, outtext):
    res = range_insert_symbol_logic(intext)
    mystr = ''.join([x for x in res])
    outtext.should.be.equal(res)

if __name__ == "__main__":
    unittest2.main()

