import unittest2
import logging
import sys
import os
import sure

from _vim import range_insert_symbol_logic

class VimTest(unittest2.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass


    def testRangeInsert(self):
        testInOut("alpha", "a") # phonetic
        testInOut("words in", "words,space,in") # phonetic
        testInOut("faye", "f") # shorttalk
        testInOut("big faye", "F") # capital
        testInOut("hello", "hello") # word
        testInOut("tangible", "tangible") # word that is substring of phonetic key
        testInOut("hello world", "hello,space,world") # word with space


def testInOut(intext, outtext):
    res = range_insert_symbol_logic(intext)
    mystr = ''.join([x for x in res])
    mystr.should.be.equal(outtext)

if __name__ == "__main__":
    unittest2.main()

