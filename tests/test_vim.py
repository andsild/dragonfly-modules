import unittest2
import logging
import sys
import os
import sure

from grammars._vim import lineJuggle_logic

class VimTest(unittest2.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_linejuggle(self):
        res = lineJuggle_logic(0, 1, "d", "+")
        res.should.be.equal(":.,+1d")

        res = lineJuggle_logic(3, 4, "d", "+")
        res.should.be.equal(":+3,+4d")

    def test_linejuggle_up(self):
        res = lineJuggle_logic(0, 1, "d", "-")
        res.should.be.equal(":-1,.d")


if __name__ == "__main__":
    unittest2.main()

