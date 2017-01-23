import unittest2
import sure

from utility.vim_logic import lineJuggle_logic

class VimTest(unittest2.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_linejuggle_lineBelow(self):
        res = lineJuggle_logic(1, 1, "d", "+")
        res.should.be.equal(":+1,+0d")

    def test_linejuggle_multipleLinesBelow(self):
        res = lineJuggle_logic(4, 8, "d", "+")
        res.should.be.equal(":+4,+4d")

    def test_linejuggle_mutlipleLinesAbove(self):
        res = lineJuggle_logic(3, 5, "d", "-")
        res.should.be.equal(":-5,+2d")

    def test_linejuggle_lineAbove(self):
        res = lineJuggle_logic(1, 1, "d", "-")
        res.should.be.equal(":-1,+0d")


if __name__ == "__main__":
    unittest2.main()

