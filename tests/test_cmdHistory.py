import unittest2
import sure

from utility.cmdHistory import get_backspaces_for_commands

class Test_test_cmdHistory(unittest2.TestCase):
    def setUp(self):
        pass
    def tearDown(self):
        pass

    def test_get_backspaces_for_commands_OneWord(self):
        number_of_backspaces = get_backspaces_for_commands([["hello"]], 1)
        number_of_backspaces.should.be.equal(5)

    def test_get_backspaces_for_commands_TwoWord(self):
        number_of_backspaces = get_backspaces_for_commands([["hello", "world"]], 1)
        number_of_backspaces.should.be.equal(11)

    def test_get_backspaces_for_commands_TwoCommandsOneWord(self):
        number_of_backspaces = get_backspaces_for_commands([["hello"], ["world"]], 2)
        number_of_backspaces.should.be.equal(10)

    def test_get_backspaces_for_commands_TwoCommandsMultipleWords(self):
        number_of_backspaces = get_backspaces_for_commands([["hello", "world"], ["abc"]], 2)
        number_of_backspaces.should.be.equal(11+3)

if __name__ == '__main__':
    unittest.main()
