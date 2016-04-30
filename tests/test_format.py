import unittest2
import logging
import sys
import os
import sure

from grammars._format import format_text_logic, FormatTypes

class FormatTest(unittest2.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_spoken_forms(self):
        testInOut("hello world", "hello world", [FormatTypes.spokenForm])
        testInOut("camel case", "camelCase", [FormatTypes.camelCase])
        testInOut("snake case", "snake_case", [FormatTypes.snakeCase])
        testInOut("pascal case", "PascalCase", [FormatTypes.pascalCase])
        testInOut("lowercase word", "lowercase word", [FormatTypes.lowerCase])
        testInOut("uppercase word", "UPPERCASE WORD", [FormatTypes.upperCase])


def testInOut(input, output, formatTypes):
    res = format_text_logic(input, formatTypes)
    res.should.be.equal(output)


if __name__ == "__main__":
    unittest2.main()

