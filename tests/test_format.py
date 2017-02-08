import unittest2
import sure

from utility._format import format_text_logic, FormatTypes

class FormatTest(unittest2.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_spoken_form(self):
        res = format_text_logic("hello world", [FormatTypes.spokenForm])
        res.should.be.equal("hello world")

    def test_camel_form(self):
        res = format_text_logic("camel case", [FormatTypes.camelCase])
        res.should.be.equal("camelCase")

    def test_snake_form(self):
        res = format_text_logic("snake case", [FormatTypes.snakeCase])
        res.should.be.equal("snake_case")

    def test_pascal_form(self):
        res = format_text_logic("pascal case", [FormatTypes.pascalCase])
        res.should.be.equal("PascalCase")

    def test_lowercase_form(self):
        res = format_text_logic("LOWERCASE WORD", [FormatTypes.lowerCase])
        res.should.be.equal("lowercase word")

    def test_uppercase_form(self):
        res = format_text_logic("uppercase word", [FormatTypes.upperCase])
        res.should.be.equal("UPPERCASE WORD")

    def test_squash_form(self):
        res = format_text_logic("squash case",[FormatTypes.squash])
        res.should.be.equal("squashcase")

if __name__ == "__main__":
    unittest2.main()

