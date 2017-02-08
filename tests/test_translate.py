import unittest2
import sure

from utility.text_translate import translate_spokenform_to_queryform_logic, translate_numbers

class TranslatorTest(unittest2.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_range_insert_logic_phonetic(self):
        res = translate_spokenform_to_queryform_logic("pooch")
        res.should.be.equal("p")

    def test_range_insert_logic_phoneticAndWord(self):
        res = translate_spokenform_to_queryform_logic("pooch hello")
        res.should.be.equal("p,hello")

    def test_range_insert_logic_word(self):
        res = translate_spokenform_to_queryform_logic("hello")
        res.should.be.equal("hello")

    def test_range_insert_logic_thePrefixesWord(self):
        res = translate_spokenform_to_queryform_logic("the hello")
        res.should.be.equal("hello")


    @unittest2.skip("TODO, dunno how to deal with advanced character names yet")
    def test_range_insert_logic_twoWordCharacterName(self):
        res = translate_spokenform_to_queryform_logic("quote")
        res.should.be.equal("quote")

    def test_range_insert_logic_SpecialCharachter(self):
        res = translate_spokenform_to_queryform_logic("(")
        res.should.be.equal("(")

    def test_range_insert_logic_SpecialCharachterUsedByBothDNSAndDragonfly(self):
        res = translate_spokenform_to_queryform_logic("_")
        res.should.be.equal("underscore")

    def test_range_insert_logic_MultiWord(self):
        res = translate_spokenform_to_queryform_logic("words in")
        res.should.be.equal("words,space,in")

    def test_range_insert_logic_ShorttalkLetter(self):
        res = translate_spokenform_to_queryform_logic("faye")
        res.should.be.equal("f")

    def test_range_insert_logic_ShorttalkCapsLetter(self):
        res = translate_spokenform_to_queryform_logic("big faye")
        res.should.be.equal("F")

    @unittest2.skip("haskell dependency, optional")
    def test_translate_numbers(self):
        res = translate_numbers("one bear against two hundred and fifty 6 tigers") 
        res.should.be.equal("1 bear against 256 tigers".rstrip().lstrip())
