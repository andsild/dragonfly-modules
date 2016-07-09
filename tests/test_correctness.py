import unittest2

import sys
import reloader

from dragonflymodules.config import GRAMMAR_IMPORTS

class TestGrammarCorrectness(unittest2.TestCase):
    """
       Having to (re)start DNS to find an errormessage is tedious.
        These tests show the errors even if DNS is not running  (or if it is, even).

        Dragonfly tries to initialize a speech engine when it is imported.
        To avoid this (speeding up test execution by a lot)
        I regrettably suggest modifying 
        (assuming dragonfly version 0.6.5)
        C:\Python27\lib\site-packages\dragonfly\dragonfly\engines\engine_natlink.py
        line 54 -> return True
        """
    def test_GRAMMARIMPORTS_ensureCorrect(self):
        for import_name in GRAMMAR_IMPORTS:
            try:
                getattr(__import__(import_name, fromlist=["rules"]), "rules")
            except Exception as exception:
                raise RuntimeError("Error when importing %s" % import_name, exception.message)