from __future__ import print_function
import unittest2
import sys

from dragonflymodules.config import GRAMMAR_IMPORTS

class TestGrammarCorrectness(unittest2.TestCase):
    """
       Having to (re)start DNS to find an errormessage is tedious.
       These tests show the errors even if DNS is not running  (or if it is, even).

        Dragonfly tries to initialize a speech engine when it is imported.
        This often results in windows' speech engine starting when running this test.
        To avoid this, I regrettably suggest modifying 
        (assuming dragonfly version 0.6.5)
        C:\Python27\lib\site-packages\dragonfly\dragonfly\engines\engine_natlink.py
        line 54 -> return True
        """
    def test_GRAMMARIMPORTS_ensureCorrect(self):
        try:
            import  win32gui
        except ImportError as ie:
            print("cannot import win32 libs, make sure to install pywin32 libraries for this test to work", file=sys.stderr)
            return

        for import_name in GRAMMAR_IMPORTS:
            try:
                getattr(__import__(import_name, fromlist=["rules"]), "rules")
            except Exception as exception:
                raise RuntimeError("Error when importing %s" % import_name, exception.message)