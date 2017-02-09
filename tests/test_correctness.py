from __future__ import print_function
import unittest2
import sys

from dragonflymodules.config import GRAMMAR_IMPORTS



class TestGrammarCorrectness(unittest2.TestCase):
    """
       The only way to validate the content of grammars is to invoke it.
       Having to (re)start DNS to find an errormessage is tedious.
       These tests parse the grammars and show the errors without requiring DNS to run.

       You have to do a small fix:
       Dragonfly tries to initialize a speech engine when it is imported.
       This launches windows' speech engine when running this test (or annoying popup).
       To amend this, I regrettably suggest modifying 
       (assuming dragonfly version 0.6.5)
       C:\Python27\lib\site-packages\dragonfly\dragonfly\engines\engine_natlink.py
       line 54 -> return True
    """
    def test_GRAMMARIMPORTS_ParseGrammar(self):
        try:
            import  win32gui
        except ImportError as ie:
            print("cannot import win32 libs, make sure to install pywin32 libraries for this test to work", file=sys.stderr)
            return

        for import_name in GRAMMAR_IMPORTS:
            try:
                getattr(__import__(import_name, fromlist=["rules"]), "rules")
            except Exception as exception:
                print("Error when importing %s" % import_name, file=sys.stderr)
                raise