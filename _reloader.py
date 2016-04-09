# This file is part of Aenea
#
# Aenea is free software: you can redistribute it and/or modify it under
# the terms of version 3 of the GNU Lesser General Public License as
# published by the Free Software Foundation.
#
# Aenea is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Aenea.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright (2014) Alex Roper
# Alex Roper <alex@aroper.net>


import os
import sys

import dragonfly

# Internal NatLink module for reloading grammars.
import natlinkmain

from dragonfly.all import Grammar, CompoundRule


def topy(path):
    if path.endswith == ".pyc":
        return path[:-1]

    return path

# Voice command rule combining spoken form and recognition processing.
class ExampleRule(CompoundRule):
    spec = "reload all grammars"                  # Spoken form of command.
    def _process_recognition(self, node, extras):   # Callback when command is spoken.
        # Do not reload anything in these directories or their subdirectories.
        dir_reload_blacklist = set(["core"])
        macro_dir = "C:\\Users\\Anders Sildnes\\dragonfly-modules"

        # Unload all grammars.
        natlinkmain.unloadEverything()

        # Unload all modules in macro_dir except for those in directories on the
        # blacklist.
        # Consider them in sorted order to try to make things as predictable as
        # possible to ease debugging.
        for name, module in sorted(sys.modules.items()):
            if module and hasattr(module, "__file__"):
                # Some builtin modules only have a name so module is None or
                # do not have a __file__ attribute.  We skip these.
                path = module.__file__

                # Convert .pyc paths to .py paths.
                path = topy(path)

                # Do not unimport this module!  This will cause major problems!
                if (path.startswith(macro_dir) and not bool(set(path.split(os.path.sep)) & dir_reload_blacklist) and path != topy(os.path.abspath(__file__))):

                    print "removing %s from cache" % name

                    # Remove the module from the cache so that it will be reloaded
                    # the next time # that it is imported.  The paths for packages
                    # end with __init__.pyc so this # takes care of them as well.
                    del sys.modules[name]

        try:
            # Reload the top-level modules in macro_dir.
            natlinkmain.findAndLoadFiles()
        except Exception as e:
            print "reloading failed: {}".format(e)
        else:
            print "finished reloading"

# Create a grammar which contains and loads the command rule.
grammar = Grammar("example grammar")                # Create a grammar to contain the command rule.
grammar.add_rule(ExampleRule())                     # Add the command rule to the grammar.
grammar.load()                         