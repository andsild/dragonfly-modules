# dragonfly scripts
dragonfly scripts for running on Windows (and some vim support)

# Environment
I do all of this work on Windows 10 using DNS 14.0 and Natlink 4.1 papa, dragonfly 0.6.5 and Python 2.7.6.    

If this is your first time installing dragonfly and Natlink, I recommend https://sourceforge.net/projects/natlink/files/pythonfornatlink/python2.7.zip/download
to install the needed dependencies (and also some pip packages, see dragonfly readme).  

Previously I used Linux and a Windows VM, however, the delay of Aenea (https://github.com/dictation-toolbox/aenea) was inconvening.

## Usage:
The jist of the _useful_ code is in the grammars repository.
You can copy whatever commands you like to your own project. 

I have deviated off the standard path with ``_natlink_hack.py''. This file handles imports for me, rather than letting natlink do it. 
The only difference you need to care about is that my files in grammar per default will not install themselves as a grammar.
 To do this, you must add an "unload" method and make an instance of `MappingRule()`. An example is shown here:  
```
from dragonfly import Grammar

grammar = Grammar('NAME FOR GRAMMAR')
grammar.add_rule(rules)
grammar.load()

def unload():
    global grammar
    if grammar:
        grammar.unload()
    grammar = None
```

My natlink_hack is not perfect (e.g. it doesn't allow for context switching), but it was written to take me further away from Natlinks' API (for which I am ever grateful for having, but I prefer staying away).


Either copy the .py files you want in your Natlink user directory or C:\Natlink\Natlink\MacroSystem.
Alternatively, copy excerpts from my code into your own grammars.


## this is a work in progress	
feel free to message me or create an issue if you would like to
