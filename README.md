[![Build Status](https://travis-ci.org/andsild/dragonfly-modules.svg?branch=master)](https://travis-ci.org/andsild/dragonfly-modules)  
# dragonfly scripts
dragonfly scripts for controlling your Windows operating system by voice

# Environment
I do all of this work on Windows 10 using DNS 14.0, Natlink 4.1 papa, dragonfly 0.6.5 and Python 2.7.6.

Applications frequently used with voice:  
  - Visual studio (with a [vim extension](https://visualstudiogallery.msdn.microsoft.com/59ca71b3-a4a3-46ca-8fe1-0e90e3f79329))  
  - Neovim  
  - Chrome  
  - Adobe acrobat reader (for PDFs)  


If this is your first time installing dragonfly and Natlink, I recommend https://sourceforge.net/projects/natlink/files/pythonfornatlink/python2.7.zip/download
to install the needed dependencies (not all of them can be fetched via CLI).  

Previously I used Linux and a Windows VM, however, the delay of the VM and [Aenea](https://github.com/dictation-toolbox/aenea) was inconvening.

## Status
I am making progress, but am still not a productive coder by voice. I.e. I can write simple programs but it takes a long time.
I very much appreciate feedback whether its a bug, question, suggestion or anything else!  
Feel free to message me at `andsild@gmail.com`

## Usage:
The jist of the _useful_ code is in the grammars repository.
You can copy whatever commands you like to your own project. 

I have deviated off the standard path with `_natlink_hack.py`. This file handles imports for me, rather than letting Natlink do it. 
The only difference is that my grammar files/rules per default will not install themselves as executable dragonfly grammars to DNS or Windows' speech engine.
Thus, they have to be added explictly using natlink_hack.py or a solution of your own. 


If you *do not* like my approach, you can still use my grammars. Copy a file from this project's `grammars` folder into your Natlink-folder, and append the code below to the respective file. There will probably be some imports that fail, I recommend either fixing the imports or simply commenting them out.
```python  
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

However, If you *do* like (or don't mind) my approach, I recommend cloning this directory to your PC, and pointing your Natlink "UserDirectory" to this repo (start menu -> Configure Natlink via GUI -> UserDirectory"). The rest should work by itself, provided you `pip install requirements.txt`.

My natlink_hack is not perfect (e.g. it doesn't allow for context switching), but it was written to take me further away from Natlinks' API which I find difficult to use (but still, I am grateful that it is written for DNS users).
