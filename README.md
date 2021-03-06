![visual studio team](https://crutux.visualstudio.com/_apis/public/build/definitions/0ea1e959-5a94-47cf-ae98-129371e022bf/1/badge) [![Build Status](https://travis-ci.org/andsild/dragonfly-modules.svg?branch=master)](https://travis-ci.org/andsild/dragonfly-modules)
# dragonfly scripts
dragonfly scripts for controlling your Windows operating system by voice

# Environment
I do all of this work on Windows 10 using DNS 14.0, Natlink 4.1 papa, dragonfly 0.6.5 and Python 2.7.6.

Applications frequently used with voice:  
  - Visual studio
  	-  using a [vim extension](https://visualstudiogallery.msdn.microsoft.com/59ca71b3-a4a3-46ca-8fe1-0e90e3f79329)
  - Neovim  
  - Chrome  
  	- Copying stack-overflow answers: [stackover co-en](https://chrome.google.com/webstore/detail/stackoverflow-co-en/kcckkenmfeecdbgcjmjamadefkjmjlca)  
    - Vim-bindings: [vimium](https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb?hl=en)
  - Adobe acrobat reader (for PDFs)  


If this is your first time installing dragonfly and Natlink, I recommend https://sourceforge.net/projects/natlink/files/pythonfornatlink/python2.7.zip/download
to install the needed dependencies (not all of them can be fetched via CLI).  

Previously I used Linux and a Windows VM, however, the delay of the VM and [Aenea](https://github.com/dictation-toolbox/aenea) was inconvening.

## Status
I can write simple programs but it takes a long time.
I very much appreciate feedback whether its a bug report, question, suggestion or anything else!  

## Usage:
The jist of the _useful_ code is in the grammars repository.
You can copy whatever commands you like to your own project. 

`_import_grammars.py` handles imports for me, rather than letting Natlink do it. 
The only difference is that my grammar files/rules per default will not install themselves as executable dragonfly grammars to DNS or Windows' speech engine.
Thus, they have to be added explictly using import_grammars.py or a solution of your own. 


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

My `import_grammars` is not perfect (e.g. it doesn't allow for context switching), but it was written to take me further away from Natlinks' API which I find difficult to use (but still, I am grateful that it is written for DNS users).
