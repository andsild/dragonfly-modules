from dragonfly import (
    Dictation,
    MappingRule,
    Function,
    IntegerRef,
)

from utility.program_manager import *

commands={
    'launch [command] prompt': Function(lambda: launch_program(["cmd", "/K", "cd %homedrive%%homepath%"])),
    'launch jim': Function(lambda: launch_program(["C:\Program Files\Git\git-bash", "--cd-to-home"])),
    'launch chrome': Function(lambda: launch_program(["C:\Program Files (x86)\Google\Chrome\Application\chrome.exe"])),
    'launch studio': Function(lambda: launch_program(["C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\devenv.exe"])),
    'launch doc': Function(lambda: launch_program(["C:\Program Files (x86)\Adobe\Acrobat Reader DC\Reader\AcroRd32.exe"])),

    'flip [command] prompt': Function(lambda: flip_program("cmd")),
    'flip jim': Function(lambda: flip_program("mintty")),
    'flip studio': Function(lambda: flip_program("devenv")),
    'flip chrome': Function(lambda: flip_program("chrome")),
    'flip editor': Function(lambda: flip_program("nvim-qt")),
    'flip doc': Function(lambda: flip_program("AcroRd32")),
}


class ProgramManager(MappingRule):
    mapping =  commands
    extras = [Dictation('text'),
        IntegerRef('n', 0, 999),]
    defaults = {
        "n": 1,  # Default repeat count.
}    

rules = ProgramManager()