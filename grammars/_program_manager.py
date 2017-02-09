from dragonfly import (
    Dictation,
    Key,
    MappingRule,
    Function,
    IntegerRef,
)
import subprocess, os

from dragonflymodules.config import default_install_dir

sub_repo_name = "WindowsAPIGateway"
full_path_for_this_file = os.path.realpath(__file__).split(os.sep)
full_path_for_this_github_repository = full_path_for_this_file[0] + os.path.join(os.path.sep, *full_path_for_this_file[1:full_path_for_this_file.index('dragonfly-modules')+1])
exename_programswitcher = os.path.join(full_path_for_this_github_repository, sub_repo_name, sub_repo_name, "bin",  "Debug", sub_repo_name + ".exe")

def launch_cmd():
    subprocess.Popen(["cmd", "/K", "cd %homedrive%%homepath%"])

def launch_git():
    subprocess.Popen(["C:\Program Files\Git\git-bash", "--cd-to-home"])

def launch_chrome():
    subprocess.Popen(["C:\Program Files (x86)\Google\Chrome\Application\chrome.exe", ])

def launch_visual_studio():
    subprocess.Popen(["C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\devenv.exe", ])

def flip_cmd():
    subprocess.Popen([exename_programswitcher, "cmd"]);

def flip_visual_studio():
    subprocess.Popen([exename_programswitcher, "devenv"]);

def flip_chrome():
    subprocess.Popen([exename_programswitcher, "chrome"]);

def flip_editor():
    subprocess.Popen([exename_programswitcher, "nvim-qt"]);

commands = {}

#TODO: undo last spoken phrase
programs_to_launch={
    'launch command prompt': Function(launch_cmd),
    'launch jim': Function(launch_git),
    'launch chrome': Function(launch_chrome),
    'launch studio': Function(launch_visual_studio),

    }

program_to_switch_to ={
    'flip prompt': Function(flip_cmd),
    'flip studio': Function(flip_visual_studio),
    'flip chrome': Function(flip_chrome),
    'flip editor': Function(flip_editor),
    }

commands.update(programs_to_launch)
if os.path.isfile(exename_programswitcher):
    commands.update(program_to_switch_to)
else:
    print "Could not find program used to switch programs (have you built %s, which should produce the file '%s'?)" % (sub_repo_name, exename_programswitcher)
    print "\tomitting \"flipcmd\" grammar"


class ProgramManager(MappingRule):
    mapping =  commands
    extras = [Dictation('text'),
        IntegerRef('n', 0, 999),]
    defaults = {
        "n": 1,  # Default repeat count.
}    

rules = ProgramManager()