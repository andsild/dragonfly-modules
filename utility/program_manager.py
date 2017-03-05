import subprocess, os

from dragonflymodules.config import default_install_dir

sub_repo_name = "WindowsAPIGateway"
full_path_for_this_file = os.path.realpath(__file__).split(os.sep)
full_path_for_this_github_repository = full_path_for_this_file[0] + os.path.join(os.path.sep, *full_path_for_this_file[1:full_path_for_this_file.index('dragonfly-modules')+1])
exename_programswitcher = os.path.join(full_path_for_this_github_repository, sub_repo_name, sub_repo_name, "bin",  "Debug", sub_repo_name + ".exe")

if not os.path.isfile(exename_programswitcher):
    print "Could not find program used to switch programs (have you built %s, which should produce the file '%s'?)" % (sub_repo_name, exename_programswitcher)
    print "\t\"flipcmd\" grammar will not work until executable is built"

def launch_program(program_args):
    subprocess.Popen(program_args)

def goto_chrome(program_args):
    subprocess.Popen(["C:\Program Files (x86)\Google\Chrome\Application\chrome.exe"] + program_args)

def flip_program(program_name):
    subprocess.Popen([exename_programswitcher, program_name]);