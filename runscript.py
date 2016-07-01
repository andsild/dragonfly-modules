from subprocess import Popen, PIPE, STDOUT
from ipdb import set_trace
import os

subRepoName="TextToNumber"
currentPath = os.getcwd()
exeName = os.path.join(currentPath, subRepoName, "dist", "build",  subRepoName, subRepoName + ".exe")

p = Popen([exeName], stdout=PIPE, stdin=PIPE, stderr=STDOUT)    
stdout = p.communicate(input=b"one hundred and fiftyone")[0]
print(stdout.decode())
