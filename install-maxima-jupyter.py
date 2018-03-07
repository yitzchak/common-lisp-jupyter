#!/usr/bin/env python3

## Maxima-Jupyter installation script

## Note: since the kernel of Jupyter is written itself in Python,
##       it is much simpler to have a Python-based installation script.

import subprocess
import sys
import shutil
import os
import json

def halt(msg):
    print(msg, file=sys.stderr)
    print("Abort.", file=sys.stderr)
    sys.exit(1)


MAXIMA_JUPYTER_HEADER = """
Maxima-Jupyter -- an enhanced interactive Maxima shell
CL-Jupyter (C) 2014-2015 Frederic Peschanski (cf. LICENSE)
Additional Maxima-specific stuff by Robert Dodier; thanks Frederic!
----"""

print(MAXIMA_JUPYTER_HEADER)

# check that we run as a script
if __name__ != "__main__":
    halt("Error: Maxima-Jupyter installation must be run as a script")

# check the python version, needs at least 3.2
if sys.version_info.major < 3 \
   or sys.version_info.minor < 3:
    halt("Error: Maxima-Jupyter requires Python v3.3 or above")

# check if ipython is available
try:
    import IPython
except ImportError:
    halt("Error: IPython not available (check your Python Path)")

# check Ipython version

ipython_version_major, ipython_version_minor, ipython_version_patch, ipython_version_tag = IPython.version_info
if ipython_version_major < 3:
    halt("Error: IPython v3.x or greater required (found v{}.{})".format(ipython_version_major, ipython_version_minor))

print("... Frontend: using IPython v{}.{}".format(ipython_version_major, ipython_version_minor))

###################################
## (Ad-hoc) command-line parsing ##
###################################

class Config:
    def __init__(self):
        # Find IPython directory. See comment by zockoshi at:
        # https://github.com/robert-dodier/maxima-jupyter/issues/4
        # self.ipython_dir = IPython.utils.path.get_ipython_dir()
        self.ipython_dir = IPython.paths.get_ipython_dir()
        self.ipython_executable = shutil.which("ipython3")
        self.maxima_jupyter_executable = None

def process_command_line(argv):
    config = Config()

    import inspect
    import os.path

    i = 1

    ipython_exec_set = False
    maxima_jupyter_exec_set = False

    while i < len(argv):
        #print("cmd line option #{}: {}".format(i, argv[i]))

        if argv[i].startswith("--ipython-exec="):
            if ipython_exec_set:
                halt("Error: --ipython-exec option set twice")
            config.ipython_executable = shutil.which(argv[i][15:])
            ipython_exec_set = True
        elif argv[i].startswith("--maxima-jupyter-exec="):
            if maxima_jupyter_exec_set:
                halt("Error: --maxima-jupyter-exec option set twice")
            config.maxima_jupyter_executable = shutil.which(argv[i][len("--maxima-jupyter-exec="):])
            maxima_jupyter_exec_set = True
        else:
            halt("Error: unexpected option '{}'".format(argv[i]))

        i += 1

    return config

config = process_command_line(sys.argv)

###################################
## Check Ipython executable      ##
###################################

if not config.ipython_executable:
    halt("Error: Ipython executable not found")
else:
    try:
        ipython_version_string = subprocess.check_output([config.ipython_executable, "--version"]).decode()
    except FileNotFoundError:
        halt("Error: cannot find ipython executable")
    except subprocess.CalledProcessError as e:
        halt("Error: {}".format(e))

    #print("ipython version string = {}".format(ipython_version_string))
    import re
    # cut off a hyphen and anything following, e.g. "2.4.2-maint" --> "2.4.2"
    foo = re.sub ("-.*$", "", ipython_version_string)
    ipython_version = tuple([int(d) for d in foo.split(".")])
    #print("ipython version = {}".format(ipython_version))
    if (ipython_version[0] != ipython_version_major) \
       or (ipython_version[1] != ipython_version_minor):
        halt("Error: mismatch ipython version ({}.{} vs {}.{})".format(ipython_version[0], ipython_version[1],
                                                                       ipython_version_major, ipython_version_minor))

##############################
## Installation of kernel   ##
##############################

if not config.maxima_jupyter_executable:
    halt ("error: Maxima-Jupyter executable not specified.")
else:
    os.makedirs(config.ipython_dir + "/kernels/maxima", exist_ok=True)

    KERNEL_SPEC = {
        "argv": [
            config.maxima_jupyter_executable,
            '{connection_file}'],
        "display_name": "Maxima",
        "language": "maxima"
    }

    with open(config.ipython_dir + "/kernels/maxima/kernel.json", "w") as kernel_spec_file:
        json.dump(KERNEL_SPEC, kernel_spec_file)

    print("maxima-jupyter: installation complete.")
