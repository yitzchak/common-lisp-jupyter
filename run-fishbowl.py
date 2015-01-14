#!/usr/bin/env python3

## Fishbowl startup script

## As a distributed program, the startup phase of
## Fishbowl is non-trivial, and requires access to
## the filesystem.   Since Python (3.x) is a requirement
## for Fishbowl, the startup script is also written in
## Python

import subprocess
import sys

def halt(msg):
    print(msg, file=sys.stderr)
    print("Abort.", file=sys.stderr)
    sys.exit(1)


FISHBOWL_HEADER = """
Fishbowl -- an enhanced interactive Common Lisp shell
(C) 2014-2015 Frederic Peschanski (cf. LICENSE)
----"""

print(FISHBOWL_HEADER)

# check that we run as a script
if __name__ != "__main__":
    halt("Error: Fishbowl startup must be run as a script")

# check the python version, needs at least 3.2
if sys.version_info.major < 3 \
   or sys.version_info.minor < 3:
    halt("Error: Fishbowl requires Python v3.3 or above")

# check if ipython is available
try:
    import IPython
except ImportError:
    halt("Error: IPython not available (check your Python Path)")

# check Ipython version

ipython_version_major, ipython_version_minor, ipython_version_patch, ipython_version_tag = IPython.version_info
if ipython_version_major != 2:
    halt("Error: IPython v2.x required (found v{}.{})".format(ipython_version_major, ipython_version_minor))

print("... Frontend: using IPython v{}.{}".format(ipython_version_major, ipython_version_minor))

###################################
## (Ad-hoc) command-line parsing ##
###################################

class Config:
    def __init__(self):
        self.ipython_dir = IPython.utils.path.get_ipython_dir()
        self.ipython_profile_dir = self.ipython_dir  # default is ipython dir
        self.ipython_profile_name = "fishbowl"
        self.lisp_implementation = "sbcl" # TODO: ccl support (others ? requires threading)
        import shutil
        self.ipython_executable = shutil.which("ipython3")
        self.ipython_command = "console"

def process_command_line(argv):
    config = Config()
    
    import inspect
    import os
    import os.path
    config.fishbowl_startup_def_dir = os.path.dirname(os.path.realpath(inspect.getsourcefile(Config)))
    #print("Fishbowl startup def dir = {}".format(config.fishbowl_startup_def_dir))

    config.fishbowl_startup_run_dir = os.path.realpath(os.getcwd())
    #print("Fishbowl startup run dir = {}".format(config.fishbowl_startup_run_dir))

    config.fishbowl_startup_script = os.path.realpath(argv[0])
    #print("Fishbowl startup script = {}".format(config.fishbowl_startup_script))

    i = 1
    if len(argv) > 1 and not (argv[i].startswith('-')):  # first argument should be the ipython command
        config.ipython_command = argv[i]
        i += 1
    # print("IPython command = {}".format(config.ipython_command))
    # default is "console"

    profile_dir_set = False
    profile_set = False
    lisp_set = False
    ipython_exec_set = False

    while i < len(argv):
        #print("cmd line option #{}: {}".format(i, argv[i]))

        if argv[i].startswith("--profile-dir="):
            if profile_dir_set:
                halt("Error: --profile-dir option set twice")
            config.ipython_profile_dir = argv[i][14:]
            profile_dir_set = True
        elif argv[i].startswith("--profile="):
            if profile_set:
                halt("Error: --profile option set twice")
            config.ipython_profile_name = argv[i][10:]
            profile_set = True
        elif argv[i].startswith("--lisp="):
            if lisp_set:
                halt("Error: --lisp option set twice")
            config.lisp_implementation = argv[i][7:]
            lisp_set = True
        elif argv[i].startswith("--ipython-exec"):
            if ipython_exec_set:
                halt("Error: --ipython-exec option set twice")
            import shutil
            config.ipython_executable = shutil.which(argv[i][15:])
            ipython_exec_set = True
        else:
            halt("Error: unexpected option '{}'".format(argv[i]))

        i += 1

    #print("IPython profile directory = {}".format(config.ipython_profile_dir))
    #print("IPython profile = {}".format(config.ipython_profile_name))
    #print("Lisp implementation = {}".format(config.lisp_implementation))
    #print("IPython executable = {}".format(config.ipython_executable))

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
    ipython_version = tuple([int(d) for d in ipython_version_string.split(".")])
    #print("ipython version = {}".format(ipython_version))
    if (ipython_version[0] != ipython_version_major) \
       or (ipython_version[1] != ipython_version_minor):
        halt("Error: mismatch ipython version ({}.{} vs {}.{})".format(ipython_version[0], ipython_version[1],
                                                                       ipython_version_major, ipython_version_minor))

###################################
## Check the lisp implementation ##
###################################

if config.lisp_implementation == "sbcl":
    try:
        sbcl_version_string = subprocess.check_output(["sbcl", "--version"]).decode()
    except FileNotFoundError:
        halt("Error: 'sbcl' executable not in PATH")
    except subprocess.CalledProcessError as e:
        halt("Error: {} from SBCL".format(e))

    #print("sbcl version string = {}".format(sbcl_version_string))

    import re
    m = re.match(r".*([0-9]\.[0-9]\.[0-9])", sbcl_version_string)
    if not m:
        halt("Error: issue with sbcl version string (please report)")
    
    config.sbcl_version = tuple([int(d) for d in m.group(1).split(".")])
    #print("sbcl version = {}".format(config.sbcl_version))
    if config.sbcl_version[0] < 1 or config.sbcl_version[1] < 2:
        halt("Error: require SBCL v1.2.x or above")
elif config.lisp_implementation == "ccl":
    halt("Error: Clozure Common Lisp not (yet) supported")
elif config.lisp_implementation == "ecl":
    halt("Error: ECL not (yet) supported")
elif config.lisp_implementation == "cmucl":
    halt("Error: CMUCL not (yet) supported")
elif config.lisp_implementation == "clisp":
    halt("Error: CLisp not (yet) supported")
else:
    halt("Error: Common Lisp implementation '{}' not supported".format(config.lisp_implementation))

print("... Kernel: using {}".format(sbcl_version_string))

##############################
## Installation of profile  ##
##############################

profile = None

if not profile:
    try:
        print("file = {}".format(config.ipython_profile_dir + "/profile_" + config.ipython_profile_name))
        profile = open(config.ipython_profile_dir + "/profile_" + config.ipython_profile_name, "r")
    except FileNotFoundError:
        # profile creation
        print("... create profile '{}'".format(config.ipython_profile_name))
        IPython.start_ipython([config.ipython_executable, 
                               "profile", "create", config.ipython_profile_name, 
                               "--profile-dir={}".format(config.ipython_profile_dir)])


# Taken from:
# https://github.com/minad/iruby/blob/master/lib/iruby/static/custom/custom.js
CUSTOM_JS_CODEMIRROR_CONFIG = r"""
$([IPython.events]).on('notebook_loaded.Notebook', function(){
    // add here logic that should be run once per **notebook load**
    IPython.notebook.metadata.language = 'lisp' ;
});
$([IPython.events]).on('app_initialized.NotebookApp', function(){
    // add here logic that shoudl be run once per **page load**
    CodeMirror.requireMode('lisp', function(){
        console.log('Lisp mode should now be available in codemirror.');
    })
   IPython.CodeCell.options_default['cm_config']['mode'] = 'lisp';
   IPython.CodeCell.options_default['cm_config']['indentUnit'] = 4;
});
document.title = document.title.replace('IPython', 'Fishbowl');
"""

CUSTOM_JS_CODEMIRROR_CONFIG_HEAD = r"""//<<<FISHBOWL_CUSTOM_JS_CODEMIRROR_CONFIG_HEAD>>>
"""

CUSTOM_JS_CODEMIRROR_CONFIG_FOOT = r"""//<<<FISHBOWL_CUSTOM_JS_CODEMIRROR_CONFIG_FOOT>>>
"""

