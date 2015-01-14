#!/usr/bin/env python3

## Fishbowl startup script

## As a distributed program, the startup phase of
## Fishbowl is non-trivial, and requires access to
## the filesystem.   Since Python (3.x) is a requirement
## for Fishbowl, the startup script is also written in
## Python

import sys

def halt(msg):
    print(msg, file=sys.stderr)
    print("Abort.", file=sys.stderr)
    sys.exit(1)

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

# (Ad-hoc) command-line parsing
class Config:
    def __init__(self):
        self.ipython_profile_name = "fishbowl"
        self.ipython_dir = IPython.utils.path.get_ipython_dir()
        self.ipython_profile_dir = self.ipython_dir  # default is ipython dir
        self.ipython_profile = "profile_default"  # default is ipython dir
        self.lisp_implementation = "sbcl" # TODO: ccl support (others ? requires threading)
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

    while i < len(argv):
        print("cmd line option #{}: {}".format(i, argv[i]))

        if argv[i].startswith("--profile_dir="):
            if profile_dir_set:
                halt("Error: --profile_dir option set twice")
            config.ipython_profile_dir = argv[i][14:]
            profile_dir_set = True
        elif argv[i].startswith("--profile="):
            if profile_set:
                halt("Error: --profile option set twice")
            config.ipython_profile = argv[i][10:]
            profile_set = True

        i += 1

    #print("IPython profile directory = {}".format(config.ipython_profile_dir))
    #print("IPython profile = {}".format(config.ipython_profile))

    return config

config = process_command_line(sys.argv)

# Taken from: https://github.com/minad/iruby/blob/master/lib/iruby/static/custom/custom.js
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

