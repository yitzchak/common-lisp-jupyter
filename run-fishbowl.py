#!/usr/bin/env python3

## Fishbowl startup script

## As a distributed program, the startup phase of
## Fishbowl/IPython is non-trivial, and requires access to
## the filesystem.   Since Python (3.x) is a requirement
## for Fishbowl, the startup script is also written in
## Python

import subprocess
import sys
import shutil
import os

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
        self.ipython_profile_dir = self.ipython_dir + "/profile_fishbowl"
        self.lisp_implementation = "sbcl" # TODO: ccl support (others ? requires threading)
        self.lisp_executable = None # we'll derive executable from implementation later if need be
        self.ipython_executable = shutil.which("ipython3")
        self.ipython_command = "console"
        self.lisp_preload = None

def process_command_line(argv):
    config = Config()
    
    import inspect
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

    if config.ipython_command not in { "console", "notebook" }:
        halt("Error: command '{}' not available\n  ==> choose 'console' (default) or 'notebook'".format(config.ipython_command))

    profile_dir_set = False
    profile_set = False
    lisp_set = False
    lisp_exec_set = False
    ipython_exec_set = False

    while i < len(argv):
        #print("cmd line option #{}: {}".format(i, argv[i]))

        if argv[i].startswith("--profile-dir="):
            if profile_dir_set or profile_set:
                halt("Error: unexpected '--profile-dir' option, profile already set")
            config.ipython_profile_dir = argv[i][14:]
            profile_dir_set = True
        elif argv[i].startswith("--profile="):
            if profile_set or profile_dir_set:
                halt("Error: unexpected '--profile' option, profile already set")
            config.ipython_profile_dir = config.ipython_dir + "/profile_" + argv[i][10:]
            profile_set = True
        elif argv[i].startswith("--lisp="):
            if lisp_set:
                halt("Error: --lisp option set twice")
            config.lisp_implementation = argv[i][7:]
            lisp_set = True
        elif argv[i].startswith("--lisp-exec="):
            if lisp_exec_set:
                halt("Error: --lisp-exec option set twice")
            config.lisp_executable = argv[i][12:]
            lisp_exec_set = True
        elif argv[i].startswith("--ipython-exec="):
            if ipython_exec_set:
                halt("Error: --ipython-exec option set twice")
            config.ipython_executable = shutil.which(argv[i][15:])
            ipython_exec_set = True
        elif argv[i].startswith("--lisp-preload="):
            config.lisp_preload = argv[i][15:]
        else:
            halt("Error: unexpected option '{}'".format(argv[i]))

        i += 1

    #print("IPython profile directory = {}".format(config.ipython_profile_dir))
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
    import re
    # cut off a hyphen and anything following, e.g. "2.4.2-maint" --> "2.4.2"
    foo = re.sub ("-.*$", "", ipython_version_string)
    ipython_version = tuple([int(d) for d in foo.split(".")])
    #print("ipython version = {}".format(ipython_version))
    if (ipython_version[0] != ipython_version_major) \
       or (ipython_version[1] != ipython_version_minor):
        halt("Error: mismatch ipython version ({}.{} vs {}.{})".format(ipython_version[0], ipython_version[1],
                                                                       ipython_version_major, ipython_version_minor))

###################################
## Check the lisp implementation ##
###################################

if config.lisp_implementation == "sbcl":
    if not config.lisp_executable:
        config.lisp_executable = 'sbcl'

    try:
        sbcl_version_string = subprocess.check_output([config.lisp_executable, "--version"]).decode()
    except FileNotFoundError:
        halt("Error: Lisp executable '{0}' not in PATH".format (config.lisp_executable))
    except subprocess.CalledProcessError as e:
        halt("Error: {} from SBCL".format(e))

    #print("sbcl version string = {}".format(sbcl_version_string))

    import re
    m = re.match(r".*([0-9]+\.[0-9]+\.[0-9]+)", sbcl_version_string)
    if not m:
        halt("Error: issue with sbcl version string (please report)")
    
    config.sbcl_version = tuple([int(d) for d in m.group(1).split(".")])
    #print("sbcl version = {}".format(config.sbcl_version))
    if config.sbcl_version[0] < 1 or config.sbcl_version[1] < 2:
        print ("warning: might require SBCL v1.2.x or above; stagger forward nonetheless")

    print("... Kernel: using {}".format(sbcl_version_string))
        
elif config.lisp_implementation == "ccl":
    if not config.lisp_executable:
        config.lisp_executable = 'ccl'

    try:
        ccl_version_string = subprocess.check_output([config.lisp_executable, "-V"]).decode()
    except FileNotFoundError:
        halt("Error: Lisp executable '{0}' not in PATH".format (config.lisp_executable))
    except subprocess.CalledProcessError as e:
        halt("Error: {} from CCL".format(e))

    #print("ccl version string = {}".format(ccl_version_string))

    
    import re
    m = re.match(r".*([0-9]+\.[0-9]+)", ccl_version_string)
    if not m:
        halt("Error: issue with ccl version string (please report)")
    
    config.ccl_version = tuple([int(d) for d in m.group(1).split(".")])
    #print("ccl version = {}".format(config.ccl_version))
    if config.ccl_version[0] < 1 or config.ccl_version[1] < 10:
        halt("Error: require CCL v1.10 or above")

    print("... Kernel: using {}".format(ccl_version_string))
        
elif config.lisp_implementation == "ecl":
    halt("Error: ECL not (yet) supported")
elif config.lisp_implementation == "cmucl":
    halt("Error: CMUCL not (yet) supported")
elif config.lisp_implementation == "clisp":
    halt("Error: CLisp not (yet) supported")
else:
    halt("Error: Common Lisp implementation '{}' not supported".format(config.lisp_implementation))


##############################
## Installation of profile  ##
##############################

print("... profile customization")

custom_js_file = None

nb_try = 0
while not custom_js_file:
    try:
        custom_js_file = open(config.ipython_profile_dir + "/static/custom/custom.js", "r")
    except FileNotFoundError:
        # profile creation
        print("... create profile '{}'".format(config.ipython_profile_dir))
        ### XXX: Issue when running ipython for different commands multiple times
        ### (MultipleInstanceError) ... So run in a subprocess
        #IPython.start_ipython([config.ipython_executable, 
        #                       "profile", "create",  
        #                       "--profile-dir={}".format(config.ipython_profile_dir)])

        try:
            subprocess.check_call([config.ipython_executable,
                                  'profile', 'create',
                                  "--profile-dir={}".format(config.ipython_profile_dir)])
        except FileNotFoundError:
            halt("Error: '{}' executable not found".format(config.ipython_executable))
        except subprocess.CalledProcessError as e:
            halt("Error: {} from IPython".format(e))


    nb_try += 1
    if nb_try > 2:
        halt("Error: could not create profile (please report)")

## copy the custom js file

shutil.copy(config.fishbowl_startup_def_dir + "/profile/custom.js",
            config.ipython_profile_dir + "/static/custom/custom.js")

os.makedirs(config.ipython_profile_dir + "/static/components/codemirror/mode/commonlisp/", exist_ok=True)
shutil.copy(config.fishbowl_startup_def_dir + "/profile/commonlisp.js",
            config.ipython_profile_dir + "/static/components/codemirror/mode/commonlisp/commonlisp.js")



##############################
## Run the IPython command  ##
##############################

print("... launch frontend")

### XXX: strange MultipleInstanceError error raise, use subprocess
# IPython.start_ipython([config.ipython_executable, config.ipython_command,  
#                        "--profile-dir={}".format(config.ipython_profile_dir),
#                        "--Session.key=b''",
#                        "--KernelManager.kernel_cmd=['sbcl', '--non-interactive', '--load', '{}/fishbowl.lisp', '{{connection_file}}']".format(config.fishbowl_startup_def_dir)])

if config.lisp_implementation == "sbcl":
    KERNEL_CMD = "--KernelManager.kernel_cmd=['{3}', '--non-interactive',{1} '--load', '{0}/fishbowl.lisp', '{0}/src', '{2}', '{{connection_file}}']".format(config.fishbowl_startup_def_dir, "'--load', '{}',".format(config.lisp_preload) if config.lisp_preload else "", config.fishbowl_startup_run_dir, config.lisp_executable)

elif config.lisp_implementation == "ccl":
    KERNEL_CMD = "--KernelManager.kernel_cmd=['{3}', '--batch',{1} '--load', '{0}/fishbowl.lisp', '--', '{0}/src', '{2}', '{{connection_file}}']".format(config.fishbowl_startup_def_dir,  "'--load', '{}',".format(config.lisp_preload) if config.lisp_preload else "", config.fishbowl_startup_run_dir, config.lisp_executable)

else:
    halt("Error: unsupported lisp implementation '{}'".format(lisp_implementation))
    
# print("KERNEL_CMD = {}".format(KERNEL_CMD))

try:
    import signal
    signal.signal(signal.SIGINT, signal.SIG_IGN)
    subprocess.check_call([config.ipython_executable,
                           config.ipython_command,
                           "--profile-dir={}".format(config.ipython_profile_dir),
                           "--Session.key=b''", KERNEL_CMD],
                          stdout=sys.stdout, stdin=sys.stdin, stderr=sys.stderr, shell=False)
except FileNotFoundError:
    halt("Error: '{}' executable not found".format(config.ipython_executable))
except subprocess.CalledProcessError as e:
    halt("Error: {} from IPython".format(e))


