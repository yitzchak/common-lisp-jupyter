#!/usr/bin/env python3

## Maxima-Jupyter installation script

## Note: since the kernel of Jupyter is written itself in Python,
##       it is much simpler to have a Python-based installation script.

import argparse
import json
import jupyter_client
import os
import sys
import tempfile


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
if sys.version_info.major < 3 or sys.version_info.minor < 3:
    halt("Error: Maxima-Jupyter requires Python v3.3 or above")


##############################
## Parsing of arguments     ##
##############################

ap = argparse.ArgumentParser()

g = ap.add_mutually_exclusive_group()
g.add_argument('--user', dest='user', action='store_true',
               help='Install as user kernel (i.e., somewhere under the user\'s home directory).')
g.add_argument('--system', dest='user', action='store_false',
               help='Install as system kernel (i.e., in the system-wide kernel registry).')
g.add_argument('--prefix',
                help='Kernel prefix path. Used for conda/virtual or packaging scripts.')

g = ap.add_mutually_exclusive_group(required=True)
g.add_argument('--exec', help='Absolute path to saved lisp kernel image.')
g.add_argument('--root', help='Absoute path to root of Maxima-Jupyter, used for embedded kernel.')

ap.add_argument('--maxima', default='maxima',
                help='Path to Maxima executable.')

args = ap.parse_args()

##############################
## Installation of kernel   ##
##############################

KERNEL_SPEC = {
    "argv": [
        'sbcl',
        '--non-interactive',
        '--load',
        format(os.path.join(args.root, 'load-cl-jupyter.lisp')),
        '--eval',
        '(jupyter-kernel:kernel-start \'cl-jupyter:kernel "{connection_file}")'
    ],
    "display_name": "Lisp",
    "language": "lisp"
}

tempdir = tempfile.mkdtemp()

with open(os.path.join(tempdir, 'kernel.json'), "w") as kernel_spec_file:
    json.dump(KERNEL_SPEC, kernel_spec_file)

jupyter_client.kernelspec.install_kernel_spec(tempdir, kernel_name='lisp',
                                              user=args.user,
                                              prefix=args.prefix)

print("maxima-jupyter: installation complete.")
