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
               help='Install as user kernel.')
g.add_argument('--system', dest='user', action='store_false',
               help='Install as system kernel.')

g = ap.add_mutually_exclusive_group(required=True)
g.add_argument('--exec', help='Absolute path to saved lisp kernel image.')
g.add_argument('--src', help='Absoute path to source files for embedded kernel.')

ap.add_argument('--maxima', default='maxima',
                help='Path to Maxima executable.')

ap.add_argument('--prefix',
                help='Prefix path to append to generated bootstrap file. Used in packaging scripts.')

args = ap.parse_args()

##############################
## Installation of kernel   ##
##############################

registry_path = args.src

if not registry_path.endswith('/'):
    registry_path += '/'

bootstrap_path = os.path.join(args.src, 'bootstrap.lisp')
actual_bootstrap_path = bootstrap_path if args.prefix is None else os.path.join(bootstrap_path, args.prefix)

with open(actual_bootstrap_path, 'w') as bootstrap_file:
    bootstrap_file.write('''(push #p"{0}" asdf:*central-registry*)
(ql:quickload "maxima-jupyter")
(maxima::$load "stringproc")'''.format(registry_path))

KERNEL_SPEC = {
    "argv": [
        args.exec,
        '{connection_file}'
    ] if args.src is None else [
        args.maxima,
        '--very-quiet',
        '--preload-lisp={0}'.format(bootstrap_path),
        '--batch-string=kernel_start("{connection_file}")$'
    ],
    "display_name": "Maxima",
    "language": "maxima"
}

tempdir = tempfile.mkdtemp()

with open(os.path.join(tempdir, 'kernel.json'), "w") as kernel_spec_file:
    json.dump(KERNEL_SPEC, kernel_spec_file)

jupyter_client.kernelspec.install_kernel_spec(tempdir, kernel_name='maxima',
                                              user=args.user)

print("maxima-jupyter: installation complete.")
