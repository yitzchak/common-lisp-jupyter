#! /bin/sh

ipython3 $1 --profile sbcl --Session.key="b''" --KernelManager.kernel_cmd="['sbcl', '--non-interactive', '--load', 'fishbowl.lisp', '{connection_file}']"
