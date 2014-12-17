#! /bin/sh

ipython3 console --profile sbcl --Session.key="b''" --KernelManager.kernel_cmd="['sbcl', '--load', 'uncommonshell.lisp', '{connection_file}']"
