#! /bin/sh

ipython3 console --profile sbcl --Session.key="b''" --Application.verbose_crash=True --KernelManager.kernel_cmd="['sbcl', '--non-interactive', '--load', 'uncommonshell.lisp', '{connection_file}']"
