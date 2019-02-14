# common-lisp-jupyter

[![Binder][mybinder-badge]][mybinder]
[![Quicklisp][quicklisp-badge]][quicklisp]
[![Build Status][travis-badge]][travis]
[![Build status][appveyor-badge]][appveyor]

A Common Lisp kernel for Jupyter, based on Maxima-Jupyter by Robert Dodier which
was based on cl-jupyter, a Jupyter kernel for Common Lisp, by Frederic
Peschanski.

This file describes the installation and usage of common-lisp-jupyter on a local
machine, but you can try out common-lisp-jupyter without installing anything by
clicking on the Binder badge above.

## Examples

[about.ipynb][]

## Installation

common-lisp-jupyter may be installed on a machine using a local installation, a
[repo2docker][] installation, or via a Docker image.

## Local Installation

### Requirements

- [Roswell][] with a supported Common Lisp implementation. Currently
  [SBCL][Steel Bank Common Lisp] and [CCL][Clozure Common Lisp] are known to
  work. Other implementations which support the [Bordeaux Threads][] package
  might work.

- [Jupyter][]

- [ZeroMQ][] library including development headers. On debian-based systems, you can
  satisfy this requirement by installing the package `libczmq-dev`. On
  Arch-based systems the package is named `zeromq`. There are several ways to
  satisfy the requirement on Windows. For more details see the
  [Windows Installation][] instruction in the wiki.

### Installing

- Install Roswell using the [Roswell Installation Guide][].

- Install common-lisp-jupyter by roswell
```
ros install yitzchak/common-lisp-jupyter
```
- Add the PATH in the initialization file (such as `~/.bashrc`)
```
export PATH=$PATH:~/.roswell/bin
```

### Running common-lisp-jupyter

common-lisp-jupyter may be run from a local installation in console mode by the
following.

```sh
jupyter console --kernel=common-lisp
```

Notebook mode is initiated by the following.

```sh
jupyter notebook
```

## repo2docker Usage

common-lisp-jupyter may be run as a Docker image managed by repo2docker which
will fetch the current code from GitHub and handle all the details of running
the Jupyter Notebook server.

First you need to install repo2docker (`sudo` may be required)

```sh
pip install jupyter-repo2docker
```

Once repo2docker is installed then the following will build and start the
server. Directions on accessing the server will be displayed once the image is
built.

```sh
jupyter-repo2docker --user-id=1000 --user-name=jupyter https://github.com/yitzchak/common-lisp-jupyter
```

## Docker Image

A Docker image of common-lisp-jupyter may be built using the following command
(`sudo` may be required). This image is based on the docker image
`archlinux/base`.

```sh
docker build --tag=common-lisp-jupyter .
```

After the image is built the console may be run with

```sh
docker run -it common-lisp-jupyter jupyter console --kernel=common-lisp
```

## Writing Jupyter Kernels

New Jupyter kernels can be created by defining a new sub-class of
`jupyter:kernel` and by defining methods for the generic functions
`jupyter:evaluate` and `jupyter:is-complete`. For reference, please see
[cl-jupyter.lisp][] for the Common Lisp kernel that is included in the package.

The derived class of `jupyter:kernel` should initialize the following slots.
Most of these slots are used to reply to `kernel_info` messages. Documentation
for each can be found in the declaration of `jupyter:kernel`.

- `name`
- `package`
- `version`
- `banner`
- `language-name`
- `language-version`
- `mime-type`
- `file-exension`
- `pygments-lexer`
- `codemirror-mode`
- `help-links`

The method `jupyter:evaluate` should evaluate all code included in the `input`
argument and return a list of evaluation results. Each result should be wrapped
in an appropriate sub-class of `jupyter:result`. For instance, to return a S-Expr
result one would call `jupyter:make-lisp-result`. `jupyter:evaluate` will be
called with the package declared in the kernel class as the current default.
For example, the Common Lisp kernel evaluates code in the `COMMON-LISP-USER`
package.

The Jupyter message `is_complete_request` is also supported via the
`is-complete` method. The return result should be one of allowed status
messages, i.e. `"complete"`, `"incomplete"`, `"invalid"`, or `"unknown"`.

User level installation of kernels can be accomplished by a call to
`jupyter:install-kernel`. [cl-jupyter.lisp][] has an example of this call made
during the installation phase of Roswell.

<!--refs-->

[about.ipynb]: http://nbviewer.jupyter.org/github/yitzchak/common-lisp-jupyter/blob/master/examples/about.ipynb
[appveyor-badge]: https://ci.appveyor.com/api/projects/status/j2voo262b2v9qq3t/branch/master?svg=true
[appveyor]: https://ci.appveyor.com/project/yitzchak/common-lisp-jupyter/branch/master
[Bordeaux Threads]: https://common-lisp.net/project/bordeaux-threads/
[CCL]: https://ccl.clozure.com/
[cl-jupyter.lisp]: https://github.com/yitzchak/common-lisp-jupyter/blob/master/src/cl-kernel.lisp
[Jupyter]: https://jupyter.org/
[mybinder-badge]: https://mybinder.org/badge_logo.svg
[mybinder]: https://mybinder.org/v2/gh/yitzchak/common-lisp-jupyter/master
[nbviewer]: http://nbviewer.jupyter.org
[quicklisp-badge]: http://quickdocs.org/badge/common-lisp-jupyter.svg
[quicklisp]: http://quickdocs.org/common-lisp-jupyter
[repo2docker]: https://repo2docker.readthedocs.io/en/latest/
[Roswell Installation Guide]: https://github.com/roswell/roswell/wiki/Installation
[Roswell]: https://github.com/roswell/roswell
[SBCL]: http://www.sbcl.org/
[travis-badge]: https://travis-ci.com/yitzchak/common-lisp-jupyter.svg?branch=master
[travis]: https://travis-ci.com/yitzchak/common-lisp-jupyter
[Windows Installation]: https://github.com/yitzchak/common-lisp-jupyter/wiki/Windows-Installation
[ZeroMQ]: http://zeromq.org/
