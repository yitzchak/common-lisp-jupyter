# common-lisp-jupyter

[![Binder][mybinder-badge]][mybinder]
[![Quicklisp][quicklisp-badge]][quicklisp]
[![Build Status][travis-badge]][travis]
[![Build status][appveyor-badge]][appveyor]

A Common Lisp kernel for Jupyter along with a library for building Jupyter
kernels, based on [Maxima-Jupyter][] by Robert Dodier which was based on
[cl-jupyter][] by Frederic Peschanski.

This file describes the installation and usage of common-lisp-jupyter on a local
machine, but you can try out common-lisp-jupyter without installing anything by
clicking on the Binder badge above.

## Motivation

In developing Maxima-Jupyter there were a number of enhancements and features
added that cl-jupyter does not support. Because the structure of Maxima-Jupyter
is significantly different from cl-jupyter back-porting these changes would
probably be difficult. Therefore common-lisp-jupyter was created as library to
support both Maxima-Jupyter and the included Common Lisp kernel. The library
component handles all Jupyter messaging and most of the common kernel management
tasks. This leaves only code evaluation and completion testing left to the
derived kernel.

## Examples

- [about.ipynb][]
- [widgets.ipynb][]

## Comparison to cl-jupyter

In comparison to cl-jupyter the included kernel `common-lisp` has the following
features.

- Markdown and PDF rendering

- Handling and rendering of multiple value return

- Correct setting of the REPL variables `-`, `+`, `++`, `+++`, `*`, `**`, `**`,
  `/`, `//` and `///`

- Automatic detection of MIME types for files

- Handles code inspection, code completeness checking, code completion and
  shutdown requests.

- Can send clear output requests.

- Improved JSON serialization via [jsown][]

- Improved message handling

- Automatic detection of prompts on `*query-io*` and use `input_request` message
  to facilitate responses.

- COMM message handling and registration.

- Lisp interface to core IPython widgets is included in the `jupyter-widgets`
  package.

## Installation

common-lisp-jupyter may be installed on a machine using a local installation, a
[repo2docker][] installation, or via a Docker image.

## Local Installation

### Requirements

- [Roswell][] with a supported Common Lisp implementation. Currently
  [Steel Bank Common Lisp][SBCL] and [Clozure Common Lisp][CCL] are known to
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

A prebuilt docker image is available via Docker Hub. This image maybe run run
the following command.

```sh
docker run --network=host -it yitzchak/common-lisp-jupyter jupyter notebook --ip=127.0.0.1
```

A local Docker image of common-lisp-jupyter may be built after this repo has
been cloned using the following command (`sudo` may be required). This image is
based on the docker image `archlinux/base`.

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
`jupyter:evaluate-code` and `jupyter:code-is-complete`. For reference, please
see [cl-jupyter.lisp][] for the Common Lisp kernel that is included in the
package.

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

The method `jupyter:evaluate-code` should evaluate all code included in the
`input` argument and return a list of evaluation results. Each result should be
wrapped in an appropriate sub-class of `jupyter:result`. For instance, to return
a S-Expr result one would call `jupyter:make-lisp-result`.
`jupyter:evaluate-code` will be called with the package declared in the kernel
class as the current default. For example, the Common Lisp kernel evaluates code
in the `COMMON-LISP-USER` package.

The Jupyter message `is_complete_request` is also supported via the
`code-is-complete` method. The return result should be one of allowed status
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
[cl-jupyter]: https://github.com/fredokun/cl-jupyter/
[jsown]: http://quickdocs.org/jsown/
[Jupyter]: https://jupyter.org/
[Maxima-Jupyter]: https://github.com/robert-dodier/maxima-jupyter/
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
[widgets.ipynb]: http://nbviewer.jupyter.org/github/yitzchak/common-lisp-jupyter/blob/master/examples/widgets.ipynb
[Windows Installation]: https://github.com/yitzchak/common-lisp-jupyter/wiki/Windows-Installation
[ZeroMQ]: http://zeromq.org/
