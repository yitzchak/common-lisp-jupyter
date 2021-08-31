common-lisp-jupyter may be installed on a machine using a local installation, a
[repo2docker][] installation, or via a Docker image.

# Local Installation

## Requirements

- [Roswell][] or a system-wide installed Common Lisp implementation. Currently
  [Clozure Common Lisp][CCL], [CLASP][], [Embeddable Common Lisp][ECL] and
  [Steel Bank Common Lisp][SBCL] are known to work. Other implementations which
  support the [Bordeaux Threads][] package might work. For current
  implementation status please the [Wiki][implementation-status].

- [Jupyter][]

- [ZeroMQ][] library including development headers. On debian-based systems, you
  can satisfy this requirement by installing the package `libczmq-dev`. On
  Arch-based systems the package is named `zeromq`. In homebrew the package is
  named `czmq`. There are several ways to satisfy the requirement on Windows.
  For more details see the [Windows Installation][] instruction in the wiki.
  
- [jupyterlab-debugger--restarts][] is needed to enable Common Lisp style 
  restarts in the JupyterLab debugger.

## Installation Type

Jupyter kernels may be installed as a system-wide kernel which is accessible
to all users or as a user specific kernel which is only available to the user
that installed the kernel.

System-wide kernels are typically installed to a directory accessible by all 
users. For example, on Linux or MacOS these kernels are generally installed to
`/usr/share/jupyter/kernels/` or `/usr/local/share/jupyter/kernels/`. The former
is called a "system kernel" and the latter is called a "system local kernel."

User specific kernels are installed to directory in the user's home directory. 
On Linux this is in `~/.local/share/jupyter/kernels`.

For both user and system installations, common-lisp-jupyter has dependencies
defined via its ASDF definition that need to be satisfied. These dependencies
need to satified via a Roswell installation or a Quicklisp installation. Other
package managers such as guix or other operating system package managers may be 
able to accomplish this, but these two are the primary tested methods.

Since Quicklisp (included in Roswell) is a per-user package manager this makes
system kernel installation a bit tricky. A "non-bundled" system installation
will rely on each user having a functioning Quicklisp installation and will
install needed dependencies for each user the first time they load the kernel
in JupyterLab or jupyter-console.

A "bundled" system installation will use the current user's Quicklisp
installation to discover the needed dependencies and create a static bundle
of those files to install in the system kernel's path. For this type of 
installation Quicklisp does not need to be installed for the individual users.

For user kernels either a "non-image" or an "image" installation can be used.
An image based installation will use `uiop:dump-image` to create a saved image
of the kernel to facilitate quick load times. The "non-image" based installation
will use `ql:quickload` or `asdf:load-system` to initiate the kernel load so
that current Quicklisp/ASDF system will be used every time the kernel is loaded.

Unless there is are specific requirements that dictate the type of kernel
installation a user "non-image" kernel is recomended. If a system kernel is
needed then a "bundled" kernel is recommended.

## Installing via Quicklisp/ASDF

Install [Quicklisp][] and use `(ql:add-to-init-file)`. If you already have
Quicklisp installed you may need to update your distribution with 
`(ql:update-dist "quicklisp")` to resolve package conflicts.

- To install an image based user kernel evaluate `(cl-jupyter:install-image)`
- To install a non-image based user kernel evaluate `(cl-jupyter:install)`
- To install a Quicklisp/ASDF based system which uses system or user packages
  available via `ql:quickload` or `asdf:load-system` evaluate
  `(cl-jupyter:install :system t :prefix "pkg/")`. Afterward 
  copy the contents of the `pkg` directory to the system root. For instance in 
  bash `sudo cp -r pkg/* /`. If you want to install to `/usr/local/share` then
  add the `:local t` key.
- To install a Quicklisp bundle based system evaluate
  `(cl-jupyter:install :system t :local t :bundle t :prefix "pkg/")`. Afterward 
  copy the contents of the `pkg` directory to the system root. For instance in 
  bash `sudo cp -r pkg/* /`

## Installing via Roswell

- Install Roswell using the [Roswell Installation Guide][]. If you already have
  Roswell installed you may need to update your Quicklisp distribution with
  `(ql:update-dist "quicklisp")` inside a `ros run` shell to resolve package
  conflicts.

- Add the PATH in the initialization file (such as `~/.bashrc`)
```sh
export PATH=$PATH:~/.roswell/bin
```

- Install common-lisp-jupyter by roswell
```sh
ros install common-lisp-jupyter
```

## Running common-lisp-jupyter

common-lisp-jupyter may be run from a local installation in console mode by the
following.

```sh
jupyter console --kernel=common-lisp
```

Notebook mode is initiated by the following.

```sh
jupyter notebook
```

# repo2docker Usage

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

# Docker Image

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

<!--refs-->

[about.ipynb]: http://nbviewer.jupyter.org/github/yitzchak/common-lisp-jupyter/blob/master/examples/about.ipynb
[Bordeaux Threads]: https://common-lisp.net/project/bordeaux-threads/
[CCL]: https://ccl.clozure.com/
[CLASP]: https://github.com/clasp-developers/clasp
[ci]: https://github.com/yitzchak/common-lisp-jupyter/actions/
[ci-badge]: https://github.com/yitzchak/common-lisp-jupyter/workflows/ci/badge.svg
[cl-jupyter]: https://github.com/fredokun/cl-jupyter/
[cl-jupyter.lisp]: https://github.com/yitzchak/common-lisp-jupyter/blob/master/src/cl-kernel.lisp
[cytoscape-clj]: https://github.com/yitzchak/cytoscape-clj
[ECL]: https://common-lisp.net/project/ecl/
[jsown]: http://quickdocs.org/jsown/
[julia.ipynb]: http://nbviewer.jupyter.org/github/yitzchak/common-lisp-jupyter/blob/master/examples/julia.ipynb
[Jupyter]: https://jupyter.org/
[kekule-clj]: https://github.com/yitzchak/kekule-clj
[Maxima-Jupyter]: https://github.com/robert-dodier/maxima-jupyter/
[mybinder-badge]: https://mybinder.org/badge_logo.svg
[mybinder]: https://mybinder.org/v2/gh/yitzchak/common-lisp-jupyter/master?urlpath=lab
[nbviewer]: http://nbviewer.jupyter.org
[ngl-clj]: https://github.com/yitzchak/ngl-clj
[Quicklisp]: https://www.quicklisp.org/
[repo2docker]: https://repo2docker.readthedocs.io/en/latest/
[Roswell]: https://github.com/roswell/roswell
[Roswell Installation Guide]: https://github.com/roswell/roswell/wiki/Installation
[SBCL]: http://www.sbcl.org/
[sheet-clj]: https://github.com/yitzchak/sheet-clj
[uiop:dump-image]: https://common-lisp.net/project/asdf/uiop.html#index-dump_002dimage
[widgets.ipynb]: http://nbviewer.jupyter.org/github/yitzchak/common-lisp-jupyter/blob/master/examples/widgets.ipynb
[Windows Installation]: https://github.com/yitzchak/common-lisp-jupyter/wiki/Windows-Installation
[ZeroMQ]: http://zeromq.org/
[implementation-status]: https://github.com/yitzchak/common-lisp-jupyter/wiki/Implementation-Status
[jupyterlab-debugger--restarts]: https://github.com/yitzchak/jupyterlab-debugger-restarts
