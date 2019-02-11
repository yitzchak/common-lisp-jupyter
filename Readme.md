# Common-Lisp-Jupyter

[![Binder][mybinder-badge]][mybinder]

A Common Lisp kernel for Jupyter, based on maxima-jupyter by Robert Dodier which
was based on CL-Jupyter, a Jupyter kernel for Common Lisp, by Frederic
Peschanski.

This file describes the installation and usage of CL-Jupyter on a local
machine, but you can try out CL-Jupyter without installing anything by
clicking on the Binder badge above.

## Examples

[about.ipynb][]

## Installation

CL-Jupyter may be installed on a machine using a local installation, a
[repo2docker][] installation, or via a Docker image.

## Local Installation

### Requirements

- [roswell](https://github.com/roswell/roswell)
- Jupyter, or IPython 3.x

### Installing

Please install roswell at first.

[Roswell Installation Guide](https://github.com/roswell/roswell/wiki/Installation)

After that, please follow the steps bellow.

- Install cl-jupyter by roswell
```
ros install yitzchak/cl-jupyter
```
- Add the PATH in the initialization file (such as ~/.bashrc)
```
export PATH=$PATH:~/.roswell/bin
```

### Running CL-Jupyter

CL-Jupyter may be run from a local installation in console mode by the following.

```sh
jupyter console --kernel=lisp
```

Notebook mode is initiated by the following.

```sh
jupyter notebook
```

When you enter stuff to be evaluated, you must include the usual trailing
semicolon or dollar sign:

```
In [1]: 2*21;
Out[1]: 42

In [2]:
```

## repo2docker Usage

CL-Jupyter may be run as a Docker image managed by repo2docker which will
fetch the current code from GitHub and handle all the details of running the
Jupyter Notebook server.

First you need to install repo2docker (`sudo` may be required)

```sh
pip install jupyter-repo2docker
```

Once repo2docker is installed then the following will build and start the
server. Directions on accessing the server will be displayed once the image
is built.

```sh
jupyter-repo2docker --user-id=1000 --user-name=mj https://github.com/yitzchak/cl-jupyter
```

## Docker Image

A Docker image of CL-Jupyter may be built using the following command
(`sudo` may be required). This image is based on the docker image
`base/archlinux`.

```sh
docker build --tag=cl-jupyter .
```

After the image is built the console may be run with

```sh
docker run -it cl-jupyter jupyter console --kernel=lisp
```

<!--refs-->

[about.ipynb]: http://nbviewer.jupyter.org/github/yitzchak/cl-jupyter/blob/master/examples/about.ipynb
[mybinder-badge]: https://mybinder.org/badge_logo.svg
[mybinder]: https://mybinder.org/v2/gh/yitzchak/cl-jupyter/master
[nbviewer]: http://nbviewer.jupyter.org
[repo2docker]: https://repo2docker.readthedocs.io/en/latest/
