# Maxima-Jupyter

[![Binder][mybinder-badge]][mybinder]

An enhanced interactive environment for the computer algebra system Maxima,
based on CL-Jupyter, a Jupyter kernel for Common Lisp, by Frederic Peschanski.
Thanks, Frederic!

This file describes the installation and usage of Maxima-Jupyter on a local
machine, but you can try out Maxima-Jupyter without installing anything by
clicking on the Binder badge above.

## Examples

- [MaximaJupyterExample.ipynb][] &mdash; General usage of Maxima from within
  Jupyter Notebook.

- [MaximaJupyterTalk.ipynb][] &mdash; My notes for a talk given to the Portland
  Python User Group.

- [Plots.ipynb][] &mdash; Usage of plotting facilities from within Jupyter
  Notebook.

These examples make use of [nbviewer][].
You can submit a link to your own notebook to tell nbviewer to render it.

Note that the Github notebook renderer (i.e., what you see if you click on a
notebook file in the Github file browser) is currently (November 2018) somewhat
suboptimal ([bug report][]); it renders math as plain text, not as typeset
formulas.

## Installation

Maxima-Jupyter may be installed on a machine using a local installation, a
[repo2docker][] installation, or via a Docker image.

## Local Installation

### Requirements

To try Maxima-Jupyter you need :

 - a Maxima executable

   - built with a Common Lisp implementation which has native threads

     - SBCL works for sure

     - Clozure CL works for sure

     - Other implementations which support the Bordeaux Threads package
       might work. The [Bordeaux Threads project description][] says
       "Supports all major Common Lisp implementations: SBCL, CCL, Lispworks, Allegro, ABCL, ECL, Clisp."
       Aside from SBCL and CCL (i.e. Clozure CL) which are known to work,
       the others in that list are untested with maxima-jupyter.

     - Note also that ECL might theoretically work, since it is supported
       by Bordeaux Threads. However, Maxima-Jupyter developers have not been
       successful in getting ECL to work with Maxima-Jupyter, so they
       recommend against it. SBCL and Clozure CL are known to work, try
       those instead.

     - Note specifically that GCL **is not** supported by Bordeaux Threads,
       and therefore cannot work with maxima-jupyter.

   - You might or might not need to build Maxima. (A) If you have available
     a Maxima binary package compiled with a compatible Lisp implementation
     (i.e. SBCL, Clozure CL, Lispworks, etc. as enumerated above),
     then you do not need to build Maxima. (B) Otherwise, you must install
     a compatible Lisp implementation and compile Maxima yourself.

 - [Quicklisp][]

   - When you load Maxima-Jupyter into Maxima for the first time,
     Quicklisp will download some dependencies automatically.
     Good luck.

 - Python 3.2 or above

 - Jupyter, or IPython 3.x

 - If the build aborts because the file `zmq.h` is missing, you may need to
   install the development files for the high-level C binding for ZeroMQ.
   On debian-based systems, you can satisfy this requirement by installing
   the package `libczmq-dev`.

### Installing Maxima-Jupyter

First you must install Jupyter, then you can install Maxima-Jupyter.

I installed Jupyter via:

     python3 -m pip install jupyter

For Maxima-Jupyter, there are two kernel installation methods.
In both methods, the effect of the installation command is to create a file
named `kernel.json` which tells Jupyter where to find Maxima-Jupyter.
Note that Maxima-Jupyter installation DOES NOT copy any Maxima-Jupyter files;
it only creates `kernel.json` which points to the location of Maxima-Jupyter
in your file system.

With the `--user` option in Method 1 or Method 2,
the `kernel.json` file is created in a directory somewhere under your home directory.
Otherwise, `kernel.json` is created in a system directory.
You might need superuser privilege (via `sudo` for example) to execute a system installation,
if the directory into which `kernel.json` is copied is not user-writable.

Note that `jupyter --paths` lists file system paths used by Jupyter;
kernels are sought in the paths under `data`.
Also, `jupyter kernelspec list` tells the kernels which are known to Jupyter.

For the record, on my system, a system installation copies `kernel.json`
into `/usr/local/share/jupyter/kernels/maxima/kernel.json`
and a user installation copies `kernel.json`
into `/home/robert/.local/share/jupyter/kernels/maxima/kernel.json`.

#### Method 1. Maxima-Jupyter binary executable installation

The first installation method is to create a binary executable image,
as detailed in [make-maxima-jupyter-recipe.txt][].
After creating that image, execute one of these two commands to tell Jupyter about it.

For a system installation,

```sh
python3 ./install-maxima-jupyter.py --exec=path/to/maxima-jupyter-image
```

For a user installation,

```sh
python3 ./install-maxima-jupyter.py --exec=path/to/maxima-jupyter-image --user
```

#### Method 2. Maxima-Jupyter loadable source installation

The second installation method executes Maxima and then loads Maxima-Jupyter into Maxima.
The advantange to this method is that the normal initialization behavior of Maxima,
such as loading `maxima-init.mac`, is preserved.

Note that in order for this method to work, Quicklisp needs be loaded by default
in every Maxima session. See Quicklisp documentation for details.

For a system installation,

```sh
python3 ./install-maxima-jupyter.py --root=`pwd`
```

where the shell command `pwd` emits the current working directory
(which must be the Maxima-Jupyter top-level directory,
since it contains `install-maxima-jupyter.py`).

For a user installation,

```sh
python3 ./install-maxima-jupyter.py --root=`pwd` --user
```

The option `--maxima` may also be used to specify the location of the Maxima executable.
If not specified, the command which launches Maxima is just `maxima`,
therefore the first instance of `maxima` in the PATH environment variable
is the one which is executed.

#### Method 3. Installation on Arch/Manjaro

The package for Arch Linux is [maxima-jupyter-git][]. Building and installing
(including dependencies) can be accomplished with:

```sh
yaourt -Sy maxima-jupyter-git
```

Alternatively use ``makepkg``:

```sh
curl -L -O https://aur.archlinux.org/cgit/aur.git/snapshot/maxima-jupyter-git.tar.gz
tar -xvf maxima-jupyter-git.tar.gz
cd maxima-jupyter-git
makepkg -Csri
```

Please consult the [Arch Wiki][] for more information regarding installing
packages from the AUR.

### Code Highlighting Installation

Highlighting Maxima code is handled by CodeMirror in the notebook
and Pygments in HTML export.

The CodeMirror mode for Maxima is [maxima.js][]. To install it, find the
CodeMirror mode installation directory, create a directory named `maxima` there,
copy [maxima.js][] to the `maxima` directory, and update
`codemirror/mode/meta.js` as shown in [codemirror-mode-meta-patch][]. Yes, this
is pretty painful, sorry about that.

The Pygments lexer for Maxima is maxima_lexer.py. To install it, find the
Pygments installation directory, copy [maxima_lexer.py][] to that directory, and
update `lexers/_mapping.py` as shown in [pygments-mapping-patch][]. Yes, this is
pretty painful too.

### Running Maxima-Jupyter

Maxima-Jupyter may be run from a local installation in console mode by the following.

```sh
jupyter console --kernel=maxima
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

Maxima-Jupyter may be run as a Docker image managed by repo2docker which will
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
jupyter-repo2docker --user-id=1000 --user-name=mj https://github.com/robert-dodier/maxima-jupyter
```

## Docker Image

A Docker image of Maxima-Jupyter may be built using the following command
(`sudo` may be required). This image is based on the docker image
`base/archlinux`.

```sh
docker build --tag=maxima-jupyter .
```

After the image is built the console may be run with

```sh
docker run -it maxima-jupyter jupyter console --kernel=maxima
```

----

Have fun and keep me posted. Feel free to send pull requests, comments, etc.

Robert Dodier
robert.dodier@gmail.com
robert-dodier @ github

<!--refs-->

[Arch Wiki]: https://wiki.archlinux.org/index.php/Arch_User_Repository#Installing_packages
[Bordeaux Threads project description]: https://common-lisp.net/project/bordeaux-threads/
[bug report]: https://github.com/jupyter/notebook/issues/1962
[codemirror-mode-meta-patch]: https://github.com/robert-dodier/maxima-jupyter/blob/master/codemirror-mode-meta-patch
[make-maxima-jupyter-recipe.txt]: https://github.com/robert-dodier/maxima-jupyter/blob/master/make-maxima-jupyter-recipe.txt
[maxima_lexer.py]: https://github.com/robert-dodier/maxima-jupyter/blob/master/maxima_lexer.py
[maxima-jupyter-git]: https://aur.archlinux.org/packages/maxima-jupyter-git/
[maxima.js]: https://github.com/robert-dodier/maxima-jupyter/blob/master/maxima.js
[MaximaJupyterExample.ipynb]: http://nbviewer.jupyter.org/github/robert-dodier/maxima-jupyter/blob/master/examples/MaximaJupyterExample.ipynb
[MaximaJupyterTalk.ipynb]: http://nbviewer.jupyter.org/github/robert-dodier/maxima-jupyter/blob/master/examples/MaximaJupyterTalk.ipynb
[mybinder-badge]: https://mybinder.org/badge_logo.svg
[mybinder]: https://mybinder.org/v2/gh/robert-dodier/maxima-jupyter/master
[nbviewer]: http://nbviewer.jupyter.org
[Plots.ipynb]: http://nbviewer.jupyter.org/github/robert-dodier/maxima-jupyter/blob/master/examples/Plots.ipynb
[pygments-mapping-patch]: https://github.com/robert-dodier/maxima-jupyter/blob/master/pygments-mapping-patch
[Quicklisp]: http://www.quicklisp.org
[repo2docker]: https://repo2docker.readthedocs.io/en/latest/
