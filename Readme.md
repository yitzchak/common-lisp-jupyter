Maxima-Jupyter
==========

An enhanced interactive environment for the computer algebra system Maxima,
based on CL-Jupyter, a Jupyter kernel for Common Lisp, by Frederic Peschanski.
Thanks, Frederic!

## Requirements ##

To try Maxima-Jupyter you need :

 - a Maxima executable

   - built with a Common Lisp implementation which has native threads

     - Clozure CL works for sure; SBCL should work (incompletely tested)

     - Other implementations may be possible

   - You don't need to build Maxima! See `make-maxima-jupyter-recipe.txt`
     about creating the Maxima-Jupyter executable.

 - Quicklisp (see: http://www.quicklisp.org)

   - When you load Maxima-Jupyter into Maxima for the first time,
     Quicklisp will download some dependencies automatically.
     Good luck.

 - Python 3.2 or above

 - Jupyter, or IPython 3.x


## Quick install ##

I installed Jupyter via:

     python3 -m pip install jupyter

There are two kernel installation techniques. The first is to create a saved
image as detailed in `make-maxima-jupyter-recipe.txt`. Once this image has been
created then the installation script can be used with:

```sh
python3 ./install-maxima-jupyter.py --exec=path/to/maxima-jupyter
```

Adding the option `--user` will install a user kernel instead of a system
kernel.

The second installation method will run the kernel from an interactive Maxima
session. The advantange to this technique is that the normal initialization
behavior of Maxima, such as loading `maxima-init.mac` from the current directory
will be preserved. After the files in `src` have been copied to an appropriate
location such as `/usr/share/maxima-jupyter` for a system installation or
`~/maxima-jupyter` for a user installation then the installation script called:

```sh
python3 ./install-maxima-jupyter.py --src=path/to/maxima-jupyter-src
```

The option `--maxima` may also be used to specify the location of the Maxima
executable. Please note that in order for this method to work quicklisp needs be
loaded by default in every Maxima session. See quicklisp documentation for
details.

## Installation on Arch/Manjaro

The package for Arch Linux is
[maxima-jupyter-git](https://aur.archlinux.org/packages/maxima-jupyter-git/).
Building and installing (including dependencies) can be accomplished with:

    yaourt -Sy maxima-jupyter-git

Alternatively use ``makepkg``:

    curl -L -O https://aur.archlinux.org/cgit/aur.git/snapshot/maxima-jupyter-git.tar.gz
    tar -xvf maxima-jupyter-git.tar.gz
    cd maxima-jupyter-git
    makepkg -Csri

Please consult the
[Arch Wiki](https://wiki.archlinux.org/index.php/Arch_User_Repository#Installing_packages)
for more information regarding installing packages from the AUR.

## Code highlighting installation ##

Highlighting Maxima code is handled by CodeMirror in the notebook
and Pygments in HTML export.

The CodeMirror mode for Maxima is maxima.js. To install it,
find the CodeMirror mode installation directory, create a directory named "maxima" there,
copy maxima.js to the maxima directory, and update codemirror/mode/meta.js
as shown in codemirror-mode-meta-patch.
Yes, this is pretty painful, sorry about that.

The Pygments lexer for Maxima is maxima_lexer.py.
To install it, find the Pygments installation directory,
copy maxima_lexer.py to that directory,
and update lexers/_mapping.py as shown in pygments-mapping-patch.
Yes, this is pretty painful too.

## Running Maxima-Jupyter

### Console mode

    jupyter console --kernel=maxima

When you enter stuff to be evaluated, you omit the usual trailing
semicolon or dollar sign:

```
In [1]: 2*21
Out[1]: 42

In [2]:
```

### Notebooks

I created this project in order to combine Maxima with the IPython notebook
(with the goal of using the notebook to create blog posts containing text,
formulas, and plots). To execute the notebook server:

    jupyter notebook

The file [MaximaJupyterExample.ipynb](http://nbviewer.ipython.org/github/robert-dodier/maxima-jupyter/blob/master/MaximaJupyterExample.ipynb) is an example of a Maxima-Jupyter notebook.

[MaximaJupyterTalk.ipynb](http://nbviewer.ipython.org/github/robert-dodier/maxima-jupyter/blob/master/MaximaJupyterTalk.ipynb) are my notes for a talk given to the Portland Python User Group.

Note that the Github notebook renderer is currently (August 2015) broken ([bug report here](https://github.com/jupyter/nbviewer/issues/452)); it renders all math formulas in a tiny font.

----

Have fun and keep me posted. Feel free to send pull requests, comments, etc.

Robert Dodier
robert.dodier@gmail.com
robert-dodier @ github
