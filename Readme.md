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

   - You don't need to build Maxima! See notes below about creating
     the Maxima-Jupyter executable.

 - Quicklisp (see: http://www.quicklisp.org)

   - When you load CL-Jupyter into Maxima for the first time,
     Quicklisp will download some dependencies automatically.
     Good luck.

 - Python 3.2 or above

 - Jupyter, or IPython 3.x
 
   - I installed Jupyter via:

     python3 -m pip install jupyter


## Quick install ##

The installation script creates a kernel description so Jupyter knows how to execute the kernel:

    python3 ./install-maxima-jupyter.py --maxima-jupyter-exec=path/to/maxima-jupyter

The executable named by `--maxima-jupyter-exec` is an executable
saved Lisp image. The saved image needs to contain Maxima and CL-Jupyter.
I create these images by executing Maxima and loading CL-Jupyter into
Maxima, and then saving an image. See `make-maxima-jupyter-recipe.txt`.

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

    jupyter console --Session.key="b''" --kernel=maxima

**Remark**: the `--Session.key="b''"` option is for the moment required because Maxima-Jupyter
does not yet support message encryption.

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

    jupyter notebook --Session.key="b''"

The file [MaximaJupyterExample.ipynb](http://nbviewer.ipython.org/github/robert-dodier/maxima-jupyter/blob/master/MaximaJupyterExample.ipynb) is an example of a Maxima-Jupyter notebook.

Note that the Github notebook renderer is currently (August 2015) broken ([bug report here](https://github.com/jupyter/nbviewer/issues/452)); it renders all math formulas in a tiny font.

----

Have fun and keep me posted. Feel free to send pull requests, comments, etc.

Robert Dodier
robert.dodier@gmail.com
robert-dodier @ github
