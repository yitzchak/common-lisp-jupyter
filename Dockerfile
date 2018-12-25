FROM base/archlinux:2018.12.01

RUN pacman -Sy --noconfirm base-devel jupyter gnuplot maxima

ARG NB_USER=mj
ARG NB_UID=1000

ENV USER ${NB_USER}
ENV HOME /home/${NB_USER}

RUN useradd --create-home --shell=/bin/false --uid=${NB_UID} ${NB_USER}

WORKDIR ${HOME}/maxima-jupyter

COPY . ${HOME}/maxima-jupyter
COPY maxima.js /usr/lib/python3.7/site-packages/notebook/static/components/codemirror/mode/maxima
COPY maxima_lexer.py /usr/lib/python3.7/site-packages/pygments/lexers
RUN patch /usr/lib/python3.7/site-packages/notebook/static/components/codemirror/mode/meta.js codemirror-mode-meta-patch
RUN patch /usr/lib/python3.7/site-packages/pygments/lexers/_mapping.py pygments-mapping-patch
RUN chown -R ${NB_UID} ${HOME} && chgrp -R ${NB_USER} ${HOME}

USER ${NB_USER}

RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --load docker-install-quicklisp.lisp
RUN python install-maxima-jupyter.py --user --root=`pwd`
RUN echo quit | jupyter-console --no-confirm-exit --kernel=maxima --ZMQTerminalInteractiveShell.kernel_timeout=240

WORKDIR ${HOME}/maxima-jupyter/examples
