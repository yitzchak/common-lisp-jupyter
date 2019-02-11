FROM archlinux/base:latest

RUN pacman -Sy --noconfirm base-devel jupyter git readline

ARG NB_USER=jupyter
ARG NB_UID=1000

ENV USER ${NB_USER}
ENV HOME /home/${NB_USER}
ENV PATH "${HOME}/.roswell/bin:${PATH}"

RUN useradd --create-home --shell=/bin/false --uid=${NB_UID} ${NB_USER}

WORKDIR ${HOME}

USER ${NB_USER}
RUN git clone https://aur.archlinux.org/roswell.git
WORKDIR ${HOME}/roswell
RUN makepkg

USER root
RUN pacman -U --noconfirm roswell-19.1.10.96-1-x86_64.pkg.tar.xz

WORKDIR ${HOME}/cl-jupyter

COPY . ${HOME}/cl-jupyter
RUN chown -R ${NB_UID} ${HOME} && chgrp -R ${NB_USER} ${HOME}

USER ${NB_USER}

RUN ros install sbcl-bin
RUN ros install ./cl-jupyter.asd; exit 0
RUN ros install ./cl-jupyter.asd
# RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
# RUN sbcl --load quicklisp.lisp --load docker-install-quicklisp.lisp
# RUN python install-cl-jupyter.py --user --root=`pwd`
# RUN echo quit | jupyter-console --no-confirm-exit --kernel=cl --ZMQTerminalInteractiveShell.kernel_timeout=240

WORKDIR ${HOME}/cl-jupyter/examples
