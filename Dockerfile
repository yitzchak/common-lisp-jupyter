FROM archlinux/base:latest

RUN pacman -Syu --noconfirm base-devel jupyter git readline

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

WORKDIR ${HOME}/common-lisp-jupyter

COPY . ${HOME}/common-lisp-jupyter
RUN chown -R ${NB_UID} ${HOME} && chgrp -R ${NB_USER} ${HOME}

USER ${NB_USER}

RUN ros install sbcl-bin
RUN ros install ./common-lisp-jupyter.asd; exit 0
RUN ros install ./common-lisp-jupyter.asd

WORKDIR ${HOME}/common-lisp-jupyter/examples
