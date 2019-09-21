FROM archlinux/base:latest

ARG NB_USER=app
ARG NB_UID=1000

ENV USER ${NB_USER}
ENV HOME /home/${NB_USER}
ENV PATH "${HOME}/.roswell/bin:${HOME}/.local/bin:${PATH}"

RUN echo -e "[multilib]\nInclude = /etc/pacman.d/mirrorlist" >> /etc/pacman.conf

RUN pacman -Syu --noconfirm --needed base-devel git jre8-openjdk lib32-zeromq \
  maven npm readline python-pip; \
  useradd --create-home --shell=/bin/false --uid=${NB_UID} ${NB_USER}

WORKDIR ${HOME}

USER ${NB_USER}
RUN git clone https://aur.archlinux.org/roswell.git && \
  cd roswell && makepkg

USER root
RUN ls -t roswell/*.pkg.tar.xz | xargs pacman -U --noconfirm

USER ${NB_USER}
RUN rm -rf roswell; ros install sbcl-bin; ros install abcl-bin; \
  ros install ccl-bin; ros install cmu-bin; ros use sbcl-bin; \
  pip install --user jupyter jupyterlab; \
  jupyter serverextension enable --user --py jupyterlab; \
  jupyter labextension install @jupyter-widgets/jupyterlab-manager; \
  jupyter nbextension enable --user --py widgetsnbextension

USER root
COPY . ${HOME}/common-lisp-jupyter
RUN chown -R ${NB_UID} common-lisp-jupyter && \
  chgrp -R ${NB_USER} common-lisp-jupyter

USER ${NB_USER}
RUN cd common-lisp-jupyter; ros install ./common-lisp-jupyter.asd; exit 0
RUN cd common-lisp-jupyter; ros install ./common-lisp-jupyter.asd && \
  ros run --lisp abcl-bin --eval "(ql:quickload :common-lisp-jupyter)" \
  --eval "(cl-jupyter:install-roswell :implementation \"abcl-bin\")" --quit && \
  ros run --lisp ccl-bin --eval "(ql:quickload :common-lisp-jupyter)" \
  --eval "(cl-jupyter:install-roswell :implementation \"ccl-bin\")" --quit && \
  ros run --lisp cmu-bin --eval "(ql:quickload :common-lisp-jupyter)" \
  --eval "(cl-jupyter:install-roswell :implementation \"cmu-bin\")" --quit

WORKDIR ${HOME}/common-lisp-jupyter/examples
