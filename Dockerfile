FROM archlinux/base:latest

ARG NB_USER=app
ARG NB_UID=1000

ENV USER ${NB_USER}
ENV HOME /home/${NB_USER}
ENV PATH "${HOME}/.roswell/bin:${PATH}"

RUN echo "[multilib]" >> /etc/pacman.conf
RUN echo "Include = /etc/pacman.d/mirrorlist" >> /etc/pacman.conf

RUN pacman -Syu --noconfirm --needed base-devel git jre8-openjdk jupyter-notebook jupyterlab lib32-zeromq \
  maven readline \
  python-async_generator \
  python-oauthlib \
  python-alembic \
  python-sqlalchemy \
  python-requests \
  bower \
  npm \
  python-pyopenssl \
  python-pypandoc 
  
RUN useradd --create-home --shell=/bin/false --uid=${NB_UID} ${NB_USER}

WORKDIR ${HOME}

USER ${NB_USER}
RUN git clone https://aur.archlinux.org/roswell.git
RUN git clone https://aur.archlinux.org/jupyterhub-git.git
RUN git clone https://aur.archlinux.org/nodejs-configurable-http-proxy.git
RUN git clone https://aur.archlinux.org/python-certipy.git
RUN git clone https://aur.archlinux.org/python-pamela.git
WORKDIR ${HOME}/roswell
RUN makepkg
WORKDIR ${HOME}/nodejs-configurable-http-proxy
RUN makepkg
WORKDIR ${HOME}/python-certipy
RUN makepkg
WORKDIR ${HOME}/python-pamela
RUN makepkg

USER root
WORKDIR ${HOME}/roswell
RUN ls -t *.pkg.tar.xz | xargs pacman -U --noconfirm
WORKDIR ${HOME}/nodejs-configurable-http-proxy
RUN ls -t *.pkg.tar.xz | xargs pacman -U --noconfirm
WORKDIR ${HOME}/python-certipy
RUN ls -t *.pkg.tar.xz | xargs pacman -U --noconfirm
WORKDIR ${HOME}/python-pamela
RUN ls -t *.pkg.tar.xz | xargs pacman -U --noconfirm

USER ${NB_USER}
WORKDIR ${HOME}/jupyterhub-git
RUN makepkg

USER root
WORKDIR ${HOME}/jupyterhub-git
RUN ls -t *.pkg.tar.xz | xargs pacman -U --noconfirm
RUN jupyter lab build --dev-build=False
#     npm cache clean --force && \
#     rm -rf $CONDA_DIR/share/jupyter/lab/staging && \
#     rm -rf /home/$NB_USER/.cache/yarn && \
#     rm -rf /home/$NB_USER/.node-gyp && \
#     fix-permissions $CONDA_DIR && \
#     fix-permissions /home/$NB_USER


WORKDIR ${HOME}/common-lisp-jupyter

COPY . ${HOME}/common-lisp-jupyter
RUN chown -R ${NB_UID} ${HOME} && chgrp -R ${NB_USER} ${HOME}

USER ${NB_USER}

RUN ros install sbcl-bin
RUN ros install ./common-lisp-jupyter.asd; exit 0
RUN ros install ./common-lisp-jupyter.asd
RUN echo quit | jupyter-console --no-confirm-exit --kernel=common-lisp \
  --ZMQTerminalInteractiveShell.kernel_timeout=240

RUN ros install abcl-bin
RUN ros run --lisp abcl-bin --eval "(ql:quickload :common-lisp-jupyter)" \
  --eval "(cl-jupyter:install-roswell :implementation \"abcl-bin\")" --quit
RUN echo quit | jupyter-console --no-confirm-exit --kernel=common-lisp_abcl-bin \
  --ZMQTerminalInteractiveShell.kernel_timeout=240

RUN ros install ccl-bin
RUN ros run --lisp ccl-bin --eval "(ql:quickload :common-lisp-jupyter)" \
  --eval "(cl-jupyter:install-roswell :implementation \"ccl-bin\")" --quit
RUN echo quit | jupyter-console --no-confirm-exit --kernel=common-lisp_ccl-bin \
  --ZMQTerminalInteractiveShell.kernel_timeout=240

RUN ros install cmu-bin
RUN ros run --lisp cmu-bin --eval "(ql:quickload :common-lisp-jupyter)" \
  --eval "(cl-jupyter:install-roswell :implementation \"cmu-bin\")" --quit
RUN echo quit | jupyter-console --no-confirm-exit --kernel=common-lisp_cmu-bin \
  --ZMQTerminalInteractiveShell.kernel_timeout=240

RUN ros use sbcl-bin

WORKDIR ${HOME}/common-lisp-jupyter/examples
