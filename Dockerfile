FROM archlinux:latest

ARG NB_USER=app
ARG NB_UID=1000

ENV USER ${NB_USER}
ENV HOME /home/${NB_USER}
ENV JUPYTER_PATH=${HOME}/.local/share/jupyter/
ENV JUPYTERLAB_DIR=${HOME}/.local/share/jupyter/lab/
ENV PATH "${HOME}/.local/bin:${PATH}"

RUN echo -e "[multilib]\nInclude = /etc/pacman.d/mirrorlist" >> /etc/pacman.conf

RUN patched_glibc=glibc-linux4-2.33-4-x86_64.pkg.tar.zst && \
    curl -LO "https://repo.archlinuxcn.org/x86_64/$patched_glibc" && \
    bsdtar -C / -xvf "$patched_glibc"

RUN pacman -Syu --noconfirm --needed base-devel jre8-openjdk lib32-zeromq git \
      maven npm readline jupyterlab jupyter_console sbcl ecl; \
    useradd --create-home --shell=/bin/false --uid=${NB_UID} ${NB_USER}

WORKDIR ${HOME}

USER ${NB_USER}

RUN jupyter serverextension enable --user --py jupyterlab; \
    jupyter labextension install @jupyter-widgets/jupyterlab-manager; \
    jupyter nbextension enable --user --py widgetsnbextension; \
    curl -kLO https://beta.quicklisp.org/quicklisp.lisp; \
    sbcl --non-interactive --load quicklisp.lisp \
      --eval "(quicklisp-quickstart:install)" \
      --eval "(ql-util:without-prompting (ql:add-to-init-file))"; \
    ecl --load quicklisp/setup.lisp \
      --eval "(ql-util:without-prompting (ql:add-to-init-file))" \
      --eval "(ext:quit)"

USER root
COPY . ${HOME}/quicklisp/local-projects/common-lisp-jupyter
RUN chown -R ${NB_UID} quicklisp/local-projects/common-lisp-jupyter && \
    chgrp -R ${NB_USER} quicklisp/local-projects/common-lisp-jupyter

USER ${NB_USER}
RUN sbcl --non-interactive --eval "(ql:quickload :common-lisp-jupyter)" \
      --eval "(clj:install :use-implementation t)"; \
    ecl --eval "(ql:quickload :common-lisp-jupyter)" \
      --eval "(clj:install :use-implementation t)" \
      --eval "(ext:quit)"

WORKDIR ${HOME}/quicklisp/local-projects/common-lisp-jupyter/examples
