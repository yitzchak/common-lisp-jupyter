FROM ubuntu:focal

SHELL ["/bin/bash", "-c"]

ARG NB_USER=jovyan
ARG NB_UID=1000

ENV CCL_VERSION "1.12.1-1"
ENV LLVM_VERSION "13.r5140.g972b6a3a3471-1"
ENV CLASP_VERSION "0.4.2.r4473.g7ba57bd76-1"
ENV DEBIAN_FRONTEND=noninteractive
ENV USER ${NB_USER}
ENV HOME /home/${NB_USER}
ENV PATH "${HOME}/.local/bin:${HOME}/miniconda/bin/:${PATH}"

RUN dpkg --add-architecture i386 && \
    apt-get update && \
    apt-get dist-upgrade -y && \
    apt-get install -y wget curl jq gpg git ssh sudo nano gettext locales sbcl \
                       ecl libzmq3-dev libzmq3-dev:i386 lsb-release && \
    echo 'en_US.UTF-8 UTF-8' >/etc/locale.gen && \
    sudo locale-gen

RUN wget https://github.com/yitzchak/mpr/releases/download/ccl_${CCL_VERSION}/ccl_${CCL_VERSION}_amd64_ubuntu_focal.deb && \
    wget https://github.com/yitzchak/mpr/releases/download/llvm13_${LLVM_VERSION}/llvm13_${LLVM_VERSION}_amd64_ubuntu_focal.deb && \
    wget https://github.com/yitzchak/mpr/releases/download/clasp-cl-git_${CLASP_VERSION}/clasp-cl-git_${CLASP_VERSION}_amd64_ubuntu_focal.deb && \
    apt-get install -y ./ccl_${CCL_VERSION}_amd64_ubuntu_focal.deb \
                       ./llvm13_${LLVM_VERSION}_amd64_ubuntu_focal.deb \
                       ./clasp-cl-git_${CLASP_VERSION}_amd64_ubuntu_focal.deb && \
    rm ccl_${CCL_VERSION}_amd64_ubuntu_focal.deb \
       llvm13_${LLVM_VERSION}_amd64_ubuntu_focal.deb \
       clasp-cl-git_${CLASP_VERSION}_amd64_ubuntu_focal.deb

RUN useradd --create-home --shell=/bin/false --uid=${NB_UID} ${NB_USER} && \
    usermod -aG sudo $NB_USER && \
    passwd -d $NB_USER

WORKDIR ${HOME}

USER ${NB_USER}

RUN wget https://repo.anaconda.com/miniconda/Miniconda3-py39_4.9.2-Linux-x86_64.sh && \
    bash Miniconda3-py39_4.9.2-Linux-x86_64.sh -b -p $HOME/miniconda && \
    ./miniconda/bin/conda init && \
    rm Miniconda3-py39_4.9.2-Linux-x86_64.sh

RUN conda install -c conda-forge nodejs jupyterlab jupyter_console

RUN jupyter serverextension enable --user --py jupyterlab; \
    jupyter labextension install @jupyter-widgets/jupyterlab-manager \
    	jupyterlab-edit-magic cytoscape-clj kekule-clj resizable-box-clj \
    	ngl-clj jupyterlab-debugger-restarts; \
    jupyter nbextension enable --user --py widgetsnbextension; \
    jupyter lab build; \
    curl -kLO https://beta.quicklisp.org/quicklisp.lisp; \
    sbcl --non-interactive --load quicklisp.lisp \
      --eval "(quicklisp-quickstart:install)" \
      --eval "(ql-util:without-prompting (ql:add-to-init-file))"; \
    ccl --load quicklisp/setup.lisp \
      --eval "(ql-util:without-prompting (ql:add-to-init-file))" \
      --eval "(uiop:quit)"; \
    iclasp-boehmprecise --non-interactive --load quicklisp/setup.lisp \
      --eval "(ql-util:without-prompting (ql:add-to-init-file))"; \
    ecl --load quicklisp/setup.lisp \
      --eval "(ql-util:without-prompting (ql:add-to-init-file))" \
      --eval "(uiop:quit)"

USER root
COPY . ${HOME}/quicklisp/local-projects/common-lisp-jupyter
RUN chown -R ${NB_UID} quicklisp/local-projects/common-lisp-jupyter && \
    chgrp -R ${NB_USER} quicklisp/local-projects/common-lisp-jupyter

USER ${NB_USER}
RUN git clone https://github.com/yitzchak/shasht.git ~/quicklisp/local-projects/shasht; \
    git clone https://github.com/yitzchak/cytoscape-clj.git ~/quicklisp/local-projects/cytoscape-clj; \
    mv ~/quicklisp/local-projects/common-lisp-jupyter/examples ~/lab; \
    mv ~/quicklisp/local-projects/cytoscape-clj/examples ~/lab/cytoscape-clj; \
    git clone https://github.com/yitzchak/kekule-clj.git ~/quicklisp/local-projects/kekule-clj; \
    mv ~/quicklisp/local-projects/kekule-clj/examples ~/lab/kekule-clj; \
    git clone https://github.com/yitzchak/resizable-box-clj.git ~/quicklisp/local-projects/resizable-box-clj; \
    git clone https://github.com/yitzchak/ngl-clj.git ~/quicklisp/local-projects/ngl-clj; \
    mv ~/quicklisp/local-projects/ngl-clj/examples ~/lab/ngl-clj; \
    sbcl --non-interactive \
      --eval "(ql:quickload '(:common-lisp-jupyter :cytoscape-clj :kekule-clj :resizable-box-clj :ngl-clj))" \
      --eval "(clj:install :use-implementation t)"; \
    ccl \
      --eval "(ql:quickload '(:common-lisp-jupyter :cytoscape-clj :kekule-clj :resizable-box-clj :ngl-clj))" \
      --eval "(clj:install :use-implementation t)" \
      --eval "(uiop:quit)"; \
    iclasp-boehmprecise --non-interactive \
      --eval "(ql:quickload :common-lisp-jupyter)" \
      --eval "(clj:install :use-implementation t)"; \
    iclasp-boehmprecise --non-interactive \
      --eval "(ql:quickload '(:cytoscape-clj :kekule-clj :resizable-box-clj :ngl-clj))"; \
    ecl \
      --eval "(ql:quickload '(:common-lisp-jupyter :cytoscape-clj :kekule-clj :resizable-box-clj :ngl-clj))" \
      --eval "(clj:install :use-implementation t)" \
      --eval "(uiop:quit)"

WORKDIR ${HOME}/lab

