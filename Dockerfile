FROM ghcr.io/yitzchak/archlinux-cl:latest

RUN sudo pacman -Syu --noconfirm jupyterlab jupyter-console jupyterlab-widgets

COPY . /home/root/quicklisp/local-projects/common-lisp-jupyter

RUN git clone https://github.com/yitzchak/delta-vega.git ~/quicklisp/local-projects/delta-vega; \
    git clone https://github.com/yitzchak/resizable-box-clj.git ~/quicklisp/local-projects/resizable-box-clj; \
    git clone https://github.com/yitzchak/ngl-clj.git ~/quicklisp/local-projects/ngl-clj; \
    sbcl --non-interactive \
      --eval "(ql:quickload '(:common-lisp-jupyter :cytoscape-clj :kekule-clj :resizable-box-clj :ngl-clj :delta-vega))" \
      --eval "(clj:install :implementation t)"; \
    abcl --batch \
      --eval "(ql:quickload '(:common-lisp-jupyter :cytoscape-clj :kekule-clj :resizable-box-clj :ngl-clj :delta-vega))" \
      --eval "(clj:install :implementation t)"; \
    ccl \
      --eval "(ql:quickload '(:common-lisp-jupyter :cytoscape-clj :kekule-clj :resizable-box-clj :ngl-clj :delta-vega))" \
      --eval "(clj:install :implementation t)"; \
      --eval "(uiop:quit)"; \
    clasp --non-interactive \
      --eval "(ql:quickload '(:common-lisp-jupyter :cytoscape-clj :kekule-clj :resizable-box-clj :ngl-clj :delta-vega))" \
      --eval "(clj:install :implementation t)"; \
    ecl \
      --eval "(ql:quickload '(:common-lisp-jupyter :cytoscape-clj :kekule-clj :resizable-box-clj :ngl-clj :delta-vega))" \
      --eval "(clj:install :implementation t)" \
      --eval "(uiop:quit)"
