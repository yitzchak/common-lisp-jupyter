(ql:quickload '(:tanstaafl :common-lisp-jupyter))

(let ((tanstaafl:*documentation-in-markdown* t)
      (tanstaafl::*packages* '(:jupyter :jupyter/common-lisp :jupyter/convert :jupyter/widgets)))
  (dolist (pkg tanstaafl::*packages*)
    (tanstaafl:generate pkg "docs/")))
