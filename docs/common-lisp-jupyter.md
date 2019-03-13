# Package common-lisp-jupyter


## Class `kernel`

### Superclasses

- `kernel`
- `jupyter:kernel`
- `standard-object`

### Initial Arguments

- `:name`
- `:version`
- `:banner`
- `:language-name`
- `:language-version`
- `:mime-type`
- `:file-extension`
- `:pygments-lexer`
- `:codemirror-mode`
- `:help-links`
- `:package`
- `:transport`
- `:ip`
- `:shell-port`
- `:stdin-port`
- `:iopub-port`
- `:control-port`
- `:hb-port`
- `:signature-scheme`
- `:key`
- `:prompt-prefix`
- `:prompt-suffix`
- `:input-queue`


## Function `install`

Install Common Lisp kernel based on implementation

### Definition

```common-lisp
(install &key bin-path ev-flag preamble)
```


## Function `install-image`

Install Common Lisp kernel based on image of current implementation

### Definition

```common-lisp
(install-image)
```
