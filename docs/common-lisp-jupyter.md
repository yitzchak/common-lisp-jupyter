# Package common-lisp-jupyter


## *Function* `install`

### Syntax

```common-lisp
(install &key bin-path use-implementation system local prefix)
```

### Description

Install Common Lisp kernel based on the current implementation.
- `bin-path` specifies path to LISP binary.
- `use-implementation` toggles including implementation details in kernel name.
- `system` toggles system versus user installation.
- `local` toggles `/usr/local/share versus` `/usr/share` for system installations.
- `prefix` key specifies directory prefix for packaging.


## *Function* `install-image`

### Syntax

```common-lisp
(install-image &key use-implementation prefix)
```

### Description

Install Common Lisp kernel based on image of current implementation.
- `use-implementation` toggles including implementation details in kernel name.
- `prefix` key specifies directory prefix for packaging.


## *Function* `install-roswell`

### Syntax

```common-lisp
(install-roswell &key implementation)
```

### Description

Install Common Lisp kernel using Roswell. `implementation` key toggles
including implementation details in kernel name.


## *Class* `kernel`

### Superclasses

- `kernel`
- `jupyter:kernel`
- `standard-object`

### Initial Arguments

- `:sink`
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
- `:connection-file`
- `:prompt-prefix`
- `:prompt-suffix`
- `:input-queue`
