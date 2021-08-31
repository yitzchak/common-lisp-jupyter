
# JUPYTER/COMMON\-LISP

Provides Common Lisp kernel support.

## Nicknames

common\-lisp\-jupyter, clj, cl\-jupyter

## Exports

install, install\-image, install\-roswell, 

## install

## Function

Install Common Lisp kernel based on the current implementation.
- `bin-path` specifies path to LISP binary.
- `use-implementation` toggles including implementation details in kernel name.
- `system` toggles system versus user installation.
- `bundle` creates a quicklisp bundle for system installations.
- `local` toggles `/usr/local/share versus` `/usr/share` for system installations.
- `prefix` key specifies directory prefix for packaging.
- `root` key specifies the root under which the Jupyter folder is found. Is automatically determined if not provided.

```lisp
(install &key bin-path use-implementation system bundle local prefix root)
```

## install\-image

## Function

Install Common Lisp kernel based on image of current implementation.
- `use-implementation` toggles including implementation details in kernel name.
- `prefix` key specifies directory prefix for packaging.
- `root` key specifies the root under which the Jupyter folder is found. Is automatically determined if not provided.

```lisp
(install-image &key use-implementation prefix root)
```

## install\-roswell

## Function

Install Common Lisp kernel using Roswell. `implementation` key toggles
including implementation details in kernel name.

```lisp
(install-roswell &key implementation)
```

## kernel

#### Class

##### Precedence List

kernel, kernel, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter::name &mdash; Kernel name. Used as a unique identifier in kernel description.
    - :initarg :name
- jupyter::version &mdash; Kernel version.
    - :initarg :version
- jupyter::banner &mdash; Banner text used to describe kernel. Used in kernel_info_reply messages.
    - :initarg :banner
- jupyter::language\-name &mdash; Display name of implementation language. Used in kernel_info_reply messages.
    - :initarg :language\-name
- jupyter::language\-version &mdash; Version of implementation language. Used in kernel_info_reply messages.
    - :initarg :language\-version
- jupyter::mime\-type &mdash; Default MIME type for source files. Used in kernel_info_reply messages.
    - :initarg :mime\-type
- jupyter::file\-extension &mdash; Default file extension for source files. Used in kernel_info_reply messages.
    - :initarg :file\-extension
- jupyter::pygments\-lexer &mdash; Name of Pygments lexer for source files. Used in kernel_info_reply messages.
    - :initarg :pygments\-lexer
- jupyter::codemirror\-mode &mdash; CodeMirror mode for source files. Used in kernel_info_reply messages.
    - :initarg :codemirror\-mode
- jupyter::help\-links &mdash; An association list of help links. The car is the description and the cdr is
       URL. Used in kernel_info_reply messages.
    - :initarg :help\-links
- package &mdash; The package in which evaluate-code, code-is-complete and others are called.
    - :initarg :package
- readtable &mdash; The readtable used bu evaluate-code, code-is-complete and others.
    - :initarg :readtable
- jupyter::connection\-file &mdash; Pathname of connection file.
    - :initarg :connection\-file
- jupyter::transport \[string\] &mdash; Transport protocol from connection file.
- jupyter::ip \[string\] &mdash; IP address from connection file.
- jupyter::shell\-port \[fixnum\] &mdash; SHELL port from connection file.
- jupyter::stdin\-port \[fixnum\] &mdash; STDIN port from connection file.
- jupyter::iopub\-port \[fixnum\] &mdash; IOPUB port from connection file.
- jupyter::control\-port \[fixnum\] &mdash; CONTROL port from connection file.
- jupyter::hb\-port \[fixnum\] &mdash; HB port from connection file.
- jupyter::signature\-scheme \[string\] &mdash; Signature scheme from connection file.
- jupyter::key &mdash; Signing key from connection file.
- jupyter::prompt\-prefix &mdash; String prefix using in *standard-output* to indicate the start of prompt.
    - :initarg :prompt\-prefix
- jupyter::prompt\-suffix &mdash; String suffix using in *standard-output* to indicate the end of prompt.
    - :initarg :prompt\-suffix
- jupyter::ctx &mdash; pzmq ctx handle.
- jupyter::mac &mdash; Message authentification.
- jupyter::hb &mdash; Heartbeat channel.
- jupyter::shell &mdash; SHELL channel.
- jupyter::stdin &mdash; STDIN channel.
- jupyter::control &mdash; CONTROL channel.
- jupyter::iopub &mdash; IOPUB channel.
- jupyter::session &mdash; Session identifier.
- jupyter::input\-queue &mdash; Input queue used to feed values into execute_result payloads.
    - :initarg :input\-queue
- jupyter::history &mdash; Kernel history manager.
- jupyter::execution\-count &mdash; Kernel execution count.
- jupyter::comms &mdash; Currently open comms.
- jupyter::control\-thread &mdash; Control thread
    - :initarg :control\-thread
- jupyter::shell\-thread &mdash; Shell thread
- jupyter::html\-output &mdash; HTML display output stream
- jupyter::markdown\-output &mdash; Markdown display output stream
- jupyter::error\-output &mdash; Error output stream
- jupyter::standard\-output &mdash; Standard output stream
- jupyter::standard\-input &mdash; Standard input stream
- jupyter::tmp\-file\-prefix &mdash; Prefix for temporary debugger files
- jupyter::tmp\-file\-suffix &mdash; Suffix for temporary debugger files
- jupyter::hash\-seed &mdash; Hash seed for temporary debugger files
- jupyter::breakpoints &mdash; Currently set breakpoints.
- jupyter::debugger &mdash; Whether the debugger is supported
    - :initarg :debugger
- jupyter::debugger\-started &mdash; Whether the debugger has been started
- jupyter::threads
- environment

##### Methods

- jupyter:inspect\-code

    ```lisp
    (inspect-code (k kernel) code cursor-pos detail-level)
    ```


- jupyter:code\-is\-complete

    ```lisp
    (code-is-complete (k kernel) code)
    ```


- jupyter:complete\-code

    ```lisp
    (complete-code (k kernel) match-set code cursor-pos)
    ```


- jupyter:evaluate\-code

    ```lisp
    (evaluate-code (k kernel) code &optional source-path breakpoints)
    ```


- jupyter:debug\-activate\-breakpoints

    ```lisp
    (debug-activate-breakpoints (kernel kernel) source breakpoints)
    ```


- jupyter:debug\-remove\-breakpoint

    ```lisp
    (debug-remove-breakpoint (kernel kernel) source breakpoint)
    ```


- jupyter:debug\-new\-breakpoint

    ```lisp
    (debug-new-breakpoint (kernel kernel) source line)
    ```


- jupyter:debug\-initialize

    ```lisp
    (debug-initialize (k kernel))
    ```


- jupyter:debug\-next

    ```lisp
    (debug-next (k kernel) environment)
    ```


- jupyter:debug\-out

    ```lisp
    (debug-out (k kernel) environment)
    ```


- jupyter:debug\-in

    ```lisp
    (debug-in (k kernel) environment)
    ```


- jupyter:debug\-continue

    ```lisp
    (debug-continue (k kernel) environment &optional restart-number)
    ```


- jupyter:debug\-abort

    ```lisp
    (debug-abort (k kernel) environment)
    ```


- jupyter:debug\-inspect\-variables

    ```lisp
    (debug-inspect-variables (kernel kernel) environment)
    ```


- jupyter:debug\-evaluate

    ```lisp
    (debug-evaluate (kernel kernel) environment code frame)
    ```


- jupyter:start

    ```lisp
    (start (k kernel))
    ```


- kernel\-environment &mdash; automatically generated reader method

    ```lisp
    (kernel-environment (kernel kernel))
    ```


- jupyter:stop

    ```lisp
    (stop (k kernel))
    ```


- jupyter:start

    ```lisp
    (start (k kernel))
    ```


- jupyter:kernel\-debugger\-started &mdash; Whether the debugger has been started

    ```lisp
    (setf (kernel-debugger-started (kernel kernel)) new-value)
    ```


- jupyter:kernel\-debugger\-started &mdash; Whether the debugger has been started

    ```lisp
    (kernel-debugger-started (kernel kernel))
    ```


- jupyter:kernel\-prompt\-suffix &mdash; String suffix using in *standard-output* to indicate the end of prompt.

    ```lisp
    (kernel-prompt-suffix (kernel kernel))
    ```


- jupyter:kernel\-prompt\-prefix &mdash; String prefix using in *standard-output* to indicate the start of prompt.

    ```lisp
    (kernel-prompt-prefix (kernel kernel))
    ```

