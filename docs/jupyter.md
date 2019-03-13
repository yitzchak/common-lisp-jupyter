# Package jupyter


## Function `run-kernel`

Run a kernel based on a kernel class and a connection file.

### Definition

```common-lisp
(run-kernel kernel-class connection-file-name)
```


## Generic Function `render`

Render evaluation result as a mime bundle for execute_result
  or display_data.

### Definition

```common-lisp
(render result)
```


## Class `comm`

### Superclasses

- `comm`
- `standard-object`

### Initial Arguments

- `:id`
- `:target-name`
- `:kernel`


## Function `jpeg`

Create a JPEG image result based on an inline value.

### Definition

```common-lisp
(jpeg value &optional display-data)
```


## Variable `*page-output*`

Output stream sent to Jupyter pager. Available during calls to evaluate-code.

### Definition

```common-lisp
nil
```


## Function `send-comm-open`

### Definition

```common-lisp
(send-comm-open comm &optional data metadata)
```


## Function `ps-file`

Create a PostScript result based on a file path.

### Definition

```common-lisp
(ps-file path &optional display-data)
```


## Function `get-comm`

### Definition

```common-lisp
(get-comm id)
```


## Generic Function `on-comm-message`

### Definition

```common-lisp
(on-comm-message comm data metadata)
```


## Function `jpeg-file`

Create a JPEG image result based on a file path.

### Definition

```common-lisp
(jpeg-file path &optional display-data)
```


## Class `kernel`

Kernel state representation.

### Superclasses

- `kernel`
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

### Slots

- `name` &mdash; Kernel name. Used as a unique identifier in kernel
         description.
- `version` &mdash; Kernel version.
- `banner` &mdash; Banner text used to describe kernel. Used in
           kernel_info_reply messages.
- `language-name` &mdash; Display name of implementation language. Used
                  in kernel_info_reply messages.
- `language-version` &mdash; Version of implementation language. Used in
                     kernel_info_reply messages.
- `mime-type` &mdash; Default MIME type for source files. Used in
              kernel_info_reply messages.
- `file-extension` &mdash; Default file extension for source files. Used
                   in kernel_info_reply messages.
- `pygments-lexer` &mdash; Name of Pygments lexer for source files. Used
                   in kernel_info_reply messages.
- `codemirror-mode` &mdash; CodeMirror mode for source files. Used in
                    kernel_info_reply messages.
- `help-links` &mdash; An association list of help links. The car is the
               description and the cdr is URL. Used in kernel_info_reply
               messages.
- `package` &mdash; The package in which evaluate-code,
            code-is-complete and others are called.
- `transport` &mdash; Transport protocol from connection file.
- `ip` &mdash; IP address from connection file.
- `shell-port` &mdash; SHELL port from connection file.
- `stdin-port` &mdash; STDIN port from connection file.
- `iopub-port` &mdash; IOPUB port from connection file.
- `control-port` &mdash; CONTROL port from connection file.
- `hb-port` &mdash; HB port from connection file.
- `signature-scheme` &mdash; Signature scheme from connection file.
- `key` &mdash; Signing key from connection file.
- `prompt-prefix` &mdash; String prefix using in *standard-output* to
                  indicate the start of prompt.
- `prompt-suffix` &mdash; String suffix using in *standard-output* to
                  indicate the end of prompt.
- `ctx` &mdash; pzmq ctx handle.
- `hb` &mdash; Heartbeat channel.
- `shell` &mdash; SHELL channel.
- `stdin` &mdash; STDIN channel.
- `iopub` &mdash; IOPUB channel.
- `session` &mdash; Session identifier.
- `input-queue` &mdash; Input queue used to feed values into
                execute_result payloads.
- `history` &mdash; Kernel history manager.
- `execution-count` &mdash; Kernel execution count.
- `comms` &mdash; Currently open comms.


## Function `inline-result`

Create a result based on an inline value.

### Definition

```common-lisp
(inline-result value mime-type &optional display-data)
```

## Class `inline-result`

### Superclasses

- `inline-result`
- `result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:value`
- `:mime-type`


## Function `send-comm-message`

### Definition

```common-lisp
(send-comm-message comm &optional data metadata)
```


## Function `markdown`

Create a Markdown result based on an inline value.

### Definition

```common-lisp
(markdown value &optional display-data)
```


## Function `html`

Create a HTML result based on an inline value.

### Definition

```common-lisp
(html value &optional display-data)
```


## Function `info`

Display informational message regarding kernel status.

### Definition

```common-lisp
(info &rest args)
```


## Function `svg-file`

Create a SVG result based on a file path.

### Definition

```common-lisp
(svg-file path &optional display-data)
```


## Generic Function `on-comm-close`

### Definition

```common-lisp
(on-comm-close comm data metadata)
```


## Function `clear`

Send clear output message to frontend.

### Definition

```common-lisp
(clear &optional wait)
```


## Generic Function `kernel-prompt-suffix`

### Definition

```common-lisp
(kernel-prompt-suffix sb-pcl::object)
```


## Function `make-inline-result`

Make a result based on an inline value. The handle argument is used by the
  convenience functions to instantly process the result.

### Definition

```common-lisp
(make-inline-result value &key mime-type display-data handle)
```


## Function `latex`

Create a LaTeX result based on an inline value.

### Definition

```common-lisp
(latex value &optional display-data)
```


## Function `send-result`

Send a result either as display data or an execute result.

### Definition

```common-lisp
(send-result result)
```


## Class `quit-condition`

A condition for identifying a request for kernel shutdown.

### Superclasses

- `quit-condition`
- `error`
- `serious-condition`
- `condition`


## Generic Function `evaluate-code`

Evaluate code along with paged output. Kernel implementations
  must return a list of evaluated results. Each result should be wrapped with an
  appropriate `result` class instance. Sending the results to the client will be
  handled by the calling method.

### Definition

```common-lisp
(evaluate-code kernel code)
```


## Function `json-getf`

Safe accessor for the internal JSON format that behaves like getf

### Definition

```common-lisp
(json-getf object indicator &optional default)
```


## Function `quit-eval-error-p`

Predicate to determine if result is an quit result.

### Definition

```common-lisp
(quit-eval-error-p result)
```


## Generic Function `inspect-code`

Inspect code at cursor-pos with detail-level. Successful
  inspection should return a single wrapped result.

### Definition

```common-lisp
(inspect-code kernel code cursor-pos detail-level)
```


## Generic Function `complete-code`

Complete code at cursor-pos. Successful completion should
  return three values, first a list of strings, then the cursor start position
  and finally the cursor end position.

### Definition

```common-lisp
(complete-code kernel code cursor-pos)
```


## Function `png-file`

Create a PNG image result based on a file path.

### Definition

```common-lisp
(png-file path &optional display-data)
```


## Function `text`

Create a plain text result based on an inline value.

### Definition

```common-lisp
(text value &optional display-data)
```


## Function `file`

Create a result based on a file path. The mime type with automatically be
  determined from the file extension.

### Definition

```common-lisp
(file path &optional display-data)
```


## Function `enqueue-input`

Add code to input queue.

### Definition

```common-lisp
(enqueue-input kernel code)
```


## Generic Function `create-comm`

### Definition

```common-lisp
(create-comm target-name id data metadata)
```


## Function `pdf-file`

Create a PDF result based on a file path.

### Definition

```common-lisp
(pdf-file path &optional display-data)
```


## Function `make-error-result`

Make a result based on an error. The quit the parameter indicates that the
  kernel should exit. The handle argument is used by the convenience functions
  to instantly process the result.

### Definition

```common-lisp
(make-error-result ename evalue &key quit traceback)
```


## Function `gif-file`

Create a GIF image result based on a file path.

### Definition

```common-lisp
(gif-file path &optional display-data)
```


## Generic Function `code-is-complete`

Check code for completeness. Kernel implementations should
  result one of the permitted values of complete, incomplete, unknown or
  invalid.

### Definition

```common-lisp
(code-is-complete kernel code)
```


## Function `install-kernel`

Install a kernel spec file given a kernel name and a language name.

### Definition

```common-lisp
(install-kernel &key argv class name language resources)
```


## Function `svg`

Create a SVG result based on an inline value.

### Definition

```common-lisp
(svg value &optional display-data)
```


## Class `result`

Base class for encapsulation of evaluation result.

### Superclasses

- `result`
- `standard-object`

### Initial Arguments

- `:display-data`

### Slots

- `display-data` &mdash; Show result as display_data in client.


## Function `make-lisp-result`

Make a lisp result based on an inline value.

### Definition

```common-lisp
(make-lisp-result value &key display-data)
```


## Function `make-file-result`

Make a result based on a file. The handle argument is used by the convenience
  functions to instantly process the result.

### Definition

```common-lisp
(make-file-result path &key mime-type display-data handle)
```


## Macro `handling-errors`

Macro for catching any conditions including quit-conditions during code
  evaluation.

### Definition

```common-lisp
(handling-errors
  &body
  body)
```


## Generic Function `on-comm-open`

### Definition

```common-lisp
(on-comm-open comm data metadata)
```


## Generic Function `comm-id`

### Definition

```common-lisp
(comm-id sb-pcl::object)
```


## Generic Function `kernel-prompt-prefix`

### Definition

```common-lisp
(kernel-prompt-prefix sb-pcl::object)
```


## Function `png`

Create a PNG image result based on an inline value.

### Definition

```common-lisp
(png value &optional display-data)
```


## Function `send-comm-close`

### Definition

```common-lisp
(send-comm-close comm &optional data metadata)
```
