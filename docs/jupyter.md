# Package jupyter


## *Variable* `*page-output*`

### Definition

```common-lisp
nil
```

### Description

Output stream sent to Jupyter pager. Available during calls to evaluate-code.


## *Function* `clear`

### Syntax

```common-lisp
(clear &optional wait)
```

### Description

Send clear output message to frontend.


## *Generic Function* `code-is-complete`

### Syntax

```common-lisp
(code-is-complete kernel code)
```

### Description

Check code for completeness. Kernel implementations should
  result one of the permitted values of complete, incomplete, unknown or
  invalid.


## *Class* `comm`

### Superclasses

- `comm`
- `standard-object`

### Initial Arguments

- `:id`
- `:target-name`
- `:kernel`


## *Generic Function* `comm-id`

### Syntax

```common-lisp
(comm-id sb-pcl::object)
```


## *Generic Function* `command-line`

### Syntax

```common-lisp
(command-line instance)
```

### Description

Get the command line for an installer instance.


## *Generic Function* `complete-code`

### Syntax

```common-lisp
(complete-code kernel code cursor-pos)
```

### Description

Complete code at cursor-pos. Successful completion should
  return three values, first a list of strings, then the cursor start position
  and finally the cursor end position.


## *Generic Function* `create-comm`

### Syntax

```common-lisp
(create-comm target-name id data metadata buffers)
```


## *Function* `enqueue-input`

### Syntax

```common-lisp
(enqueue-input kernel code)
```

### Description

Add code to input queue.


## *Generic Function* `evaluate-code`

### Syntax

```common-lisp
(evaluate-code kernel code)
```

### Description

Evaluate code along with paged output. Kernel implementations
  must return a list of evaluated results. Each result should be wrapped with an
  appropriate `result` class instance. Sending the results to the client will be
  handled by the calling method.


## *Function* `file`

### Syntax

```common-lisp
(file path &optional display-data)
```

### Description

Create a result based on a file path. The mime type with automatically be
  determined from the file extension.


## *Function* `get-comm`

### Syntax

```common-lisp
(get-comm id)
```


## *Function* `gif-file`

### Syntax

```common-lisp
(gif-file path &optional display-data)
```

### Description

Create a GIF image result based on a file path.


## *Macro* `handling-errors`

### Syntax

```common-lisp
(handling-errors
  &body
  body)
```

### Description

Macro for catching any conditions including quit-conditions during code
  evaluation.


## *Function* `html`

### Syntax

```common-lisp
(html value &optional display-data)
```

### Description

Create a HTML result based on an inline value.


## *Function* `inform`

### Syntax

```common-lisp
(inform level src format-control &rest format-arguments)
```


## *Function* `inline-result`

### Syntax

```common-lisp
(inline-result value mime-type &optional display-data)
```

### Description

Create a result based on an inline value.

## *Class* `inline-result`

### Superclasses

- `inline-result`
- `result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:value`
- `:mime-type`


## *Generic Function* `inspect-code`

### Syntax

```common-lisp
(inspect-code kernel code cursor-pos detail-level)
```

### Description

Inspect code at cursor-pos with detail-level. Successful
  inspection should return a single wrapped result.


## *Generic Function* `install`

### Syntax

```common-lisp
(install instance)
```

### Description

Install a kernel based on an installer instance.


## *Class* `installer`

### Superclasses

- `installer`
- `standard-object`

### Slots

- `class` &mdash; Class that implements the kernel. Used by image based installations.

- `display-name` &mdash; Name of the kernel displayed to the user.

- `implementation` &mdash; Path to specific binary used by the kernel.

- `kernel-name` &mdash; Name of the kernel.

- `language` &mdash; Language that the kernel supports.

- `local` &mdash; Is the installation a local or packaged installation?

- `local-systems` &mdash; List of systems to package into local-projects.

- `prefix` &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

- `resources` &mdash; List of paths of resource files such as icons.

- `systems` &mdash; List of systems to bundle for system installs.


### Initial Arguments

- `:class`
- `:display-name`
- `:implementation`
- `:kernel-name`
- `:language`
- `:local`
- `:local-systems`
- `:prefix`
- `:resources`
- `:systems`

### Description

Base installer class.


## *Generic Function* `installer-class`

### Syntax

```common-lisp
(installer-class sb-pcl::object)
```

## *Generic Function* `installer-class`

### Syntax

```common-lisp
(installer-class sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `installer-display-name`

### Syntax

```common-lisp
(installer-display-name sb-pcl::object)
```

## *Generic Function* `installer-display-name`

### Syntax

```common-lisp
(installer-display-name sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `installer-implementation`

### Syntax

```common-lisp
(installer-implementation sb-pcl::object)
```

## *Generic Function* `installer-implementation`

### Syntax

```common-lisp
(installer-implementation sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `installer-kernel-name`

### Syntax

```common-lisp
(installer-kernel-name sb-pcl::object)
```

## *Generic Function* `installer-kernel-name`

### Syntax

```common-lisp
(installer-kernel-name sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `installer-language`

### Syntax

```common-lisp
(installer-language sb-pcl::object)
```

## *Generic Function* `installer-language`

### Syntax

```common-lisp
(installer-language sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `installer-local`

### Syntax

```common-lisp
(installer-local sb-pcl::object)
```

## *Generic Function* `installer-local`

### Syntax

```common-lisp
(installer-local sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `installer-local-systems`

### Syntax

```common-lisp
(installer-local-systems sb-pcl::object)
```

## *Generic Function* `installer-local-systems`

### Syntax

```common-lisp
(installer-local-systems sb-pcl::new-value sb-pcl::object)
```


## *Function* `installer-path`

### Syntax

```common-lisp
(installer-path instance &rest parts)
```

### Description

Resolve each of the path parts then combine all into a single path using merge-pathnames.


## *Generic Function* `installer-prefix`

### Syntax

```common-lisp
(installer-prefix sb-pcl::object)
```

## *Generic Function* `installer-prefix`

### Syntax

```common-lisp
(installer-prefix sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `installer-resources`

### Syntax

```common-lisp
(installer-resources sb-pcl::object)
```

## *Generic Function* `installer-resources`

### Syntax

```common-lisp
(installer-resources sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `installer-systems`

### Syntax

```common-lisp
(installer-systems sb-pcl::object)
```

## *Generic Function* `installer-systems`

### Syntax

```common-lisp
(installer-systems sb-pcl::new-value sb-pcl::object)
```


## *Function* `jpeg`

### Syntax

```common-lisp
(jpeg value &optional display-data)
```

### Description

Create a JPEG image result based on an inline value.


## *Function* `jpeg-file`

### Syntax

```common-lisp
(jpeg-file path &optional display-data)
```

### Description

Create a JPEG image result based on a file path.


## *Function* `json-getf`

### Syntax

```common-lisp
(json-getf object indicator &optional default)
```

### Description

Safe accessor for the internal JSON format that behaves like getf


## *Class* `kernel`

### Superclasses

- `kernel`
- `standard-object`

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

- `connection-file` &mdash; Pathname of connection file.

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

- `mac` &mdash; Message authification.

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

### Description

Kernel state representation.


## *Generic Function* `kernel-prompt-prefix`

### Syntax

```common-lisp
(kernel-prompt-prefix sb-pcl::object)
```


## *Generic Function* `kernel-prompt-suffix`

### Syntax

```common-lisp
(kernel-prompt-suffix sb-pcl::object)
```


## *Function* `latex`

### Syntax

```common-lisp
(latex value &optional display-data)
```

### Description

Create a LaTeX result based on an inline value.


## *Function* `make-error-result`

### Syntax

```common-lisp
(make-error-result ename evalue &key quit traceback)
```

### Description

Make a result based on an error. The quit the parameter indicates that the
  kernel should exit. The handle argument is used by the convenience functions
  to instantly process the result.


## *Function* `make-file-result`

### Syntax

```common-lisp
(make-file-result path &key mime-type display-data handle)
```

### Description

Make a result based on a file. The handle argument is used by the convenience
  functions to instantly process the result.


## *Function* `make-inline-result`

### Syntax

```common-lisp
(make-inline-result value &key mime-type display-data handle)
```

### Description

Make a result based on an inline value. The handle argument is used by the
  convenience functions to instantly process the result.


## *Function* `make-lisp-result`

### Syntax

```common-lisp
(make-lisp-result value &key display-data)
```

### Description

Make a lisp result based on an inline value.


## *Function* `markdown`

### Syntax

```common-lisp
(markdown value &optional display-data)
```

### Description

Create a Markdown result based on an inline value.


## *Generic Function* `on-comm-close`

### Syntax

```common-lisp
(on-comm-close comm data metadata buffers)
```


## *Generic Function* `on-comm-message`

### Syntax

```common-lisp
(on-comm-message comm data metadata buffers)
```


## *Generic Function* `on-comm-open`

### Syntax

```common-lisp
(on-comm-open comm data metadata buffers)
```


## *Function* `pdf-file`

### Syntax

```common-lisp
(pdf-file path &optional display-data)
```

### Description

Create a PDF result based on a file path.


## *Function* `png`

### Syntax

```common-lisp
(png value &optional display-data)
```

### Description

Create a PNG image result based on an inline value.


## *Function* `png-file`

### Syntax

```common-lisp
(png-file path &optional display-data)
```

### Description

Create a PNG image result based on a file path.


## *Function* `ps-file`

### Syntax

```common-lisp
(ps-file path &optional display-data)
```

### Description

Create a PostScript result based on a file path.


## *Class* `quit-condition`

### Superclasses

- `quit-condition`
- `error`
- `serious-condition`
- `condition`

### Description

A condition for identifying a request for kernel shutdown.


## *Function* `quit-eval-error-p`

### Syntax

```common-lisp
(quit-eval-error-p result)
```

### Description

Predicate to determine if result is an quit result.


## *Generic Function* `render`

### Syntax

```common-lisp
(render result)
```

### Description

Render evaluation result as a mime bundle for execute_result
  or display_data.


## *Class* `result`

### Superclasses

- `result`
- `standard-object`

### Slots

- `display-data` &mdash; Show result as display_data in client.


### Initial Arguments

- `:display-data`

### Description

Base class for encapsulation of evaluation result.


## *Function* `run-kernel`

### Syntax

```common-lisp
(run-kernel kernel-class connection-file)
```

### Description

Run a kernel based on a kernel class and a connection file.


## *Function* `send-comm-close`

### Syntax

```common-lisp
(send-comm-close comm &optional data metadata buffers)
```


## *Function* `send-comm-message`

### Syntax

```common-lisp
(send-comm-message comm &optional data metadata buffers)
```


## *Function* `send-comm-open`

### Syntax

```common-lisp
(send-comm-open comm &optional data metadata buffers)
```


## *Function* `send-result`

### Syntax

```common-lisp
(send-result result)
```

### Description

Send a result either as display data or an execute result.


## *Function* `svg`

### Syntax

```common-lisp
(svg value &optional display-data)
```

### Description

Create a SVG result based on an inline value.


## *Function* `svg-file`

### Syntax

```common-lisp
(svg-file path &optional display-data)
```

### Description

Create a SVG result based on a file path.


## *Class* `system-installer`

### Superclasses

- `system-installer`
- `installer`
- `standard-object`

### Initial Arguments

- `:class`
- `:display-name`
- `:implementation`
- `:kernel-name`
- `:language`
- `:local`
- `:local-systems`
- `:prefix`
- `:resources`
- `:systems`

### Description

System installer class.


## *Function* `text`

### Syntax

```common-lisp
(text value &optional display-data)
```

### Description

Create a plain text result based on an inline value.


## *Class* `user-image-installer`

### Superclasses

- `user-image-installer`
- `user-installer`
- `installer`
- `standard-object`

### Initial Arguments

- `:class`
- `:display-name`
- `:implementation`
- `:kernel-name`
- `:language`
- `:local`
- `:local-systems`
- `:prefix`
- `:resources`
- `:systems`

### Description

User image installer class.


## *Class* `user-installer`

### Superclasses

- `user-installer`
- `installer`
- `standard-object`

### Initial Arguments

- `:class`
- `:display-name`
- `:implementation`
- `:kernel-name`
- `:language`
- `:local`
- `:local-systems`
- `:prefix`
- `:resources`
- `:systems`

### Description

User installer class.
