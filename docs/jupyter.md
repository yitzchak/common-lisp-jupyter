
# JUPYTER

Core package for Jupyter support including kernel and installer abstract classes.

## Nicknames

j

## Exports

\*debug\-environment\*, \*debug\-frame\*, \*debugger\*, \*html\-output\*, 
\*kernel\*, \*markdown\-output\*, \*page\-output\*, \*thread\-id\*, 
add\-thread, clear, code\-is\-complete, comm, comm\-id, command\-line, 
complete\-code, create\-comm, debug\-abort, debug\-activate\-breakpoints, 
debug\-breakpoint, debug\-breakpoint\-data, debug\-breakpoint\-line, 
debug\-continue, debug\-dump\-cell, debug\-enter\-loop, debug\-environment, 
debug\-environment\-condition, debug\-environment\-restarts, debug\-evaluate, 
debug\-frame, debug\-in, debug\-initialize, debug\-inspect\-variables, 
debug\-new\-breakpoint, debug\-next, debug\-object, debug\-object\-children, 
debug\-object\-children\-resolve, debug\-object\-column, debug\-object\-data, 
debug\-object\-environment, debug\-object\-id, debug\-object\-line, 
debug\-object\-name, debug\-object\-parent, debug\-object\-source, 
debug\-object\-type, debug\-object\-value, debug\-out, 
debug\-remove\-breakpoint, debug\-scope, debug\-source, debug\-source\-name, 
debug\-source\-path, debug\-stop, debug\-variable, display, edit, 
enqueue\-input, evaluate\-code, execute\-result, file, get\-comm, gif\-file, 
handling\-comm\-errors, handling\-errors, html, inform, inline\-result, 
inspect\-code, install, installer, installer\-class, installer\-display\-name, 
installer\-implementation, installer\-kernel\-name, installer\-language, 
installer\-local, installer\-local\-systems, installer\-path, 
installer\-prefix, installer\-resources, installer\-systems, javascript, jpeg, 
jpeg\-file, json, json\-file, kernel, kernel\-debugger\-started, 
kernel\-prompt\-prefix, kernel\-prompt\-suffix, latex, make\-mime\-bundle, 
make\-object, make\-offset\-match\-set, make\-substring\-match\-set, 
make\-uuid, markdown, match\-set\-add, mime\-bundle\-data, 
mime\-bundle\-metadata, on\-comm\-close, on\-comm\-message, on\-comm\-open, 
pdf\-file, png, png\-file, ps\-file, quit, remove\-debug\-object, 
remove\-thread, result, run\-kernel, send\-comm\-close, send\-comm\-message, 
send\-comm\-open, send\-debug\-event, start, stop, svg, svg\-file, 
system\-bundle\-installer, system\-installer, text, user\-image\-installer, 
user\-installer, user\-thread\-p, vega, vega\-file, vega\-lite, 

## \*debug\-environment\*

### Dynamic Variable

```lisp
nil
```

## \*debug\-frame\*

### Dynamic Variable

```lisp
nil
```

## \*debugger\*

### Dynamic Variable

```lisp
t
```

## \*html\-output\*

### Dynamic Variable

```lisp
nil
```

## \*kernel\*

### Dynamic Variable

```lisp
nil
```

## \*markdown\-output\*

### Dynamic Variable

```lisp
nil
```

## \*page\-output\*

### Dynamic Variable

```lisp
nil
```

## \*thread\-id\*

### Dynamic Variable

```lisp
nil
```

## add\-thread

## Function

Create a thread queue in the kernel and assign the thread an id number.

```lisp
(add-thread kernel-instance)
```

## clear

## Function

Send clear output message to frontend.

```lisp
(clear &optional (wait nil))
```

## code\-is\-complete

## Generic Function

Check code for completeness. Kernel implementations should
  result one of the permitted values of complete, incomplete, unknown or
  invalid.

```lisp
(code-is-complete kernel code)
```

## comm

#### Class

##### Precedence List

comm, source, standard\-object, slot\-object, t

##### Slots

- sink
    - :initarg :sink
- comm\-id
    - :initarg :comm\-id
- target\-name
    - :initarg :target\-name
- kernel
    - :initarg :kernel

##### Methods

- comm\-kernel &mdash; automatically generated reader method

    ```lisp
    (comm-kernel (comm comm))
    ```


- comm\-target\-name &mdash; automatically generated reader method

    ```lisp
    (comm-target-name (comm comm))
    ```


- comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- source\-sink &mdash; automatically generated writer method

    ```lisp
    (setf (source-sink (source source)) new-value)
    ```


- source\-sink &mdash; automatically generated reader method

    ```lisp
    (source-sink (source source))
    ```


## comm\-id

## Generic Function

```lisp
(comm-id object)
```

## command\-line

## Generic Function

Get the command line for an installer instance.

```lisp
(command-line instance)
```

## complete\-code

## Generic Function

Complete code at cursor-pos. Successful matches should be added to match-set
  via match-set-add. Errors should be returned as `(values ename evalue traceback)`.

```lisp
(complete-code kernel match-set code cursor-pos)
```

## create\-comm

## Generic Function

```lisp
(create-comm target-name id data metadata buffers)
```

## debug\-abort

## Generic Function

Abort the current stopped thread.

```lisp
(debug-abort kernel environment)
```

## debug\-activate\-breakpoints

## Generic Function

Activate a breakpoint.

```lisp
(debug-activate-breakpoints kernel source breakpoints)
```

## debug\-breakpoint

#### Class

A line oriented breakpoint.

##### Precedence List

debug\-breakpoint, standard\-object, slot\-object, t

##### Slots

- line \[integer\] &mdash; The line number associated with the breakpoint.
    - :initarg :line
- data &mdash; Implementation specific data for the breakpoint
    - :initarg :data

##### Methods

- debug\-breakpoint\-data &mdash; Implementation specific data for the breakpoint

    ```lisp
    (setf (debug-breakpoint-data (debug-breakpoint debug-breakpoint)) new-value)
    ```


- debug\-breakpoint\-data &mdash; Implementation specific data for the breakpoint

    ```lisp
    (debug-breakpoint-data (debug-breakpoint debug-breakpoint))
    ```


- debug\-breakpoint\-line &mdash; The line number associated with the breakpoint.

    ```lisp
    (debug-breakpoint-line (debug-breakpoint debug-breakpoint))
    ```


## debug\-breakpoint\-data

## Generic Function

```lisp
(debug-breakpoint-data object)
```

## debug\-breakpoint\-line

## Generic Function

```lisp
(debug-breakpoint-line object)
```

## debug\-continue

## Generic Function

Continue execution of the stopped thread.

```lisp
(debug-continue kernel environment &optional restart-number)
```

## debug\-dump\-cell

## Generic Function

Save the code to the provided source-path.

```lisp
(debug-dump-cell kernel code source-path)
```

## debug\-enter\-loop

## Function

Re-enter the debug loop after a restart which implements a debugger command.

```lisp
(debug-enter-loop)
```

## debug\-environment

#### Class

A debug environment for a stopped thread.

##### Precedence List

debug\-environment, standard\-object, slot\-object, t

##### Slots

- frames &mdash; The frames associated with the environment
    - :initarg :frames
- objects &mdash; The debug-objects created in the environment. The id number is the same as the index in the array.
- condition &mdash; The condition which caused the debugger to be entered.
    - :initarg :condition
- restarts &mdash; Applicable restarts for the environment.
    - :initarg :restarts

##### Methods

- debug\-environment\-restarts &mdash; Applicable restarts for the environment.

    ```lisp
    (setf (debug-environment-restarts (debug-environment debug-environment))
            new-value)
    ```


- debug\-environment\-restarts &mdash; Applicable restarts for the environment.

    ```lisp
    (debug-environment-restarts (debug-environment debug-environment))
    ```


- debug\-environment\-condition &mdash; The condition which caused the debugger to be entered.

    ```lisp
    (setf (debug-environment-condition (debug-environment debug-environment))
            new-value)
    ```


- debug\-environment\-condition &mdash; The condition which caused the debugger to be entered.

    ```lisp
    (debug-environment-condition (debug-environment debug-environment))
    ```


- debug\-environment\-objects &mdash; The debug-objects created in the environment. The id number is the same as the index in the array.

    ```lisp
    (debug-environment-objects (debug-environment debug-environment))
    ```


- debug\-environment\-frames &mdash; The frames associated with the environment

    ```lisp
    (setf (debug-environment-frames (debug-environment debug-environment))
            new-value)
    ```


- debug\-environment\-frames &mdash; The frames associated with the environment

    ```lisp
    (debug-environment-frames (debug-environment debug-environment))
    ```


## debug\-environment\-condition

## Generic Function

```lisp
(debug-environment-condition object)
```

## debug\-environment\-restarts

## Generic Function

```lisp
(debug-environment-restarts object)
```

## debug\-evaluate

## Generic Function

Evaluate code in the context of a frame

```lisp
(debug-evaluate kernel environment code frame)
```

## debug\-frame

#### Class

A debugger frame.

##### Precedence List

debug\-frame, debug\-object, standard\-object, slot\-object, t

##### Slots

- id \[integer\] &mdash; The id of the object.
    - :initarg :id
- environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.
    - :initarg :environment
- parent &mdash; The parent object
    - :initarg :parent
- children &mdash; Children of the object, i.e. scopes, slots, etc.
- name \[string\] &mdash; The name of the object.
    - :initarg :name
- data &mdash; Implementation specific data associated with the object.
    - :initarg :data
- source &mdash; The source reference of the frame.
    - :initarg :source
- line \[integer\] &mdash; A line number of the frame.
    - :initarg :line
- column \[integer\] &mdash; The column number of the frame.
    - :initarg :column

##### Methods

- debug\-object\-column &mdash; The column number of the frame.

    ```lisp
    (setf (debug-object-column (debug-frame debug-frame)) new-value)
    ```


- debug\-object\-column &mdash; The column number of the frame.

    ```lisp
    (debug-object-column (debug-frame debug-frame))
    ```


- debug\-object\-line &mdash; A line number of the frame.

    ```lisp
    (setf (debug-object-line (debug-frame debug-frame)) new-value)
    ```


- debug\-object\-line &mdash; A line number of the frame.

    ```lisp
    (debug-object-line (debug-frame debug-frame))
    ```


- debug\-object\-source &mdash; The source reference of the frame.

    ```lisp
    (setf (debug-object-source (debug-frame debug-frame)) new-value)
    ```


- debug\-object\-source &mdash; The source reference of the frame.

    ```lisp
    (debug-object-source (debug-frame debug-frame))
    ```


- debug\-object\-children

    ```lisp
    (debug-object-children (instance debug-object))
    ```


- debug\-object\-data &mdash; Implementation specific data associated with the object.

    ```lisp
    (setf (debug-object-data (debug-object debug-object)) new-value)
    ```


- debug\-object\-data &mdash; Implementation specific data associated with the object.

    ```lisp
    (debug-object-data (debug-object debug-object))
    ```


- debug\-object\-name &mdash; The name of the object.

    ```lisp
    (setf (debug-object-name (debug-object debug-object)) new-value)
    ```


- debug\-object\-name &mdash; The name of the object.

    ```lisp
    (debug-object-name (debug-object debug-object))
    ```


- debug\-object\-children &mdash; Children of the object, i.e. scopes, slots, etc.

    ```lisp
    (setf (debug-object-children (debug-object debug-object)) new-value)
    ```


- debug\-object\-children &mdash; Children of the object, i.e. scopes, slots, etc.

    ```lisp
    (debug-object-children (debug-object debug-object))
    ```


- debug\-object\-parent &mdash; The parent object

    ```lisp
    (setf (debug-object-parent (debug-object debug-object)) new-value)
    ```


- debug\-object\-parent &mdash; The parent object

    ```lisp
    (debug-object-parent (debug-object debug-object))
    ```


- debug\-object\-environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.

    ```lisp
    (setf (debug-object-environment (debug-object debug-object)) new-value)
    ```


- debug\-object\-environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.

    ```lisp
    (debug-object-environment (debug-object debug-object))
    ```


- debug\-object\-id &mdash; The id of the object.

    ```lisp
    (setf (debug-object-id (debug-object debug-object)) new-value)
    ```


- debug\-object\-id &mdash; The id of the object.

    ```lisp
    (debug-object-id (debug-object debug-object))
    ```


## debug\-in

## Generic Function

Step into a function on the stopped thread.

```lisp
(debug-in kernel environment)
```

## debug\-initialize

## Generic Function

Perform any kernel specific initialization of the debugger and return capabilities.

```lisp
(debug-initialize kernel)
```

## debug\-inspect\-variables

## Generic Function

Return a list of debug-objects represents the variables in the global scope.

```lisp
(debug-inspect-variables kernel environment)
```

## debug\-new\-breakpoint

## Generic Function

Create a new breakpoint or return NIL if not possible

```lisp
(debug-new-breakpoint kernel source line)
```

## debug\-next

## Generic Function

Step to the next form on the stopped thread.

```lisp
(debug-next kernel environment)
```

## debug\-object

#### Class

A debug object. Superclass of frames, scopes, variables, etc.

##### Precedence List

debug\-object, standard\-object, slot\-object, t

##### Slots

- id \[integer\] &mdash; The id of the object.
    - :initarg :id
- environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.
    - :initarg :environment
- parent &mdash; The parent object
    - :initarg :parent
- children &mdash; Children of the object, i.e. scopes, slots, etc.
- name \[string\] &mdash; The name of the object.
    - :initarg :name
- data &mdash; Implementation specific data associated with the object.
    - :initarg :data

##### Methods

- debug\-object\-children

    ```lisp
    (debug-object-children (instance debug-object))
    ```


- debug\-object\-data &mdash; Implementation specific data associated with the object.

    ```lisp
    (setf (debug-object-data (debug-object debug-object)) new-value)
    ```


- debug\-object\-data &mdash; Implementation specific data associated with the object.

    ```lisp
    (debug-object-data (debug-object debug-object))
    ```


- debug\-object\-name &mdash; The name of the object.

    ```lisp
    (setf (debug-object-name (debug-object debug-object)) new-value)
    ```


- debug\-object\-name &mdash; The name of the object.

    ```lisp
    (debug-object-name (debug-object debug-object))
    ```


- debug\-object\-children &mdash; Children of the object, i.e. scopes, slots, etc.

    ```lisp
    (setf (debug-object-children (debug-object debug-object)) new-value)
    ```


- debug\-object\-children &mdash; Children of the object, i.e. scopes, slots, etc.

    ```lisp
    (debug-object-children (debug-object debug-object))
    ```


- debug\-object\-parent &mdash; The parent object

    ```lisp
    (setf (debug-object-parent (debug-object debug-object)) new-value)
    ```


- debug\-object\-parent &mdash; The parent object

    ```lisp
    (debug-object-parent (debug-object debug-object))
    ```


- debug\-object\-environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.

    ```lisp
    (setf (debug-object-environment (debug-object debug-object)) new-value)
    ```


- debug\-object\-environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.

    ```lisp
    (debug-object-environment (debug-object debug-object))
    ```


- debug\-object\-id &mdash; The id of the object.

    ```lisp
    (setf (debug-object-id (debug-object debug-object)) new-value)
    ```


- debug\-object\-id &mdash; The id of the object.

    ```lisp
    (debug-object-id (debug-object debug-object))
    ```


## debug\-object\-children

## Generic Function

```lisp
(debug-object-children object)
```

## debug\-object\-children\-resolve

## Generic Function

Return a list of debug-objects for the children of the instance.

```lisp
(debug-object-children-resolve instance)
```

## debug\-object\-column

## Generic Function

```lisp
(debug-object-column object)
```

## debug\-object\-data

## Generic Function

```lisp
(debug-object-data object)
```

## debug\-object\-environment

## Generic Function

```lisp
(debug-object-environment object)
```

## debug\-object\-id

## Generic Function

```lisp
(debug-object-id object)
```

## debug\-object\-line

## Generic Function

```lisp
(debug-object-line object)
```

## debug\-object\-name

## Generic Function

```lisp
(debug-object-name object)
```

## debug\-object\-parent

## Generic Function

```lisp
(debug-object-parent object)
```

## debug\-object\-source

## Generic Function

```lisp
(debug-object-source object)
```

## debug\-object\-type

## Generic Function

```lisp
(debug-object-type object)
```

## debug\-object\-value

## Generic Function

```lisp
(debug-object-value object)
```

## debug\-out

## Generic Function

Step out on the stopped thread.

```lisp
(debug-out kernel environment)
```

## debug\-remove\-breakpoint

## Generic Function

Remove a specific breakpoint

```lisp
(debug-remove-breakpoint kernel source breakpoint)
```

## debug\-scope

#### Class

A scope inside a frame.

##### Precedence List

debug\-scope, debug\-object, standard\-object, slot\-object, t

##### Slots

- id \[integer\] &mdash; The id of the object.
    - :initarg :id
- environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.
    - :initarg :environment
- parent &mdash; The parent object
    - :initarg :parent
- children &mdash; Children of the object, i.e. scopes, slots, etc.
- name \[string\] &mdash; The name of the object.
    - :initarg :name
- data &mdash; Implementation specific data associated with the object.
    - :initarg :data
- presentation\-hint \[string\] &mdash; Any presentation hints associated with the scope.
    - :initarg :presentation\-hint

##### Methods

- debug\-presentation\-hint &mdash; Any presentation hints associated with the scope.

    ```lisp
    (debug-presentation-hint (debug-scope debug-scope))
    ```


- debug\-object\-children

    ```lisp
    (debug-object-children (instance debug-object))
    ```


- debug\-object\-data &mdash; Implementation specific data associated with the object.

    ```lisp
    (setf (debug-object-data (debug-object debug-object)) new-value)
    ```


- debug\-object\-data &mdash; Implementation specific data associated with the object.

    ```lisp
    (debug-object-data (debug-object debug-object))
    ```


- debug\-object\-name &mdash; The name of the object.

    ```lisp
    (setf (debug-object-name (debug-object debug-object)) new-value)
    ```


- debug\-object\-name &mdash; The name of the object.

    ```lisp
    (debug-object-name (debug-object debug-object))
    ```


- debug\-object\-children &mdash; Children of the object, i.e. scopes, slots, etc.

    ```lisp
    (setf (debug-object-children (debug-object debug-object)) new-value)
    ```


- debug\-object\-children &mdash; Children of the object, i.e. scopes, slots, etc.

    ```lisp
    (debug-object-children (debug-object debug-object))
    ```


- debug\-object\-parent &mdash; The parent object

    ```lisp
    (setf (debug-object-parent (debug-object debug-object)) new-value)
    ```


- debug\-object\-parent &mdash; The parent object

    ```lisp
    (debug-object-parent (debug-object debug-object))
    ```


- debug\-object\-environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.

    ```lisp
    (setf (debug-object-environment (debug-object debug-object)) new-value)
    ```


- debug\-object\-environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.

    ```lisp
    (debug-object-environment (debug-object debug-object))
    ```


- debug\-object\-id &mdash; The id of the object.

    ```lisp
    (setf (debug-object-id (debug-object debug-object)) new-value)
    ```


- debug\-object\-id &mdash; The id of the object.

    ```lisp
    (debug-object-id (debug-object debug-object))
    ```


## debug\-source

#### Class

A source reference in the debugger.

##### Precedence List

debug\-source, standard\-object, slot\-object, t

##### Slots

- name \[string\] &mdash; The name associated with the source.
    - :initarg :name
- path \[(or pathname string)\] &mdash; The path of the source.
    - :initarg :path

##### Methods

- debug\-source\-path &mdash; The path of the source.

    ```lisp
    (debug-source-path (debug-source debug-source))
    ```


- debug\-source\-name &mdash; The name associated with the source.

    ```lisp
    (debug-source-name (debug-source debug-source))
    ```


## debug\-source\-name

## Generic Function

```lisp
(debug-source-name object)
```

## debug\-source\-path

## Generic Function

```lisp
(debug-source-path object)
```

## debug\-stop

## Function

Enter a stopped state on the current thread. This function will dispatch messages received from
  the CONTROL thread. Resumption of the thread is done through continue restarts so this function
  will not return.

```lisp
(debug-stop reason environment)
```

## debug\-variable

#### Class

A debugger variable

##### Precedence List

debug\-variable, debug\-object, standard\-object, slot\-object, t

##### Slots

- id \[integer\] &mdash; The id of the object.
    - :initarg :id
- environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.
    - :initarg :environment
- parent &mdash; The parent object
    - :initarg :parent
- children &mdash; Children of the object, i.e. scopes, slots, etc.
- name \[string\] &mdash; The name of the object.
    - :initarg :name
- data &mdash; Implementation specific data associated with the object.
    - :initarg :data
- value \[string\] &mdash; A printed representation of the value.
    - :initarg :value
- type \[string\] &mdash; The type associated with the variable
    - :initarg :type

##### Methods

- debug\-object\-type &mdash; The type associated with the variable

    ```lisp
    (setf (debug-object-type (debug-variable debug-variable)) new-value)
    ```


- debug\-object\-type &mdash; The type associated with the variable

    ```lisp
    (debug-object-type (debug-variable debug-variable))
    ```


- debug\-object\-value &mdash; A printed representation of the value.

    ```lisp
    (setf (debug-object-value (debug-variable debug-variable)) new-value)
    ```


- debug\-object\-value &mdash; A printed representation of the value.

    ```lisp
    (debug-object-value (debug-variable debug-variable))
    ```


- debug\-object\-children

    ```lisp
    (debug-object-children (instance debug-object))
    ```


- debug\-object\-data &mdash; Implementation specific data associated with the object.

    ```lisp
    (setf (debug-object-data (debug-object debug-object)) new-value)
    ```


- debug\-object\-data &mdash; Implementation specific data associated with the object.

    ```lisp
    (debug-object-data (debug-object debug-object))
    ```


- debug\-object\-name &mdash; The name of the object.

    ```lisp
    (setf (debug-object-name (debug-object debug-object)) new-value)
    ```


- debug\-object\-name &mdash; The name of the object.

    ```lisp
    (debug-object-name (debug-object debug-object))
    ```


- debug\-object\-children &mdash; Children of the object, i.e. scopes, slots, etc.

    ```lisp
    (setf (debug-object-children (debug-object debug-object)) new-value)
    ```


- debug\-object\-children &mdash; Children of the object, i.e. scopes, slots, etc.

    ```lisp
    (debug-object-children (debug-object debug-object))
    ```


- debug\-object\-parent &mdash; The parent object

    ```lisp
    (setf (debug-object-parent (debug-object debug-object)) new-value)
    ```


- debug\-object\-parent &mdash; The parent object

    ```lisp
    (debug-object-parent (debug-object debug-object))
    ```


- debug\-object\-environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.

    ```lisp
    (setf (debug-object-environment (debug-object debug-object)) new-value)
    ```


- debug\-object\-environment &mdash; An environment reference for object. If the object does not have children then
     it doesn't need an environment reference.

    ```lisp
    (debug-object-environment (debug-object debug-object))
    ```


- debug\-object\-id &mdash; The id of the object.

    ```lisp
    (setf (debug-object-id (debug-object debug-object)) new-value)
    ```


- debug\-object\-id &mdash; The id of the object.

    ```lisp
    (debug-object-id (debug-object debug-object))
    ```


## display

## Function

Send a result as mime bundle display data. `result` must implement the `mime-bundle-data`
method and optionally `mime-bundle-metadata`. If an `id` is specified then future calls with the
same `id` and `update` is `t`.

```lisp
(display result &key id update)
```

## edit

## Function

```lisp
(edit path &optional (line-number 0))
```

## enqueue\-input

## Function

Add code to input queue.

```lisp
(enqueue-input kernel code)
```

## evaluate\-code

## Generic Function

Evaluate code along with paged output. Evaluation results should be sent
  with `execute-result`. Errors should be returned as `(values ename evalue traceback)`

```lisp
(evaluate-code kernel code &optional source-path breakpoints)
```

## execute\-result

## Function

Send a result as mime bundle execution result. `result` must implement the `mime-bundle-data`
method and optionally `mime-bundle-metadata`.

```lisp
(execute-result result)
```

## file

## Function

Create a result based on a file path. The mime type will automatically be
  determined from the file extension.

```lisp
(file path &key display update id)
```

## get\-comm

## Function

```lisp
(get-comm id)
```

## gif\-file

## Function

Create a GIF image result based on a file path.

```lisp
(gif-file path &key display update id)
```

## handling\-comm\-errors

## Macro

Macro for catching any conditions during comm messages.

```lisp
(handling-comm-errors
  &body
  body)
```

## handling\-errors

## Macro

Macro for catching any conditions during code evaluation.

```lisp
(handling-errors
  &body
  body)
```

## html

## Function

Create a HTML result based on an inline value.

```lisp
(html value &key display update id)
```

## inform

## Function

```lisp
(inform level src format-control &rest format-arguments)
```

## inline\-result

## Function

Create a result based on an inline value.

```lisp
(inline-result value mime-type &key display update id)
```

## inspect\-code

## Generic Function

Inspect code at cursor-pos with detail-level. Successful
  inspection should return a single result that implements mime-bundle-data and
  optionally mime-bundle-metadata. Errors should be returned as
  `(values nil ename evalue traceback)`.

```lisp
(inspect-code kernel code cursor-pos detail-level)
```

## install

## Generic Function

Install a kernel based on an installer instance.

```lisp
(install instance)
```

## installer

#### Class

Base installer class.

##### Precedence List

installer, standard\-object, slot\-object, t

##### Slots

- class &mdash; Class that implements the kernel. Used by image based installations.
    - :initarg :class
- debugger &mdash; Whether or not the kernel supports debugging.
    - :initarg :debugger
- display\-name &mdash; Name of the kernel displayed to the user.
    - :initarg :display\-name
- implementation &mdash; Path to specific binary used by the kernel.
    - :initarg :implementation
- kernel\-name &mdash; Name of the kernel.
    - :initarg :kernel\-name
- language &mdash; Language that the kernel supports.
    - :initarg :language
- local &mdash; Is the installation a local or packaged installation?
    - :initarg :local
- local\-systems &mdash; List of systems to package into local-projects.
    - :initarg :local\-systems
- prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.
    - :initarg :prefix
- resources &mdash; List of paths of resource files such as icons.
    - :initarg :resources
- root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.
    - :initarg :root
- systems &mdash; List of systems to bundle for system installs.
    - :initarg :systems

##### Methods

- install &mdash; Do common installation tasks before the specific ones association with this instance.

    ```lisp
    (install (instance installer))
    ```


- installer\-systems &mdash; List of systems to bundle for system installs.

    ```lisp
    (setf (installer-systems (installer installer)) new-value)
    ```


- installer\-systems &mdash; List of systems to bundle for system installs.

    ```lisp
    (installer-systems (installer installer))
    ```


- installer\-root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.

    ```lisp
    (setf (installer-root (installer installer)) new-value)
    ```


- installer\-root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.

    ```lisp
    (installer-root (installer installer))
    ```


- installer\-resources &mdash; List of paths of resource files such as icons.

    ```lisp
    (setf (installer-resources (installer installer)) new-value)
    ```


- installer\-resources &mdash; List of paths of resource files such as icons.

    ```lisp
    (installer-resources (installer installer))
    ```


- installer\-prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

    ```lisp
    (setf (installer-prefix (installer installer)) new-value)
    ```


- installer\-prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

    ```lisp
    (installer-prefix (installer installer))
    ```


- installer\-local\-systems &mdash; List of systems to package into local-projects.

    ```lisp
    (setf (installer-local-systems (installer installer)) new-value)
    ```


- installer\-local\-systems &mdash; List of systems to package into local-projects.

    ```lisp
    (installer-local-systems (installer installer))
    ```


- installer\-local &mdash; Is the installation a local or packaged installation?

    ```lisp
    (setf (installer-local (installer installer)) new-value)
    ```


- installer\-local &mdash; Is the installation a local or packaged installation?

    ```lisp
    (installer-local (installer installer))
    ```


- installer\-language &mdash; Language that the kernel supports.

    ```lisp
    (setf (installer-language (installer installer)) new-value)
    ```


- installer\-language &mdash; Language that the kernel supports.

    ```lisp
    (installer-language (installer installer))
    ```


- installer\-kernel\-name &mdash; Name of the kernel.

    ```lisp
    (setf (installer-kernel-name (installer installer)) new-value)
    ```


- installer\-kernel\-name &mdash; Name of the kernel.

    ```lisp
    (installer-kernel-name (installer installer))
    ```


- installer\-implementation &mdash; Path to specific binary used by the kernel.

    ```lisp
    (setf (installer-implementation (installer installer)) new-value)
    ```


- installer\-implementation &mdash; Path to specific binary used by the kernel.

    ```lisp
    (installer-implementation (installer installer))
    ```


- installer\-display\-name &mdash; Name of the kernel displayed to the user.

    ```lisp
    (setf (installer-display-name (installer installer)) new-value)
    ```


- installer\-display\-name &mdash; Name of the kernel displayed to the user.

    ```lisp
    (installer-display-name (installer installer))
    ```


- installer\-debugger &mdash; Whether or not the kernel supports debugging.

    ```lisp
    (setf (installer-debugger (installer installer)) new-value)
    ```


- installer\-debugger &mdash; Whether or not the kernel supports debugging.

    ```lisp
    (installer-debugger (installer installer))
    ```


- installer\-class &mdash; Class that implements the kernel. Used by image based installations.

    ```lisp
    (setf (installer-class (installer installer)) new-value)
    ```


- installer\-class &mdash; Class that implements the kernel. Used by image based installations.

    ```lisp
    (installer-class (installer installer))
    ```


## installer\-class

## Generic Function

```lisp
(installer-class object)
```

## installer\-display\-name

## Generic Function

```lisp
(installer-display-name object)
```

## installer\-implementation

## Generic Function

```lisp
(installer-implementation object)
```

## installer\-kernel\-name

## Generic Function

```lisp
(installer-kernel-name object)
```

## installer\-language

## Generic Function

```lisp
(installer-language object)
```

## installer\-local

## Generic Function

```lisp
(installer-local object)
```

## installer\-local\-systems

## Generic Function

```lisp
(installer-local-systems object)
```

## installer\-path

## Function

Resolve each of the path parts then combine all into a single path using merge-pathnames.

```lisp
(installer-path instance &rest parts)
```

## installer\-prefix

## Generic Function

```lisp
(installer-prefix object)
```

## installer\-resources

## Generic Function

```lisp
(installer-resources object)
```

## installer\-systems

## Generic Function

```lisp
(installer-systems object)
```

## javascript

## Function

Create a JavaScript text result based on an inline value.

```lisp
(javascript value &key display update id)
```

## jpeg

## Function

Create a JPEG image result based on an inline value.

```lisp
(jpeg value &key display update id)
```

## jpeg\-file

## Function

Create a JPEG image result based on a file path.

```lisp
(jpeg-file path &key display update id)
```

## json

## Function

Create a plain text result based on an inline value.

```lisp
(json value &key display update id expanded)
```

## json\-file

## Function

Create a JSON result based on a file path.

```lisp
(json-file path &key display update id expanded)
```

## kernel

#### Class

Kernel state representation.

##### Precedence List

kernel, source, standard\-object, slot\-object, t

##### Slots

- sink
    - :initarg :sink
- name &mdash; Kernel name. Used as a unique identifier in kernel description.
    - :initarg :name
- version &mdash; Kernel version.
    - :initarg :version
- banner &mdash; Banner text used to describe kernel. Used in kernel_info_reply messages.
    - :initarg :banner
- language\-name &mdash; Display name of implementation language. Used in kernel_info_reply messages.
    - :initarg :language\-name
- language\-version &mdash; Version of implementation language. Used in kernel_info_reply messages.
    - :initarg :language\-version
- mime\-type &mdash; Default MIME type for source files. Used in kernel_info_reply messages.
    - :initarg :mime\-type
- file\-extension &mdash; Default file extension for source files. Used in kernel_info_reply messages.
    - :initarg :file\-extension
- pygments\-lexer &mdash; Name of Pygments lexer for source files. Used in kernel_info_reply messages.
    - :initarg :pygments\-lexer
- codemirror\-mode &mdash; CodeMirror mode for source files. Used in kernel_info_reply messages.
    - :initarg :codemirror\-mode
- help\-links &mdash; An association list of help links. The car is the description and the cdr is
       URL. Used in kernel_info_reply messages.
    - :initarg :help\-links
- package &mdash; The package in which evaluate-code, code-is-complete and others are called.
    - :initarg :package
- readtable &mdash; The readtable used bu evaluate-code, code-is-complete and others.
    - :initarg :readtable
- connection\-file &mdash; Pathname of connection file.
    - :initarg :connection\-file
- transport \[string\] &mdash; Transport protocol from connection file.
- ip \[string\] &mdash; IP address from connection file.
- shell\-port \[fixnum\] &mdash; SHELL port from connection file.
- stdin\-port \[fixnum\] &mdash; STDIN port from connection file.
- iopub\-port \[fixnum\] &mdash; IOPUB port from connection file.
- control\-port \[fixnum\] &mdash; CONTROL port from connection file.
- hb\-port \[fixnum\] &mdash; HB port from connection file.
- signature\-scheme \[string\] &mdash; Signature scheme from connection file.
- key &mdash; Signing key from connection file.
- prompt\-prefix &mdash; String prefix using in *standard-output* to indicate the start of prompt.
    - :initarg :prompt\-prefix
- prompt\-suffix &mdash; String suffix using in *standard-output* to indicate the end of prompt.
    - :initarg :prompt\-suffix
- ctx &mdash; pzmq ctx handle.
- mac &mdash; Message authentification.
- hb &mdash; Heartbeat channel.
- shell &mdash; SHELL channel.
- stdin &mdash; STDIN channel.
- control &mdash; CONTROL channel.
- iopub &mdash; IOPUB channel.
- session &mdash; Session identifier.
- input\-queue &mdash; Input queue used to feed values into execute_result payloads.
    - :initarg :input\-queue
- history &mdash; Kernel history manager.
- execution\-count &mdash; Kernel execution count.
- comms &mdash; Currently open comms.
- control\-thread &mdash; Control thread
    - :initarg :control\-thread
- shell\-thread &mdash; Shell thread
- html\-output &mdash; HTML display output stream
- markdown\-output &mdash; Markdown display output stream
- error\-output &mdash; Error output stream
- standard\-output &mdash; Standard output stream
- standard\-input &mdash; Standard input stream
- tmp\-file\-prefix &mdash; Prefix for temporary debugger files
- tmp\-file\-suffix &mdash; Suffix for temporary debugger files
- hash\-seed &mdash; Hash seed for temporary debugger files
- breakpoints &mdash; Currently set breakpoints.
- debugger &mdash; Whether the debugger is supported
    - :initarg :debugger
- debugger\-started &mdash; Whether the debugger has been started
- threads

##### Methods

- stop

    ```lisp
    (stop (k kernel))
    ```


- start

    ```lisp
    (start (k kernel))
    ```


- kernel\-threads &mdash; automatically generated reader method

    ```lisp
    (kernel-threads (kernel kernel))
    ```


- kernel\-debugger\-started &mdash; Whether the debugger has been started

    ```lisp
    (setf (kernel-debugger-started (kernel kernel)) new-value)
    ```


- kernel\-debugger\-started &mdash; Whether the debugger has been started

    ```lisp
    (kernel-debugger-started (kernel kernel))
    ```


- kernel\-debugger &mdash; Whether the debugger is supported

    ```lisp
    (setf (kernel-debugger (kernel kernel)) new-value)
    ```


- kernel\-debugger &mdash; Whether the debugger is supported

    ```lisp
    (kernel-debugger (kernel kernel))
    ```


- kernel\-breakpoints &mdash; Currently set breakpoints.

    ```lisp
    (setf (kernel-breakpoints (kernel kernel)) new-value)
    ```


- kernel\-breakpoints &mdash; Currently set breakpoints.

    ```lisp
    (kernel-breakpoints (kernel kernel))
    ```


- kernel\-hash\-seed &mdash; Hash seed for temporary debugger files

    ```lisp
    (setf (kernel-hash-seed (kernel kernel)) new-value)
    ```


- kernel\-hash\-seed &mdash; Hash seed for temporary debugger files

    ```lisp
    (kernel-hash-seed (kernel kernel))
    ```


- kernel\-tmp\-file\-suffix &mdash; Suffix for temporary debugger files

    ```lisp
    (setf (kernel-tmp-file-suffix (kernel kernel)) new-value)
    ```


- kernel\-tmp\-file\-suffix &mdash; Suffix for temporary debugger files

    ```lisp
    (kernel-tmp-file-suffix (kernel kernel))
    ```


- kernel\-tmp\-file\-prefix &mdash; Prefix for temporary debugger files

    ```lisp
    (setf (kernel-tmp-file-prefix (kernel kernel)) new-value)
    ```


- kernel\-tmp\-file\-prefix &mdash; Prefix for temporary debugger files

    ```lisp
    (kernel-tmp-file-prefix (kernel kernel))
    ```


- kernel\-standard\-input &mdash; Standard input stream

    ```lisp
    (setf (kernel-standard-input (kernel kernel)) new-value)
    ```


- kernel\-standard\-input &mdash; Standard input stream

    ```lisp
    (kernel-standard-input (kernel kernel))
    ```


- kernel\-standard\-output &mdash; Standard output stream

    ```lisp
    (setf (kernel-standard-output (kernel kernel)) new-value)
    ```


- kernel\-standard\-output &mdash; Standard output stream

    ```lisp
    (kernel-standard-output (kernel kernel))
    ```


- kernel\-error\-output &mdash; Error output stream

    ```lisp
    (setf (kernel-error-output (kernel kernel)) new-value)
    ```


- kernel\-error\-output &mdash; Error output stream

    ```lisp
    (kernel-error-output (kernel kernel))
    ```


- kernel\-markdown\-output &mdash; Markdown display output stream

    ```lisp
    (kernel-markdown-output (kernel kernel))
    ```


- kernel\-html\-output &mdash; HTML display output stream

    ```lisp
    (kernel-html-output (kernel kernel))
    ```


- kernel\-shell\-thread &mdash; Shell thread

    ```lisp
    (setf (kernel-shell-thread (kernel kernel)) new-value)
    ```


- kernel\-shell\-thread &mdash; Shell thread

    ```lisp
    (kernel-shell-thread (kernel kernel))
    ```


- kernel\-control\-thread &mdash; Control thread

    ```lisp
    (setf (kernel-control-thread (kernel kernel)) new-value)
    ```


- kernel\-control\-thread &mdash; Control thread

    ```lisp
    (kernel-control-thread (kernel kernel))
    ```


- kernel\-comms &mdash; Currently open comms.

    ```lisp
    (kernel-comms (kernel kernel))
    ```


- kernel\-execution\-count &mdash; Kernel execution count.

    ```lisp
    (setf (kernel-execution-count (kernel kernel)) new-value)
    ```


- kernel\-execution\-count &mdash; Kernel execution count.

    ```lisp
    (kernel-execution-count (kernel kernel))
    ```


- kernel\-history &mdash; Kernel history manager.

    ```lisp
    (setf (kernel-history (kernel kernel)) new-value)
    ```


- kernel\-history &mdash; Kernel history manager.

    ```lisp
    (kernel-history (kernel kernel))
    ```


- kernel\-input\-queue &mdash; Input queue used to feed values into execute_result payloads.

    ```lisp
    (kernel-input-queue (kernel kernel))
    ```


- kernel\-session &mdash; Session identifier.

    ```lisp
    (setf (kernel-session (kernel kernel)) new-value)
    ```


- kernel\-session &mdash; Session identifier.

    ```lisp
    (kernel-session (kernel kernel))
    ```


- kernel\-iopub &mdash; IOPUB channel.

    ```lisp
    (setf (kernel-iopub (kernel kernel)) new-value)
    ```


- kernel\-iopub &mdash; IOPUB channel.

    ```lisp
    (kernel-iopub (kernel kernel))
    ```


- kernel\-control &mdash; CONTROL channel.

    ```lisp
    (setf (kernel-control (kernel kernel)) new-value)
    ```


- kernel\-control &mdash; CONTROL channel.

    ```lisp
    (kernel-control (kernel kernel))
    ```


- kernel\-stdin &mdash; STDIN channel.

    ```lisp
    (setf (kernel-stdin (kernel kernel)) new-value)
    ```


- kernel\-stdin &mdash; STDIN channel.

    ```lisp
    (kernel-stdin (kernel kernel))
    ```


- kernel\-shell &mdash; SHELL channel.

    ```lisp
    (setf (kernel-shell (kernel kernel)) new-value)
    ```


- kernel\-shell &mdash; SHELL channel.

    ```lisp
    (kernel-shell (kernel kernel))
    ```


- kernel\-hb &mdash; Heartbeat channel.

    ```lisp
    (setf (kernel-hb (kernel kernel)) new-value)
    ```


- kernel\-hb &mdash; Heartbeat channel.

    ```lisp
    (kernel-hb (kernel kernel))
    ```


- kernel\-mac &mdash; Message authentification.

    ```lisp
    (setf (kernel-mac (kernel kernel)) new-value)
    ```


- kernel\-mac &mdash; Message authentification.

    ```lisp
    (kernel-mac (kernel kernel))
    ```


- kernel\-ctx &mdash; pzmq ctx handle.

    ```lisp
    (setf (kernel-ctx (kernel kernel)) new-value)
    ```


- kernel\-ctx &mdash; pzmq ctx handle.

    ```lisp
    (kernel-ctx (kernel kernel))
    ```


- kernel\-prompt\-suffix &mdash; String suffix using in *standard-output* to indicate the end of prompt.

    ```lisp
    (kernel-prompt-suffix (kernel kernel))
    ```


- kernel\-prompt\-prefix &mdash; String prefix using in *standard-output* to indicate the start of prompt.

    ```lisp
    (kernel-prompt-prefix (kernel kernel))
    ```


- kernel\-key &mdash; Signing key from connection file.

    ```lisp
    (setf (kernel-key (kernel kernel)) new-value)
    ```


- kernel\-key &mdash; Signing key from connection file.

    ```lisp
    (kernel-key (kernel kernel))
    ```


- kernel\-signature\-scheme &mdash; Signature scheme from connection file.

    ```lisp
    (setf (kernel-signature-scheme (kernel kernel)) new-value)
    ```


- kernel\-signature\-scheme &mdash; Signature scheme from connection file.

    ```lisp
    (kernel-signature-scheme (kernel kernel))
    ```


- kernel\-hb\-port &mdash; HB port from connection file.

    ```lisp
    (setf (kernel-hb-port (kernel kernel)) new-value)
    ```


- kernel\-hb\-port &mdash; HB port from connection file.

    ```lisp
    (kernel-hb-port (kernel kernel))
    ```


- kernel\-control\-port &mdash; CONTROL port from connection file.

    ```lisp
    (setf (kernel-control-port (kernel kernel)) new-value)
    ```


- kernel\-control\-port &mdash; CONTROL port from connection file.

    ```lisp
    (kernel-control-port (kernel kernel))
    ```


- kernel\-iopub\-port &mdash; IOPUB port from connection file.

    ```lisp
    (setf (kernel-iopub-port (kernel kernel)) new-value)
    ```


- kernel\-iopub\-port &mdash; IOPUB port from connection file.

    ```lisp
    (kernel-iopub-port (kernel kernel))
    ```


- kernel\-stdin\-port &mdash; STDIN port from connection file.

    ```lisp
    (setf (kernel-stdin-port (kernel kernel)) new-value)
    ```


- kernel\-stdin\-port &mdash; STDIN port from connection file.

    ```lisp
    (kernel-stdin-port (kernel kernel))
    ```


- kernel\-shell\-port &mdash; SHELL port from connection file.

    ```lisp
    (setf (kernel-shell-port (kernel kernel)) new-value)
    ```


- kernel\-shell\-port &mdash; SHELL port from connection file.

    ```lisp
    (kernel-shell-port (kernel kernel))
    ```


- kernel\-ip &mdash; IP address from connection file.

    ```lisp
    (setf (kernel-ip (kernel kernel)) new-value)
    ```


- kernel\-ip &mdash; IP address from connection file.

    ```lisp
    (kernel-ip (kernel kernel))
    ```


- kernel\-transport &mdash; Transport protocol from connection file.

    ```lisp
    (setf (kernel-transport (kernel kernel)) new-value)
    ```


- kernel\-transport &mdash; Transport protocol from connection file.

    ```lisp
    (kernel-transport (kernel kernel))
    ```


- kernel\-connection\-file &mdash; Pathname of connection file.

    ```lisp
    (kernel-connection-file (kernel kernel))
    ```


- kernel\-readtable &mdash; The readtable used bu evaluate-code, code-is-complete and others.

    ```lisp
    (setf (kernel-readtable (kernel kernel)) new-value)
    ```


- kernel\-readtable &mdash; The readtable used bu evaluate-code, code-is-complete and others.

    ```lisp
    (kernel-readtable (kernel kernel))
    ```


- kernel\-package &mdash; The package in which evaluate-code, code-is-complete and others are called.

    ```lisp
    (setf (kernel-package (kernel kernel)) new-value)
    ```


- kernel\-package &mdash; The package in which evaluate-code, code-is-complete and others are called.

    ```lisp
    (kernel-package (kernel kernel))
    ```


- kernel\-help\-links &mdash; An association list of help links. The car is the description and the cdr is
       URL. Used in kernel_info_reply messages.

    ```lisp
    (kernel-help-links (kernel kernel))
    ```


- kernel\-codemirror\-mode &mdash; CodeMirror mode for source files. Used in kernel_info_reply messages.

    ```lisp
    (kernel-codemirror-mode (kernel kernel))
    ```


- kernel\-pygments\-lexer &mdash; Name of Pygments lexer for source files. Used in kernel_info_reply messages.

    ```lisp
    (kernel-pygments-lexer (kernel kernel))
    ```


- kernel\-file\-extension &mdash; Default file extension for source files. Used in kernel_info_reply messages.

    ```lisp
    (kernel-file-extension (kernel kernel))
    ```


- kernel\-mime\-type &mdash; Default MIME type for source files. Used in kernel_info_reply messages.

    ```lisp
    (kernel-mime-type (kernel kernel))
    ```


- kernel\-language\-version &mdash; Version of implementation language. Used in kernel_info_reply messages.

    ```lisp
    (kernel-language-version (kernel kernel))
    ```


- kernel\-language\-name &mdash; Display name of implementation language. Used in kernel_info_reply messages.

    ```lisp
    (kernel-language-name (kernel kernel))
    ```


- kernel\-banner &mdash; Banner text used to describe kernel. Used in kernel_info_reply messages.

    ```lisp
    (kernel-banner (kernel kernel))
    ```


- kernel\-version &mdash; Kernel version.

    ```lisp
    (kernel-version (kernel kernel))
    ```


- kernel\-name &mdash; Kernel name. Used as a unique identifier in kernel description.

    ```lisp
    (kernel-name (kernel kernel))
    ```


- source\-sink &mdash; automatically generated writer method

    ```lisp
    (setf (source-sink (source source)) new-value)
    ```


- source\-sink &mdash; automatically generated reader method

    ```lisp
    (source-sink (source source))
    ```


## kernel\-debugger\-started

## Generic Function

```lisp
(kernel-debugger-started object)
```

## kernel\-prompt\-prefix

## Generic Function

```lisp
(kernel-prompt-prefix object)
```

## kernel\-prompt\-suffix

## Generic Function

```lisp
(kernel-prompt-suffix object)
```

## latex

## Function

Create a LaTeX result based on an inline value.

```lisp
(latex value &key display update id)
```

## make\-mime\-bundle

## Function

```lisp
(make-mime-bundle data &optional metadata)
```

## make\-object

## Function

Create a hash table based on args (in plist format) and a test of equal.

```lisp
(make-object &rest args)
```

## make\-offset\-match\-set

## Function

```lisp
(make-offset-match-set &key ((parent parent) nil) ((offset offset) nil))
```

## make\-substring\-match\-set

## Function

```lisp
(make-substring-match-set &key ((parent parent) nil) ((start start) nil)
                          ((end end) nil))
```

## make\-uuid

## Function

```lisp
(make-uuid &optional as-bytes)
```

## markdown

## Function

Create a Markdown result based on an inline value.

```lisp
(markdown value &key display update id)
```

## match\-set\-add

## Generic Function

```lisp
(match-set-add instance text start end &key type)
```

## mime\-bundle\-data

## Generic Function

Return a JSON object with keys that mime types and the values are a rendering
of `value` in that mime type.

```lisp
(mime-bundle-data value)
```

## mime\-bundle\-metadata

## Generic Function

Return metadata specific to `value`.

```lisp
(mime-bundle-metadata value)
```

## on\-comm\-close

## Generic Function

```lisp
(on-comm-close comm data metadata buffers)
```

## on\-comm\-message

## Generic Function

```lisp
(on-comm-message comm data metadata buffers)
```

## on\-comm\-open

## Generic Function

```lisp
(on-comm-open comm data metadata buffers)
```

## pdf\-file

## Function

Create a PDF result based on a file path.

```lisp
(pdf-file path &key display update id)
```

## png

## Function

Create a PNG image result based on an inline value.

```lisp
(png value &key display update id)
```

## png\-file

## Function

Create a PNG image result based on a file path.

```lisp
(png-file path &key display update id)
```

## ps\-file

## Function

Create a PostScript result based on a file path.

```lisp
(ps-file path &key display update id)
```

## quit

## Function

```lisp
(quit &optional keep-kernel)
```

## remove\-debug\-object

## remove\-thread

## Function

Remove the thread queue and reset the thread id number.

```lisp
(remove-thread kernel-instance &optional (thread-id *thread-id*))
```

## result

## run\-kernel

## Function

Run a kernel based on a kernel class and a connection file.

```lisp
(run-kernel kernel-class &optional
            (connection-file (first (command-line-arguments))))
```

## send\-comm\-close

## Function

```lisp
(send-comm-close comm &optional data metadata buffers)
```

## send\-comm\-message

## Function

```lisp
(send-comm-message comm &optional data metadata buffers)
```

## send\-comm\-open

## Function

```lisp
(send-comm-open comm &optional data metadata buffers)
```

## send\-debug\-event

## Function

```lisp
(send-debug-event event &optional body &aux (iopub (kernel-iopub *kernel*)))
```

## start

## Generic Function

Start the resource.

```lisp
(start ch)
```

## stop

## Generic Function

Stop the resource.

```lisp
(stop ch)
```

## svg

## Function

Create a SVG result based on an inline value.

```lisp
(svg value &key display update id)
```

## svg\-file

## Function

Create a SVG result based on a file path.

```lisp
(svg-file path &key display update id)
```

## system\-bundle\-installer

#### Class

System bundle installer class.

##### Precedence List

system\-bundle\-installer, system\-installer, installer, standard\-object, slot\-object, t

##### Slots

- class &mdash; Class that implements the kernel. Used by image based installations.
    - :initarg :class
- debugger &mdash; Whether or not the kernel supports debugging.
    - :initarg :debugger
- display\-name &mdash; Name of the kernel displayed to the user.
    - :initarg :display\-name
- implementation &mdash; Path to specific binary used by the kernel.
    - :initarg :implementation
- kernel\-name &mdash; Name of the kernel.
    - :initarg :kernel\-name
- language &mdash; Language that the kernel supports.
    - :initarg :language
- local &mdash; Is the installation a local or packaged installation?
    - :initarg :local
- local\-systems &mdash; List of systems to package into local-projects.
    - :initarg :local\-systems
- prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.
    - :initarg :prefix
- resources &mdash; List of paths of resource files such as icons.
    - :initarg :resources
- root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.
    - :initarg :root
- systems &mdash; List of systems to bundle for system installs.
    - :initarg :systems

##### Methods

- install &mdash; Install system bundle kernel.

    ```lisp
    (install (instance system-bundle-installer))
    ```


- install &mdash; Install system kernel.

    ```lisp
    (install (instance system-installer))
    ```


- installer\-path\-part &mdash; Get the root directory for a system installation.

    ```lisp
    (installer-path-part (instance system-installer) (type (eql root)))
    ```


- install &mdash; Do common installation tasks before the specific ones association with this instance.

    ```lisp
    (install (instance installer))
    ```


- installer\-systems &mdash; List of systems to bundle for system installs.

    ```lisp
    (setf (installer-systems (installer installer)) new-value)
    ```


- installer\-systems &mdash; List of systems to bundle for system installs.

    ```lisp
    (installer-systems (installer installer))
    ```


- installer\-root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.

    ```lisp
    (setf (installer-root (installer installer)) new-value)
    ```


- installer\-root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.

    ```lisp
    (installer-root (installer installer))
    ```


- installer\-resources &mdash; List of paths of resource files such as icons.

    ```lisp
    (setf (installer-resources (installer installer)) new-value)
    ```


- installer\-resources &mdash; List of paths of resource files such as icons.

    ```lisp
    (installer-resources (installer installer))
    ```


- installer\-prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

    ```lisp
    (setf (installer-prefix (installer installer)) new-value)
    ```


- installer\-prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

    ```lisp
    (installer-prefix (installer installer))
    ```


- installer\-local\-systems &mdash; List of systems to package into local-projects.

    ```lisp
    (setf (installer-local-systems (installer installer)) new-value)
    ```


- installer\-local\-systems &mdash; List of systems to package into local-projects.

    ```lisp
    (installer-local-systems (installer installer))
    ```


- installer\-local &mdash; Is the installation a local or packaged installation?

    ```lisp
    (setf (installer-local (installer installer)) new-value)
    ```


- installer\-local &mdash; Is the installation a local or packaged installation?

    ```lisp
    (installer-local (installer installer))
    ```


- installer\-language &mdash; Language that the kernel supports.

    ```lisp
    (setf (installer-language (installer installer)) new-value)
    ```


- installer\-language &mdash; Language that the kernel supports.

    ```lisp
    (installer-language (installer installer))
    ```


- installer\-kernel\-name &mdash; Name of the kernel.

    ```lisp
    (setf (installer-kernel-name (installer installer)) new-value)
    ```


- installer\-kernel\-name &mdash; Name of the kernel.

    ```lisp
    (installer-kernel-name (installer installer))
    ```


- installer\-implementation &mdash; Path to specific binary used by the kernel.

    ```lisp
    (setf (installer-implementation (installer installer)) new-value)
    ```


- installer\-implementation &mdash; Path to specific binary used by the kernel.

    ```lisp
    (installer-implementation (installer installer))
    ```


- installer\-display\-name &mdash; Name of the kernel displayed to the user.

    ```lisp
    (setf (installer-display-name (installer installer)) new-value)
    ```


- installer\-display\-name &mdash; Name of the kernel displayed to the user.

    ```lisp
    (installer-display-name (installer installer))
    ```


- installer\-debugger &mdash; Whether or not the kernel supports debugging.

    ```lisp
    (setf (installer-debugger (installer installer)) new-value)
    ```


- installer\-debugger &mdash; Whether or not the kernel supports debugging.

    ```lisp
    (installer-debugger (installer installer))
    ```


- installer\-class &mdash; Class that implements the kernel. Used by image based installations.

    ```lisp
    (setf (installer-class (installer installer)) new-value)
    ```


- installer\-class &mdash; Class that implements the kernel. Used by image based installations.

    ```lisp
    (installer-class (installer installer))
    ```


## system\-installer

#### Class

System installer class.

##### Precedence List

system\-installer, installer, standard\-object, slot\-object, t

##### Slots

- class &mdash; Class that implements the kernel. Used by image based installations.
    - :initarg :class
- debugger &mdash; Whether or not the kernel supports debugging.
    - :initarg :debugger
- display\-name &mdash; Name of the kernel displayed to the user.
    - :initarg :display\-name
- implementation &mdash; Path to specific binary used by the kernel.
    - :initarg :implementation
- kernel\-name &mdash; Name of the kernel.
    - :initarg :kernel\-name
- language &mdash; Language that the kernel supports.
    - :initarg :language
- local &mdash; Is the installation a local or packaged installation?
    - :initarg :local
- local\-systems &mdash; List of systems to package into local-projects.
    - :initarg :local\-systems
- prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.
    - :initarg :prefix
- resources &mdash; List of paths of resource files such as icons.
    - :initarg :resources
- root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.
    - :initarg :root
- systems &mdash; List of systems to bundle for system installs.
    - :initarg :systems

##### Methods

- install &mdash; Install system kernel.

    ```lisp
    (install (instance system-installer))
    ```


- installer\-path\-part &mdash; Get the root directory for a system installation.

    ```lisp
    (installer-path-part (instance system-installer) (type (eql root)))
    ```


- install &mdash; Do common installation tasks before the specific ones association with this instance.

    ```lisp
    (install (instance installer))
    ```


- installer\-systems &mdash; List of systems to bundle for system installs.

    ```lisp
    (setf (installer-systems (installer installer)) new-value)
    ```


- installer\-systems &mdash; List of systems to bundle for system installs.

    ```lisp
    (installer-systems (installer installer))
    ```


- installer\-root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.

    ```lisp
    (setf (installer-root (installer installer)) new-value)
    ```


- installer\-root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.

    ```lisp
    (installer-root (installer installer))
    ```


- installer\-resources &mdash; List of paths of resource files such as icons.

    ```lisp
    (setf (installer-resources (installer installer)) new-value)
    ```


- installer\-resources &mdash; List of paths of resource files such as icons.

    ```lisp
    (installer-resources (installer installer))
    ```


- installer\-prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

    ```lisp
    (setf (installer-prefix (installer installer)) new-value)
    ```


- installer\-prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

    ```lisp
    (installer-prefix (installer installer))
    ```


- installer\-local\-systems &mdash; List of systems to package into local-projects.

    ```lisp
    (setf (installer-local-systems (installer installer)) new-value)
    ```


- installer\-local\-systems &mdash; List of systems to package into local-projects.

    ```lisp
    (installer-local-systems (installer installer))
    ```


- installer\-local &mdash; Is the installation a local or packaged installation?

    ```lisp
    (setf (installer-local (installer installer)) new-value)
    ```


- installer\-local &mdash; Is the installation a local or packaged installation?

    ```lisp
    (installer-local (installer installer))
    ```


- installer\-language &mdash; Language that the kernel supports.

    ```lisp
    (setf (installer-language (installer installer)) new-value)
    ```


- installer\-language &mdash; Language that the kernel supports.

    ```lisp
    (installer-language (installer installer))
    ```


- installer\-kernel\-name &mdash; Name of the kernel.

    ```lisp
    (setf (installer-kernel-name (installer installer)) new-value)
    ```


- installer\-kernel\-name &mdash; Name of the kernel.

    ```lisp
    (installer-kernel-name (installer installer))
    ```


- installer\-implementation &mdash; Path to specific binary used by the kernel.

    ```lisp
    (setf (installer-implementation (installer installer)) new-value)
    ```


- installer\-implementation &mdash; Path to specific binary used by the kernel.

    ```lisp
    (installer-implementation (installer installer))
    ```


- installer\-display\-name &mdash; Name of the kernel displayed to the user.

    ```lisp
    (setf (installer-display-name (installer installer)) new-value)
    ```


- installer\-display\-name &mdash; Name of the kernel displayed to the user.

    ```lisp
    (installer-display-name (installer installer))
    ```


- installer\-debugger &mdash; Whether or not the kernel supports debugging.

    ```lisp
    (setf (installer-debugger (installer installer)) new-value)
    ```


- installer\-debugger &mdash; Whether or not the kernel supports debugging.

    ```lisp
    (installer-debugger (installer installer))
    ```


- installer\-class &mdash; Class that implements the kernel. Used by image based installations.

    ```lisp
    (setf (installer-class (installer installer)) new-value)
    ```


- installer\-class &mdash; Class that implements the kernel. Used by image based installations.

    ```lisp
    (installer-class (installer installer))
    ```


## text

## Function

Create a plain text result based on an inline value.

```lisp
(text value &key display update id)
```

## user\-image\-installer

#### Class

User image installer class.

##### Precedence List

user\-image\-installer, user\-installer, installer, standard\-object, slot\-object, t

##### Slots

- class &mdash; Class that implements the kernel. Used by image based installations.
    - :initarg :class
- debugger &mdash; Whether or not the kernel supports debugging.
    - :initarg :debugger
- display\-name &mdash; Name of the kernel displayed to the user.
    - :initarg :display\-name
- implementation &mdash; Path to specific binary used by the kernel.
    - :initarg :implementation
- kernel\-name &mdash; Name of the kernel.
    - :initarg :kernel\-name
- language &mdash; Language that the kernel supports.
    - :initarg :language
- local &mdash; Is the installation a local or packaged installation?
    - :initarg :local
- local\-systems &mdash; List of systems to package into local-projects.
    - :initarg :local\-systems
- prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.
    - :initarg :prefix
- resources &mdash; List of paths of resource files such as icons.
    - :initarg :resources
- root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.
    - :initarg :root
- systems &mdash; List of systems to bundle for system installs.
    - :initarg :systems

##### Methods

- install &mdash; Create an image for the user image based kernels.

    ```lisp
    (install (instance user-image-installer))
    ```


- command\-line &mdash; Get the command for a user image installer.

    ```lisp
    (command-line (instance user-image-installer))
    ```


- install &mdash; Install user kernel.

    ```lisp
    (install (instance user-installer))
    ```


- installer\-path\-part &mdash; Get the root directory for a user installation

    ```lisp
    (installer-path-part (instance user-installer) (type (eql root)))
    ```


- install &mdash; Do common installation tasks before the specific ones association with this instance.

    ```lisp
    (install (instance installer))
    ```


- installer\-systems &mdash; List of systems to bundle for system installs.

    ```lisp
    (setf (installer-systems (installer installer)) new-value)
    ```


- installer\-systems &mdash; List of systems to bundle for system installs.

    ```lisp
    (installer-systems (installer installer))
    ```


- installer\-root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.

    ```lisp
    (setf (installer-root (installer installer)) new-value)
    ```


- installer\-root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.

    ```lisp
    (installer-root (installer installer))
    ```


- installer\-resources &mdash; List of paths of resource files such as icons.

    ```lisp
    (setf (installer-resources (installer installer)) new-value)
    ```


- installer\-resources &mdash; List of paths of resource files such as icons.

    ```lisp
    (installer-resources (installer installer))
    ```


- installer\-prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

    ```lisp
    (setf (installer-prefix (installer installer)) new-value)
    ```


- installer\-prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

    ```lisp
    (installer-prefix (installer installer))
    ```


- installer\-local\-systems &mdash; List of systems to package into local-projects.

    ```lisp
    (setf (installer-local-systems (installer installer)) new-value)
    ```


- installer\-local\-systems &mdash; List of systems to package into local-projects.

    ```lisp
    (installer-local-systems (installer installer))
    ```


- installer\-local &mdash; Is the installation a local or packaged installation?

    ```lisp
    (setf (installer-local (installer installer)) new-value)
    ```


- installer\-local &mdash; Is the installation a local or packaged installation?

    ```lisp
    (installer-local (installer installer))
    ```


- installer\-language &mdash; Language that the kernel supports.

    ```lisp
    (setf (installer-language (installer installer)) new-value)
    ```


- installer\-language &mdash; Language that the kernel supports.

    ```lisp
    (installer-language (installer installer))
    ```


- installer\-kernel\-name &mdash; Name of the kernel.

    ```lisp
    (setf (installer-kernel-name (installer installer)) new-value)
    ```


- installer\-kernel\-name &mdash; Name of the kernel.

    ```lisp
    (installer-kernel-name (installer installer))
    ```


- installer\-implementation &mdash; Path to specific binary used by the kernel.

    ```lisp
    (setf (installer-implementation (installer installer)) new-value)
    ```


- installer\-implementation &mdash; Path to specific binary used by the kernel.

    ```lisp
    (installer-implementation (installer installer))
    ```


- installer\-display\-name &mdash; Name of the kernel displayed to the user.

    ```lisp
    (setf (installer-display-name (installer installer)) new-value)
    ```


- installer\-display\-name &mdash; Name of the kernel displayed to the user.

    ```lisp
    (installer-display-name (installer installer))
    ```


- installer\-debugger &mdash; Whether or not the kernel supports debugging.

    ```lisp
    (setf (installer-debugger (installer installer)) new-value)
    ```


- installer\-debugger &mdash; Whether or not the kernel supports debugging.

    ```lisp
    (installer-debugger (installer installer))
    ```


- installer\-class &mdash; Class that implements the kernel. Used by image based installations.

    ```lisp
    (setf (installer-class (installer installer)) new-value)
    ```


- installer\-class &mdash; Class that implements the kernel. Used by image based installations.

    ```lisp
    (installer-class (installer installer))
    ```


## user\-installer

#### Class

User installer class.

##### Precedence List

user\-installer, installer, standard\-object, slot\-object, t

##### Slots

- class &mdash; Class that implements the kernel. Used by image based installations.
    - :initarg :class
- debugger &mdash; Whether or not the kernel supports debugging.
    - :initarg :debugger
- display\-name &mdash; Name of the kernel displayed to the user.
    - :initarg :display\-name
- implementation &mdash; Path to specific binary used by the kernel.
    - :initarg :implementation
- kernel\-name &mdash; Name of the kernel.
    - :initarg :kernel\-name
- language &mdash; Language that the kernel supports.
    - :initarg :language
- local &mdash; Is the installation a local or packaged installation?
    - :initarg :local
- local\-systems &mdash; List of systems to package into local-projects.
    - :initarg :local\-systems
- prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.
    - :initarg :prefix
- resources &mdash; List of paths of resource files such as icons.
    - :initarg :resources
- root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.
    - :initarg :root
- systems &mdash; List of systems to bundle for system installs.
    - :initarg :systems

##### Methods

- install &mdash; Install user kernel.

    ```lisp
    (install (instance user-installer))
    ```


- installer\-path\-part &mdash; Get the root directory for a user installation

    ```lisp
    (installer-path-part (instance user-installer) (type (eql root)))
    ```


- install &mdash; Do common installation tasks before the specific ones association with this instance.

    ```lisp
    (install (instance installer))
    ```


- installer\-systems &mdash; List of systems to bundle for system installs.

    ```lisp
    (setf (installer-systems (installer installer)) new-value)
    ```


- installer\-systems &mdash; List of systems to bundle for system installs.

    ```lisp
    (installer-systems (installer installer))
    ```


- installer\-root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.

    ```lisp
    (setf (installer-root (installer installer)) new-value)
    ```


- installer\-root &mdash; The root directory under which the Jupyter folder is found. If nil then it will be determined automatically.

    ```lisp
    (installer-root (installer installer))
    ```


- installer\-resources &mdash; List of paths of resource files such as icons.

    ```lisp
    (setf (installer-resources (installer installer)) new-value)
    ```


- installer\-resources &mdash; List of paths of resource files such as icons.

    ```lisp
    (installer-resources (installer installer))
    ```


- installer\-prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

    ```lisp
    (setf (installer-prefix (installer installer)) new-value)
    ```


- installer\-prefix &mdash; Directory to put installed files into. Used by packaging system, should be nil otherwise.

    ```lisp
    (installer-prefix (installer installer))
    ```


- installer\-local\-systems &mdash; List of systems to package into local-projects.

    ```lisp
    (setf (installer-local-systems (installer installer)) new-value)
    ```


- installer\-local\-systems &mdash; List of systems to package into local-projects.

    ```lisp
    (installer-local-systems (installer installer))
    ```


- installer\-local &mdash; Is the installation a local or packaged installation?

    ```lisp
    (setf (installer-local (installer installer)) new-value)
    ```


- installer\-local &mdash; Is the installation a local or packaged installation?

    ```lisp
    (installer-local (installer installer))
    ```


- installer\-language &mdash; Language that the kernel supports.

    ```lisp
    (setf (installer-language (installer installer)) new-value)
    ```


- installer\-language &mdash; Language that the kernel supports.

    ```lisp
    (installer-language (installer installer))
    ```


- installer\-kernel\-name &mdash; Name of the kernel.

    ```lisp
    (setf (installer-kernel-name (installer installer)) new-value)
    ```


- installer\-kernel\-name &mdash; Name of the kernel.

    ```lisp
    (installer-kernel-name (installer installer))
    ```


- installer\-implementation &mdash; Path to specific binary used by the kernel.

    ```lisp
    (setf (installer-implementation (installer installer)) new-value)
    ```


- installer\-implementation &mdash; Path to specific binary used by the kernel.

    ```lisp
    (installer-implementation (installer installer))
    ```


- installer\-display\-name &mdash; Name of the kernel displayed to the user.

    ```lisp
    (setf (installer-display-name (installer installer)) new-value)
    ```


- installer\-display\-name &mdash; Name of the kernel displayed to the user.

    ```lisp
    (installer-display-name (installer installer))
    ```


- installer\-debugger &mdash; Whether or not the kernel supports debugging.

    ```lisp
    (setf (installer-debugger (installer installer)) new-value)
    ```


- installer\-debugger &mdash; Whether or not the kernel supports debugging.

    ```lisp
    (installer-debugger (installer installer))
    ```


- installer\-class &mdash; Class that implements the kernel. Used by image based installations.

    ```lisp
    (setf (installer-class (installer installer)) new-value)
    ```


- installer\-class &mdash; Class that implements the kernel. Used by image based installations.

    ```lisp
    (installer-class (installer installer))
    ```


## user\-thread\-p

## Function

Return non-NIL if the current thread is not the control thread.

```lisp
(user-thread-p)
```

## vega

## Function

Create a Vega result based on an inline value.

```lisp
(vega value &key display update id)
```

## vega\-file

## Function

Create a Vega graph based on a file path.

```lisp
(vega-file path &key display update id)
```

## vega\-lite

## Function

Create a VegaLite result based on an inline value.

```lisp
(vega-lite value &key display update id)
```

## vega\-lite\-file

## Function

Create a VegaLite graph based on a file path.

```lisp
(vega-lite-file path &key display update id)
```