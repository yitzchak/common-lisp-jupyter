(defpackage #:jupyter
  (:use #:common-lisp)
  (:import-from :shasht #:make-object)
  (:nicknames :j)
  (:documentation "Core package for Jupyter support including kernel and installer abstract classes.")
  (:export
    #:make-object
    ; log
    #:inform
    ; mime-bundle
    #:display
    #:execute-result
    #:file
    #:gif-file
    #:html
    #:inline-result
    #:javascript
    #:jpeg
    #:jpeg-file
    #:json
    #:json-file
    #:latex
    #:markdown
    #:make-mime-bundle
    #:mime-bundle-data
    #:mime-bundle-metadata
    #:pdf-file
    #:png
    #:png-file
    #:ps-file
    #:result
    #:svg
    #:svg-file
    #:text
    #:vega
    #:vega-file
    #:vega-lite
    #:vega-lite-file
    ; matches
    #:match-set-add
    #:make-offset-match-set
    #:make-substring-match-set
    ; kernel
    #:add-thread
    #:clear
    #:code-is-complete
    #:comm
    #:comm-id
    #:complete-code
    #:create-comm
    #:debug-abort
    #:debug-activate-breakpoints
    #:debug-breakpoint
    #:debug-breakpoint-data
    #:debug-breakpoint-line
    #:debug-continue
    #:debug-dump-cell
    #:debug-enter-loop
    #:*debug-environment*
    #:debug-environment
    #:debug-environment-condition
    #:debug-environment-restarts
    #:debug-evaluate
    #:*debug-frame*
    #:debug-frame
    #:*debugger*
    #:debug-in
    #:debug-initialize
    #:debug-inspect-variables
    #:debug-new-breakpoint
    #:debug-next
    #:debug-object
    #:debug-object-children
    #:debug-object-children-resolve
    #:debug-object-column
    #:debug-object-data
    #:debug-object-environment
    #:debug-object-id
    #:debug-object-line
    #:debug-object-name
    #:debug-object-parent
    #:debug-object-source
    #:debug-object-type
    #:debug-object-value
    #:debug-out
    #:debug-remove-breakpoint
    #:debug-scope
    #:debug-source
    #:debug-source-name
    #:debug-source-path
    #:debug-stop
    #:debug-variable
    #:edit
    #:enqueue-input
    #:evaluate-code
    #:get-comm
    #:handling-comm-errors
    #:handling-errors
    #:*html-output*
    #:inform
    #:inspect-code
    #:*kernel*
    #:kernel
    #:kernel-debugger-started
    #:kernel-prompt-prefix
    #:kernel-prompt-suffix
    #:make-uuid
    #:*markdown-output*
    #:on-comm-close
    #:on-comm-message
    #:on-comm-open
    #:*page-output*
    #:quit
    #:remove-debug-object
    #:remove-thread
    #:run-kernel
    #:send-comm-close
    #:send-comm-message
    #:send-comm-open
    #:send-debug-event
    #:start
    #:stop
    #:*thread-id*
    #:user-thread-p
    ; installer
    #:command-line
    #:install
    #:installer
    #:installer-class
    #:installer-display-name
    #:installer-implementation
    #:installer-kernel-name
    #:installer-language
    #:installer-local
    #:installer-local-systems
    #:installer-path
    #:installer-prefix
    #:installer-resources
    #:installer-systems
    #:system-installer
    #:system-bundle-installer
    #:user-image-installer
    #:user-installer))


(defpackage #:jupyter/markdown-formatter
  (:use #:common-lisp)
  (:nicknames :mdf)
  (:documentation "Various format extensions for markdown")
  (:export
    #:code
    #:*indent-level*
    #:pre
    #:text))


(defpackage #:jupyter/widgets
  (:use #:common-lisp)
  (:nicknames :jw :jupyter-widgets)
  (:documentation "Package for core Jupyter Widget support.")
  (:export
    #:accordion
    #:audio
    #:blur
    #:bounded-float-text
    #:bounded-int-text
    #:box
    #:button
    #:button-style
    #:checkbox
    #:color-picker
    #:combobox
    #:controller
    #:controller-axis
    #:controller-button
    #:date-picker
    #:defwidget
    #:description-style
    #:directional-link
    #:dom-widget
    #:dropdown
    #:file-upload
    #:float-log-slider
    #:float-progress
    #:float-range-slider
    #:float-slider
    #:float-text
    #:focus
    #:grid-box
    #:has-traits
    #:h-box
    #:html
    #:html-math
    #:image
    #:int-progress
    #:int-range-slider
    #:int-slider
    #:int-text
    #:label
    #:layout
    #:link
    #:make-output-widget-stream
    #:notify-trait-change
    #:observe
    #:on-button-click
    #:on-custom-message
    #:on-trait-change
    #:output
    #:password
    #:play
    #:progress-style
    #:radio-buttons
    #:register-widgets
    #:select
    #:selection-range-slider
    #:selection-slider
    #:select-multiple
    #:send-custom
    #:sidecar
    #:slider-style
    #:style
    #:styled-widget
    #:tab
    #:text
    #:text-area
    #:toggle-button
    #:toggle-buttons
    #:toggle-button-style
    #:trait-metaclass
    #:valid
    #:v-box
    #:video
    #:widget
    #:widget-accept
    #:widget-align-content
    #:widget-align-items
    #:widget-align-self
    #:widget-autoplay
    #:widget-axes
    #:widget-bar-color
    #:widget-bar-style
    #:widget-base
    #:widget-border
    #:widget-bottom
    #:widget-box-style
    #:widget-button-color
    #:widget-buttons
    #:widget-button-style
    #:widget-button-width
    #:widget-children
    #:widget-concise
    #:widget-connected
    #:widget-continuous-update
    #:widget-controls
    #:widget-data
    #:widget-description
    #:widget-description-tooltip
    #:widget-description-width
    #:widget-disabled
    #:widget-display
    #:widget-%dom-classes
    #:widget-ensure-option
    #:widget-error
    #:widget-flex
    #:widget-flex-flow
    #:widget-font-weight
    #:widget-format
    #:widget-grid-area
    #:widget-grid-auto-columns
    #:widget-grid-auto-flow
    #:widget-grid-auto-rows
    #:widget-grid-column
    #:widget-grid-gap
    #:widget-grid-row
    #:widget-grid-template-areas
    #:widget-grid-template-columns
    #:widget-grid-template-rows
    #:widget-handle-color
    #:widget-height
    #:widget-icon
    #:widget-icons
    #:widget-indent
    #:widget-index
    #:widget-interval
    #:widget-justify-content
    #:widget-justify-items
    #:widget-layout
    #:widget-left
    #:widget-loop
    #:widget-mapping
    #:widget-margin
    #:widget-max
    #:widget-max-height
    #:widget-max-width
    #:widget-metadata
    #:widget-min
    #:widget-min-height
    #:widget-min-width
    #:widget-%module-module
    #:widget-%module-module-version
    #:widget-%module-name
    #:widget-msg-id
    #:widget-multiple
    #:widget-name
    #:widget-object-fit
    #:widget-object-position
    #:widget-on-trait-change
    #:widget-options
    #:widget-%options-labels
    #:widget-order
    #:widget-orientation
    #:widget-outputs
    #:widget-overflow
    #:widget-overflow-x
    #:widget-overflow-y
    #:widget-padding
    #:widget-placeholder
    #:widget-%playing
    #:widget-pressed
    #:widget-readout
    #:widget-readout-format
    #:widget-%repeat
    #:widget-right
    #:widget-rows
    #:widget-selected-index
    #:widget-show-repeat
    #:widget-source
    #:widget-step
    #:widget-style
    #:widget-target
    #:widget-timestamp
    #:widget-%titles
    #:widget-tooltip
    #:widget-tooltips
    #:widget-top
    #:widget-value
    #:widget-%view-module
    #:widget-%view-module-version
    #:widget-%view-name
    #:widget-visibility
    #:widget-width
    #:with-output
    ;; interactive
    #:make-interactive-alist
    #:make-interactive-hash-table
    #:make-interactive-plist))

(defpackage #:jupyter/common-lisp
  (:nicknames :cl-jupyter :clj :common-lisp-jupyter)
  (:use #:common-lisp)
  (:documentation "Provides Common Lisp kernel support.")
  (:export
    #:install
    #:install-image
    #:install-roswell
    #:kernel))

(defpackage #:jupyter/convert
  (:use #:common-lisp)
  (:nicknames :jupyter-convert)
  (:documentation "Provides LISP source code conversion to notebooks.")
  (:export
    #:to-notebook))

(in-package #:jupyter)


#+sbcl (require :sb-introspect)

