(defpackage #:jupyter
  (:use #:common-lisp)
  (:import-from :shasht #:make-object)
  (:nicknames :j)
  (:documentation "Core package for Jupyter support including kernel and installer abstract classes.")
  (:export #:make-object
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
	   #:debug-evaluate-code
	   #:debug-evaluate-form
	   #:*debug-frame*
	   #:debug-frame
	   #:*debugger*
	   #:debug-in
	   #:debug-initialize
	   #:debug-inspect-variables
	   #:debug-new-breakpoint
	   #:debug-next
	   #:debug-module
	   #:debug-modules
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
	   #:evaluate-form
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
  (:export #:code
	   #:*indent-level*
	   #:pre
	   #:text))


(defpackage #:jupyter/widgets
  (:use #:common-lisp)
  (:nicknames :jw :jupyter-widgets)
  (:documentation "Package for core Jupyter Widget support.")
  (:export #:accordion
	   #:audio
	   #:blur
	   #:bounded-float-text
	   #:bounded-int-text
	   #:box
	   #:button
	   #:button-style
	   #:checkbox
	   #:checkbox-style
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
	   #:h-box
	   #:has-traits
	   #:html
	   #:html-math
	   #:html-math-style
	   #:html-style
	   #:image
	   #:int-progress
	   #:int-range-slider
	   #:int-slider
	   #:int-text
	   #:label
	   #:label-style
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
	   #:select-multiple
	   #:selection-range-slider
	   #:selection-slider
	   #:send-custom
	   #:sidecar
	   #:slider-style
	   #:style
	   #:styled-widget
	   #:tab
	   #:text
	   #:text-area
	   #:text-style
	   #:toggle-button
	   #:toggle-button-style
	   #:toggle-buttons
	   #:toggle-buttons-style
	   #:trait-metaclass
	   #:v-box
	   #:valid
	   #:video
	   #:widget
	   #:widget-%dom-classes
	   #:widget-%module-module
	   #:widget-%module-module-version
	   #:widget-%module-name
	   #:widget-%options-labels
	   #:widget-%playing
	   #:widget-%repeat
	   #:widget-%titles
	   #:widget-%view-module
	   #:widget-%view-module-version
	   #:widget-%view-name
	   #:widget-accept
	   #:widget-align-content
	   #:widget-align-items
	   #:widget-align-self
	   #:widget-autoplay
	   #:widget-axes
	   #:widget-background
	   #:widget-bar-color
	   #:widget-bar-style
	   #:widget-base
	   #:widget-border
	   #:widget-border-bottom
	   #:widget-border-left
	   #:widget-border-right
	   #:widget-border-top
	   #:widget-bottom
	   #:widget-box-style
	   #:widget-button-color
	   #:widget-button-style
	   #:widget-button-width
	   #:widget-buttons
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
	   #:widget-ensure-option
	   #:widget-error
	   #:widget-flex
	   #:widget-flex-flow
	   #:widget-font-color
	   #:widget-font-family
	   #:widget-font-size
	   #:widget-font-style
	   #:widget-font-variant
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
	   #:widget-msg-id
	   #:widget-multiple
	   #:widget-name
	   #:widget-object-fit
	   #:widget-object-position
	   #:widget-on-trait-change
	   #:widget-options
	   #:widget-order
	   #:widget-orientation
	   #:widget-outputs
	   #:widget-overflow
	   #:widget-padding
	   #:widget-placeholder
	   #:widget-pressed
	   #:widget-readout
	   #:widget-readout-format
	   #:widget-right
	   #:widget-rows
	   #:widget-selected-index
	   #:widget-show-repeat
	   #:widget-source
	   #:widget-step
	   #:widget-style
	   #:widget-tabbable
	   #:widget-target
	   #:widget-text-decoration
	   #:widget-timestamp
	   #:widget-tooltip
	   #:widget-tooltips
	   #:widget-top
	   #:widget-value
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
  (:export #:debug-environment
	   #:install
	   #:install-image
	   #:install-roswell
	   #:kernel))

(defpackage #:jupyter/convert
  (:use #:common-lisp)
  (:nicknames :jupyter-convert)
  (:documentation "Provides LISP source code conversion to notebooks.")
  (:export #:to-notebook))

(in-package #:jupyter)

#+sbcl (require :sb-introspect)

