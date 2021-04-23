(defpackage #:jupyter
  (:use #:common-lisp)
  (:import-from :shasht #:make-object)
  (:nicknames :j)
  (:documentation "Core package for Jupyter support including kernel and installer abstract classes.")
  (:export
    #:make-object
    ; log
    #:inform
    ; results
    #:display-data
    #:execute-result
    #:mime-bundle-data
    #:mime-bundle-metadata
    #:file
    #:gif-file
    #:html
    #:inline-result
    #:javascript
    #:jpeg
    #:jpeg-file
    #:latex
    #:make-error-result
    #:make-file-result
    #:make-inline-result
    #:make-lisp-result
    #:markdown
    #:pdf-file
    #:png
    #:png-file
    #:ps-file
    #:quit-eval-error-p
    #:render
    #:result
    #:svg
    #:svg-file
    #:text
    ; matches
    #:match-set-add
    #:make-offset-match-set
    ; kernel
    #:clear
    #:code-is-complete
    #:comm
    #:comm-id
    #:complete-code
    #:create-comm
    #:*debugger*
    #:debugging-errors
    #:enqueue-input
    #:evaluate-code
    #:get-comm
    #:handling-errors
    #:inform
    #:inspect-code
    #:kernel
    #:kernel-prompt-prefix
    #:kernel-prompt-suffix
    #:make-uuid
    #:on-comm-close
    #:on-comm-message
    #:on-comm-open
    #:*page-output*
    #:quit
    #:quit-condition
    #:run-kernel
    #:send-comm-close
    #:send-comm-message
    #:send-comm-open
    #:send-result
    #:start
    #:stop
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


(defpackage #:jupyter-widgets
  (:use #:common-lisp)
  (:nicknames :jw)
  (:documentation "Package for core Jupyter Widget support.")
  (:export
    #:accordion
    #:audio
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
    #:description-style
    #:directional-link
    #:%display
    #:display
    #:dom-widget
    #:dropdown
    #:file-upload
    #:float-log-slider
    #:float-progress
    #:float-range-slider
    #:float-slider
    #:float-text
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

(defpackage #:common-lisp-jupyter
  (:nicknames :cl-jupyter :clj)
  (:use #:common-lisp)
  (:documentation "Provides Common Lisp kernel support.")
  (:export
    #:install
    #:install-image
    #:install-roswell
    #:kernel))

(defpackage #:jupyter-convert
  (:use #:common-lisp)
  (:documentation "Provides LISP source code conversion to notebooks.")
  (:export
    #:to-notebook))

(in-package #:jupyter)


#+sbcl (require :sb-introspect)

