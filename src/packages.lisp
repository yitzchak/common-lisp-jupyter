(defpackage #:jupyter
  (:use #:cl #:iterate)
  (:export
    ; utils
    #:info
    #:install-kernel
    ; results
    #:file
    #:gif-file
    #:html
    #:inline-result
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
    ; kernel
    #:*page-output*
    #:clear
    #:code-is-complete
    #:comm
    #:comm-id
    #:complete-code
    #:create-comm
    #:enqueue-input
    #:evaluate-code
    #:handling-errors
    #:inspect-code
    #:kernel
    #:kernel-prompt-prefix
    #:kernel-prompt-suffix
    #:on-comm-close
    #:on-comm-message
    #:on-comm-open
    #:quit-condition
    #:run-kernel
    #:get-comm
    #:send-comm-close
    #:send-comm-message
    #:send-comm-open
    #:send-result))

(defpackage #:jupyter-widgets
  (:use #:cl #:iterate)
  (:export
    #:accordion
    #:bounded-float-text
    #:bounded-int-text
    #:box
    #:button
    #:button-style
    #:checkbox
    #:color-picker
    #:controller
    #:controller-axis
    #:controller-button
    #:date-picker
    #:description-style
    #:dom-widget
    #:float-log-slider
    #:float-progress
    #:float-range-slider
    #:float-slider
    #:float-text
    #:grid-box
    #:h-box
    #:int-progress
    #:int-range-slider
    #:int-slider
    #:int-text
    #:layout
    #:make-widget
    #:on-button-click
    #:password
    #:progress-style
    #:radio-buttons
    #:select
    #:select-multiple
    #:selection-range-slider
    #:selection-slider
    #:slider-style
    #:tab
    #:text
    #:text-area
    #:v-box
    #:widget-%dom-classes
    #:widget-%module-module
    #:widget-%module-module-version
    #:widget-%module-name
    #:widget-%options-labels
    #:widget-%titles
    #:widget-%view-module
    #:widget-%view-module-version
    #:widget-%view-name
    #:widget-align-content
    #:widget-align-items
    #:widget-align-self
    #:widget-axes
    #:widget-bar-color
    #:widget-bar-style
    #:widget-base
    #:widget-border
    #:widget-bottom
    #:widget-box-style
    #:widget-button-color
    #:widget-button-style
    #:widget-buttons
    #:widget-children
    #:widget-concise
    #:widget-connected
    #:widget-continuous-update
    #:widget-description
    #:widget-description-tooltip
    #:widget-description-width
    #:widget-disabled
    #:widget-display
    #:widget-flex
    #:widget-flex-flow
    #:widget-font-weight
    #:widget-grid-area
    #:widget-grid-auto-columns
    #:widget-grid-auto-flow
    #:widget-grid-column
    #:widget-grid-gap
    #:widget-grid-template-areas
    #:widget-grid-template-columns
    #:widget-grid-template-rows
    #:widget-handle-color
    #:widget-height
    #:widget-icon
    #:widget-indent
    #:widget-index
    #:widget-justify-content
    #:widget-layout
    #:widget-left
    #:widget-mapping
    #:widget-margin
    #:widget-max
    #:widget-max-height
    #:widget-max-width
    #:widget-min
    #:widget-min-height
    #:widget-min-width
    #:widget-name
    #:widget-order
    #:widget-orientation
    #:widget-overflow
    #:widget-overflow-x
    #:widget-overflow-y
    #:widget-padding
    #:widget-placeholder
    #:widget-pressed
    #:widget-readout
    #:widget-readout-format
    #:widget-right
    #:widget-rows
    #:widget-selected-index
    #:widget-step
    #:widget-style
    #:widget-timestamp
    #:widget-tooltip
    #:widget-top
    #:widget-value
    #:widget-visibility
    #:widget-width
    #:widget))

(defpackage #:common-lisp-jupyter
  (:nicknames :cl-jupyter)
  (:use #:cl #:iterate)
  (:export
    #:kernel))

(in-package #:jupyter)
