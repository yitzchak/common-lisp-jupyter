
# JUPYTER/WIDGETS

Package for core Jupyter Widget support.

## Nicknames

jupyter\-widgets, jw

## Exports

accordion, audio, blur, bounded\-float\-text, bounded\-int\-text, box, button, 
button\-style, checkbox, color\-picker, combobox, controller, 
controller\-axis, controller\-button, date\-picker, defwidget, 
description\-style, directional\-link, dom\-widget, dropdown, file\-upload, 
float\-log\-slider, float\-progress, float\-range\-slider, float\-slider, 
float\-text, focus, grid\-box, h\-box, has\-traits, html, html\-math, image, 
int\-progress, int\-range\-slider, int\-slider, int\-text, label, layout, 
link, make\-accordion, make\-audio, make\-bounded\-float\-text, 
make\-bounded\-int\-text, make\-box, make\-button, make\-button\-style, 
make\-checkbox, make\-color\-picker, make\-combobox, make\-controller, 
make\-controller\-axis, make\-controller\-button, make\-date\-picker, 
make\-description\-style, make\-directional\-link, make\-dropdown, 
make\-file\-upload, make\-float\-log\-slider, make\-float\-progress, 
make\-float\-range\-slider, make\-float\-slider, make\-float\-text, 
make\-grid\-box, make\-h\-box, make\-html, make\-html\-math, make\-image, 
make\-int\-progress, make\-int\-range\-slider, make\-int\-slider, 
make\-int\-text, make\-interactive\-alist, make\-interactive\-hash\-table, 
make\-interactive\-plist, make\-label, make\-layout, make\-link, make\-output, 
make\-output\-widget\-stream, make\-password, make\-play, 
make\-progress\-style, make\-radio\-buttons, make\-select, 
make\-select\-multiple, make\-selection\-range\-slider, 
make\-selection\-slider, make\-sidecar, make\-slider\-style, make\-tab, 
make\-text, make\-text\-area, make\-toggle\-button, make\-toggle\-buttons, 
make\-toggle\-buttons\-style, make\-v\-box, make\-valid, make\-video, 
notify\-trait\-change, observe, on\-button\-click, on\-custom\-message, 
on\-trait\-change, output, password, play, progress\-style, radio\-buttons, 
register\-widgets, select, select\-multiple, selection\-range\-slider, 
selection\-slider, send\-custom, sidecar, slider\-style, style, 
styled\-widget, tab, text, text\-area, toggle\-button, toggle\-button\-style, 
toggle\-buttons, trait\-metaclass, v\-box, valid, video, widget, 
widget\-%dom\-classes, widget\-%module\-module, 
widget\-%module\-module\-version, widget\-%module\-name, 
widget\-%options\-labels, widget\-%playing, widget\-%repeat, widget\-%titles, 
widget\-%view\-module, widget\-%view\-module\-version, widget\-%view\-name, 
widget\-accept, widget\-align\-content, widget\-align\-items, 
widget\-align\-self, widget\-autoplay, widget\-axes, widget\-bar\-color, 
widget\-bar\-style, widget\-base, widget\-border, widget\-bottom, 
widget\-box\-style, widget\-button\-color, widget\-button\-style, 
widget\-button\-width, widget\-buttons, widget\-children, widget\-concise, 
widget\-connected, widget\-continuous\-update, widget\-controls, widget\-data, 
widget\-description, widget\-description\-tooltip, widget\-description\-width, 
widget\-disabled, widget\-display, widget\-ensure\-option, widget\-error, 
widget\-flex, widget\-flex\-flow, widget\-font\-weight, widget\-format, 
widget\-grid\-area, widget\-grid\-auto\-columns, widget\-grid\-auto\-flow, 
widget\-grid\-auto\-rows, widget\-grid\-column, widget\-grid\-gap, 
widget\-grid\-row, widget\-grid\-template\-areas, 
widget\-grid\-template\-columns, widget\-grid\-template\-rows, 
widget\-handle\-color, widget\-height, widget\-icon, widget\-icons, 
widget\-indent, widget\-index, widget\-interval, widget\-justify\-content, 
widget\-justify\-items, widget\-layout, widget\-left, widget\-loop, 
widget\-mapping, widget\-margin, widget\-max, widget\-max\-height, 
widget\-max\-width, widget\-metadata, widget\-min, widget\-min\-height, 
widget\-min\-width, widget\-msg\-id, widget\-multiple, widget\-name, 
widget\-object\-fit, widget\-object\-position, widget\-on\-trait\-change, 
widget\-options, widget\-order, widget\-orientation, widget\-outputs, 
widget\-overflow, widget\-overflow\-x, widget\-overflow\-y, widget\-padding, 
widget\-placeholder, widget\-pressed, widget\-readout, 
widget\-readout\-format, widget\-right, widget\-rows, widget\-selected\-index, 
widget\-show\-repeat, widget\-source, widget\-step, widget\-style, 
widget\-target, widget\-timestamp, widget\-tooltip, widget\-tooltips, 
widget\-top, widget\-value, widget\-visibility, widget\-width, 

## accordion

#### Class

Displays children each on a separate accordion page.

##### Precedence List

accordion, box, dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- box\-style &mdash; Use a predefined styling for the box.
    - :initarg :box\-style
- children &mdash; List of widget children.
    - :initarg :children
- %titles &mdash; Titles of the pages.
    - :initarg :%titles
- selected\-index &mdash; The index of the selected page. This is either an integer selecting a particular sub-widget, or nil to have no widgets selected.
    - :initarg :selected\-index

##### Methods

- validate\-trait

    ```lisp
    (validate-trait (w accordion) (type (eql int)) name value)
    ```


- widget\-selected\-index &mdash; The index of the selected page. This is either an integer selecting a particular sub-widget, or nil to have no widgets selected.

    ```lisp
    (setf (widget-selected-index (accordion accordion)) new-value)
    ```


- widget\-selected\-index &mdash; The index of the selected page. This is either an integer selecting a particular sub-widget, or nil to have no widgets selected.

    ```lisp
    (widget-selected-index (accordion accordion))
    ```


- widget\-%titles &mdash; Titles of the pages.

    ```lisp
    (setf (widget-%titles (accordion accordion)) new-value)
    ```


- widget\-%titles &mdash; Titles of the pages.

    ```lisp
    (widget-%titles (accordion accordion))
    ```


- widget\-children &mdash; List of widget children.

    ```lisp
    (setf (widget-children (box box)) new-value)
    ```


- widget\-children &mdash; List of widget children.

    ```lisp
    (widget-children (box box))
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (setf (widget-box-style (box box)) new-value)
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (widget-box-style (box box))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## audio

#### Class

Displays an audio clip as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
audio data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to "mp3").

If you pass `"url"` to the `"format"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8.

##### Precedence List

audio, dom\-widget, widget, has\-traits, comm, source, format\-slot, byte\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; The value as a byte string.
    - :initarg :value
- format &mdash; The format of the media.
    - :initarg :format
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- autoplay &mdash; When true, the audio starts when it's displayed.
    - :initarg :autoplay
- controls &mdash; Specifies that media controls should be displayed (such as a play/pause button etc)
    - :initarg :controls
- loop &mdash; When true, the audio will start from the beginning after finishing
    - :initarg :loop

##### Methods

- widget\-loop &mdash; When true, the audio will start from the beginning after finishing

    ```lisp
    (setf (widget-loop (audio audio)) new-value)
    ```


- widget\-loop &mdash; When true, the audio will start from the beginning after finishing

    ```lisp
    (widget-loop (audio audio))
    ```


- widget\-controls &mdash; Specifies that media controls should be displayed (such as a play/pause button etc)

    ```lisp
    (setf (widget-controls (audio audio)) new-value)
    ```


- widget\-controls &mdash; Specifies that media controls should be displayed (such as a play/pause button etc)

    ```lisp
    (widget-controls (audio audio))
    ```


- widget\-autoplay &mdash; When true, the audio starts when it's displayed.

    ```lisp
    (setf (widget-autoplay (audio audio)) new-value)
    ```


- widget\-autoplay &mdash; When true, the audio starts when it's displayed.

    ```lisp
    (widget-autoplay (audio audio))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-format &mdash; The format of the media.

    ```lisp
    (setf (widget-format (format-slot format-slot)) new-value)
    ```


- widget\-format &mdash; The format of the media.

    ```lisp
    (widget-format (format-slot format-slot))
    ```


- widget\-value &mdash; The value as a byte string.

    ```lisp
    (setf (widget-value (byte-value-slot byte-value-slot)) new-value)
    ```


- widget\-value &mdash; The value as a byte string.

    ```lisp
    (widget-value (byte-value-slot byte-value-slot))
    ```


## blur

## Function

```lisp
(blur widget)
```

## bounded\-float\-text

#### Class

Displays a float value within a textbox. Value must be within the range
specified. For a textbox in which the value doesn't need to be within a specific
range, use float-text.

##### Precedence List

bounded\-float\-text, float\-text, base\-text, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, continuous\-update\-slot, float\-step\-slot, float\-value\-slot, float\-min\-max\-slots, standard\-object, slot\-object, t

##### Slots

- max &mdash; Max value
    - :initarg :max
- min &mdash; Min value
    - :initarg :min
- value &mdash; Float value
    - :initarg :value
- step &mdash; Minimum step to increment the value
    - :initarg :step
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip

##### Methods

- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (setf (widget-step (float-step-slot float-step-slot)) new-value)
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (widget-step (float-step-slot float-step-slot))
    ```


- widget\-value &mdash; Float value

    ```lisp
    (setf (widget-value (float-value-slot float-value-slot)) new-value)
    ```


- widget\-value &mdash; Float value

    ```lisp
    (widget-value (float-value-slot float-value-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w float-min-max-slots) (type (eql float-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w float-min-max-slots) (type (eql float)) name value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (setf (widget-min (float-min-max-slots float-min-max-slots)) new-value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (widget-min (float-min-max-slots float-min-max-slots))
    ```


- widget\-max &mdash; Max value

    ```lisp
    (setf (widget-max (float-min-max-slots float-min-max-slots)) new-value)
    ```


- widget\-max &mdash; Max value

    ```lisp
    (widget-max (float-min-max-slots float-min-max-slots))
    ```


## bounded\-int\-text

#### Class

Textbox widget that represents an integer bounded from above and below.

##### Precedence List

bounded\-int\-text, int\-text, base\-text, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, continuous\-update\-slot, int\-step\-slot, int\-value\-slot, int\-min\-max\-slots, standard\-object, slot\-object, t

##### Slots

- max &mdash; Max value
    - :initarg :max
- min &mdash; Min value
    - :initarg :min
- value &mdash; Int value
    - :initarg :value
- step &mdash; Minimum step to increment the value
    - :initarg :step
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip

##### Methods

- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (setf (widget-step (int-step-slot int-step-slot)) new-value)
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (widget-step (int-step-slot int-step-slot))
    ```


- widget\-value &mdash; Int value

    ```lisp
    (setf (widget-value (int-value-slot int-value-slot)) new-value)
    ```


- widget\-value &mdash; Int value

    ```lisp
    (widget-value (int-value-slot int-value-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w int-min-max-slots) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w int-min-max-slots) (type (eql int)) name value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (setf (widget-min (int-min-max-slots int-min-max-slots)) new-value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (widget-min (int-min-max-slots int-min-max-slots))
    ```


- widget\-max &mdash; Max value

    ```lisp
    (setf (widget-max (int-min-max-slots int-min-max-slots)) new-value)
    ```


- widget\-max &mdash; Max value

    ```lisp
    (widget-max (int-min-max-slots int-min-max-slots))
    ```


## box

#### Class

Displays multiple widgets in a group. The widgets are laid out horizontally.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar title-widget (make-instance 'html :value "<em>Box Example</em>"))
(defvar slider (make-instance 'int-slider))
(make-instance 'box :children (list title-widget slider))
```

##### Precedence List

box, dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- box\-style &mdash; Use a predefined styling for the box.
    - :initarg :box\-style
- children &mdash; List of widget children.
    - :initarg :children

##### Methods

- widget\-children &mdash; List of widget children.

    ```lisp
    (setf (widget-children (box box)) new-value)
    ```


- widget\-children &mdash; List of widget children.

    ```lisp
    (widget-children (box box))
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (setf (widget-box-style (box box)) new-value)
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (widget-box-style (box box))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## button

#### Class

Button widget.
This widget has an `on-button-click` method that allows you to listen for the
user clicking on the button.  The click event itself is stateless.

##### Precedence List

button, styled\-widget, dom\-widget, widget, has\-traits, comm, source, button\-style\-slot, disabled\-slot, icon\-slot, tooltip\-slot, standard\-object, slot\-object, t

##### Slots

- tooltip &mdash; Tooltip caption.
    - :initarg :tooltip
- icon &mdash; Font-awesome icon name, without the 'fa-' prefix.
    - :initarg :icon
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- button\-style &mdash; Use a predefined styling for the button.
    - :initarg :button\-style
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Button label.
    - :initarg :description
- on\-click
    - :initarg :on\-click

##### Methods

- on\-custom\-message

    ```lisp
    (on-custom-message (w button) content buffers)
    ```


- widget\-on\-click &mdash; automatically generated writer method

    ```lisp
    (setf (widget-on-click (button button)) new-value)
    ```


- widget\-on\-click &mdash; automatically generated reader method

    ```lisp
    (widget-on-click (button button))
    ```


- widget\-description &mdash; Button label.

    ```lisp
    (setf (widget-description (button button)) new-value)
    ```


- widget\-description &mdash; Button label.

    ```lisp
    (widget-description (button button))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-button\-style &mdash; Use a predefined styling for the button.

    ```lisp
    (setf (widget-button-style (button-style-slot button-style-slot)) new-value)
    ```


- widget\-button\-style &mdash; Use a predefined styling for the button.

    ```lisp
    (widget-button-style (button-style-slot button-style-slot))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-icon &mdash; Font-awesome icon name, without the 'fa-' prefix.

    ```lisp
    (setf (widget-icon (icon-slot icon-slot)) new-value)
    ```


- widget\-icon &mdash; Font-awesome icon name, without the 'fa-' prefix.

    ```lisp
    (widget-icon (icon-slot icon-slot))
    ```


- widget\-tooltip &mdash; Tooltip caption.

    ```lisp
    (setf (widget-tooltip (tooltip-slot tooltip-slot)) new-value)
    ```


- widget\-tooltip &mdash; Tooltip caption.

    ```lisp
    (widget-tooltip (tooltip-slot tooltip-slot))
    ```


## button\-style

#### Class

Button style widget

##### Precedence List

button\-style, style, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- button\-color &mdash; Color of the button
    - :initarg :button\-color
- font\-weight &mdash; Button text font weight.
    - :initarg :font\-weight

##### Methods

- widget\-font\-weight &mdash; Button text font weight.

    ```lisp
    (setf (widget-font-weight (button-style button-style)) new-value)
    ```


- widget\-font\-weight &mdash; Button text font weight.

    ```lisp
    (widget-font-weight (button-style button-style))
    ```


- widget\-button\-color &mdash; Color of the button

    ```lisp
    (setf (widget-button-color (button-style button-style)) new-value)
    ```


- widget\-button\-color &mdash; Color of the button

    ```lisp
    (widget-button-color (button-style button-style))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## checkbox

#### Class

Displays a boolean `value` in the form of a checkbox.

##### Precedence List

checkbox, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, bool\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Bool value
    - :initarg :value
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- indent &mdash; Indent the control to align with other controls with a description.
    - :initarg :indent

##### Methods

- widget\-indent &mdash; Indent the control to align with other controls with a description.

    ```lisp
    (setf (widget-indent (checkbox checkbox)) new-value)
    ```


- widget\-indent &mdash; Indent the control to align with other controls with a description.

    ```lisp
    (widget-indent (checkbox checkbox))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-value &mdash; Bool value

    ```lisp
    (setf (widget-value (bool-value-slot bool-value-slot)) new-value)
    ```


- widget\-value &mdash; Bool value

    ```lisp
    (widget-value (bool-value-slot bool-value-slot))
    ```


## color\-picker

#### Class

Color picker widget

##### Precedence List

color\-picker, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, standard\-object, slot\-object, t

##### Slots

- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- concise &mdash; Display short version with just a color selector.
    - :initarg :concise
- value &mdash; The color value.
    - :initarg :value

##### Methods

- widget\-value &mdash; The color value.

    ```lisp
    (setf (widget-value (color-picker color-picker)) new-value)
    ```


- widget\-value &mdash; The color value.

    ```lisp
    (widget-value (color-picker color-picker))
    ```


- widget\-concise &mdash; Display short version with just a color selector.

    ```lisp
    (setf (widget-concise (color-picker color-picker)) new-value)
    ```


- widget\-concise &mdash; Display short version with just a color selector.

    ```lisp
    (widget-concise (color-picker color-picker))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


## combobox

#### Class

##### Precedence List

combobox, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, continuous\-update\-slot, disabled\-slot, placeholder\-slot, string\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; String value
    - :initarg :value
- placeholder &mdash; Placeholder text to display when nothing has been typed.
    - :initarg :placeholder
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- ensure\-option &mdash; If set, ensure value is in options. Implies continuous_update=False.
    - :initarg :ensure\-option
- options &mdash; Dropdown options for the combobox
    - :initarg :options

##### Methods

- widget\-options &mdash; Dropdown options for the combobox

    ```lisp
    (setf (widget-options (combobox combobox)) new-value)
    ```


- widget\-options &mdash; Dropdown options for the combobox

    ```lisp
    (widget-options (combobox combobox))
    ```


- widget\-ensure\-option &mdash; If set, ensure value is in options. Implies continuous_update=False.

    ```lisp
    (setf (widget-ensure-option (combobox combobox)) new-value)
    ```


- widget\-ensure\-option &mdash; If set, ensure value is in options. Implies continuous_update=False.

    ```lisp
    (widget-ensure-option (combobox combobox))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (setf (widget-placeholder (placeholder-slot placeholder-slot)) new-value)
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (widget-placeholder (placeholder-slot placeholder-slot))
    ```


- widget\-value &mdash; String value

    ```lisp
    (setf (widget-value (string-value-slot string-value-slot)) new-value)
    ```


- widget\-value &mdash; String value

    ```lisp
    (widget-value (string-value-slot string-value-slot))
    ```


## controller

#### Class

Represents a game controller.

##### Precedence List

controller, dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- axes &mdash; The axes on the gamepad.
    - :initarg :axes
- buttons &mdash; The buttons on the gamepad.
    - :initarg :buttons
- connected &mdash; Whether the gamepad is connected.
    - :initarg :connected
- index &mdash; The id number of the controller.
    - :initarg :index
- mapping &mdash; The name of the control mapping.
    - :initarg :mapping
- name &mdash; The name of the controller.
    - :initarg :name
- timestamp &mdash; The last time the data from this gamepad was updated.
    - :initarg :timestamp

##### Methods

- widget\-timestamp &mdash; The last time the data from this gamepad was updated.

    ```lisp
    (setf (widget-timestamp (controller controller)) new-value)
    ```


- widget\-timestamp &mdash; The last time the data from this gamepad was updated.

    ```lisp
    (widget-timestamp (controller controller))
    ```


- widget\-name &mdash; The name of the controller.

    ```lisp
    (setf (widget-name (controller controller)) new-value)
    ```


- widget\-name &mdash; The name of the controller.

    ```lisp
    (widget-name (controller controller))
    ```


- widget\-mapping &mdash; The name of the control mapping.

    ```lisp
    (setf (widget-mapping (controller controller)) new-value)
    ```


- widget\-mapping &mdash; The name of the control mapping.

    ```lisp
    (widget-mapping (controller controller))
    ```


- widget\-index &mdash; The id number of the controller.

    ```lisp
    (setf (widget-index (controller controller)) new-value)
    ```


- widget\-index &mdash; The id number of the controller.

    ```lisp
    (widget-index (controller controller))
    ```


- widget\-connected &mdash; Whether the gamepad is connected.

    ```lisp
    (setf (widget-connected (controller controller)) new-value)
    ```


- widget\-connected &mdash; Whether the gamepad is connected.

    ```lisp
    (widget-connected (controller controller))
    ```


- widget\-buttons &mdash; The buttons on the gamepad.

    ```lisp
    (setf (widget-buttons (controller controller)) new-value)
    ```


- widget\-buttons &mdash; The buttons on the gamepad.

    ```lisp
    (widget-buttons (controller controller))
    ```


- widget\-axes &mdash; The axes on the gamepad.

    ```lisp
    (setf (widget-axes (controller controller)) new-value)
    ```


- widget\-axes &mdash; The axes on the gamepad.

    ```lisp
    (widget-axes (controller controller))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## controller\-axis

#### Class

Represents a gamepad or joystick axis.

##### Precedence List

controller\-axis, dom\-widget, widget, has\-traits, comm, source, float\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Float value
    - :initarg :value
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout

##### Methods

- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-value &mdash; Float value

    ```lisp
    (setf (widget-value (float-value-slot float-value-slot)) new-value)
    ```


- widget\-value &mdash; Float value

    ```lisp
    (widget-value (float-value-slot float-value-slot))
    ```


## controller\-button

#### Class

Represents a gamepad or joystick button.

##### Precedence List

controller\-button, dom\-widget, widget, has\-traits, comm, source, float\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Float value
    - :initarg :value
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- pressed &mdash; Whether the button is pressed.
    - :initarg :pressed

##### Methods

- widget\-pressed &mdash; Whether the button is pressed.

    ```lisp
    (setf (widget-pressed (controller-button controller-button)) new-value)
    ```


- widget\-pressed &mdash; Whether the button is pressed.

    ```lisp
    (widget-pressed (controller-button controller-button))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-value &mdash; Float value

    ```lisp
    (setf (widget-value (float-value-slot float-value-slot)) new-value)
    ```


- widget\-value &mdash; Float value

    ```lisp
    (widget-value (float-value-slot float-value-slot))
    ```


## date\-picker

#### Class

Date picker widget

##### Precedence List

date\-picker, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, standard\-object, slot\-object, t

##### Slots

- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- value &mdash; The date value.
    - :initarg :value

##### Methods

- widget\-value &mdash; The date value.

    ```lisp
    (setf (widget-value (date-picker date-picker)) new-value)
    ```


- widget\-value &mdash; The date value.

    ```lisp
    (widget-value (date-picker date-picker))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


## defwidget

## Macro

```lisp
(defwidget name &rest rest)
```

## description\-style

#### Class

##### Precedence List

description\-style, style, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- description\-width &mdash; Width of the description to the side of the control.
    - :initarg :description\-width

##### Methods

- widget\-description\-width &mdash; Width of the description to the side of the control.

    ```lisp
    (setf (widget-description-width (description-style description-style))
            new-value)
    ```


- widget\-description\-width &mdash; Width of the description to the side of the control.

    ```lisp
    (widget-description-width (description-style description-style))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## directional\-link

## Generic Function

Create a link between traits in the client if possible

```lisp
(directional-link source source-trait target target-trait &optional sync)
```

#### Class

A directional link

##### Precedence List

directional\-link, link, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- source
    - :initarg :source
- target
    - :initarg :target

##### Methods

- widget\-target &mdash; automatically generated writer method

    ```lisp
    (setf (widget-target (link link)) new-value)
    ```


- widget\-target &mdash; automatically generated reader method

    ```lisp
    (widget-target (link link))
    ```


- widget\-source &mdash; automatically generated writer method

    ```lisp
    (setf (widget-source (link link)) new-value)
    ```


- widget\-source &mdash; automatically generated reader method

    ```lisp
    (widget-source (link link))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## dom\-widget

#### Class

Base class for all Jupyter widgets which have DOM view.

##### Precedence List

dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout

##### Methods

- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## dropdown

#### Class

Allows you to select a single item from a dropdown.

##### Precedence List

dropdown, select, base\-select, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, %options\-labels\-slot, disabled\-slot, index\-slot, standard\-object, slot\-object, t

##### Slots

- index &mdash; Selected index
    - :initarg :index
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- %options\-labels &mdash; The labels for the options.
    - :initarg :%options\-labels
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- rows &mdash; The number of rows to display.
    - :initarg :rows
- options &mdash; The option values that correspond to the labels
    - :initarg :options

##### Methods

- on\-trait\-change

    ```lisp
    (on-trait-change (instance select) type (name (eql index)) old-value
                     new-value source)
    ```


- widget\-value

    ```lisp
    (setf (widget-value (instance select)) new-value)
    ```


- widget\-value

    ```lisp
    (widget-value (instance select))
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (setf (widget-options (select select)) new-value)
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (widget-options (select select))
    ```


- widget\-rows &mdash; The number of rows to display.

    ```lisp
    (setf (widget-rows (base-select base-select)) new-value)
    ```


- widget\-rows &mdash; The number of rows to display.

    ```lisp
    (widget-rows (base-select base-select))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int)) name value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (setf (widget-%options-labels (%options-labels-slot %options-labels-slot))
            new-value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (widget-%options-labels (%options-labels-slot %options-labels-slot))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-index &mdash; Selected index

    ```lisp
    (setf (widget-index (index-slot index-slot)) new-value)
    ```


- widget\-index &mdash; Selected index

    ```lisp
    (widget-index (index-slot index-slot))
    ```


## file\-upload

#### Class

##### Precedence List

file\-upload, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, button\-style\-slot, disabled\-slot, icon\-slot, standard\-object, slot\-object, t

##### Slots

- icon &mdash; Font-awesome icon name, without the 'fa-' prefix.
    - :initarg :icon
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- button\-style &mdash; Use a predefined styling for the button.
    - :initarg :button\-style
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- accept &mdash; If set, ensure value is in options. Implies continuous_update=False.	File types to accept, empty string for all.
    - :initarg :accept
- data &mdash; List of file content (bytes)
    - :initarg :data
- error &mdash; Error message
    - :initarg :error
- metadata &mdash; List of file metadata
    - :initarg :metadata
- multiple &mdash; If True, allow for multiple files upload
    - :initarg :multiple

##### Methods

- on\-trait\-change

    ```lisp
    (on-trait-change (instance file-upload) type (name (eql metadata))
                     old-value new-value source)
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (instance file-upload) type (name (eql data)) old-value
                     new-value source)
    ```


- widget\-value

    ```lisp
    (widget-value (instance file-upload))
    ```


- widget\-multiple &mdash; If True, allow for multiple files upload

    ```lisp
    (setf (widget-multiple (file-upload file-upload)) new-value)
    ```


- widget\-multiple &mdash; If True, allow for multiple files upload

    ```lisp
    (widget-multiple (file-upload file-upload))
    ```


- widget\-metadata &mdash; List of file metadata

    ```lisp
    (setf (widget-metadata (file-upload file-upload)) new-value)
    ```


- widget\-metadata &mdash; List of file metadata

    ```lisp
    (widget-metadata (file-upload file-upload))
    ```


- widget\-error &mdash; Error message

    ```lisp
    (setf (widget-error (file-upload file-upload)) new-value)
    ```


- widget\-error &mdash; Error message

    ```lisp
    (widget-error (file-upload file-upload))
    ```


- widget\-data &mdash; List of file content (bytes)

    ```lisp
    (setf (widget-data (file-upload file-upload)) new-value)
    ```


- widget\-data &mdash; List of file content (bytes)

    ```lisp
    (widget-data (file-upload file-upload))
    ```


- widget\-accept &mdash; If set, ensure value is in options. Implies continuous_update=False.	File types to accept, empty string for all.

    ```lisp
    (setf (widget-accept (file-upload file-upload)) new-value)
    ```


- widget\-accept &mdash; If set, ensure value is in options. Implies continuous_update=False.	File types to accept, empty string for all.

    ```lisp
    (widget-accept (file-upload file-upload))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-button\-style &mdash; Use a predefined styling for the button.

    ```lisp
    (setf (widget-button-style (button-style-slot button-style-slot)) new-value)
    ```


- widget\-button\-style &mdash; Use a predefined styling for the button.

    ```lisp
    (widget-button-style (button-style-slot button-style-slot))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-icon &mdash; Font-awesome icon name, without the 'fa-' prefix.

    ```lisp
    (setf (widget-icon (icon-slot icon-slot)) new-value)
    ```


- widget\-icon &mdash; Font-awesome icon name, without the 'fa-' prefix.

    ```lisp
    (widget-icon (icon-slot icon-slot))
    ```


## float\-log\-slider

#### Class

Slider/trackbar of logarithmic floating values with the specified range.

##### Precedence List

float\-log\-slider, number\-slider, base\-slider, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, orientation\-slot, continuous\-update\-slot, float\-min\-max\-slots, float\-step\-slot, float\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Float value
    - :initarg :value
- step &mdash; Minimum step to increment the value
    - :initarg :step
- max &mdash; Max value
    - :initarg :max
- min &mdash; Min value
    - :initarg :min
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- orientation &mdash; Vertical or horizontal.
    - :initarg :orientation
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- readout &mdash; Display the current value of the slider next to it.
    - :initarg :readout
- readout\-format &mdash; Format for the readout
    - :initarg :readout\-format
- base &mdash; Base for the logarithm
    - :initarg :base

##### Methods

- widget\-base &mdash; Base for the logarithm

    ```lisp
    (setf (widget-base (float-log-slider float-log-slider)) new-value)
    ```


- widget\-base &mdash; Base for the logarithm

    ```lisp
    (widget-base (float-log-slider float-log-slider))
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (number-slider number-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (number-slider number-slider))
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (base-slider base-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (base-slider base-slider))
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (setf (widget-readout (base-slider base-slider)) new-value)
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (widget-readout (base-slider base-slider))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (setf (widget-orientation (orientation-slot orientation-slot)) new-value)
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (widget-orientation (orientation-slot orientation-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w float-min-max-slots) (type (eql float-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w float-min-max-slots) (type (eql float)) name value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (setf (widget-min (float-min-max-slots float-min-max-slots)) new-value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (widget-min (float-min-max-slots float-min-max-slots))
    ```


- widget\-max &mdash; Max value

    ```lisp
    (setf (widget-max (float-min-max-slots float-min-max-slots)) new-value)
    ```


- widget\-max &mdash; Max value

    ```lisp
    (widget-max (float-min-max-slots float-min-max-slots))
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (setf (widget-step (float-step-slot float-step-slot)) new-value)
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (widget-step (float-step-slot float-step-slot))
    ```


- widget\-value &mdash; Float value

    ```lisp
    (setf (widget-value (float-value-slot float-value-slot)) new-value)
    ```


- widget\-value &mdash; Float value

    ```lisp
    (widget-value (float-value-slot float-value-slot))
    ```


## float\-progress

#### Class

Displays a progress bar.

##### Precedence List

float\-progress, base\-progress, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, orientation\-slot, float\-min\-max\-slots, float\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Float value
    - :initarg :value
- max &mdash; Max value
    - :initarg :max
- min &mdash; Min value
    - :initarg :min
- orientation &mdash; Vertical or horizontal.
    - :initarg :orientation
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- bar\-style &mdash; Use a predefined styling for the progess bar.
    - :initarg :bar\-style

##### Methods

- widget\-bar\-style &mdash; Use a predefined styling for the progess bar.

    ```lisp
    (setf (widget-bar-style (base-progress base-progress)) new-value)
    ```


- widget\-bar\-style &mdash; Use a predefined styling for the progess bar.

    ```lisp
    (widget-bar-style (base-progress base-progress))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (setf (widget-orientation (orientation-slot orientation-slot)) new-value)
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (widget-orientation (orientation-slot orientation-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w float-min-max-slots) (type (eql float-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w float-min-max-slots) (type (eql float)) name value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (setf (widget-min (float-min-max-slots float-min-max-slots)) new-value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (widget-min (float-min-max-slots float-min-max-slots))
    ```


- widget\-max &mdash; Max value

    ```lisp
    (setf (widget-max (float-min-max-slots float-min-max-slots)) new-value)
    ```


- widget\-max &mdash; Max value

    ```lisp
    (widget-max (float-min-max-slots float-min-max-slots))
    ```


- widget\-value &mdash; Float value

    ```lisp
    (setf (widget-value (float-value-slot float-value-slot)) new-value)
    ```


- widget\-value &mdash; Float value

    ```lisp
    (widget-value (float-value-slot float-value-slot))
    ```


## float\-range\-slider

#### Class

Slider/trackbar that represents a pair of floats bounded by minimum and maximum
value.

##### Precedence List

float\-range\-slider, number\-slider, base\-slider, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, orientation\-slot, continuous\-update\-slot, float\-min\-max\-slots, float\-step\-slot, standard\-object, slot\-object, t

##### Slots

- step &mdash; Minimum step to increment the value
    - :initarg :step
- max &mdash; Max value
    - :initarg :max
- min &mdash; Min value
    - :initarg :min
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- orientation &mdash; Vertical or horizontal.
    - :initarg :orientation
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- readout &mdash; Display the current value of the slider next to it.
    - :initarg :readout
- readout\-format &mdash; Format for the readout
    - :initarg :readout\-format
- value &mdash; Float range
    - :initarg :value

##### Methods

- widget\-value &mdash; Float range

    ```lisp
    (setf (widget-value (float-range-slider float-range-slider)) new-value)
    ```


- widget\-value &mdash; Float range

    ```lisp
    (widget-value (float-range-slider float-range-slider))
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (number-slider number-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (number-slider number-slider))
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (base-slider base-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (base-slider base-slider))
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (setf (widget-readout (base-slider base-slider)) new-value)
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (widget-readout (base-slider base-slider))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (setf (widget-orientation (orientation-slot orientation-slot)) new-value)
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (widget-orientation (orientation-slot orientation-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w float-min-max-slots) (type (eql float-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w float-min-max-slots) (type (eql float)) name value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (setf (widget-min (float-min-max-slots float-min-max-slots)) new-value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (widget-min (float-min-max-slots float-min-max-slots))
    ```


- widget\-max &mdash; Max value

    ```lisp
    (setf (widget-max (float-min-max-slots float-min-max-slots)) new-value)
    ```


- widget\-max &mdash; Max value

    ```lisp
    (widget-max (float-min-max-slots float-min-max-slots))
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (setf (widget-step (float-step-slot float-step-slot)) new-value)
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (widget-step (float-step-slot float-step-slot))
    ```


## float\-slider

#### Class

Slider/trackbar of floating values with the specified range.

##### Precedence List

float\-slider, number\-slider, base\-slider, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, orientation\-slot, continuous\-update\-slot, float\-min\-max\-slots, float\-step\-slot, float\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Float value
    - :initarg :value
- step &mdash; Minimum step to increment the value
    - :initarg :step
- max &mdash; Max value
    - :initarg :max
- min &mdash; Min value
    - :initarg :min
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- orientation &mdash; Vertical or horizontal.
    - :initarg :orientation
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- readout &mdash; Display the current value of the slider next to it.
    - :initarg :readout
- readout\-format &mdash; Format for the readout
    - :initarg :readout\-format

##### Methods

- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (number-slider number-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (number-slider number-slider))
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (base-slider base-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (base-slider base-slider))
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (setf (widget-readout (base-slider base-slider)) new-value)
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (widget-readout (base-slider base-slider))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (setf (widget-orientation (orientation-slot orientation-slot)) new-value)
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (widget-orientation (orientation-slot orientation-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w float-min-max-slots) (type (eql float-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w float-min-max-slots) (type (eql float)) name value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (setf (widget-min (float-min-max-slots float-min-max-slots)) new-value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (widget-min (float-min-max-slots float-min-max-slots))
    ```


- widget\-max &mdash; Max value

    ```lisp
    (setf (widget-max (float-min-max-slots float-min-max-slots)) new-value)
    ```


- widget\-max &mdash; Max value

    ```lisp
    (widget-max (float-min-max-slots float-min-max-slots))
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (setf (widget-step (float-step-slot float-step-slot)) new-value)
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (widget-step (float-step-slot float-step-slot))
    ```


- widget\-value &mdash; Float value

    ```lisp
    (setf (widget-value (float-value-slot float-value-slot)) new-value)
    ```


- widget\-value &mdash; Float value

    ```lisp
    (widget-value (float-value-slot float-value-slot))
    ```


## float\-text

#### Class

Displays a float value within a textbox. For a textbox in which the value must
be within a specific range, use BoundedFloatText.

##### Precedence List

float\-text, base\-text, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, continuous\-update\-slot, float\-step\-slot, float\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Float value
    - :initarg :value
- step &mdash; Minimum step to increment the value
    - :initarg :step
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip

##### Methods

- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (setf (widget-step (float-step-slot float-step-slot)) new-value)
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (widget-step (float-step-slot float-step-slot))
    ```


- widget\-value &mdash; Float value

    ```lisp
    (setf (widget-value (float-value-slot float-value-slot)) new-value)
    ```


- widget\-value &mdash; Float value

    ```lisp
    (widget-value (float-value-slot float-value-slot))
    ```


## focus

## Function

```lisp
(focus widget)
```

## grid\-box

#### Class

##### Precedence List

grid\-box, box, dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- box\-style &mdash; Use a predefined styling for the box.
    - :initarg :box\-style
- children &mdash; List of widget children.
    - :initarg :children

##### Methods

- widget\-children &mdash; List of widget children.

    ```lisp
    (setf (widget-children (box box)) new-value)
    ```


- widget\-children &mdash; List of widget children.

    ```lisp
    (widget-children (box box))
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (setf (widget-box-style (box box)) new-value)
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (widget-box-style (box box))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## h\-box

#### Class

Displays multiple widgets horizontally using the flexible box model.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar title-widget (make-instance 'html :value "<em>Box Example</em>"))
(defvar slider (make-instance 'int-slider))
(make-instance 'h-box :children (list title-widget slider))
```

##### Precedence List

h\-box, box, dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- box\-style &mdash; Use a predefined styling for the box.
    - :initarg :box\-style
- children &mdash; List of widget children.
    - :initarg :children

##### Methods

- widget\-children &mdash; List of widget children.

    ```lisp
    (setf (widget-children (box box)) new-value)
    ```


- widget\-children &mdash; List of widget children.

    ```lisp
    (widget-children (box box))
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (setf (widget-box-style (box box)) new-value)
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (widget-box-style (box box))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## has\-traits

#### Class

##### Precedence List

has\-traits, standard\-object, slot\-object, t

##### Slots

- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change

##### Methods

- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


## html

#### Class

Renders the string `value` as HTML.

##### Precedence List

html, label, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, placeholder\-slot, string\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; String value
    - :initarg :value
- placeholder &mdash; Placeholder text to display when nothing has been typed.
    - :initarg :placeholder
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip

##### Methods

- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (setf (widget-placeholder (placeholder-slot placeholder-slot)) new-value)
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (widget-placeholder (placeholder-slot placeholder-slot))
    ```


- widget\-value &mdash; String value

    ```lisp
    (setf (widget-value (string-value-slot string-value-slot)) new-value)
    ```


- widget\-value &mdash; String value

    ```lisp
    (widget-value (string-value-slot string-value-slot))
    ```


## html\-math

#### Class

Renders the string `value` as HTML, and render mathematics.

##### Precedence List

html\-math, label, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, placeholder\-slot, string\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; String value
    - :initarg :value
- placeholder &mdash; Placeholder text to display when nothing has been typed.
    - :initarg :placeholder
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip

##### Methods

- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (setf (widget-placeholder (placeholder-slot placeholder-slot)) new-value)
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (widget-placeholder (placeholder-slot placeholder-slot))
    ```


- widget\-value &mdash; String value

    ```lisp
    (setf (widget-value (string-value-slot string-value-slot)) new-value)
    ```


- widget\-value &mdash; String value

    ```lisp
    (widget-value (string-value-slot string-value-slot))
    ```


## image

#### Class

Displays an image as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
image data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to "png").

If you pass `"url"` to the `"format"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8.

##### Precedence List

image, dom\-widget, widget, has\-traits, comm, source, format\-slot, height\-slot, width\-slot, byte\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; The value as a byte string.
    - :initarg :value
- width &mdash; Width of the media in pixels.
    - :initarg :width
- height &mdash; Height of the media in pixels.
    - :initarg :height
- format &mdash; The format of the media.
    - :initarg :format
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout

##### Methods

- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-format &mdash; The format of the media.

    ```lisp
    (setf (widget-format (format-slot format-slot)) new-value)
    ```


- widget\-format &mdash; The format of the media.

    ```lisp
    (widget-format (format-slot format-slot))
    ```


- widget\-height &mdash; Height of the media in pixels.

    ```lisp
    (setf (widget-height (height-slot height-slot)) new-value)
    ```


- widget\-height &mdash; Height of the media in pixels.

    ```lisp
    (widget-height (height-slot height-slot))
    ```


- widget\-width &mdash; Width of the media in pixels.

    ```lisp
    (setf (widget-width (width-slot width-slot)) new-value)
    ```


- widget\-width &mdash; Width of the media in pixels.

    ```lisp
    (widget-width (width-slot width-slot))
    ```


- widget\-value &mdash; The value as a byte string.

    ```lisp
    (setf (widget-value (byte-value-slot byte-value-slot)) new-value)
    ```


- widget\-value &mdash; The value as a byte string.

    ```lisp
    (widget-value (byte-value-slot byte-value-slot))
    ```


## int\-progress

#### Class

Progress bar that represents an integer bounded from above and below.

##### Precedence List

int\-progress, base\-progress, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, orientation\-slot, int\-min\-max\-slots, int\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Int value
    - :initarg :value
- max &mdash; Max value
    - :initarg :max
- min &mdash; Min value
    - :initarg :min
- orientation &mdash; Vertical or horizontal.
    - :initarg :orientation
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- bar\-style &mdash; Use a predefined styling for the progess bar.
    - :initarg :bar\-style

##### Methods

- widget\-bar\-style &mdash; Use a predefined styling for the progess bar.

    ```lisp
    (setf (widget-bar-style (base-progress base-progress)) new-value)
    ```


- widget\-bar\-style &mdash; Use a predefined styling for the progess bar.

    ```lisp
    (widget-bar-style (base-progress base-progress))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (setf (widget-orientation (orientation-slot orientation-slot)) new-value)
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (widget-orientation (orientation-slot orientation-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w int-min-max-slots) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w int-min-max-slots) (type (eql int)) name value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (setf (widget-min (int-min-max-slots int-min-max-slots)) new-value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (widget-min (int-min-max-slots int-min-max-slots))
    ```


- widget\-max &mdash; Max value

    ```lisp
    (setf (widget-max (int-min-max-slots int-min-max-slots)) new-value)
    ```


- widget\-max &mdash; Max value

    ```lisp
    (widget-max (int-min-max-slots int-min-max-slots))
    ```


- widget\-value &mdash; Int value

    ```lisp
    (setf (widget-value (int-value-slot int-value-slot)) new-value)
    ```


- widget\-value &mdash; Int value

    ```lisp
    (widget-value (int-value-slot int-value-slot))
    ```


## int\-range\-slider

#### Class

Slider/trackbar that represents a pair of ints bounded by minimum and maximum
value.

##### Precedence List

int\-range\-slider, number\-slider, base\-slider, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, orientation\-slot, continuous\-update\-slot, int\-min\-max\-slots, int\-step\-slot, standard\-object, slot\-object, t

##### Slots

- step &mdash; Minimum step to increment the value
    - :initarg :step
- max &mdash; Max value
    - :initarg :max
- min &mdash; Min value
    - :initarg :min
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- orientation &mdash; Vertical or horizontal.
    - :initarg :orientation
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- readout &mdash; Display the current value of the slider next to it.
    - :initarg :readout
- readout\-format &mdash; Format for the readout
    - :initarg :readout\-format
- value &mdash; Int range value
    - :initarg :value

##### Methods

- widget\-value &mdash; Int range value

    ```lisp
    (setf (widget-value (int-range-slider int-range-slider)) new-value)
    ```


- widget\-value &mdash; Int range value

    ```lisp
    (widget-value (int-range-slider int-range-slider))
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (number-slider number-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (number-slider number-slider))
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (base-slider base-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (base-slider base-slider))
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (setf (widget-readout (base-slider base-slider)) new-value)
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (widget-readout (base-slider base-slider))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (setf (widget-orientation (orientation-slot orientation-slot)) new-value)
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (widget-orientation (orientation-slot orientation-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w int-min-max-slots) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w int-min-max-slots) (type (eql int)) name value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (setf (widget-min (int-min-max-slots int-min-max-slots)) new-value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (widget-min (int-min-max-slots int-min-max-slots))
    ```


- widget\-max &mdash; Max value

    ```lisp
    (setf (widget-max (int-min-max-slots int-min-max-slots)) new-value)
    ```


- widget\-max &mdash; Max value

    ```lisp
    (widget-max (int-min-max-slots int-min-max-slots))
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (setf (widget-step (int-step-slot int-step-slot)) new-value)
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (widget-step (int-step-slot int-step-slot))
    ```


## int\-slider

#### Class

Slider widget that represents an integer bounded from above and below.

##### Precedence List

int\-slider, number\-slider, base\-slider, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, orientation\-slot, continuous\-update\-slot, int\-min\-max\-slots, int\-step\-slot, int\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Int value
    - :initarg :value
- step &mdash; Minimum step to increment the value
    - :initarg :step
- max &mdash; Max value
    - :initarg :max
- min &mdash; Min value
    - :initarg :min
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- orientation &mdash; Vertical or horizontal.
    - :initarg :orientation
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- readout &mdash; Display the current value of the slider next to it.
    - :initarg :readout
- readout\-format &mdash; Format for the readout
    - :initarg :readout\-format

##### Methods

- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (number-slider number-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (number-slider number-slider))
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (base-slider base-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (base-slider base-slider))
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (setf (widget-readout (base-slider base-slider)) new-value)
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (widget-readout (base-slider base-slider))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (setf (widget-orientation (orientation-slot orientation-slot)) new-value)
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (widget-orientation (orientation-slot orientation-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w int-min-max-slots) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w int-min-max-slots) (type (eql int)) name value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (setf (widget-min (int-min-max-slots int-min-max-slots)) new-value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (widget-min (int-min-max-slots int-min-max-slots))
    ```


- widget\-max &mdash; Max value

    ```lisp
    (setf (widget-max (int-min-max-slots int-min-max-slots)) new-value)
    ```


- widget\-max &mdash; Max value

    ```lisp
    (widget-max (int-min-max-slots int-min-max-slots))
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (setf (widget-step (int-step-slot int-step-slot)) new-value)
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (widget-step (int-step-slot int-step-slot))
    ```


- widget\-value &mdash; Int value

    ```lisp
    (setf (widget-value (int-value-slot int-value-slot)) new-value)
    ```


- widget\-value &mdash; Int value

    ```lisp
    (widget-value (int-value-slot int-value-slot))
    ```


## int\-text

#### Class

Textbox widget that represents an integer.

##### Precedence List

int\-text, base\-text, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, continuous\-update\-slot, int\-step\-slot, int\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Int value
    - :initarg :value
- step &mdash; Minimum step to increment the value
    - :initarg :step
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip

##### Methods

- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (setf (widget-step (int-step-slot int-step-slot)) new-value)
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (widget-step (int-step-slot int-step-slot))
    ```


- widget\-value &mdash; Int value

    ```lisp
    (setf (widget-value (int-value-slot int-value-slot)) new-value)
    ```


- widget\-value &mdash; Int value

    ```lisp
    (widget-value (int-value-slot int-value-slot))
    ```


## label

#### Class

Label widget.

It also renders math inside the string `value` as Latex (requires $ $ or
$$ $$ and similar latex tags).

##### Precedence List

label, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, placeholder\-slot, string\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; String value
    - :initarg :value
- placeholder &mdash; Placeholder text to display when nothing has been typed.
    - :initarg :placeholder
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip

##### Methods

- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (setf (widget-placeholder (placeholder-slot placeholder-slot)) new-value)
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (widget-placeholder (placeholder-slot placeholder-slot))
    ```


- widget\-value &mdash; String value

    ```lisp
    (setf (widget-value (string-value-slot string-value-slot)) new-value)
    ```


- widget\-value &mdash; String value

    ```lisp
    (widget-value (string-value-slot string-value-slot))
    ```


## layout

#### Class

Layout specification

Defines a layout that can be expressed using CSS.  Supports a subset of
https://developer.mozilla.org/en-US/docs/Web/CSS/Reference

When a property is also accessible via a shorthand property, we only
expose the shorthand.

##### Precedence List

layout, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- align\-content &mdash; The align-content CSS attribute.
    - :initarg :align\-content
- align\-items &mdash; The align-items CSS attribute.
    - :initarg :align\-items
- align\-self &mdash; The align-self CSS attribute.
    - :initarg :align\-self
- border &mdash; The border CSS attribute.
    - :initarg :border
- bottom &mdash; The bottom CSS attribute.
    - :initarg :bottom
- display &mdash; The display CSS attribute.
    - :initarg :display
- flex &mdash; The flex CSS attribute.
    - :initarg :flex
- flex\-flow &mdash; The flex-flow CSS attribute.
    - :initarg :flex\-flow
- grid\-area &mdash; The grid-area CSS attribute.
    - :initarg :grid\-area
- grid\-auto\-columns &mdash; The grid-auto-columns CSS attribute.
    - :initarg :grid\-auto\-columns
- grid\-auto\-flow &mdash; The grid-auto-flow CSS attribute.
    - :initarg :grid\-auto\-flow
- grid\-auto\-rows &mdash; The grid-auto-rows CSS attribute.
    - :initarg :grid\-auto\-rows
- grid\-column &mdash; The grid-column CSS attribute.
    - :initarg :grid\-column
- grid\-gap &mdash; The grid-gap CSS attribute.
    - :initarg :grid\-gap
- grid\-row &mdash; The grid-row CSS attribute.
    - :initarg :grid\-row
- grid\-template\-areas &mdash; The grid-template-areas CSS attribute.
    - :initarg :grid\-template\-areas
- grid\-template\-columns &mdash; The grid-template-columns CSS attribute.
    - :initarg :grid\-template\-columns
- grid\-template\-rows &mdash; The grid-template-rows CSS attribute.
    - :initarg :grid\-template\-rows
- height &mdash; The height CSS attribute.
    - :initarg :height
- justify\-content &mdash; The justify-content CSS attribute.
    - :initarg :justify\-content
- justify\-items &mdash; The justify-items CSS attribute.
    - :initarg :justify\-items
- left &mdash; The left CSS attribute.
    - :initarg :left
- margin &mdash; The margin CSS attribute.
    - :initarg :margin
- max\-height &mdash; The max-height CSS attribute.
    - :initarg :max\-height
- max\-width &mdash; The max-width CSS attribute.
    - :initarg :max\-width
- min\-height &mdash; The min-height CSS attribute.
    - :initarg :min\-height
- min\-width &mdash; The min-width CSS attribute.
    - :initarg :min\-width
- object\-fit &mdash; The object-fit CSS attribute.
    - :initarg :object\-fit
- object\-position &mdash; The object-position CSS attribute.
    - :initarg :object\-position
- order &mdash; The order CSS attribute.
    - :initarg :order
- overflow &mdash; The overflow CSS attribute.
    - :initarg :overflow
- overflow\-x &mdash; The overflow-x CSS attribute.
    - :initarg :overflow\-x
- overflow\-y &mdash; The overflow-y CSS attribute.
    - :initarg :overflow\-y
- padding &mdash; The padding CSS attribute.
    - :initarg :padding
- right &mdash; The right CSS attribute.
    - :initarg :right
- top &mdash; The top CSS attribute.
    - :initarg :top
- visibility &mdash; The visibility CSS attribute.
    - :initarg :visibility
- width &mdash; The width CSS attribute.
    - :initarg :width

##### Methods

- widget\-width &mdash; The width CSS attribute.

    ```lisp
    (setf (widget-width (layout layout)) new-value)
    ```


- widget\-width &mdash; The width CSS attribute.

    ```lisp
    (widget-width (layout layout))
    ```


- widget\-visibility &mdash; The visibility CSS attribute.

    ```lisp
    (setf (widget-visibility (layout layout)) new-value)
    ```


- widget\-visibility &mdash; The visibility CSS attribute.

    ```lisp
    (widget-visibility (layout layout))
    ```


- widget\-top &mdash; The top CSS attribute.

    ```lisp
    (setf (widget-top (layout layout)) new-value)
    ```


- widget\-top &mdash; The top CSS attribute.

    ```lisp
    (widget-top (layout layout))
    ```


- widget\-right &mdash; The right CSS attribute.

    ```lisp
    (setf (widget-right (layout layout)) new-value)
    ```


- widget\-right &mdash; The right CSS attribute.

    ```lisp
    (widget-right (layout layout))
    ```


- widget\-padding &mdash; The padding CSS attribute.

    ```lisp
    (setf (widget-padding (layout layout)) new-value)
    ```


- widget\-padding &mdash; The padding CSS attribute.

    ```lisp
    (widget-padding (layout layout))
    ```


- widget\-overflow\-y &mdash; The overflow-y CSS attribute.

    ```lisp
    (setf (widget-overflow-y (layout layout)) new-value)
    ```


- widget\-overflow\-y &mdash; The overflow-y CSS attribute.

    ```lisp
    (widget-overflow-y (layout layout))
    ```


- widget\-overflow\-x &mdash; The overflow-x CSS attribute.

    ```lisp
    (setf (widget-overflow-x (layout layout)) new-value)
    ```


- widget\-overflow\-x &mdash; The overflow-x CSS attribute.

    ```lisp
    (widget-overflow-x (layout layout))
    ```


- widget\-overflow &mdash; The overflow CSS attribute.

    ```lisp
    (setf (widget-overflow (layout layout)) new-value)
    ```


- widget\-overflow &mdash; The overflow CSS attribute.

    ```lisp
    (widget-overflow (layout layout))
    ```


- widget\-order &mdash; The order CSS attribute.

    ```lisp
    (setf (widget-order (layout layout)) new-value)
    ```


- widget\-order &mdash; The order CSS attribute.

    ```lisp
    (widget-order (layout layout))
    ```


- widget\-object\-position &mdash; The object-position CSS attribute.

    ```lisp
    (setf (widget-object-position (layout layout)) new-value)
    ```


- widget\-object\-position &mdash; The object-position CSS attribute.

    ```lisp
    (widget-object-position (layout layout))
    ```


- widget\-object\-fit &mdash; The object-fit CSS attribute.

    ```lisp
    (setf (widget-object-fit (layout layout)) new-value)
    ```


- widget\-object\-fit &mdash; The object-fit CSS attribute.

    ```lisp
    (widget-object-fit (layout layout))
    ```


- widget\-min\-width &mdash; The min-width CSS attribute.

    ```lisp
    (setf (widget-min-width (layout layout)) new-value)
    ```


- widget\-min\-width &mdash; The min-width CSS attribute.

    ```lisp
    (widget-min-width (layout layout))
    ```


- widget\-min\-height &mdash; The min-height CSS attribute.

    ```lisp
    (setf (widget-min-height (layout layout)) new-value)
    ```


- widget\-min\-height &mdash; The min-height CSS attribute.

    ```lisp
    (widget-min-height (layout layout))
    ```


- widget\-max\-width &mdash; The max-width CSS attribute.

    ```lisp
    (setf (widget-max-width (layout layout)) new-value)
    ```


- widget\-max\-width &mdash; The max-width CSS attribute.

    ```lisp
    (widget-max-width (layout layout))
    ```


- widget\-max\-height &mdash; The max-height CSS attribute.

    ```lisp
    (setf (widget-max-height (layout layout)) new-value)
    ```


- widget\-max\-height &mdash; The max-height CSS attribute.

    ```lisp
    (widget-max-height (layout layout))
    ```


- widget\-margin &mdash; The margin CSS attribute.

    ```lisp
    (setf (widget-margin (layout layout)) new-value)
    ```


- widget\-margin &mdash; The margin CSS attribute.

    ```lisp
    (widget-margin (layout layout))
    ```


- widget\-left &mdash; The left CSS attribute.

    ```lisp
    (setf (widget-left (layout layout)) new-value)
    ```


- widget\-left &mdash; The left CSS attribute.

    ```lisp
    (widget-left (layout layout))
    ```


- widget\-justify\-items &mdash; The justify-items CSS attribute.

    ```lisp
    (setf (widget-justify-items (layout layout)) new-value)
    ```


- widget\-justify\-items &mdash; The justify-items CSS attribute.

    ```lisp
    (widget-justify-items (layout layout))
    ```


- widget\-justify\-content &mdash; The justify-content CSS attribute.

    ```lisp
    (setf (widget-justify-content (layout layout)) new-value)
    ```


- widget\-justify\-content &mdash; The justify-content CSS attribute.

    ```lisp
    (widget-justify-content (layout layout))
    ```


- widget\-height &mdash; The height CSS attribute.

    ```lisp
    (setf (widget-height (layout layout)) new-value)
    ```


- widget\-height &mdash; The height CSS attribute.

    ```lisp
    (widget-height (layout layout))
    ```


- widget\-grid\-template\-rows &mdash; The grid-template-rows CSS attribute.

    ```lisp
    (setf (widget-grid-template-rows (layout layout)) new-value)
    ```


- widget\-grid\-template\-rows &mdash; The grid-template-rows CSS attribute.

    ```lisp
    (widget-grid-template-rows (layout layout))
    ```


- widget\-grid\-template\-columns &mdash; The grid-template-columns CSS attribute.

    ```lisp
    (setf (widget-grid-template-columns (layout layout)) new-value)
    ```


- widget\-grid\-template\-columns &mdash; The grid-template-columns CSS attribute.

    ```lisp
    (widget-grid-template-columns (layout layout))
    ```


- widget\-grid\-template\-areas &mdash; The grid-template-areas CSS attribute.

    ```lisp
    (setf (widget-grid-template-areas (layout layout)) new-value)
    ```


- widget\-grid\-template\-areas &mdash; The grid-template-areas CSS attribute.

    ```lisp
    (widget-grid-template-areas (layout layout))
    ```


- widget\-grid\-row &mdash; The grid-row CSS attribute.

    ```lisp
    (setf (widget-grid-row (layout layout)) new-value)
    ```


- widget\-grid\-row &mdash; The grid-row CSS attribute.

    ```lisp
    (widget-grid-row (layout layout))
    ```


- widget\-grid\-gap &mdash; The grid-gap CSS attribute.

    ```lisp
    (setf (widget-grid-gap (layout layout)) new-value)
    ```


- widget\-grid\-gap &mdash; The grid-gap CSS attribute.

    ```lisp
    (widget-grid-gap (layout layout))
    ```


- widget\-grid\-column &mdash; The grid-column CSS attribute.

    ```lisp
    (setf (widget-grid-column (layout layout)) new-value)
    ```


- widget\-grid\-column &mdash; The grid-column CSS attribute.

    ```lisp
    (widget-grid-column (layout layout))
    ```


- widget\-grid\-auto\-rows &mdash; The grid-auto-rows CSS attribute.

    ```lisp
    (setf (widget-grid-auto-rows (layout layout)) new-value)
    ```


- widget\-grid\-auto\-rows &mdash; The grid-auto-rows CSS attribute.

    ```lisp
    (widget-grid-auto-rows (layout layout))
    ```


- widget\-grid\-auto\-flow &mdash; The grid-auto-flow CSS attribute.

    ```lisp
    (setf (widget-grid-auto-flow (layout layout)) new-value)
    ```


- widget\-grid\-auto\-flow &mdash; The grid-auto-flow CSS attribute.

    ```lisp
    (widget-grid-auto-flow (layout layout))
    ```


- widget\-grid\-auto\-columns &mdash; The grid-auto-columns CSS attribute.

    ```lisp
    (setf (widget-grid-auto-columns (layout layout)) new-value)
    ```


- widget\-grid\-auto\-columns &mdash; The grid-auto-columns CSS attribute.

    ```lisp
    (widget-grid-auto-columns (layout layout))
    ```


- widget\-grid\-area &mdash; The grid-area CSS attribute.

    ```lisp
    (setf (widget-grid-area (layout layout)) new-value)
    ```


- widget\-grid\-area &mdash; The grid-area CSS attribute.

    ```lisp
    (widget-grid-area (layout layout))
    ```


- widget\-flex\-flow &mdash; The flex-flow CSS attribute.

    ```lisp
    (setf (widget-flex-flow (layout layout)) new-value)
    ```


- widget\-flex\-flow &mdash; The flex-flow CSS attribute.

    ```lisp
    (widget-flex-flow (layout layout))
    ```


- widget\-flex &mdash; The flex CSS attribute.

    ```lisp
    (setf (widget-flex (layout layout)) new-value)
    ```


- widget\-flex &mdash; The flex CSS attribute.

    ```lisp
    (widget-flex (layout layout))
    ```


- widget\-display &mdash; The display CSS attribute.

    ```lisp
    (setf (widget-display (layout layout)) new-value)
    ```


- widget\-display &mdash; The display CSS attribute.

    ```lisp
    (widget-display (layout layout))
    ```


- widget\-bottom &mdash; The bottom CSS attribute.

    ```lisp
    (setf (widget-bottom (layout layout)) new-value)
    ```


- widget\-bottom &mdash; The bottom CSS attribute.

    ```lisp
    (widget-bottom (layout layout))
    ```


- widget\-border &mdash; The border CSS attribute.

    ```lisp
    (setf (widget-border (layout layout)) new-value)
    ```


- widget\-border &mdash; The border CSS attribute.

    ```lisp
    (widget-border (layout layout))
    ```


- widget\-align\-self &mdash; The align-self CSS attribute.

    ```lisp
    (setf (widget-align-self (layout layout)) new-value)
    ```


- widget\-align\-self &mdash; The align-self CSS attribute.

    ```lisp
    (widget-align-self (layout layout))
    ```


- widget\-align\-items &mdash; The align-items CSS attribute.

    ```lisp
    (setf (widget-align-items (layout layout)) new-value)
    ```


- widget\-align\-items &mdash; The align-items CSS attribute.

    ```lisp
    (widget-align-items (layout layout))
    ```


- widget\-align\-content &mdash; The align-content CSS attribute.

    ```lisp
    (setf (widget-align-content (layout layout)) new-value)
    ```


- widget\-align\-content &mdash; The align-content CSS attribute.

    ```lisp
    (widget-align-content (layout layout))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## link

## Generic Function

Create a link between traits in the client if possible

```lisp
(link source source-trait target target-trait &optional sync)
```

#### Class

Link Widget

##### Precedence List

link, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- source
    - :initarg :source
- target
    - :initarg :target

##### Methods

- widget\-target &mdash; automatically generated writer method

    ```lisp
    (setf (widget-target (link link)) new-value)
    ```


- widget\-target &mdash; automatically generated reader method

    ```lisp
    (widget-target (link link))
    ```


- widget\-source &mdash; automatically generated writer method

    ```lisp
    (setf (widget-source (link link)) new-value)
    ```


- widget\-source &mdash; automatically generated reader method

    ```lisp
    (widget-source (link link))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## make\-accordion

## Function

```lisp
(make-accordion &rest initargs &key comm-id layout box-style children
                selected-index &allow-other-keys)
```

## make\-audio

## Function

```lisp
(make-audio &rest initargs &key value format comm-id layout autoplay controls
            loop &allow-other-keys)
```

## make\-bounded\-float\-text

## Function

```lisp
(make-bounded-float-text &rest initargs &key max min value step
                         continuous-update disabled comm-id layout style
                         description description-tooltip &allow-other-keys)
```

## make\-bounded\-int\-text

## Function

```lisp
(make-bounded-int-text &rest initargs &key max min value step continuous-update
                       disabled comm-id layout style description
                       description-tooltip &allow-other-keys)
```

## make\-box

## Function

```lisp
(make-box &rest initargs &key comm-id layout box-style children
          &allow-other-keys)
```

## make\-button

## Function

```lisp
(make-button &rest initargs &key tooltip icon disabled button-style comm-id
             layout style description &allow-other-keys)
```

## make\-button\-style

## Function

```lisp
(make-button-style &rest initargs &key comm-id button-color font-weight
                   &allow-other-keys)
```

## make\-checkbox

## Function

```lisp
(make-checkbox &rest initargs &key value disabled comm-id layout style
               description description-tooltip indent &allow-other-keys)
```

## make\-color\-picker

## Function

```lisp
(make-color-picker &rest initargs &key disabled comm-id layout style
                   description description-tooltip concise value
                   &allow-other-keys)
```

## make\-combobox

## Function

```lisp
(make-combobox &rest initargs &key value placeholder disabled continuous-update
               comm-id layout style description description-tooltip
               ensure-option options &allow-other-keys)
```

## make\-controller

## Function

```lisp
(make-controller &rest initargs &key comm-id layout axes buttons connected
                 index mapping name timestamp &allow-other-keys)
```

## make\-controller\-axis

## Function

```lisp
(make-controller-axis &rest initargs &key value comm-id layout
                      &allow-other-keys)
```

## make\-controller\-button

## Function

```lisp
(make-controller-button &rest initargs &key value comm-id layout pressed
                        &allow-other-keys)
```

## make\-date\-picker

## Function

```lisp
(make-date-picker &rest initargs &key disabled comm-id layout style description
                  description-tooltip value &allow-other-keys)
```

## make\-description\-style

## Function

```lisp
(make-description-style &rest initargs &key comm-id description-width
                        &allow-other-keys)
```

## make\-directional\-link

## Function

```lisp
(make-directional-link &rest initargs &key comm-id source target
                       &allow-other-keys)
```

## make\-dropdown

## Function

```lisp
(make-dropdown &rest initargs &key index disabled comm-id layout style
               description description-tooltip rows options &allow-other-keys)
```

## make\-file\-upload

## Function

```lisp
(make-file-upload &rest initargs &key icon disabled button-style comm-id layout
                  style description description-tooltip accept data error
                  metadata multiple &allow-other-keys)
```

## make\-float\-log\-slider

## Function

```lisp
(make-float-log-slider &rest initargs &key value step max min continuous-update
                       orientation disabled comm-id layout style description
                       description-tooltip readout readout-format base
                       &allow-other-keys)
```

## make\-float\-progress

## Function

```lisp
(make-float-progress &rest initargs &key value max min orientation comm-id
                     layout style description description-tooltip bar-style
                     &allow-other-keys)
```

## make\-float\-range\-slider

## Function

```lisp
(make-float-range-slider &rest initargs &key step max min continuous-update
                         orientation disabled comm-id layout style description
                         description-tooltip readout readout-format value
                         &allow-other-keys)
```

## make\-float\-slider

## Function

```lisp
(make-float-slider &rest initargs &key value step max min continuous-update
                   orientation disabled comm-id layout style description
                   description-tooltip readout readout-format &allow-other-keys)
```

## make\-float\-text

## Function

```lisp
(make-float-text &rest initargs &key value step continuous-update disabled
                 comm-id layout style description description-tooltip
                 &allow-other-keys)
```

## make\-grid\-box

## Function

```lisp
(make-grid-box &rest initargs &key comm-id layout box-style children
               &allow-other-keys)
```

## make\-h\-box

## Function

```lisp
(make-h-box &rest initargs &key comm-id layout box-style children
            &allow-other-keys)
```

## make\-html

## Function

```lisp
(make-html &rest initargs &key value placeholder comm-id layout style
           description description-tooltip &allow-other-keys)
```

## make\-html\-math

## Function

```lisp
(make-html-math &rest initargs &key value placeholder comm-id layout style
                description description-tooltip &allow-other-keys)
```

## make\-image

## Function

```lisp
(make-image &rest initargs &key value width height format comm-id layout
            &allow-other-keys)
```

## make\-int\-progress

## Function

```lisp
(make-int-progress &rest initargs &key value max min orientation comm-id layout
                   style description description-tooltip bar-style
                   &allow-other-keys)
```

## make\-int\-range\-slider

## Function

```lisp
(make-int-range-slider &rest initargs &key step max min continuous-update
                       orientation disabled comm-id layout style description
                       description-tooltip readout readout-format value
                       &allow-other-keys)
```

## make\-int\-slider

## Function

```lisp
(make-int-slider &rest initargs &key value step max min continuous-update
                 orientation disabled comm-id layout style description
                 description-tooltip readout readout-format &allow-other-keys)
```

## make\-int\-text

## Function

```lisp
(make-int-text &rest initargs &key value step continuous-update disabled
               comm-id layout style description description-tooltip
               &allow-other-keys)
```

## make\-interactive\-alist

## Macro

```lisp
(make-interactive-alist schemas place &key key test owner name)
```

## make\-interactive\-hash\-table

## Macro

```lisp
(make-interactive-hash-table schemas place &key owner name)
```

## make\-interactive\-plist

## Macro

```lisp
(make-interactive-plist schemas place &key owner name)
```

## make\-label

## Function

```lisp
(make-label &rest initargs &key value placeholder comm-id layout style
            description description-tooltip &allow-other-keys)
```

## make\-layout

## Function

```lisp
(make-layout &rest initargs &key comm-id align-content align-items align-self
             border bottom display flex flex-flow grid-area grid-auto-columns
             grid-auto-flow grid-auto-rows grid-column grid-gap grid-row
             grid-template-areas grid-template-columns grid-template-rows
             height justify-content justify-items left margin max-height
             max-width min-height min-width object-fit object-position order
             overflow overflow-x overflow-y padding right top visibility width
             &allow-other-keys)
```

## make\-link

## Function

```lisp
(make-link &rest initargs &key comm-id source target &allow-other-keys)
```

## make\-output

## Function

```lisp
(make-output &rest initargs &key comm-id layout msg-id outputs
             &allow-other-keys)
```

## make\-output\-widget\-stream

## Function

```lisp
(make-output-widget-stream output &optional error-output)
```

## make\-password

## Function

```lisp
(make-password &rest initargs &key value placeholder continuous-update disabled
               comm-id layout style description description-tooltip
               &allow-other-keys)
```

## make\-play

## Function

```lisp
(make-play &rest initargs &key value step max min disabled comm-id layout style
           description description-tooltip interval show-repeat
           &allow-other-keys)
```

## make\-progress\-style

## Function

```lisp
(make-progress-style &rest initargs &key comm-id description-width bar-color
                     &allow-other-keys)
```

## make\-radio\-buttons

## Function

```lisp
(make-radio-buttons &rest initargs &key index disabled comm-id layout style
                    description description-tooltip rows options
                    &allow-other-keys)
```

## make\-select

## Function

```lisp
(make-select &rest initargs &key index disabled comm-id layout style
             description description-tooltip rows options &allow-other-keys)
```

## make\-select\-multiple

## Function

```lisp
(make-select-multiple &rest initargs &key disabled comm-id layout style
                      description description-tooltip rows index options
                      &allow-other-keys)
```

## make\-selection\-range\-slider

## Function

```lisp
(make-selection-range-slider &rest initargs &key continuous-update orientation
                             disabled comm-id layout style description
                             description-tooltip readout readout-format options
                             index &allow-other-keys)
```

## make\-selection\-slider

## Function

```lisp
(make-selection-slider &rest initargs &key index continuous-update orientation
                       disabled comm-id layout style description
                       description-tooltip readout readout-format options
                       &allow-other-keys)
```

## make\-sidecar

## Function

```lisp
(make-sidecar &rest initargs &key comm-id layout msg-id outputs title
              &allow-other-keys)
```

## make\-slider\-style

## Function

```lisp
(make-slider-style &rest initargs &key comm-id description-width handle-color
                   &allow-other-keys)
```

## make\-tab

## Function

```lisp
(make-tab &rest initargs &key comm-id layout box-style children selected-index
          &allow-other-keys)
```

## make\-text

## Function

```lisp
(make-text &rest initargs &key value placeholder continuous-update disabled
           comm-id layout style description description-tooltip
           &allow-other-keys)
```

## make\-text\-area

## Function

```lisp
(make-text-area &rest initargs &key value placeholder continuous-update
                disabled comm-id layout style description description-tooltip
                rows &allow-other-keys)
```

## make\-toggle\-button

## Function

```lisp
(make-toggle-button &rest initargs &key value tooltip icon disabled
                    button-style comm-id layout style description
                    description-tooltip &allow-other-keys)
```

## make\-toggle\-buttons

## Function

```lisp
(make-toggle-buttons &rest initargs &key index disabled button-style comm-id
                     layout style description description-tooltip icons options
                     tooltips &allow-other-keys)
```

## make\-toggle\-buttons\-style

## Function

```lisp
(make-toggle-buttons-style &rest initargs &key comm-id button-width
                           description-width font-weight &allow-other-keys)
```

## make\-v\-box

## Function

```lisp
(make-v-box &rest initargs &key comm-id layout box-style children
            &allow-other-keys)
```

## make\-valid

## Function

```lisp
(make-valid &rest initargs &key value disabled comm-id layout style description
            description-tooltip readout &allow-other-keys)
```

## make\-video

## Function

```lisp
(make-video &rest initargs &key width height value format comm-id layout
            autoplay controls loop &allow-other-keys)
```

## notify\-trait\-change

## Function

```lisp
(notify-trait-change object type name old-value new-value &optional
                     (source *trait-source*))
```

## observe

## Function

```lisp
(observe instance name/s handler)
```

## on\-button\-click

## Function

```lisp
(on-button-click widget handler)
```

## on\-custom\-message

## Generic Function

```lisp
(on-custom-message widget content buffers)
```

## on\-trait\-change

## Generic Function

```lisp
(on-trait-change object type name old-value new-value source)
```

## output

#### Class

Widget used as a context manager to display output.

This widget can capture and display stdout, stderr, and rich output.  To use it,
create an instance of it and display it.

You can then use the widget as a context manager: any output produced while in
the context will be captured and displayed in the widget instead of the standard
output area.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar out (make-instance 'output))
(with-output out
  (print "prints to output area")
```

##### Precedence List

output, dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- msg\-id &mdash; Parent message id of messages to capture
    - :initarg :msg\-id
- outputs &mdash; The output messages synced from the frontend.
    - :initarg :outputs

##### Methods

- widget\-outputs &mdash; The output messages synced from the frontend.

    ```lisp
    (setf (widget-outputs (output output)) new-value)
    ```


- widget\-outputs &mdash; The output messages synced from the frontend.

    ```lisp
    (widget-outputs (output output))
    ```


- widget\-msg\-id &mdash; Parent message id of messages to capture

    ```lisp
    (setf (widget-msg-id (output output)) new-value)
    ```


- widget\-msg\-id &mdash; Parent message id of messages to capture

    ```lisp
    (widget-msg-id (output output))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## password

#### Class

Single line textbox widget.

##### Precedence List

password, text, base\-text, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, continuous\-update\-slot, placeholder\-slot, string\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; String value
    - :initarg :value
- placeholder &mdash; Placeholder text to display when nothing has been typed.
    - :initarg :placeholder
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip

##### Methods

- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (setf (widget-placeholder (placeholder-slot placeholder-slot)) new-value)
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (widget-placeholder (placeholder-slot placeholder-slot))
    ```


- widget\-value &mdash; String value

    ```lisp
    (setf (widget-value (string-value-slot string-value-slot)) new-value)
    ```


- widget\-value &mdash; String value

    ```lisp
    (widget-value (string-value-slot string-value-slot))
    ```


## play

#### Class

Play/repeat buttons to step through values automatically, and optionally loop.

##### Precedence List

play, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, int\-min\-max\-slots, int\-step\-slot, int\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Int value
    - :initarg :value
- step &mdash; Minimum step to increment the value
    - :initarg :step
- max &mdash; Max value
    - :initarg :max
- min &mdash; Min value
    - :initarg :min
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- %playing &mdash; Whether the control is currently playing.
    - :initarg :%playing
- %repeat &mdash; Whether the control will repeat in a continous loop.
    - :initarg :%repeat
- interval &mdash; The maximum value for the play control.
    - :initarg :interval
- show\-repeat &mdash; Show the repeat toggle button in the widget.
    - :initarg :show\-repeat

##### Methods

- widget\-show\-repeat &mdash; Show the repeat toggle button in the widget.

    ```lisp
    (setf (widget-show-repeat (play play)) new-value)
    ```


- widget\-show\-repeat &mdash; Show the repeat toggle button in the widget.

    ```lisp
    (widget-show-repeat (play play))
    ```


- widget\-interval &mdash; The maximum value for the play control.

    ```lisp
    (setf (widget-interval (play play)) new-value)
    ```


- widget\-interval &mdash; The maximum value for the play control.

    ```lisp
    (widget-interval (play play))
    ```


- widget\-%repeat &mdash; Whether the control will repeat in a continous loop.

    ```lisp
    (setf (widget-%repeat (play play)) new-value)
    ```


- widget\-%repeat &mdash; Whether the control will repeat in a continous loop.

    ```lisp
    (widget-%repeat (play play))
    ```


- widget\-%playing &mdash; Whether the control is currently playing.

    ```lisp
    (setf (widget-%playing (play play)) new-value)
    ```


- widget\-%playing &mdash; Whether the control is currently playing.

    ```lisp
    (widget-%playing (play play))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w int-min-max-slots) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w int-min-max-slots) (type (eql int)) name value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (setf (widget-min (int-min-max-slots int-min-max-slots)) new-value)
    ```


- widget\-min &mdash; Min value

    ```lisp
    (widget-min (int-min-max-slots int-min-max-slots))
    ```


- widget\-max &mdash; Max value

    ```lisp
    (setf (widget-max (int-min-max-slots int-min-max-slots)) new-value)
    ```


- widget\-max &mdash; Max value

    ```lisp
    (widget-max (int-min-max-slots int-min-max-slots))
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (setf (widget-step (int-step-slot int-step-slot)) new-value)
    ```


- widget\-step &mdash; Minimum step to increment the value

    ```lisp
    (widget-step (int-step-slot int-step-slot))
    ```


- widget\-value &mdash; Int value

    ```lisp
    (setf (widget-value (int-value-slot int-value-slot)) new-value)
    ```


- widget\-value &mdash; Int value

    ```lisp
    (widget-value (int-value-slot int-value-slot))
    ```


## progress\-style

#### Class

Progress style widget.

##### Precedence List

progress\-style, description\-style, style, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- description\-width &mdash; Width of the description to the side of the control.
    - :initarg :description\-width
- bar\-color &mdash; Color of the slider handle.
    - :initarg :bar\-color

##### Methods

- widget\-bar\-color &mdash; Color of the slider handle.

    ```lisp
    (setf (widget-bar-color (progress-style progress-style)) new-value)
    ```


- widget\-bar\-color &mdash; Color of the slider handle.

    ```lisp
    (widget-bar-color (progress-style progress-style))
    ```


- widget\-description\-width &mdash; Width of the description to the side of the control.

    ```lisp
    (setf (widget-description-width (description-style description-style))
            new-value)
    ```


- widget\-description\-width &mdash; Width of the description to the side of the control.

    ```lisp
    (widget-description-width (description-style description-style))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## radio\-buttons

#### Class

Group of radio buttons that represent an enumeration. Only one radio button can
be toggled at any point in time.

##### Precedence List

radio\-buttons, select, base\-select, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, %options\-labels\-slot, disabled\-slot, index\-slot, standard\-object, slot\-object, t

##### Slots

- index &mdash; Selected index
    - :initarg :index
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- %options\-labels &mdash; The labels for the options.
    - :initarg :%options\-labels
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- rows &mdash; The number of rows to display.
    - :initarg :rows
- options &mdash; The option values that correspond to the labels
    - :initarg :options

##### Methods

- on\-trait\-change

    ```lisp
    (on-trait-change (instance select) type (name (eql index)) old-value
                     new-value source)
    ```


- widget\-value

    ```lisp
    (setf (widget-value (instance select)) new-value)
    ```


- widget\-value

    ```lisp
    (widget-value (instance select))
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (setf (widget-options (select select)) new-value)
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (widget-options (select select))
    ```


- widget\-rows &mdash; The number of rows to display.

    ```lisp
    (setf (widget-rows (base-select base-select)) new-value)
    ```


- widget\-rows &mdash; The number of rows to display.

    ```lisp
    (widget-rows (base-select base-select))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int)) name value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (setf (widget-%options-labels (%options-labels-slot %options-labels-slot))
            new-value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (widget-%options-labels (%options-labels-slot %options-labels-slot))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-index &mdash; Selected index

    ```lisp
    (setf (widget-index (index-slot index-slot)) new-value)
    ```


- widget\-index &mdash; Selected index

    ```lisp
    (widget-index (index-slot index-slot))
    ```


## register\-widgets

## Macro

```lisp
(register-widgets &rest names)
```

## select

#### Class

Listbox that only allows one item to be selected at any given time.

##### Precedence List

select, base\-select, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, %options\-labels\-slot, disabled\-slot, index\-slot, standard\-object, slot\-object, t

##### Slots

- index &mdash; Selected index
    - :initarg :index
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- %options\-labels &mdash; The labels for the options.
    - :initarg :%options\-labels
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- rows &mdash; The number of rows to display.
    - :initarg :rows
- options &mdash; The option values that correspond to the labels
    - :initarg :options

##### Methods

- on\-trait\-change

    ```lisp
    (on-trait-change (instance select) type (name (eql index)) old-value
                     new-value source)
    ```


- widget\-value

    ```lisp
    (setf (widget-value (instance select)) new-value)
    ```


- widget\-value

    ```lisp
    (widget-value (instance select))
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (setf (widget-options (select select)) new-value)
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (widget-options (select select))
    ```


- widget\-rows &mdash; The number of rows to display.

    ```lisp
    (setf (widget-rows (base-select base-select)) new-value)
    ```


- widget\-rows &mdash; The number of rows to display.

    ```lisp
    (widget-rows (base-select base-select))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int)) name value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (setf (widget-%options-labels (%options-labels-slot %options-labels-slot))
            new-value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (widget-%options-labels (%options-labels-slot %options-labels-slot))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-index &mdash; Selected index

    ```lisp
    (setf (widget-index (index-slot index-slot)) new-value)
    ```


- widget\-index &mdash; Selected index

    ```lisp
    (widget-index (index-slot index-slot))
    ```


## select\-multiple

#### Class

Listbox that allows many items to be selected at any given time.

##### Precedence List

select\-multiple, base\-select, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, %options\-labels\-slot, disabled\-slot, standard\-object, slot\-object, t

##### Slots

- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- %options\-labels &mdash; The labels for the options.
    - :initarg :%options\-labels
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- rows &mdash; The number of rows to display.
    - :initarg :rows
- index &mdash; Selected indicies
    - :initarg :index
- options &mdash; The option values that correspond to the labels
    - :initarg :options

##### Methods

- on\-trait\-change

    ```lisp
    (on-trait-change (instance select-multiple) type (name (eql index))
                     old-value new-value source)
    ```


- widget\-value

    ```lisp
    (setf (widget-value (instance select-multiple)) new-value)
    ```


- widget\-value

    ```lisp
    (widget-value (instance select-multiple))
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (setf (widget-options (select-multiple select-multiple)) new-value)
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (widget-options (select-multiple select-multiple))
    ```


- widget\-index &mdash; Selected indicies

    ```lisp
    (setf (widget-index (select-multiple select-multiple)) new-value)
    ```


- widget\-index &mdash; Selected indicies

    ```lisp
    (widget-index (select-multiple select-multiple))
    ```


- widget\-rows &mdash; The number of rows to display.

    ```lisp
    (setf (widget-rows (base-select base-select)) new-value)
    ```


- widget\-rows &mdash; The number of rows to display.

    ```lisp
    (widget-rows (base-select base-select))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int)) name value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (setf (widget-%options-labels (%options-labels-slot %options-labels-slot))
            new-value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (widget-%options-labels (%options-labels-slot %options-labels-slot))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


## selection\-range\-slider

#### Class

Slider to select multiple contiguous items from a list.

##### Precedence List

selection\-range\-slider, label\-slider, base\-slider, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, orientation\-slot, continuous\-update\-slot, %options\-labels\-slot, standard\-object, slot\-object, t

##### Slots

- %options\-labels &mdash; The labels for the options.
    - :initarg :%options\-labels
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- orientation &mdash; Vertical or horizontal.
    - :initarg :orientation
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- readout &mdash; Display the current value of the slider next to it.
    - :initarg :readout
- readout\-format &mdash; Format for the readout
    - :initarg :readout\-format
- options &mdash; The option values that correspond to the labels
    - :initarg :options
- index &mdash; Min and max selected indices
    - :initarg :index

##### Methods

- on\-trait\-change

    ```lisp
    (on-trait-change (instance selection-range-slider) type (name (eql index))
                     old-value new-value source)
    ```


- widget\-value

    ```lisp
    (setf (widget-value (instance selection-range-slider)) new-value)
    ```


- widget\-value

    ```lisp
    (widget-value (instance selection-range-slider))
    ```


- widget\-index &mdash; Min and max selected indices

    ```lisp
    (setf (widget-index (selection-range-slider selection-range-slider))
            new-value)
    ```


- widget\-index &mdash; Min and max selected indices

    ```lisp
    (widget-index (selection-range-slider selection-range-slider))
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (setf (widget-options (label-slider label-slider)) new-value)
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (widget-options (label-slider label-slider))
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (base-slider base-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (base-slider base-slider))
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (setf (widget-readout (base-slider base-slider)) new-value)
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (widget-readout (base-slider base-slider))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (setf (widget-orientation (orientation-slot orientation-slot)) new-value)
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (widget-orientation (orientation-slot orientation-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int)) name value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (setf (widget-%options-labels (%options-labels-slot %options-labels-slot))
            new-value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (widget-%options-labels (%options-labels-slot %options-labels-slot))
    ```


## selection\-slider

#### Class

Slider to select a single item from a list or dictionary.

##### Precedence List

selection\-slider, label\-slider, base\-slider, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, orientation\-slot, continuous\-update\-slot, %options\-labels\-slot, index\-slot, standard\-object, slot\-object, t

##### Slots

- index &mdash; Selected index
    - :initarg :index
- %options\-labels &mdash; The labels for the options.
    - :initarg :%options\-labels
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- orientation &mdash; Vertical or horizontal.
    - :initarg :orientation
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- readout &mdash; Display the current value of the slider next to it.
    - :initarg :readout
- readout\-format &mdash; Format for the readout
    - :initarg :readout\-format
- options &mdash; The option values that correspond to the labels
    - :initarg :options

##### Methods

- on\-trait\-change

    ```lisp
    (on-trait-change (instance selection-slider) type (name (eql index))
                     old-value new-value source)
    ```


- widget\-value

    ```lisp
    (setf (widget-value (instance selection-slider)) new-value)
    ```


- widget\-value

    ```lisp
    (widget-value (instance selection-slider))
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (setf (widget-options (label-slider label-slider)) new-value)
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (widget-options (label-slider label-slider))
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (setf (widget-readout-format (base-slider base-slider)) new-value)
    ```


- widget\-readout\-format &mdash; Format for the readout

    ```lisp
    (widget-readout-format (base-slider base-slider))
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (setf (widget-readout (base-slider base-slider)) new-value)
    ```


- widget\-readout &mdash; Display the current value of the slider next to it.

    ```lisp
    (widget-readout (base-slider base-slider))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (setf (widget-orientation (orientation-slot orientation-slot)) new-value)
    ```


- widget\-orientation &mdash; Vertical or horizontal.

    ```lisp
    (widget-orientation (orientation-slot orientation-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int)) name value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (setf (widget-%options-labels (%options-labels-slot %options-labels-slot))
            new-value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (widget-%options-labels (%options-labels-slot %options-labels-slot))
    ```


- widget\-index &mdash; Selected index

    ```lisp
    (setf (widget-index (index-slot index-slot)) new-value)
    ```


- widget\-index &mdash; Selected index

    ```lisp
    (widget-index (index-slot index-slot))
    ```


## send\-custom

## Function

```lisp
(send-custom widget content &optional buffers)
```

## sidecar

#### Class

Widget used as a context manager to display output.

This widget can capture and display stdout, stderr, and rich output.  To use it,
create an instance of it and display it.

You can then use the widget as a context manager: any output produced while in
the context will be captured and displayed in the widget instead of the standard
output area.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar out (make-instance 'output))
(with-output out
  (print "prints to output area")
```

##### Precedence List

sidecar, output, dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- msg\-id &mdash; Parent message id of messages to capture
    - :initarg :msg\-id
- outputs &mdash; The output messages synced from the frontend.
    - :initarg :outputs
- title
    - :initarg :title

##### Methods

- widget\-title &mdash; automatically generated writer method

    ```lisp
    (setf (widget-title (sidecar sidecar)) new-value)
    ```


- widget\-title &mdash; automatically generated reader method

    ```lisp
    (widget-title (sidecar sidecar))
    ```


- widget\-outputs &mdash; The output messages synced from the frontend.

    ```lisp
    (setf (widget-outputs (output output)) new-value)
    ```


- widget\-outputs &mdash; The output messages synced from the frontend.

    ```lisp
    (widget-outputs (output output))
    ```


- widget\-msg\-id &mdash; Parent message id of messages to capture

    ```lisp
    (setf (widget-msg-id (output output)) new-value)
    ```


- widget\-msg\-id &mdash; Parent message id of messages to capture

    ```lisp
    (widget-msg-id (output output))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## slider\-style

#### Class

##### Precedence List

slider\-style, description\-style, style, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- description\-width &mdash; Width of the description to the side of the control.
    - :initarg :description\-width
- handle\-color &mdash; Color of the slider handle.
    - :initarg :handle\-color

##### Methods

- widget\-handle\-color &mdash; Color of the slider handle.

    ```lisp
    (setf (widget-handle-color (slider-style slider-style)) new-value)
    ```


- widget\-handle\-color &mdash; Color of the slider handle.

    ```lisp
    (widget-handle-color (slider-style slider-style))
    ```


- widget\-description\-width &mdash; Width of the description to the side of the control.

    ```lisp
    (setf (widget-description-width (description-style description-style))
            new-value)
    ```


- widget\-description\-width &mdash; Width of the description to the side of the control.

    ```lisp
    (widget-description-width (description-style description-style))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## style

#### Class

##### Precedence List

style, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version

##### Methods

- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## styled\-widget

#### Class

##### Precedence List

styled\-widget, dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style

##### Methods

- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## tab

#### Class

Displays children each on a separate accordion tab.

##### Precedence List

tab, accordion, box, dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- box\-style &mdash; Use a predefined styling for the box.
    - :initarg :box\-style
- children &mdash; List of widget children.
    - :initarg :children
- %titles &mdash; Titles of the pages.
    - :initarg :%titles
- selected\-index &mdash; The index of the selected page. This is either an integer selecting a particular sub-widget, or nil to have no widgets selected.
    - :initarg :selected\-index

##### Methods

- validate\-trait

    ```lisp
    (validate-trait (w accordion) (type (eql int)) name value)
    ```


- widget\-selected\-index &mdash; The index of the selected page. This is either an integer selecting a particular sub-widget, or nil to have no widgets selected.

    ```lisp
    (setf (widget-selected-index (accordion accordion)) new-value)
    ```


- widget\-selected\-index &mdash; The index of the selected page. This is either an integer selecting a particular sub-widget, or nil to have no widgets selected.

    ```lisp
    (widget-selected-index (accordion accordion))
    ```


- widget\-%titles &mdash; Titles of the pages.

    ```lisp
    (setf (widget-%titles (accordion accordion)) new-value)
    ```


- widget\-%titles &mdash; Titles of the pages.

    ```lisp
    (widget-%titles (accordion accordion))
    ```


- widget\-children &mdash; List of widget children.

    ```lisp
    (setf (widget-children (box box)) new-value)
    ```


- widget\-children &mdash; List of widget children.

    ```lisp
    (widget-children (box box))
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (setf (widget-box-style (box box)) new-value)
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (widget-box-style (box box))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## text

#### Class

Single line textbox widget.

##### Precedence List

text, base\-text, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, continuous\-update\-slot, placeholder\-slot, string\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; String value
    - :initarg :value
- placeholder &mdash; Placeholder text to display when nothing has been typed.
    - :initarg :placeholder
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip

##### Methods

- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (setf (widget-placeholder (placeholder-slot placeholder-slot)) new-value)
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (widget-placeholder (placeholder-slot placeholder-slot))
    ```


- widget\-value &mdash; String value

    ```lisp
    (setf (widget-value (string-value-slot string-value-slot)) new-value)
    ```


- widget\-value &mdash; String value

    ```lisp
    (widget-value (string-value-slot string-value-slot))
    ```


## text\-area

#### Class

Multiline text area widget.

##### Precedence List

text\-area, text, base\-text, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, continuous\-update\-slot, placeholder\-slot, string\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; String value
    - :initarg :value
- placeholder &mdash; Placeholder text to display when nothing has been typed.
    - :initarg :placeholder
- continuous\-update &mdash; Update the value of the widget as the user is holding the slider.
    - :initarg :continuous\-update
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- rows &mdash; The number of rows to display.
    - :initarg :rows

##### Methods

- widget\-rows &mdash; The number of rows to display.

    ```lisp
    (setf (widget-rows (text-area text-area)) new-value)
    ```


- widget\-rows &mdash; The number of rows to display.

    ```lisp
    (widget-rows (text-area text-area))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (setf (widget-continuous-update
           (continuous-update-slot continuous-update-slot))
            new-value)
    ```


- widget\-continuous\-update &mdash; Update the value of the widget as the user is holding the slider.

    ```lisp
    (widget-continuous-update (continuous-update-slot continuous-update-slot))
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (setf (widget-placeholder (placeholder-slot placeholder-slot)) new-value)
    ```


- widget\-placeholder &mdash; Placeholder text to display when nothing has been typed.

    ```lisp
    (widget-placeholder (placeholder-slot placeholder-slot))
    ```


- widget\-value &mdash; String value

    ```lisp
    (setf (widget-value (string-value-slot string-value-slot)) new-value)
    ```


- widget\-value &mdash; String value

    ```lisp
    (widget-value (string-value-slot string-value-slot))
    ```


## toggle\-button

#### Class

Displays a boolean `value` in the form of a toggle button.

##### Precedence List

toggle\-button, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, button\-style\-slot, disabled\-slot, icon\-slot, tooltip\-slot, bool\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Bool value
    - :initarg :value
- tooltip &mdash; Tooltip caption.
    - :initarg :tooltip
- icon &mdash; Font-awesome icon name, without the 'fa-' prefix.
    - :initarg :icon
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- button\-style &mdash; Use a predefined styling for the button.
    - :initarg :button\-style
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip

##### Methods

- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-button\-style &mdash; Use a predefined styling for the button.

    ```lisp
    (setf (widget-button-style (button-style-slot button-style-slot)) new-value)
    ```


- widget\-button\-style &mdash; Use a predefined styling for the button.

    ```lisp
    (widget-button-style (button-style-slot button-style-slot))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-icon &mdash; Font-awesome icon name, without the 'fa-' prefix.

    ```lisp
    (setf (widget-icon (icon-slot icon-slot)) new-value)
    ```


- widget\-icon &mdash; Font-awesome icon name, without the 'fa-' prefix.

    ```lisp
    (widget-icon (icon-slot icon-slot))
    ```


- widget\-tooltip &mdash; Tooltip caption.

    ```lisp
    (setf (widget-tooltip (tooltip-slot tooltip-slot)) new-value)
    ```


- widget\-tooltip &mdash; Tooltip caption.

    ```lisp
    (widget-tooltip (tooltip-slot tooltip-slot))
    ```


- widget\-value &mdash; Bool value

    ```lisp
    (setf (widget-value (bool-value-slot bool-value-slot)) new-value)
    ```


- widget\-value &mdash; Bool value

    ```lisp
    (widget-value (bool-value-slot bool-value-slot))
    ```


## toggle\-button\-style

## toggle\-buttons

#### Class

Group of toggle buttons that represent an enumeration. Only one toggle button
can be toggled at any point in time.

##### Precedence List

toggle\-buttons, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, %options\-labels\-slot, button\-style\-slot, disabled\-slot, index\-slot, standard\-object, slot\-object, t

##### Slots

- index &mdash; Selected index
    - :initarg :index
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- button\-style &mdash; Use a predefined styling for the button.
    - :initarg :button\-style
- %options\-labels &mdash; The labels for the options.
    - :initarg :%options\-labels
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- icons &mdash; Icons names for each button (FontAwesome names without the fa- prefix).
    - :initarg :icons
- options &mdash; The option values that correspond to the labels
    - :initarg :options
- tooltips &mdash; Tooltips for each button.
    - :initarg :tooltips

##### Methods

- on\-trait\-change

    ```lisp
    (on-trait-change (instance toggle-buttons) type (name (eql index))
                     old-value new-value source)
    ```


- widget\-value

    ```lisp
    (setf (widget-value (instance toggle-buttons)) new-value)
    ```


- widget\-value

    ```lisp
    (widget-value (instance toggle-buttons))
    ```


- widget\-tooltips &mdash; Tooltips for each button.

    ```lisp
    (setf (widget-tooltips (toggle-buttons toggle-buttons)) new-value)
    ```


- widget\-tooltips &mdash; Tooltips for each button.

    ```lisp
    (widget-tooltips (toggle-buttons toggle-buttons))
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (setf (widget-options (toggle-buttons toggle-buttons)) new-value)
    ```


- widget\-options &mdash; The option values that correspond to the labels

    ```lisp
    (widget-options (toggle-buttons toggle-buttons))
    ```


- widget\-icons &mdash; Icons names for each button (FontAwesome names without the fa- prefix).

    ```lisp
    (setf (widget-icons (toggle-buttons toggle-buttons)) new-value)
    ```


- widget\-icons &mdash; Icons names for each button (FontAwesome names without the fa- prefix).

    ```lisp
    (widget-icons (toggle-buttons toggle-buttons))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int-list)) name value)
    ```


- validate\-trait

    ```lisp
    (validate-trait (w %options-labels-slot) (type (eql int)) name value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (setf (widget-%options-labels (%options-labels-slot %options-labels-slot))
            new-value)
    ```


- widget\-%options\-labels &mdash; The labels for the options.

    ```lisp
    (widget-%options-labels (%options-labels-slot %options-labels-slot))
    ```


- widget\-button\-style &mdash; Use a predefined styling for the button.

    ```lisp
    (setf (widget-button-style (button-style-slot button-style-slot)) new-value)
    ```


- widget\-button\-style &mdash; Use a predefined styling for the button.

    ```lisp
    (widget-button-style (button-style-slot button-style-slot))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-index &mdash; Selected index

    ```lisp
    (setf (widget-index (index-slot index-slot)) new-value)
    ```


- widget\-index &mdash; Selected index

    ```lisp
    (widget-index (index-slot index-slot))
    ```


## trait\-metaclass

#### Class

##### Precedence List

trait\-metaclass, standard\-class, std\-class, slot\-class, pcl\-class, class, dependent\-update\-mixin, plist\-mixin, definition\-source\-mixin, standard\-specializer, specializer, metaobject, standard\-object, slot\-object, t

##### Slots

- sb\-pcl::%type &mdash; 
- sb\-pcl::source &mdash; 
    - :initarg sb\-pcl::source
- sb\-pcl::plist &mdash; 
    - :initarg sb\-pcl::plist
- sb\-pcl::name &mdash; 
    - :initarg :name
- sb\-pcl::class\-eq\-specializer &mdash; 
- sb\-pcl::direct\-superclasses &mdash; 
- sb\-pcl::direct\-subclasses &mdash; 
- sb\-pcl::direct\-methods &mdash; 
- sb\-pcl::%documentation &mdash; 
    - :initarg :documentation
- sb\-pcl::safe\-p &mdash; 
    - :initarg sb\-pcl::safe\-p
- sb\-pcl::finalized\-p &mdash; 
- sb\-pcl::%class\-precedence\-list &mdash; 
- sb\-pcl::cpl\-available\-p &mdash; 
- sb\-pcl::can\-precede\-list &mdash; 
- sb\-pcl::incompatible\-superclass\-list &mdash; 
- sb\-kernel:wrapper &mdash; 
- sb\-pcl::prototype &mdash; 
- sb\-pcl::direct\-slots &mdash; 
- sb\-pcl::slots &mdash; 

## v\-box

#### Class

Displays multiple widgets vertically using the flexible box model.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar title-widget (make-instance 'html :value "<em>Box Example</em>"))
(defvar slider (make-instance 'int-slider))
(make-instance 'v-box :children (list title-widget slider))
```

##### Precedence List

v\-box, box, dom\-widget, widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- box\-style &mdash; Use a predefined styling for the box.
    - :initarg :box\-style
- children &mdash; List of widget children.
    - :initarg :children

##### Methods

- widget\-children &mdash; List of widget children.

    ```lisp
    (setf (widget-children (box box)) new-value)
    ```


- widget\-children &mdash; List of widget children.

    ```lisp
    (widget-children (box box))
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (setf (widget-box-style (box box)) new-value)
    ```


- widget\-box\-style &mdash; Use a predefined styling for the box.

    ```lisp
    (widget-box-style (box box))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## valid

#### Class

Displays a boolean `value` in the form of a green check (True / valid) or a red
cross (False / invalid).

##### Precedence List

valid, description\-widget, styled\-widget, dom\-widget, widget, has\-traits, comm, source, disabled\-slot, bool\-value\-slot, standard\-object, slot\-object, t

##### Slots

- value &mdash; Bool value
    - :initarg :value
- disabled &mdash; Enable or disable user changes.
    - :initarg :disabled
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- style &mdash; Reference to style widget.
    - :initarg :style
- description &mdash; Description of the control.
    - :initarg :description
- description\-tooltip &mdash; Tooltip for the description (defaults to description).
    - :initarg :description\-tooltip
- readout &mdash; Message displayed when the value is False
    - :initarg :readout

##### Methods

- widget\-readout &mdash; Message displayed when the value is False

    ```lisp
    (setf (widget-readout (valid valid)) new-value)
    ```


- widget\-readout &mdash; Message displayed when the value is False

    ```lisp
    (widget-readout (valid valid))
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (setf (widget-description-tooltip (description-widget description-widget))
            new-value)
    ```


- widget\-description\-tooltip &mdash; Tooltip for the description (defaults to description).

    ```lisp
    (widget-description-tooltip (description-widget description-widget))
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (setf (widget-description (description-widget description-widget))
            new-value)
    ```


- widget\-description &mdash; Description of the control.

    ```lisp
    (widget-description (description-widget description-widget))
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (setf (widget-style (styled-widget styled-widget)) new-value)
    ```


- widget\-style &mdash; Reference to style widget.

    ```lisp
    (widget-style (styled-widget styled-widget))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (setf (widget-disabled (disabled-slot disabled-slot)) new-value)
    ```


- widget\-disabled &mdash; Enable or disable user changes.

    ```lisp
    (widget-disabled (disabled-slot disabled-slot))
    ```


- widget\-value &mdash; Bool value

    ```lisp
    (setf (widget-value (bool-value-slot bool-value-slot)) new-value)
    ```


- widget\-value &mdash; Bool value

    ```lisp
    (widget-value (bool-value-slot bool-value-slot))
    ```


## video

#### Class

Displays a video as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
video data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to "mp4").

If you pass `"url"` to the `"format"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8.

##### Precedence List

video, audio, dom\-widget, widget, has\-traits, comm, source, format\-slot, byte\-value\-slot, height\-slot, width\-slot, standard\-object, slot\-object, t

##### Slots

- width &mdash; Width of the media in pixels.
    - :initarg :width
- height &mdash; Height of the media in pixels.
    - :initarg :height
- value &mdash; The value as a byte string.
    - :initarg :value
- format &mdash; The format of the media.
    - :initarg :format
- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version
- %dom\-classes &mdash; CSS classes applied to widget DOM element
    - :initarg :%dom\-classes
- layout &mdash; Reference to layout widget.
    - :initarg :layout
- autoplay &mdash; When true, the audio starts when it's displayed.
    - :initarg :autoplay
- controls &mdash; Specifies that media controls should be displayed (such as a play/pause button etc)
    - :initarg :controls
- loop &mdash; When true, the audio will start from the beginning after finishing
    - :initarg :loop

##### Methods

- widget\-loop &mdash; When true, the audio will start from the beginning after finishing

    ```lisp
    (setf (widget-loop (audio audio)) new-value)
    ```


- widget\-loop &mdash; When true, the audio will start from the beginning after finishing

    ```lisp
    (widget-loop (audio audio))
    ```


- widget\-controls &mdash; Specifies that media controls should be displayed (such as a play/pause button etc)

    ```lisp
    (setf (widget-controls (audio audio)) new-value)
    ```


- widget\-controls &mdash; Specifies that media controls should be displayed (such as a play/pause button etc)

    ```lisp
    (widget-controls (audio audio))
    ```


- widget\-autoplay &mdash; When true, the audio starts when it's displayed.

    ```lisp
    (setf (widget-autoplay (audio audio)) new-value)
    ```


- widget\-autoplay &mdash; When true, the audio starts when it's displayed.

    ```lisp
    (widget-autoplay (audio audio))
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (setf (widget-layout (dom-widget dom-widget)) new-value)
    ```


- widget\-layout &mdash; Reference to layout widget.

    ```lisp
    (widget-layout (dom-widget dom-widget))
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (setf (widget-%dom-classes (dom-widget dom-widget)) new-value)
    ```


- widget\-%dom\-classes &mdash; CSS classes applied to widget DOM element

    ```lisp
    (widget-%dom-classes (dom-widget dom-widget))
    ```


- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


- widget\-format &mdash; The format of the media.

    ```lisp
    (setf (widget-format (format-slot format-slot)) new-value)
    ```


- widget\-format &mdash; The format of the media.

    ```lisp
    (widget-format (format-slot format-slot))
    ```


- widget\-value &mdash; The value as a byte string.

    ```lisp
    (setf (widget-value (byte-value-slot byte-value-slot)) new-value)
    ```


- widget\-value &mdash; The value as a byte string.

    ```lisp
    (widget-value (byte-value-slot byte-value-slot))
    ```


- widget\-height &mdash; Height of the media in pixels.

    ```lisp
    (setf (widget-height (height-slot height-slot)) new-value)
    ```


- widget\-height &mdash; Height of the media in pixels.

    ```lisp
    (widget-height (height-slot height-slot))
    ```


- widget\-width &mdash; Width of the media in pixels.

    ```lisp
    (setf (widget-width (width-slot width-slot)) new-value)
    ```


- widget\-width &mdash; Width of the media in pixels.

    ```lisp
    (widget-width (width-slot width-slot))
    ```


## widget

#### Class

Base class for all Jupyter widgets.

##### Precedence List

widget, has\-traits, comm, source, standard\-object, slot\-object, t

##### Slots

- jupyter::sink
    - :initarg :sink
- jupyter:comm\-id
    - :initarg :comm\-id
- jupyter::target\-name
    - :initarg :target\-name
- jupyter:kernel
    - :initarg :kernel
- on\-trait\-change &mdash; Instance specific trait notification
    - :initarg :on\-trait\-change
- %model\-name &mdash; Name of the model.
    - :initarg :%model\-name
- %model\-module &mdash; The namespace for the model.
    - :initarg :%model\-module
- %model\-module\-version &mdash; A semver requirement for namespace version containing the model.
    - :initarg :%model\-module\-version
- %view\-name &mdash; Name of the view.
    - :initarg :%view\-name
- %view\-module &mdash; The namespace for the view.
    - :initarg :%view\-module
- %view\-module\-version &mdash; A semver requirement for namespace version containing the view.
    - :initarg :%view\-module\-version

##### Methods

- directional\-link

    ```lisp
    (directional-link (source widget) source-trait (target widget) target-trait
                      &optional sync)
    ```


- link

    ```lisp
    (link (source widget) source-trait (target widget) target-trait &optional
          sync)
    ```


- serialize\-trait

    ```lisp
    (serialize-trait object type name (value widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w widget) type name old-value new-value source)
    ```


- jupyter:on\-comm\-message

    ```lisp
    (on-comm-message (w widget) data metadata buffers)
    ```


- jupyter:mime\-bundle\-data

    ```lisp
    (mime-bundle-data (w widget))
    ```


- widget\-%view\-module\-version &mdash; A semver requirement for namespace version containing the view.

    ```lisp
    (widget-%view-module-version (widget widget))
    ```


- widget\-%view\-module &mdash; The namespace for the view.

    ```lisp
    (widget-%view-module (widget widget))
    ```


- widget\-%view\-name &mdash; Name of the view.

    ```lisp
    (widget-%view-name (widget widget))
    ```


- widget\-%module\-module\-version &mdash; A semver requirement for namespace version containing the model.

    ```lisp
    (widget-%module-module-version (widget widget))
    ```


- widget\-%module\-module &mdash; The namespace for the model.

    ```lisp
    (widget-%module-module (widget widget))
    ```


- widget\-%module\-name &mdash; Name of the model.

    ```lisp
    (widget-%module-name (widget widget))
    ```


- on\-trait\-change

    ```lisp
    (on-trait-change (w has-traits) type name old-value new-value source)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (setf (widget-on-trait-change (has-traits has-traits)) new-value)
    ```


- widget\-on\-trait\-change &mdash; Instance specific trait notification

    ```lisp
    (widget-on-trait-change (has-traits has-traits))
    ```


- jupyter:comm\-id &mdash; automatically generated reader method

    ```lisp
    (comm-id (comm comm))
    ```


## widget\-%dom\-classes

## Generic Function

```lisp
(widget-%dom-classes object)
```

## widget\-%module\-module

## Generic Function

```lisp
(widget-%module-module object)
```

## widget\-%module\-module\-version

## Generic Function

```lisp
(widget-%module-module-version object)
```

## widget\-%module\-name

## Generic Function

```lisp
(widget-%module-name object)
```

## widget\-%options\-labels

## Generic Function

```lisp
(widget-%options-labels object)
```

## widget\-%playing

## Generic Function

```lisp
(widget-%playing object)
```

## widget\-%repeat

## Generic Function

```lisp
(widget-%repeat object)
```

## widget\-%titles

## Generic Function

```lisp
(widget-%titles object)
```

## widget\-%view\-module

## Generic Function

```lisp
(widget-%view-module object)
```

## widget\-%view\-module\-version

## Generic Function

```lisp
(widget-%view-module-version object)
```

## widget\-%view\-name

## Generic Function

```lisp
(widget-%view-name object)
```

## widget\-accept

## Generic Function

```lisp
(widget-accept object)
```

## widget\-align\-content

## Generic Function

```lisp
(widget-align-content object)
```

## widget\-align\-items

## Generic Function

```lisp
(widget-align-items object)
```

## widget\-align\-self

## Generic Function

```lisp
(widget-align-self object)
```

## widget\-autoplay

## Generic Function

```lisp
(widget-autoplay object)
```

## widget\-axes

## Generic Function

```lisp
(widget-axes object)
```

## widget\-bar\-color

## Generic Function

```lisp
(widget-bar-color object)
```

## widget\-bar\-style

## Generic Function

```lisp
(widget-bar-style object)
```

## widget\-base

## Generic Function

```lisp
(widget-base object)
```

## widget\-border

## Generic Function

```lisp
(widget-border object)
```

## widget\-bottom

## Generic Function

```lisp
(widget-bottom object)
```

## widget\-box\-style

## Generic Function

```lisp
(widget-box-style object)
```

## widget\-button\-color

## Generic Function

```lisp
(widget-button-color object)
```

## widget\-button\-style

## Generic Function

```lisp
(widget-button-style object)
```

## widget\-button\-width

## Generic Function

```lisp
(widget-button-width object)
```

## widget\-buttons

## Generic Function

```lisp
(widget-buttons object)
```

## widget\-children

## Generic Function

```lisp
(widget-children object)
```

## widget\-concise

## Generic Function

```lisp
(widget-concise object)
```

## widget\-connected

## Generic Function

```lisp
(widget-connected object)
```

## widget\-continuous\-update

## Generic Function

```lisp
(widget-continuous-update object)
```

## widget\-controls

## Generic Function

```lisp
(widget-controls object)
```

## widget\-data

## Generic Function

```lisp
(widget-data object)
```

## widget\-description

## Generic Function

```lisp
(widget-description object)
```

## widget\-description\-tooltip

## Generic Function

```lisp
(widget-description-tooltip object)
```

## widget\-description\-width

## Generic Function

```lisp
(widget-description-width object)
```

## widget\-disabled

## Generic Function

```lisp
(widget-disabled object)
```

## widget\-display

## Generic Function

```lisp
(widget-display object)
```

## widget\-ensure\-option

## Generic Function

```lisp
(widget-ensure-option object)
```

## widget\-error

## Generic Function

```lisp
(widget-error object)
```

## widget\-flex

## Generic Function

```lisp
(widget-flex object)
```

## widget\-flex\-flow

## Generic Function

```lisp
(widget-flex-flow object)
```

## widget\-font\-weight

## Generic Function

```lisp
(widget-font-weight object)
```

## widget\-format

## Generic Function

```lisp
(widget-format object)
```

## widget\-grid\-area

## Generic Function

```lisp
(widget-grid-area object)
```

## widget\-grid\-auto\-columns

## Generic Function

```lisp
(widget-grid-auto-columns object)
```

## widget\-grid\-auto\-flow

## Generic Function

```lisp
(widget-grid-auto-flow object)
```

## widget\-grid\-auto\-rows

## Generic Function

```lisp
(widget-grid-auto-rows object)
```

## widget\-grid\-column

## Generic Function

```lisp
(widget-grid-column object)
```

## widget\-grid\-gap

## Generic Function

```lisp
(widget-grid-gap object)
```

## widget\-grid\-row

## Generic Function

```lisp
(widget-grid-row object)
```

## widget\-grid\-template\-areas

## Generic Function

```lisp
(widget-grid-template-areas object)
```

## widget\-grid\-template\-columns

## Generic Function

```lisp
(widget-grid-template-columns object)
```

## widget\-grid\-template\-rows

## Generic Function

```lisp
(widget-grid-template-rows object)
```

## widget\-handle\-color

## Generic Function

```lisp
(widget-handle-color object)
```

## widget\-height

## Generic Function

```lisp
(widget-height object)
```

## widget\-icon

## Generic Function

```lisp
(widget-icon object)
```

## widget\-icons

## Generic Function

```lisp
(widget-icons object)
```

## widget\-indent

## Generic Function

```lisp
(widget-indent object)
```

## widget\-index

## Generic Function

```lisp
(widget-index object)
```

## widget\-interval

## Generic Function

```lisp
(widget-interval object)
```

## widget\-justify\-content

## Generic Function

```lisp
(widget-justify-content object)
```

## widget\-justify\-items

## Generic Function

```lisp
(widget-justify-items object)
```

## widget\-layout

## Generic Function

```lisp
(widget-layout object)
```

## widget\-left

## Generic Function

```lisp
(widget-left object)
```

## widget\-loop

## Generic Function

```lisp
(widget-loop object)
```

## widget\-mapping

## Generic Function

```lisp
(widget-mapping object)
```

## widget\-margin

## Generic Function

```lisp
(widget-margin object)
```

## widget\-max

## Generic Function

```lisp
(widget-max object)
```

## widget\-max\-height

## Generic Function

```lisp
(widget-max-height object)
```

## widget\-max\-width

## Generic Function

```lisp
(widget-max-width object)
```

## widget\-metadata

## Generic Function

```lisp
(widget-metadata object)
```

## widget\-min

## Generic Function

```lisp
(widget-min object)
```

## widget\-min\-height

## Generic Function

```lisp
(widget-min-height object)
```

## widget\-min\-width

## Generic Function

```lisp
(widget-min-width object)
```

## widget\-msg\-id

## Generic Function

```lisp
(widget-msg-id object)
```

## widget\-multiple

## Generic Function

```lisp
(widget-multiple object)
```

## widget\-name

## Generic Function

```lisp
(widget-name object)
```

## widget\-object\-fit

## Generic Function

```lisp
(widget-object-fit object)
```

## widget\-object\-position

## Generic Function

```lisp
(widget-object-position object)
```

## widget\-on\-trait\-change

## Generic Function

```lisp
(widget-on-trait-change object)
```

## widget\-options

## Generic Function

```lisp
(widget-options object)
```

## widget\-order

## Generic Function

```lisp
(widget-order object)
```

## widget\-orientation

## Generic Function

```lisp
(widget-orientation object)
```

## widget\-outputs

## Generic Function

```lisp
(widget-outputs object)
```

## widget\-overflow

## Generic Function

```lisp
(widget-overflow object)
```

## widget\-overflow\-x

## Generic Function

```lisp
(widget-overflow-x object)
```

## widget\-overflow\-y

## Generic Function

```lisp
(widget-overflow-y object)
```

## widget\-padding

## Generic Function

```lisp
(widget-padding object)
```

## widget\-placeholder

## Generic Function

```lisp
(widget-placeholder object)
```

## widget\-pressed

## Generic Function

```lisp
(widget-pressed object)
```

## widget\-readout

## Generic Function

```lisp
(widget-readout object)
```

## widget\-readout\-format

## Generic Function

```lisp
(widget-readout-format object)
```

## widget\-right

## Generic Function

```lisp
(widget-right object)
```

## widget\-rows

## Generic Function

```lisp
(widget-rows object)
```

## widget\-selected\-index

## Generic Function

```lisp
(widget-selected-index object)
```

## widget\-show\-repeat

## Generic Function

```lisp
(widget-show-repeat object)
```

## widget\-source

## Generic Function

```lisp
(widget-source object)
```

## widget\-step

## Generic Function

```lisp
(widget-step object)
```

## widget\-style

## Generic Function

```lisp
(widget-style object)
```

## widget\-target

## Generic Function

```lisp
(widget-target object)
```

## widget\-timestamp

## Generic Function

```lisp
(widget-timestamp object)
```

## widget\-tooltip

## Generic Function

```lisp
(widget-tooltip object)
```

## widget\-tooltips

## Generic Function

```lisp
(widget-tooltips object)
```

## widget\-top

## Generic Function

```lisp
(widget-top object)
```

## widget\-value

## Generic Function

```lisp
(widget-value object)
```

## widget\-visibility

## Generic Function

```lisp
(widget-visibility object)
```

## widget\-width

## Generic Function

```lisp
(widget-width object)
```

## with\-output

## Macro

Evaluate body with all output sent to the output widget.

```lisp
(with-output output
  &body
  body)
```