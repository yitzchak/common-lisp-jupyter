# Package jupyter-widgets


## Generic Function `widget-mapping`

### Definition

```lisp
(widget-mapping sb-pcl::object)
```

## Generic Function `widget-mapping`

### Definition

```lisp
(widget-mapping sb-pcl::new-value sb-pcl::object)
```


## Class `controller-button`

Represents a gamepad or joystick button.

### Superclasses

- `controller-button`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:pressed`

### Slots

- `pressed` &mdash; Whether the button is pressed.


## Generic Function `widget-bar-color`

### Definition

```lisp
(widget-bar-color sb-pcl::object)
```

## Generic Function `widget-bar-color`

### Definition

```lisp
(widget-bar-color sb-pcl::new-value sb-pcl::object)
```


## Class `bounded-float-text`

Displays a float value within a textbox. Value must be within the range
specified. For a textbox in which the value doesn't need to be within a specific
range, use float-text.

### Superclasses

- `bounded-float-text`
- `float-text`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:max`
- `:min`
- `:value`
- `:step`
- `:continuous-update`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Generic Function `widget-button-style`

### Definition

```lisp
(widget-button-style sb-pcl::object)
```

## Generic Function `widget-button-style`

### Definition

```lisp
(widget-button-style sb-pcl::new-value sb-pcl::object)
```


## Class `float-log-slider`

Slider/trackbar of logarithmic floating values with the specified range.

### Superclasses

- `float-log-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:step`
- `:max`
- `:min`
- `:continuous-update`
- `:orientation`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:readout`
- `:readout-format`
- `:base`

### Slots

- `base` &mdash; Base for the logarithm


## Generic Function `widget-grid-gap`

### Definition

```lisp
(widget-grid-gap sb-pcl::object)
```

## Generic Function `widget-grid-gap`

### Definition

```lisp
(widget-grid-gap sb-pcl::new-value sb-pcl::object)
```


## Class `directional-link`

A directional link

### Superclasses

- `directional-link`
- `link`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:source`
- `:target`


## Class `output`

Widget used as a context manager to display output.

This widget can capture and display stdout, stderr, and rich output.  To use it,
create an instance of it and display it.

You can then use the widget as a context manager: any output produced while in
the context will be captured and displayed in the widget instead of the standard
output area.

### Example

```lisp
(use-package :jupyter-widgets)
(defvar out (make-widget 'output))
(with-output out
  (print "prints to output area")
```

### Superclasses

- `output`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:msg-id`
- `:outputs`

### Slots

- `msg-id` &mdash; Parent message id of messages to capture
- `outputs` &mdash; The output messages synced from the frontend.


## Generic Function `widget-height`

### Definition

```lisp
(widget-height sb-pcl::object)
```

## Generic Function `widget-height`

### Definition

```lisp
(widget-height sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-readout`

### Definition

```lisp
(widget-readout sb-pcl::object)
```

## Generic Function `widget-readout`

### Definition

```lisp
(widget-readout sb-pcl::new-value sb-pcl::object)
```


## Class `tab`

Displays children each on a separate accordion tab.

### Superclasses

- `tab`
- `accordion`
- `box`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:box-style`
- `:children`
- `:%titles`
- `:selected-index`


## Class `html-math`

Renders the string `value` as HTML, and render mathematics.

### Superclasses

- `html-math`
- `label`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:placeholder`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Generic Function `widget-outputs`

### Definition

```lisp
(widget-outputs sb-pcl::object)
```

## Generic Function `widget-outputs`

### Definition

```lisp
(widget-outputs sb-pcl::new-value sb-pcl::object)
```


## Class `checkbox`

Displays a boolean `value` in the form of a checkbox.

### Superclasses

- `checkbox`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:indent`

### Slots

- `indent` &mdash; Indent the control to align with other controls with a description.


## Class `html`

Renders the string `value` as HTML.

### Superclasses

- `html`
- `label`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:placeholder`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Generic Function `widget-left`

### Definition

```lisp
(widget-left sb-pcl::object)
```

## Generic Function `widget-left`

### Definition

```lisp
(widget-left sb-pcl::new-value sb-pcl::object)
```


## Class `accordion`

Displays children each on a separate accordion page.

### Superclasses

- `accordion`
- `box`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:box-style`
- `:children`
- `:%titles`
- `:selected-index`

### Slots

- `%titles` &mdash; Titles of the pages.
- `selected-index` &mdash; The index of the selected page. This is either an integer selecting a particular sub-widget, or nil to have no widgets selected.


## Generic Function `widget-index`

### Definition

```lisp
(widget-index sb-pcl::object)
```

## Generic Function `widget-index`

### Definition

```lisp
(widget-index sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-%view-name`

### Definition

```lisp
(widget-%view-name sb-pcl::object)
```


## Generic Function `widget-loop`

### Definition

```lisp
(widget-loop sb-pcl::object)
```

## Generic Function `widget-loop`

### Definition

```lisp
(widget-loop sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-handle-color`

### Definition

```lisp
(widget-handle-color sb-pcl::object)
```

## Generic Function `widget-handle-color`

### Definition

```lisp
(widget-handle-color sb-pcl::new-value sb-pcl::object)
```


## Class `toggle-button`

Displays a boolean `value` in the form of a toggle button.

### Superclasses

- `toggle-button`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:tooltip`
- `:icon`
- `:disabled`
- `:button-style`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Generic Function `widget-icons`

### Definition

```lisp
(widget-icons sb-pcl::object)
```

## Generic Function `widget-icons`

### Definition

```lisp
(widget-icons sb-pcl::new-value sb-pcl::object)
```


## Class `radio-buttons`

Group of radio buttons that represent an enumeration. Only one radio button can
be toggled at any point in time.

### Superclasses

- `radio-buttons`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:index`
- `:disabled`
- `:%options-labels`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Class `audio`

Displays an audio clip as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
audio data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to "mp3").

If you pass `"url"` to the `"format"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8.

### Superclasses

- `audio`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:format`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:autoplay`
- `:controls`
- `:loop`

### Slots

- `autoplay` &mdash; When true, the audio starts when it's displayed.
- `controls` &mdash; Specifies that media controls should be displayed (such as a play/pause button etc)
- `loop` &mdash; When true, the audio will start from the beginning after finishing


## Generic Function `widget-name`

### Definition

```lisp
(widget-name sb-pcl::object)
```

## Generic Function `widget-name`

### Definition

```lisp
(widget-name sb-pcl::new-value sb-pcl::object)
```


## Class `float-text`

Displays a float value within a textbox. For a textbox in which the value must
be within a specific range, use BoundedFloatText.

### Superclasses

- `float-text`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:step`
- `:continuous-update`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Class `float-progress`

Displays a progress bar.

### Superclasses

- `float-progress`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:max`
- `:min`
- `:orientation`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:bar-style`


## Class `box`

Displays multiple widgets in a group. The widgets are laid out horizontally.

Example

(use-package :jupyter-widgets)
(defvar title-widget (make-widget 'html :value "<em>Box Example</em>"))
(defvar slider (make-widget 'int-slider))
(make-widget 'box :children (list title-widget slider))

### Superclasses

- `box`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:box-style`
- `:children`

### Slots

- `box-style` &mdash; Use a predefined styling for the box.
- `children` &mdash; List of widget children.


## Generic Function `widget-overflow-y`

### Definition

```lisp
(widget-overflow-y sb-pcl::object)
```

## Generic Function `widget-overflow-y`

### Definition

```lisp
(widget-overflow-y sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-icon`

### Definition

```lisp
(widget-icon sb-pcl::object)
```

## Generic Function `widget-icon`

### Definition

```lisp
(widget-icon sb-pcl::new-value sb-pcl::object)
```


## Class `bounded-int-text`

Textbox widget that represents an integer bounded from above and below.

### Superclasses

- `bounded-int-text`
- `int-text`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:max`
- `:min`
- `:value`
- `:step`
- `:continuous-update`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Generic Function `widget-%module-module`

### Definition

```lisp
(widget-%module-module sb-pcl::object)
```


## Class `image`

Displays an image as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
image data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to "png").

If you pass `"url"` to the `"format"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8.

### Superclasses

- `image`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:width`
- `:height`
- `:format`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`


## Generic Function `widget-%module-module-version`

### Definition

```lisp
(widget-%module-module-version sb-pcl::object)
```


## Class `int-range-slider`

Slider/trackbar that represents a pair of ints bounded by minimum and maximum
value.

### Superclasses

- `int-range-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:step`
- `:max`
- `:min`
- `:continuous-update`
- `:orientation`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:readout`
- `:readout-format`
- `:value`

### Slots

- `value` &mdash; Int range value


## Generic Function `widget-children`

### Definition

```lisp
(widget-children sb-pcl::object)
```

## Generic Function `widget-children`

### Definition

```lisp
(widget-children sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-bar-style`

### Definition

```lisp
(widget-bar-style sb-pcl::object)
```

## Generic Function `widget-bar-style`

### Definition

```lisp
(widget-bar-style sb-pcl::new-value sb-pcl::object)
```


## Function `display`

Display a widget in the notebook.

### Definition

```lisp
(display widget)
```


## Generic Function `widget-font-weight`

### Definition

```lisp
(widget-font-weight sb-pcl::object)
```

## Generic Function `widget-font-weight`

### Definition

```lisp
(widget-font-weight sb-pcl::new-value sb-pcl::object)
```



## Generic Function `widget-msg-id`

### Definition

```lisp
(widget-msg-id sb-pcl::object)
```

## Generic Function `widget-msg-id`

### Definition

```lisp
(widget-msg-id sb-pcl::new-value sb-pcl::object)
```


## Class `text-area`

Multiline text area widget.

### Superclasses

- `text-area`
- `text`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:placeholder`
- `:continuous-update`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:rows`

### Slots

- `rows` &mdash; The number of rows to display.


## Generic Function `widget-grid-column`

### Definition

```lisp
(widget-grid-column sb-pcl::object)
```

## Generic Function `widget-grid-column`

### Definition

```lisp
(widget-grid-column sb-pcl::new-value sb-pcl::object)
```


## Generic Function `on-button-click`

This method is called when the button receives a click message.

### Definition

```lisp
(on-button-click w)
```


## Generic Function `widget-padding`

### Definition

```lisp
(widget-padding sb-pcl::object)
```

## Generic Function `widget-padding`

### Definition

```lisp
(widget-padding sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-orientation`

### Definition

```lisp
(widget-orientation sb-pcl::object)
```

## Generic Function `widget-orientation`

### Definition

```lisp
(widget-orientation sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-style`

### Definition

```lisp
(widget-style sb-pcl::object)
```

## Generic Function `widget-style`

### Definition

```lisp
(widget-style sb-pcl::new-value sb-pcl::object)
```


## Class `int-progress`

Progress bar that represents an integer bounded from above and below.

### Superclasses

- `int-progress`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:max`
- `:min`
- `:orientation`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:bar-style`


## Class `dom-widget`

Base class for all Jupyter widgets which have DOM view.

### Superclasses

- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`

### Slots

- `%dom-classes` &mdash; CSS classes applied to widget DOM element
- `layout` &mdash; Reference to layout widget.


## Generic Function `widget-align-content`

### Definition

```lisp
(widget-align-content sb-pcl::object)
```

## Generic Function `widget-align-content`

### Definition

```lisp
(widget-align-content sb-pcl::new-value sb-pcl::object)
```


## Class `progress-style`

Progress style widget.

### Superclasses

- `progress-style`
- `description-style`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:description-width`
- `:bar-color`

### Slots

- `bar-color` &mdash; Color of the slider handle.


## Generic Function `widget-margin`

### Definition

```lisp
(widget-margin sb-pcl::object)
```

## Generic Function `widget-margin`

### Definition

```lisp
(widget-margin sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-grid-template-areas`

### Definition

```lisp
(widget-grid-template-areas sb-pcl::object)
```

## Generic Function `widget-grid-template-areas`

### Definition

```lisp
(widget-grid-template-areas sb-pcl::new-value sb-pcl::object)
```


## Class `color-picker`

Color picker widget

### Superclasses

- `color-picker`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:concise`
- `:value`

### Slots

- `concise` &mdash; Display short version with just a color selector.
- `value` &mdash; The color value.


## Class `toggle-buttons`

Group of toggle buttons that represent an enumeration. Only one toggle button
can be toggled at any point in time.

### Superclasses

- `toggle-buttons`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:index`
- `:disabled`
- `:button-style`
- `:%options-labels`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:icons`
- `:tooltips`

### Slots

- `icons` &mdash; Icons names for each button (FontAwesome names without the fa- prefix).
- `tooltips` &mdash; Tooltips for each button.


## Class `float-slider`

Slider/trackbar of floating values with the specified range.

### Superclasses

- `float-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:step`
- `:max`
- `:min`
- `:continuous-update`
- `:orientation`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:readout`
- `:readout-format`


## Generic Function `widget-disabled`

### Definition

```lisp
(widget-disabled sb-pcl::object)
```

## Generic Function `widget-disabled`

### Definition

```lisp
(widget-disabled sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-layout`

### Definition

```lisp
(widget-layout sb-pcl::object)
```

## Generic Function `widget-layout`

### Definition

```lisp
(widget-layout sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-justify-content`

### Definition

```lisp
(widget-justify-content sb-pcl::object)
```

## Generic Function `widget-justify-content`

### Definition

```lisp
(widget-justify-content sb-pcl::new-value sb-pcl::object)
```


## Class `description-style`

### Superclasses

- `description-style`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:description-width`

### Slots

- `description-width` &mdash; Width of the description to the side of the control.


## Generic Function `widget-%titles`

### Definition

```lisp
(widget-%titles sb-pcl::object)
```

## Generic Function `widget-%titles`

### Definition

```lisp
(widget-%titles sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-timestamp`

### Definition

```lisp
(widget-timestamp sb-pcl::object)
```

## Generic Function `widget-timestamp`

### Definition

```lisp
(widget-timestamp sb-pcl::new-value sb-pcl::object)
```


## Class `date-picker`

Date picker widget

### Superclasses

- `date-picker`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:value`

### Slots

- `value` &mdash; The date value.


## Class `label`

Label widget.

It also renders math inside the string `value` as Latex (requires $ $ or
$$ $$ and similar latex tags).

### Superclasses

- `label`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:placeholder`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Class `h-box`

Displays multiple widgets horizontally using the flexible box model.

Example

(use-package :jupyter-widgets)
(defvar title-widget (make-widget 'html :value "<em>Box Example</em>"))
(defvar slider (make-widget 'int-slider))
(make-widget 'h-box :children (list title-widget slider))

### Superclasses

- `h-box`
- `box`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:box-style`
- `:children`


## Generic Function `widget-top`

### Definition

```lisp
(widget-top sb-pcl::object)
```

## Generic Function `widget-top`

### Definition

```lisp
(widget-top sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-selected-index`

### Definition

```lisp
(widget-selected-index sb-pcl::object)
```

## Generic Function `widget-selected-index`

### Definition

```lisp
(widget-selected-index sb-pcl::new-value sb-pcl::object)
```


## Class `play`

Play/repeat buttons to step through values automatically, and optionally loop.

### Superclasses

- `play`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:step`
- `:max`
- `:min`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:%playing`
- `:%repeat`
- `:interval`
- `:show-repeat`

### Slots

- `%playing` &mdash; Whether the control is currently playing.
- `%repeat` &mdash; Whether the control will repeat in a continous loop.
- `interval` &mdash; The maximum value for the play control.
- `show-repeat` &mdash; Show the repeat toggle button in the widget.


## Generic Function `widget-grid-auto-columns`

### Definition

```lisp
(widget-grid-auto-columns sb-pcl::object)
```

## Generic Function `widget-grid-auto-columns`

### Definition

```lisp
(widget-grid-auto-columns sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-button-width`

### Definition

```lisp
(widget-button-width sb-pcl::object)
```

## Generic Function `widget-button-width`

### Definition

```lisp
(widget-button-width sb-pcl::new-value sb-pcl::object)
```


## Class `select-multiple`

Listbox that allows many items to be selected at any given time.

### Superclasses

- `select-multiple`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:disabled`
- `:%options-labels`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:rows`
- `:index`

### Slots

- `index` &mdash; Selected indicies


## Generic Function `widget-border`

### Definition

```lisp
(widget-border sb-pcl::object)
```

## Generic Function `widget-border`

### Definition

```lisp
(widget-border sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-%module-name`

### Definition

```lisp
(widget-%module-name sb-pcl::object)
```


## Generic Function `widget-bottom`

### Definition

```lisp
(widget-bottom sb-pcl::object)
```

## Generic Function `widget-bottom`

### Definition

```lisp
(widget-bottom sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-value`

### Definition

```lisp
(widget-value sb-pcl::object)
```

## Generic Function `widget-value`

### Definition

```lisp
(widget-value sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-width`

### Definition

```lisp
(widget-width sb-pcl::object)
```

## Generic Function `widget-width`

### Definition

```lisp
(widget-width sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-target`

### Definition

```lisp
(widget-target sb-pcl::object)
```

## Generic Function `widget-target`

### Definition

```lisp
(widget-target sb-pcl::new-value sb-pcl::object)
```


## Class `controller-axis`

Represents a gamepad or joystick axis.

### Superclasses

- `controller-axis`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`


## Generic Function `widget-controls`

### Definition

```lisp
(widget-controls sb-pcl::object)
```

## Generic Function `widget-controls`

### Definition

```lisp
(widget-controls sb-pcl::new-value sb-pcl::object)
```


## Class `dropdown`

Allows you to select a single item from a dropdown.

### Superclasses

- `dropdown`
- `radio-buttons`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:index`
- `:disabled`
- `:%options-labels`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Generic Function `widget-connected`

### Definition

```lisp
(widget-connected sb-pcl::object)
```

## Generic Function `widget-connected`

### Definition

```lisp
(widget-connected sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-%view-module-version`

### Definition

```lisp
(widget-%view-module-version sb-pcl::object)
```


## Class `select`

Listbox that only allows one item to be selected at any given time.

### Superclasses

- `select`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:index`
- `:disabled`
- `:%options-labels`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:rows`


## Generic Function `widget-align-self`

### Definition

```lisp
(widget-align-self sb-pcl::object)
```

## Generic Function `widget-align-self`

### Definition

```lisp
(widget-align-self sb-pcl::new-value sb-pcl::object)
```


## Class `password`

Single line textbox widget.

### Superclasses

- `password`
- `text`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:placeholder`
- `:continuous-update`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Function `make-widget`

Create a Jupyter widget and inform the frontend to create a synchronized model.

### Definition

```lisp
(make-widget class &rest rest &key &allow-other-keys)
```


## Generic Function `widget-format`

### Definition

```lisp
(widget-format sb-pcl::object)
```

## Generic Function `widget-format`

### Definition

```lisp
(widget-format sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-overflow-x`

### Definition

```lisp
(widget-overflow-x sb-pcl::object)
```

## Generic Function `widget-overflow-x`

### Definition

```lisp
(widget-overflow-x sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-tooltips`

### Definition

```lisp
(widget-tooltips sb-pcl::object)
```

## Generic Function `widget-tooltips`

### Definition

```lisp
(widget-tooltips sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-flex`

### Definition

```lisp
(widget-flex sb-pcl::object)
```

## Generic Function `widget-flex`

### Definition

```lisp
(widget-flex sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-overflow`

### Definition

```lisp
(widget-overflow sb-pcl::object)
```

## Generic Function `widget-overflow`

### Definition

```lisp
(widget-overflow sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-max-width`

### Definition

```lisp
(widget-max-width sb-pcl::object)
```

## Generic Function `widget-max-width`

### Definition

```lisp
(widget-max-width sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-source`

### Definition

```lisp
(widget-source sb-pcl::object)
```

## Generic Function `widget-source`

### Definition

```lisp
(widget-source sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-min`

### Definition

```lisp
(widget-min sb-pcl::object)
```

## Generic Function `widget-min`

### Definition

```lisp
(widget-min sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-continuous-update`

### Definition

```lisp
(widget-continuous-update sb-pcl::object)
```

## Generic Function `widget-continuous-update`

### Definition

```lisp
(widget-continuous-update sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-grid-area`

### Definition

```lisp
(widget-grid-area sb-pcl::object)
```

## Generic Function `widget-grid-area`

### Definition

```lisp
(widget-grid-area sb-pcl::new-value sb-pcl::object)
```


## Class `video`

Displays a video as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
video data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to "mp4").

If you pass `"url"` to the `"format"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8.

### Superclasses

- `video`
- `audio`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:width`
- `:height`
- `:value`
- `:format`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:autoplay`
- `:controls`
- `:loop`


## Generic Function `widget-rows`

### Definition

```lisp
(widget-rows sb-pcl::object)
```

## Generic Function `widget-rows`

### Definition

```lisp
(widget-rows sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-grid-auto-flow`

### Definition

```lisp
(widget-grid-auto-flow sb-pcl::object)
```

## Generic Function `widget-grid-auto-flow`

### Definition

```lisp
(widget-grid-auto-flow sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-align-items`

### Definition

```lisp
(widget-align-items sb-pcl::object)
```

## Generic Function `widget-align-items`

### Definition

```lisp
(widget-align-items sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-autoplay`

### Definition

```lisp
(widget-autoplay sb-pcl::object)
```

## Generic Function `widget-autoplay`

### Definition

```lisp
(widget-autoplay sb-pcl::new-value sb-pcl::object)
```


## Class `button`

Button widget.
This widget has an `on-button-click` method that allows you to listen for the
user clicking on the button.  The click event itself is stateless.

### Superclasses

- `button`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:tooltip`
- `:icon`
- `:disabled`
- `:button-style`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`

### Slots

- `description` &mdash; Button label.


## Generic Function `widget-min-width`

### Definition

```lisp
(widget-min-width sb-pcl::object)
```

## Generic Function `widget-min-width`

### Definition

```lisp
(widget-min-width sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-max`

### Definition

```lisp
(widget-max sb-pcl::object)
```

## Generic Function `widget-max`

### Definition

```lisp
(widget-max sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-%view-module`

### Definition

```lisp
(widget-%view-module sb-pcl::object)
```


## Class `layout`

Layout specification

Defines a layout that can be expressed using CSS.  Supports a subset of
https://developer.mozilla.org/en-US/docs/Web/CSS/Reference

When a property is also accessible via a shorthand property, we only
expose the shorthand.

### Superclasses

- `layout`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:align-content`
- `:align-items`
- `:align-self`
- `:border`
- `:bottom`
- `:display`
- `:flex`
- `:flex-flow`
- `:grid-area`
- `:grid-auto-columns`
- `:grid-auto-flow`
- `:grid-column`
- `:grid-gap`
- `:grid-template-areas`
- `:grid-template-columns`
- `:grid-template-rows`
- `:height`
- `:justify-content`
- `:left`
- `:margin`
- `:max-height`
- `:max-width`
- `:min-height`
- `:min-width`
- `:order`
- `:overflow`
- `:overflow-x`
- `:overflow-y`
- `:padding`
- `:right`
- `:top`
- `:visibility`
- `:width`

### Slots

- `align-content` &mdash; The align-content CSS attribute.
- `align-items` &mdash; The align-items CSS attribute.
- `align-self` &mdash; The align-self CSS attribute.
- `border` &mdash; The border CSS attribute.
- `bottom` &mdash; The bottom CSS attribute.
- `display` &mdash; The display CSS attribute.
- `flex` &mdash; The flex CSS attribute.
- `flex-flow` &mdash; The flex-flow CSS attribute.
- `grid-area` &mdash; The grid-area CSS attribute.
- `grid-auto-columns` &mdash; The grid-auto-columns CSS attribute.
- `grid-auto-flow` &mdash; The grid-auto-flow CSS attribute.
- `grid-column` &mdash; The grid-column CSS attribute.
- `grid-gap` &mdash; The grid-gap CSS attribute.
- `grid-template-areas` &mdash; The grid-template-areas CSS attribute.
- `grid-template-columns` &mdash; The grid-template-columns CSS attribute.
- `grid-template-rows` &mdash; The grid-template-rows CSS attribute.
- `height` &mdash; The height CSS attribute.
- `justify-content` &mdash; The justify-content CSS attribute.
- `left` &mdash; The left CSS attribute.
- `margin` &mdash; The margin CSS attribute.
- `max-height` &mdash; The max-height CSS attribute.
- `max-width` &mdash; The max-width CSS attribute.
- `min-height` &mdash; The min-height CSS attribute.
- `min-width` &mdash; The min-width CSS attribute.
- `order` &mdash; The order CSS attribute.
- `overflow` &mdash; The overflow CSS attribute.
- `overflow-x` &mdash; The overflow-x CSS attribute.
- `overflow-y` &mdash; The overflow-y CSS attribute.
- `padding` &mdash; The padding CSS attribute.
- `right` &mdash; The right CSS attribute.
- `top` &mdash; The top CSS attribute.
- `visibility` &mdash; The visibility CSS attribute.
- `width` &mdash; The width CSS attribute.


## Generic Function `widget-right`

### Definition

```lisp
(widget-right sb-pcl::object)
```

## Generic Function `widget-right`

### Definition

```lisp
(widget-right sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-show-repeat`

### Definition

```lisp
(widget-show-repeat sb-pcl::object)
```

## Generic Function `widget-show-repeat`

### Definition

```lisp
(widget-show-repeat sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-axes`

### Definition

```lisp
(widget-axes sb-pcl::object)
```

## Generic Function `widget-axes`

### Definition

```lisp
(widget-axes sb-pcl::new-value sb-pcl::object)
```


## Macro `with-output`

Evaluate body with all output sent to the output widget.

### Definition

```lisp
(with-output o
  &body
  body)
```


## Class `text`

Single line textbox widget.

### Superclasses

- `text`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:placeholder`
- `:continuous-update`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Generic Function `widget-description`

### Definition

```lisp
(widget-description sb-pcl::object)
```

## Generic Function `widget-description`

### Definition

```lisp
(widget-description sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-step`

### Definition

```lisp
(widget-step sb-pcl::object)
```

## Generic Function `widget-step`

### Definition

```lisp
(widget-step sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-%dom-classes`

### Definition

```lisp
(widget-%dom-classes sb-pcl::object)
```

## Generic Function `widget-%dom-classes`

### Definition

```lisp
(widget-%dom-classes sb-pcl::new-value sb-pcl::object)
```


## Class `link`

Link Widget

### Superclasses

- `link`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:source`
- `:target`


## Class `int-text`

Textbox widget that represents an integer.

### Superclasses

- `int-text`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:step`
- `:continuous-update`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`


## Generic Function `widget-visibility`

### Definition

```lisp
(widget-visibility sb-pcl::object)
```

## Generic Function `widget-visibility`

### Definition

```lisp
(widget-visibility sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-description-tooltip`

### Definition

```lisp
(widget-description-tooltip sb-pcl::object)
```

## Generic Function `widget-description-tooltip`

### Definition

```lisp
(widget-description-tooltip sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-flex-flow`

### Definition

```lisp
(widget-flex-flow sb-pcl::object)
```

## Generic Function `widget-flex-flow`

### Definition

```lisp
(widget-flex-flow sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-base`

### Definition

```lisp
(widget-base sb-pcl::object)
```

## Generic Function `widget-base`

### Definition

```lisp
(widget-base sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-interval`

### Definition

```lisp
(widget-interval sb-pcl::object)
```

## Generic Function `widget-interval`

### Definition

```lisp
(widget-interval sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-%playing`

### Definition

```lisp
(widget-%playing sb-pcl::object)
```

## Generic Function `widget-%playing`

### Definition

```lisp
(widget-%playing sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-description-width`

### Definition

```lisp
(widget-description-width sb-pcl::object)
```

## Generic Function `widget-description-width`

### Definition

```lisp
(widget-description-width sb-pcl::new-value sb-pcl::object)
```


## Class `selection-slider`

Slider to select a single item from a list or dictionary.

### Superclasses

- `selection-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:index`
- `:%options-labels`
- `:continuous-update`
- `:orientation`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:readout`
- `:readout-format`


## Generic Function `widget-%repeat`

### Definition

```lisp
(widget-%repeat sb-pcl::object)
```

## Generic Function `widget-%repeat`

### Definition

```lisp
(widget-%repeat sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-%options-labels`

### Definition

```lisp
(widget-%options-labels sb-pcl::object)
```

## Generic Function `widget-%options-labels`

### Definition

```lisp
(widget-%options-labels sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-placeholder`

### Definition

```lisp
(widget-placeholder sb-pcl::object)
```

## Generic Function `widget-placeholder`

### Definition

```lisp
(widget-placeholder sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-max-height`

### Definition

```lisp
(widget-max-height sb-pcl::object)
```

## Generic Function `widget-max-height`

### Definition

```lisp
(widget-max-height sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-display`

### Definition

```lisp
(widget-display sb-pcl::object)
```

## Generic Function `widget-display`

### Definition

```lisp
(widget-display sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-readout-format`

### Definition

```lisp
(widget-readout-format sb-pcl::object)
```

## Generic Function `widget-readout-format`

### Definition

```lisp
(widget-readout-format sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-min-height`

### Definition

```lisp
(widget-min-height sb-pcl::object)
```

## Generic Function `widget-min-height`

### Definition

```lisp
(widget-min-height sb-pcl::new-value sb-pcl::object)
```


## Class `v-box`

Displays multiple widgets vertically using the flexible box model.

Example

(use-package :jupyter-widgets)
(defvar title-widget (make-widget 'html :value "<em>Box Example</em>"))
(defvar slider (make-widget 'int-slider))
(make-widget 'v-box :children (list title-widget slider))

### Superclasses

- `v-box`
- `box`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:box-style`
- `:children`


## Class `float-range-slider`

Slider/trackbar that represents a pair of floats bounded by minimum and maximum
value.

### Superclasses

- `float-range-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:step`
- `:max`
- `:min`
- `:continuous-update`
- `:orientation`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:readout`
- `:readout-format`
- `:value`

### Slots

- `value` &mdash; Float range


## Class `int-slider`

Slider widget that represents an integer bounded from above and below.

### Superclasses

- `int-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:step`
- `:max`
- `:min`
- `:continuous-update`
- `:orientation`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:readout`
- `:readout-format`


## Generic Function `widget-order`

### Definition

```lisp
(widget-order sb-pcl::object)
```

## Generic Function `widget-order`

### Definition

```lisp
(widget-order sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-box-style`

### Definition

```lisp
(widget-box-style sb-pcl::object)
```

## Generic Function `widget-box-style`

### Definition

```lisp
(widget-box-style sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-grid-template-columns`

### Definition

```lisp
(widget-grid-template-columns sb-pcl::object)
```

## Generic Function `widget-grid-template-columns`

### Definition

```lisp
(widget-grid-template-columns sb-pcl::new-value sb-pcl::object)
```


## Class `controller`

Represents a game controller.

### Superclasses

- `controller`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:axes`
- `:buttons`
- `:connected`
- `:index`
- `:mapping`
- `:name`
- `:timestamp`

### Slots

- `axes` &mdash; The axes on the gamepad.
- `buttons` &mdash; The buttons on the gamepad.
- `connected` &mdash; Whether the gamepad is connected.
- `index` &mdash; The id number of the controller.
- `mapping` &mdash; The name of the control mapping.
- `name` &mdash; The name of the controller.
- `timestamp` &mdash; The last time the data from this gamepad was updated.


## Generic Function `widget-buttons`

### Definition

```lisp
(widget-buttons sb-pcl::object)
```

## Generic Function `widget-buttons`

### Definition

```lisp
(widget-buttons sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-pressed`

### Definition

```lisp
(widget-pressed sb-pcl::object)
```

## Generic Function `widget-pressed`

### Definition

```lisp
(widget-pressed sb-pcl::new-value sb-pcl::object)
```


## Class `button-style`

Button style widget

### Superclasses

- `button-style`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:button-color`
- `:font-weight`

### Slots

- `button-color` &mdash; Color of the button
- `font-weight` &mdash; Button text font weight.


## Class `valid`

Displays a boolean `value` in the form of a green check (True / valid) or a red
cross (False / invalid).

### Superclasses

- `valid`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:value`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:readout`

### Slots

- `readout` &mdash; Message displayed when the value is False


## Class `widget`

Base class for all Jupyter widgets.

### Superclasses

- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`

### Slots

- `%model-name` &mdash; Name of the model.
- `%model-module` &mdash; The namespace for the model.
- `%model-module-version` &mdash; A semver requirement for namespace version containing the model.
- `%view-name` &mdash; Name of the view.
- `%view-module` &mdash; The namespace for the view.
- `%view-module-version` &mdash; A semver requirement for namespace version containing the view.


## Class `selection-range-slider`

Slider to select multiple contiguous items from a list.

### Superclasses

- `selection-range-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:%options-labels`
- `:continuous-update`
- `:orientation`
- `:disabled`
- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:style`
- `:description`
- `:description-tooltip`
- `:readout`
- `:readout-format`
- `:index`

### Slots

- `index` &mdash; Min and max selected indices


## Class `grid-box`

### Superclasses

- `grid-box`
- `box`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:%dom-classes`
- `:layout`
- `:box-style`
- `:children`


## Generic Function `widget-grid-template-rows`

### Definition

```lisp
(widget-grid-template-rows sb-pcl::object)
```

## Generic Function `widget-grid-template-rows`

### Definition

```lisp
(widget-grid-template-rows sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-indent`

### Definition

```lisp
(widget-indent sb-pcl::object)
```

## Generic Function `widget-indent`

### Definition

```lisp
(widget-indent sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-concise`

### Definition

```lisp
(widget-concise sb-pcl::object)
```

## Generic Function `widget-concise`

### Definition

```lisp
(widget-concise sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-button-color`

### Definition

```lisp
(widget-button-color sb-pcl::object)
```

## Generic Function `widget-button-color`

### Definition

```lisp
(widget-button-color sb-pcl::new-value sb-pcl::object)
```


## Generic Function `widget-tooltip`

### Definition

```lisp
(widget-tooltip sb-pcl::object)
```

## Generic Function `widget-tooltip`

### Definition

```lisp
(widget-tooltip sb-pcl::new-value sb-pcl::object)
```


## Class `slider-style`

### Superclasses

- `slider-style`
- `description-style`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Initial Arguments

- `:display-data`
- `:id`
- `:target-name`
- `:kernel`
- `:%model-name`
- `:%model-module`
- `:%model-module-version`
- `:%view-name`
- `:%view-module`
- `:%view-module-version`
- `:description-width`
- `:handle-color`

### Slots

- `handle-color` &mdash; Color of the slider handle.
