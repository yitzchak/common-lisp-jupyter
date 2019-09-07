# Package jupyter-widgets


## *Class* `accordion`

### Superclasses

- `accordion`
- `box`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `%titles` &mdash; Titles of the pages.

- `selected-index` &mdash; The index of the selected page. This is either an integer selecting a particular sub-widget, or nil to have no widgets selected.


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

### Description

Displays children each on a separate accordion page.


## *Class* `audio`

### Superclasses

- `audio`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `autoplay` &mdash; When true, the audio starts when it's displayed.

- `controls` &mdash; Specifies that media controls should be displayed (such as a play/pause button etc)

- `loop` &mdash; When true, the audio will start from the beginning after finishing


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

### Description

Displays an audio clip as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
audio data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to "mp3").

If you pass `"url"` to the `"format"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8.


## *Class* `bounded-float-text`

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

### Description

Displays a float value within a textbox. Value must be within the range
specified. For a textbox in which the value doesn't need to be within a specific
range, use float-text.


## *Class* `bounded-int-text`

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

### Description

Textbox widget that represents an integer bounded from above and below.


## *Class* `box`

### Superclasses

- `box`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `box-style` &mdash; Use a predefined styling for the box.

- `children` &mdash; List of widget children.


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

### Description

Displays multiple widgets in a group. The widgets are laid out horizontally.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar title-widget (make-widget 'html :value "<em>Box Example</em>"))
(defvar slider (make-widget 'int-slider))
(make-widget 'box :children (list title-widget slider))
```


## *Class* `button`

### Superclasses

- `button`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `description` &mdash; Button label.


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

### Description

Button widget.
This widget has an `on-button-click` method that allows you to listen for the
user clicking on the button.  The click event itself is stateless.


## *Class* `button-style`

### Superclasses

- `button-style`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `button-color` &mdash; Color of the button

- `font-weight` &mdash; Button text font weight.


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

### Description

Button style widget


## *Class* `checkbox`

### Superclasses

- `checkbox`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `indent` &mdash; Indent the control to align with other controls with a description.


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

### Description

Displays a boolean `value` in the form of a checkbox.


## *Class* `color-picker`

### Superclasses

- `color-picker`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `concise` &mdash; Display short version with just a color selector.

- `value` &mdash; The color value.


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

### Description

Color picker widget


## *Class* `controller`

### Superclasses

- `controller`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `axes` &mdash; The axes on the gamepad.

- `buttons` &mdash; The buttons on the gamepad.

- `connected` &mdash; Whether the gamepad is connected.

- `index` &mdash; The id number of the controller.

- `mapping` &mdash; The name of the control mapping.

- `name` &mdash; The name of the controller.

- `timestamp` &mdash; The last time the data from this gamepad was updated.


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

### Description

Represents a game controller.


## *Class* `controller-axis`

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

### Description

Represents a gamepad or joystick axis.


## *Class* `controller-button`

### Superclasses

- `controller-button`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `pressed` &mdash; Whether the button is pressed.


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

### Description

Represents a gamepad or joystick button.


## *Class* `date-picker`

### Superclasses

- `date-picker`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `value` &mdash; The date value.


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

### Description

Date picker widget


## *Class* `description-style`

### Superclasses

- `description-style`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `description-width` &mdash; Width of the description to the side of the control.


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


## *Class* `directional-link`

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

### Description

A directional link


## *Function* `display`

### Syntax

```common-lisp
(display widget)
```

### Description

Display a widget in the notebook.


## *Class* `dom-widget`

### Superclasses

- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `%dom-classes` &mdash; CSS classes applied to widget DOM element

- `layout` &mdash; Reference to layout widget.


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

### Description

Base class for all Jupyter widgets which have DOM view.


## *Class* `dropdown`

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

### Description

Allows you to select a single item from a dropdown.


## *Class* `float-log-slider`

### Superclasses

- `float-log-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `base` &mdash; Base for the logarithm


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

### Description

Slider/trackbar of logarithmic floating values with the specified range.


## *Class* `float-progress`

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

### Description

Displays a progress bar.


## *Class* `float-range-slider`

### Superclasses

- `float-range-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `value` &mdash; Float range


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

### Description

Slider/trackbar that represents a pair of floats bounded by minimum and maximum
value.


## *Class* `float-slider`

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

### Description

Slider/trackbar of floating values with the specified range.


## *Class* `float-text`

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

### Description

Displays a float value within a textbox. For a textbox in which the value must
be within a specific range, use BoundedFloatText.


## *Class* `grid-box`

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


## *Class* `h-box`

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

### Description

Displays multiple widgets horizontally using the flexible box model.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar title-widget (make-widget 'html :value "<em>Box Example</em>"))
(defvar slider (make-widget 'int-slider))
(make-widget 'h-box :children (list title-widget slider))
```


## *Class* `html`

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

### Description

Renders the string `value` as HTML.


## *Class* `html-math`

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

### Description

Renders the string `value` as HTML, and render mathematics.


## *Class* `image`

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

### Description

Displays an image as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
image data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to "png").

If you pass `"url"` to the `"format"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8.


## *Class* `int-progress`

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

### Description

Progress bar that represents an integer bounded from above and below.


## *Class* `int-range-slider`

### Superclasses

- `int-range-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `value` &mdash; Int range value


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

### Description

Slider/trackbar that represents a pair of ints bounded by minimum and maximum
value.


## *Class* `int-slider`

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

### Description

Slider widget that represents an integer bounded from above and below.


## *Class* `int-text`

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

### Description

Textbox widget that represents an integer.


## *Class* `label`

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

### Description

Label widget.

It also renders math inside the string `value` as Latex (requires $ $ or
$$ $$ and similar latex tags).


## *Class* `layout`

### Superclasses

- `layout`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

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

### Description

Layout specification

Defines a layout that can be expressed using CSS.  Supports a subset of
https://developer.mozilla.org/en-US/docs/Web/CSS/Reference

When a property is also accessible via a shorthand property, we only
expose the shorthand.


## *Class* `link`

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

### Description

Link Widget


## *Function* `make-widget`

### Syntax

```common-lisp
(make-widget class &rest rest &key &allow-other-keys)
```

### Description

Create a Jupyter widget and inform the frontend to create a synchronized model.


## *Generic Function* `on-button-click`

### Syntax

```common-lisp
(on-button-click w)
```

### Description

This method is called when the button receives a click message.


## *Generic Function* `on-trait-change`

### Syntax

```common-lisp
(on-trait-change object type name old-value new-value)
```


## *Class* `output`

### Superclasses

- `output`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `msg-id` &mdash; Parent message id of messages to capture

- `outputs` &mdash; The output messages synced from the frontend.


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

### Description

Widget used as a context manager to display output.

This widget can capture and display stdout, stderr, and rich output.  To use it,
create an instance of it and display it.

You can then use the widget as a context manager: any output produced while in
the context will be captured and displayed in the widget instead of the standard
output area.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar out (make-widget 'output))
(with-output out
  (print "prints to output area")
```


## *Class* `password`

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

### Description

Single line textbox widget.


## *Class* `play`

### Superclasses

- `play`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `%playing` &mdash; Whether the control is currently playing.

- `%repeat` &mdash; Whether the control will repeat in a continous loop.

- `interval` &mdash; The maximum value for the play control.

- `show-repeat` &mdash; Show the repeat toggle button in the widget.


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

### Description

Play/repeat buttons to step through values automatically, and optionally loop.


## *Class* `progress-style`

### Superclasses

- `progress-style`
- `description-style`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `bar-color` &mdash; Color of the slider handle.


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

### Description

Progress style widget.


## *Class* `radio-buttons`

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

### Description

Group of radio buttons that represent an enumeration. Only one radio button can
be toggled at any point in time.


## *Class* `select`

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

### Description

Listbox that only allows one item to be selected at any given time.


## *Class* `select-multiple`

### Superclasses

- `select-multiple`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `index` &mdash; Selected indicies


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

### Description

Listbox that allows many items to be selected at any given time.


## *Class* `selection-range-slider`

### Superclasses

- `selection-range-slider`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `index` &mdash; Min and max selected indices


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

### Description

Slider to select multiple contiguous items from a list.


## *Class* `selection-slider`

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

### Description

Slider to select a single item from a list or dictionary.


## *Class* `slider-style`

### Superclasses

- `slider-style`
- `description-style`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `handle-color` &mdash; Color of the slider handle.


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


## *Class* `tab`

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

### Description

Displays children each on a separate accordion tab.


## *Class* `text`

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

### Description

Single line textbox widget.


## *Class* `text-area`

### Superclasses

- `text-area`
- `text`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `rows` &mdash; The number of rows to display.


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

### Description

Multiline text area widget.


## *Class* `toggle-button`

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

### Description

Displays a boolean `value` in the form of a toggle button.



## *Class* `toggle-buttons`

### Superclasses

- `toggle-buttons`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `icons` &mdash; Icons names for each button (FontAwesome names without the fa- prefix).

- `tooltips` &mdash; Tooltips for each button.


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

### Description

Group of toggle buttons that represent an enumeration. Only one toggle button
can be toggled at any point in time.


## *Class* `v-box`

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

### Description

Displays multiple widgets vertically using the flexible box model.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar title-widget (make-widget 'html :value "<em>Box Example</em>"))
(defvar slider (make-widget 'int-slider))
(make-widget 'v-box :children (list title-widget slider))
```


## *Class* `valid`

### Superclasses

- `valid`
- `dom-widget`
- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `readout` &mdash; Message displayed when the value is False


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

### Description

Displays a boolean `value` in the form of a green check (True / valid) or a red
cross (False / invalid).


## *Class* `video`

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

### Description

Displays a video as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
video data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to "mp4").

If you pass `"url"` to the `"format"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8.


## *Class* `widget`

### Superclasses

- `widget`
- `jupyter:comm`
- `jupyter:result`
- `standard-object`

### Slots

- `%model-name` &mdash; Name of the model.

- `%model-module` &mdash; The namespace for the model.

- `%model-module-version` &mdash; A semver requirement for namespace version containing the model.

- `%view-name` &mdash; Name of the view.

- `%view-module` &mdash; The namespace for the view.

- `%view-module-version` &mdash; A semver requirement for namespace version containing the view.


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

### Description

Base class for all Jupyter widgets.


## *Generic Function* `widget-%dom-classes`

### Syntax

```common-lisp
(widget-%dom-classes sb-pcl::object)
```

## *Generic Function* `widget-%dom-classes`

### Syntax

```common-lisp
(widget-%dom-classes sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-%module-module`

### Syntax

```common-lisp
(widget-%module-module sb-pcl::object)
```


## *Generic Function* `widget-%module-module-version`

### Syntax

```common-lisp
(widget-%module-module-version sb-pcl::object)
```


## *Generic Function* `widget-%module-name`

### Syntax

```common-lisp
(widget-%module-name sb-pcl::object)
```


## *Generic Function* `widget-%options-labels`

### Syntax

```common-lisp
(widget-%options-labels sb-pcl::object)
```

## *Generic Function* `widget-%options-labels`

### Syntax

```common-lisp
(widget-%options-labels sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-%playing`

### Syntax

```common-lisp
(widget-%playing sb-pcl::object)
```

## *Generic Function* `widget-%playing`

### Syntax

```common-lisp
(widget-%playing sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-%repeat`

### Syntax

```common-lisp
(widget-%repeat sb-pcl::object)
```

## *Generic Function* `widget-%repeat`

### Syntax

```common-lisp
(widget-%repeat sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-%titles`

### Syntax

```common-lisp
(widget-%titles sb-pcl::object)
```

## *Generic Function* `widget-%titles`

### Syntax

```common-lisp
(widget-%titles sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-%view-module`

### Syntax

```common-lisp
(widget-%view-module sb-pcl::object)
```


## *Generic Function* `widget-%view-module-version`

### Syntax

```common-lisp
(widget-%view-module-version sb-pcl::object)
```


## *Generic Function* `widget-%view-name`

### Syntax

```common-lisp
(widget-%view-name sb-pcl::object)
```


## *Generic Function* `widget-align-content`

### Syntax

```common-lisp
(widget-align-content sb-pcl::object)
```

## *Generic Function* `widget-align-content`

### Syntax

```common-lisp
(widget-align-content sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-align-items`

### Syntax

```common-lisp
(widget-align-items sb-pcl::object)
```

## *Generic Function* `widget-align-items`

### Syntax

```common-lisp
(widget-align-items sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-align-self`

### Syntax

```common-lisp
(widget-align-self sb-pcl::object)
```

## *Generic Function* `widget-align-self`

### Syntax

```common-lisp
(widget-align-self sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-autoplay`

### Syntax

```common-lisp
(widget-autoplay sb-pcl::object)
```

## *Generic Function* `widget-autoplay`

### Syntax

```common-lisp
(widget-autoplay sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-axes`

### Syntax

```common-lisp
(widget-axes sb-pcl::object)
```

## *Generic Function* `widget-axes`

### Syntax

```common-lisp
(widget-axes sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-bar-color`

### Syntax

```common-lisp
(widget-bar-color sb-pcl::object)
```

## *Generic Function* `widget-bar-color`

### Syntax

```common-lisp
(widget-bar-color sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-bar-style`

### Syntax

```common-lisp
(widget-bar-style sb-pcl::object)
```

## *Generic Function* `widget-bar-style`

### Syntax

```common-lisp
(widget-bar-style sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-base`

### Syntax

```common-lisp
(widget-base sb-pcl::object)
```

## *Generic Function* `widget-base`

### Syntax

```common-lisp
(widget-base sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-border`

### Syntax

```common-lisp
(widget-border sb-pcl::object)
```

## *Generic Function* `widget-border`

### Syntax

```common-lisp
(widget-border sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-bottom`

### Syntax

```common-lisp
(widget-bottom sb-pcl::object)
```

## *Generic Function* `widget-bottom`

### Syntax

```common-lisp
(widget-bottom sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-box-style`

### Syntax

```common-lisp
(widget-box-style sb-pcl::object)
```

## *Generic Function* `widget-box-style`

### Syntax

```common-lisp
(widget-box-style sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-button-color`

### Syntax

```common-lisp
(widget-button-color sb-pcl::object)
```

## *Generic Function* `widget-button-color`

### Syntax

```common-lisp
(widget-button-color sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-button-style`

### Syntax

```common-lisp
(widget-button-style sb-pcl::object)
```

## *Generic Function* `widget-button-style`

### Syntax

```common-lisp
(widget-button-style sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-button-width`

### Syntax

```common-lisp
(widget-button-width sb-pcl::object)
```

## *Generic Function* `widget-button-width`

### Syntax

```common-lisp
(widget-button-width sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-buttons`

### Syntax

```common-lisp
(widget-buttons sb-pcl::object)
```

## *Generic Function* `widget-buttons`

### Syntax

```common-lisp
(widget-buttons sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-children`

### Syntax

```common-lisp
(widget-children sb-pcl::object)
```

## *Generic Function* `widget-children`

### Syntax

```common-lisp
(widget-children sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-concise`

### Syntax

```common-lisp
(widget-concise sb-pcl::object)
```

## *Generic Function* `widget-concise`

### Syntax

```common-lisp
(widget-concise sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-connected`

### Syntax

```common-lisp
(widget-connected sb-pcl::object)
```

## *Generic Function* `widget-connected`

### Syntax

```common-lisp
(widget-connected sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-continuous-update`

### Syntax

```common-lisp
(widget-continuous-update sb-pcl::object)
```

## *Generic Function* `widget-continuous-update`

### Syntax

```common-lisp
(widget-continuous-update sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-controls`

### Syntax

```common-lisp
(widget-controls sb-pcl::object)
```

## *Generic Function* `widget-controls`

### Syntax

```common-lisp
(widget-controls sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-description`

### Syntax

```common-lisp
(widget-description sb-pcl::object)
```

## *Generic Function* `widget-description`

### Syntax

```common-lisp
(widget-description sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-description-tooltip`

### Syntax

```common-lisp
(widget-description-tooltip sb-pcl::object)
```

## *Generic Function* `widget-description-tooltip`

### Syntax

```common-lisp
(widget-description-tooltip sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-description-width`

### Syntax

```common-lisp
(widget-description-width sb-pcl::object)
```

## *Generic Function* `widget-description-width`

### Syntax

```common-lisp
(widget-description-width sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-disabled`

### Syntax

```common-lisp
(widget-disabled sb-pcl::object)
```

## *Generic Function* `widget-disabled`

### Syntax

```common-lisp
(widget-disabled sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-display`

### Syntax

```common-lisp
(widget-display sb-pcl::object)
```

## *Generic Function* `widget-display`

### Syntax

```common-lisp
(widget-display sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-flex`

### Syntax

```common-lisp
(widget-flex sb-pcl::object)
```

## *Generic Function* `widget-flex`

### Syntax

```common-lisp
(widget-flex sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-flex-flow`

### Syntax

```common-lisp
(widget-flex-flow sb-pcl::object)
```

## *Generic Function* `widget-flex-flow`

### Syntax

```common-lisp
(widget-flex-flow sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-font-weight`

### Syntax

```common-lisp
(widget-font-weight sb-pcl::object)
```

## *Generic Function* `widget-font-weight`

### Syntax

```common-lisp
(widget-font-weight sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-format`

### Syntax

```common-lisp
(widget-format sb-pcl::object)
```

## *Generic Function* `widget-format`

### Syntax

```common-lisp
(widget-format sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-grid-area`

### Syntax

```common-lisp
(widget-grid-area sb-pcl::object)
```

## *Generic Function* `widget-grid-area`

### Syntax

```common-lisp
(widget-grid-area sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-grid-auto-columns`

### Syntax

```common-lisp
(widget-grid-auto-columns sb-pcl::object)
```

## *Generic Function* `widget-grid-auto-columns`

### Syntax

```common-lisp
(widget-grid-auto-columns sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-grid-auto-flow`

### Syntax

```common-lisp
(widget-grid-auto-flow sb-pcl::object)
```

## *Generic Function* `widget-grid-auto-flow`

### Syntax

```common-lisp
(widget-grid-auto-flow sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-grid-column`

### Syntax

```common-lisp
(widget-grid-column sb-pcl::object)
```

## *Generic Function* `widget-grid-column`

### Syntax

```common-lisp
(widget-grid-column sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-grid-gap`

### Syntax

```common-lisp
(widget-grid-gap sb-pcl::object)
```

## *Generic Function* `widget-grid-gap`

### Syntax

```common-lisp
(widget-grid-gap sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-grid-template-areas`

### Syntax

```common-lisp
(widget-grid-template-areas sb-pcl::object)
```

## *Generic Function* `widget-grid-template-areas`

### Syntax

```common-lisp
(widget-grid-template-areas sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-grid-template-columns`

### Syntax

```common-lisp
(widget-grid-template-columns sb-pcl::object)
```

## *Generic Function* `widget-grid-template-columns`

### Syntax

```common-lisp
(widget-grid-template-columns sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-grid-template-rows`

### Syntax

```common-lisp
(widget-grid-template-rows sb-pcl::object)
```

## *Generic Function* `widget-grid-template-rows`

### Syntax

```common-lisp
(widget-grid-template-rows sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-handle-color`

### Syntax

```common-lisp
(widget-handle-color sb-pcl::object)
```

## *Generic Function* `widget-handle-color`

### Syntax

```common-lisp
(widget-handle-color sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-height`

### Syntax

```common-lisp
(widget-height sb-pcl::object)
```

## *Generic Function* `widget-height`

### Syntax

```common-lisp
(widget-height sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-icon`

### Syntax

```common-lisp
(widget-icon sb-pcl::object)
```

## *Generic Function* `widget-icon`

### Syntax

```common-lisp
(widget-icon sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-icons`

### Syntax

```common-lisp
(widget-icons sb-pcl::object)
```

## *Generic Function* `widget-icons`

### Syntax

```common-lisp
(widget-icons sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-indent`

### Syntax

```common-lisp
(widget-indent sb-pcl::object)
```

## *Generic Function* `widget-indent`

### Syntax

```common-lisp
(widget-indent sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-index`

### Syntax

```common-lisp
(widget-index sb-pcl::object)
```

## *Generic Function* `widget-index`

### Syntax

```common-lisp
(widget-index sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-interval`

### Syntax

```common-lisp
(widget-interval sb-pcl::object)
```

## *Generic Function* `widget-interval`

### Syntax

```common-lisp
(widget-interval sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-justify-content`

### Syntax

```common-lisp
(widget-justify-content sb-pcl::object)
```

## *Generic Function* `widget-justify-content`

### Syntax

```common-lisp
(widget-justify-content sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-layout`

### Syntax

```common-lisp
(widget-layout sb-pcl::object)
```

## *Generic Function* `widget-layout`

### Syntax

```common-lisp
(widget-layout sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-left`

### Syntax

```common-lisp
(widget-left sb-pcl::object)
```

## *Generic Function* `widget-left`

### Syntax

```common-lisp
(widget-left sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-loop`

### Syntax

```common-lisp
(widget-loop sb-pcl::object)
```

## *Generic Function* `widget-loop`

### Syntax

```common-lisp
(widget-loop sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-mapping`

### Syntax

```common-lisp
(widget-mapping sb-pcl::object)
```

## *Generic Function* `widget-mapping`

### Syntax

```common-lisp
(widget-mapping sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-margin`

### Syntax

```common-lisp
(widget-margin sb-pcl::object)
```

## *Generic Function* `widget-margin`

### Syntax

```common-lisp
(widget-margin sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-max`

### Syntax

```common-lisp
(widget-max sb-pcl::object)
```

## *Generic Function* `widget-max`

### Syntax

```common-lisp
(widget-max sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-max-height`

### Syntax

```common-lisp
(widget-max-height sb-pcl::object)
```

## *Generic Function* `widget-max-height`

### Syntax

```common-lisp
(widget-max-height sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-max-width`

### Syntax

```common-lisp
(widget-max-width sb-pcl::object)
```

## *Generic Function* `widget-max-width`

### Syntax

```common-lisp
(widget-max-width sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-min`

### Syntax

```common-lisp
(widget-min sb-pcl::object)
```

## *Generic Function* `widget-min`

### Syntax

```common-lisp
(widget-min sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-min-height`

### Syntax

```common-lisp
(widget-min-height sb-pcl::object)
```

## *Generic Function* `widget-min-height`

### Syntax

```common-lisp
(widget-min-height sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-min-width`

### Syntax

```common-lisp
(widget-min-width sb-pcl::object)
```

## *Generic Function* `widget-min-width`

### Syntax

```common-lisp
(widget-min-width sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-msg-id`

### Syntax

```common-lisp
(widget-msg-id sb-pcl::object)
```

## *Generic Function* `widget-msg-id`

### Syntax

```common-lisp
(widget-msg-id sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-name`

### Syntax

```common-lisp
(widget-name sb-pcl::object)
```

## *Generic Function* `widget-name`

### Syntax

```common-lisp
(widget-name sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-order`

### Syntax

```common-lisp
(widget-order sb-pcl::object)
```

## *Generic Function* `widget-order`

### Syntax

```common-lisp
(widget-order sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-orientation`

### Syntax

```common-lisp
(widget-orientation sb-pcl::object)
```

## *Generic Function* `widget-orientation`

### Syntax

```common-lisp
(widget-orientation sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-outputs`

### Syntax

```common-lisp
(widget-outputs sb-pcl::object)
```

## *Generic Function* `widget-outputs`

### Syntax

```common-lisp
(widget-outputs sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-overflow`

### Syntax

```common-lisp
(widget-overflow sb-pcl::object)
```

## *Generic Function* `widget-overflow`

### Syntax

```common-lisp
(widget-overflow sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-overflow-x`

### Syntax

```common-lisp
(widget-overflow-x sb-pcl::object)
```

## *Generic Function* `widget-overflow-x`

### Syntax

```common-lisp
(widget-overflow-x sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-overflow-y`

### Syntax

```common-lisp
(widget-overflow-y sb-pcl::object)
```

## *Generic Function* `widget-overflow-y`

### Syntax

```common-lisp
(widget-overflow-y sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-padding`

### Syntax

```common-lisp
(widget-padding sb-pcl::object)
```

## *Generic Function* `widget-padding`

### Syntax

```common-lisp
(widget-padding sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-placeholder`

### Syntax

```common-lisp
(widget-placeholder sb-pcl::object)
```

## *Generic Function* `widget-placeholder`

### Syntax

```common-lisp
(widget-placeholder sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-pressed`

### Syntax

```common-lisp
(widget-pressed sb-pcl::object)
```

## *Generic Function* `widget-pressed`

### Syntax

```common-lisp
(widget-pressed sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-readout`

### Syntax

```common-lisp
(widget-readout sb-pcl::object)
```

## *Generic Function* `widget-readout`

### Syntax

```common-lisp
(widget-readout sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-readout-format`

### Syntax

```common-lisp
(widget-readout-format sb-pcl::object)
```

## *Generic Function* `widget-readout-format`

### Syntax

```common-lisp
(widget-readout-format sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-right`

### Syntax

```common-lisp
(widget-right sb-pcl::object)
```

## *Generic Function* `widget-right`

### Syntax

```common-lisp
(widget-right sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-rows`

### Syntax

```common-lisp
(widget-rows sb-pcl::object)
```

## *Generic Function* `widget-rows`

### Syntax

```common-lisp
(widget-rows sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-selected-index`

### Syntax

```common-lisp
(widget-selected-index sb-pcl::object)
```

## *Generic Function* `widget-selected-index`

### Syntax

```common-lisp
(widget-selected-index sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-show-repeat`

### Syntax

```common-lisp
(widget-show-repeat sb-pcl::object)
```

## *Generic Function* `widget-show-repeat`

### Syntax

```common-lisp
(widget-show-repeat sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-source`

### Syntax

```common-lisp
(widget-source sb-pcl::object)
```

## *Generic Function* `widget-source`

### Syntax

```common-lisp
(widget-source sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-step`

### Syntax

```common-lisp
(widget-step sb-pcl::object)
```

## *Generic Function* `widget-step`

### Syntax

```common-lisp
(widget-step sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-style`

### Syntax

```common-lisp
(widget-style sb-pcl::object)
```

## *Generic Function* `widget-style`

### Syntax

```common-lisp
(widget-style sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-target`

### Syntax

```common-lisp
(widget-target sb-pcl::object)
```

## *Generic Function* `widget-target`

### Syntax

```common-lisp
(widget-target sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-timestamp`

### Syntax

```common-lisp
(widget-timestamp sb-pcl::object)
```

## *Generic Function* `widget-timestamp`

### Syntax

```common-lisp
(widget-timestamp sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-tooltip`

### Syntax

```common-lisp
(widget-tooltip sb-pcl::object)
```

## *Generic Function* `widget-tooltip`

### Syntax

```common-lisp
(widget-tooltip sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-tooltips`

### Syntax

```common-lisp
(widget-tooltips sb-pcl::object)
```

## *Generic Function* `widget-tooltips`

### Syntax

```common-lisp
(widget-tooltips sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-top`

### Syntax

```common-lisp
(widget-top sb-pcl::object)
```

## *Generic Function* `widget-top`

### Syntax

```common-lisp
(widget-top sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-value`

### Syntax

```common-lisp
(widget-value sb-pcl::object)
```

## *Generic Function* `widget-value`

### Syntax

```common-lisp
(widget-value sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-visibility`

### Syntax

```common-lisp
(widget-visibility sb-pcl::object)
```

## *Generic Function* `widget-visibility`

### Syntax

```common-lisp
(widget-visibility sb-pcl::new-value sb-pcl::object)
```


## *Generic Function* `widget-width`

### Syntax

```common-lisp
(widget-width sb-pcl::object)
```

## *Generic Function* `widget-width`

### Syntax

```common-lisp
(widget-width sb-pcl::new-value sb-pcl::object)
```


## *Macro* `with-output`

### Syntax

```common-lisp
(with-output o
  &body
  body)
```

### Description

Evaluate body with all output sent to the output widget.
