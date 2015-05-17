# Focus

Focus provides `focus-mode` that dims the text of surrounding sections, similar to [iA Writer's](https://ia.net/writer) Focus Mode.

## Installation

This package is still in development, so has not been submitted to
[Melpa](http://melpa.org/). This section will be simplified when (or if) the
package is added to Melpa.

The mode has only been tested with Emacs 24.4 and 24.5. It has no dependencies
other than `cl-lib` which is built in.

To install the Emacs extension just download the `focus.el`

```
git clone git@github.com:larstvei/Focus.git
```

and store `focus.el` it somewhere in your `load-path`. To use it you can put

```emacs-lisp
(require 'focus)
```

in you Emacs configuration file.

## Usage

Enable the mode with <kbd> M-x focus-mode </kbd>.

### Example

This is what it looks like:

Light Theme (leuven)                     |  Dark Theme (monokai)
:---------------------------------------:|:---------------------------------------:
<img src="./demo-light.gif" width="333"> | <img src="./demo-dark.gif" width="333">
