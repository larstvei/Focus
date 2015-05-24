[![MELPA](http://melpa.org/packages/focus-badge.svg)](http://melpa.org/#/focus)
# Focus

Focus provides `focus-mode` that dims the text of surrounding sections, similar to [iA Writer's](https://ia.net/writer) Focus Mode.

## Installation

You can install Focus using elpa. It's available on [melpa](http://melpa.milkbox.net/):

<kbd> M-x package-install focus </kbd>

The package has only been tried on Emacs 24.4 and 24.5, but should work on Emacs 24 or higher.

## Usage

Enable the `focus-mode` with <kbd> M-x focus-mode </kbd>.

Also, `focus-read-only-mode` is provided, which is optimized for continuous reading. It inhibits change in the buffer, hides the cursor and provides bindings for moving between *things* (defined in [Thing At Point](http://emacswiki.org/emacs/ThingAtPoint). One can toggle `focus-read-only-mode` with <kbd> M-x focus-read-only-mode </kbd> or <kbd> C-c C-q </kbd> if `focus-mode` is enabled.

Some bindings are provided for simple navigation and exiting `focus-read-only-mode`.

Keybinding         | Description
-------------------|----------------------------
<kbd> n </kbd>     | Jump to next *thing*
<kbd> SPC </kbd>   | Jump to next *thing*
<kbd> p </kbd>     | Jump to previous *thing*
<kbd> S-SPC </kbd> | Jump to previous *thing*
<kbd> i </kbd>     | Exit `focus-read-only-mode`
<kbd> q </kbd>     | Exit `focus-read-only-mode`

### Example

This is what it looks like:

Light Theme (leuven)                     |  Dark Theme (monokai)
:---------------------------------------:|:---------------------------------------:
<img src="./demo-light.gif" width="333"> | <img src="./demo-dark.gif" width="333">
