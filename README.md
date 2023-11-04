# Surround.el

Surrounds the current selection with brackets or quotes. It can also remove bracket pairs from the end of the selected region
and it provides some convenience functions for selecting and expanding the selection region.

## Installation

```elisp
(straight-use-package
 '(surround :type git :host github :repo "edvin/surround.el"))
(surround-activate-keymap)
```

## Default keybindings

The `surround-activate-keymap` function adds the following global keybindings:

```elisp
  (keymap-global-set "C-c C-s" 'surround-region)
  (keymap-global-set "C-c C-r" 'surround-replace-wrap)
  (keymap-global-set "C-c C-x" 'surround-expand-region)
  (keymap-global-set "C-c C-u" 'surround-unwrap)
  (keymap-global-set "C-c C-l" 'surround-select-line))
```


## Usage

* `C-c C-s` (`surround-region`) Prompts for a `bracket-char` to wrap around the currently selected region. If the `bracket-char`
has a counterpart in `surround-bracket-alist`, the corresponding end bracket char is added to the end of the region.
Selecting quotes like `'` or `"` will add the same char to the end of the region, while `(`, `[` and `{` will add
the corresponding end bracket instead.

* `C-C C-r` (`surround-replace-wrap`) will replace the `bracket-char` at the ends of the currently selected region with the newly selected
`bracket-char`. Counterparts are looked up the same way as for `surround-region`.

* `C-c C-x` (`surround-expand-region`) will expand the selection region to the next matching `bracket-char`. Repeat to expand further. You can pass `C-u` first to exclude the `bracket-char` pair from the selected area. (Passing the optional `exclusive` parameter to the function).

* `C-c C-u` (`surround-unwrap`) will unwrap/remove the brackets at the end of the currently selected region. Typically you'd select the
region with `surround-expand-region` first, then either unwrap or replace the `bracket-char` pair.

* `C-c C-l` (`surround-select-line`) is a convenience function to select the whole line. It does the same as `C-a C-SPC C-e`.
