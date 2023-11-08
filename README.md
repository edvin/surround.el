# Surround.el

Surrounds the current selection with brackets or quotes. It can also remove bracket pairs from the end of the selected region
and it provides some convenience functions for selecting and expanding the selection region.

## Installation

```elisp
(straight-use-package
 '(surround :type git :host github :repo "edvin/surround.el")
 :config (surround-mode 1))
```

## Updating

Surround is not hosted in MELPA so you have to pull it from GitHub. If you use Straight.el you can update the dependency
with `M-x straight-pull-package` and selecting `surround`.

## Usage

* `C-c C-s s` (`surround-region`) Prompts for a `bracket-char` to wrap around the currently selected region. If the `bracket-char` has a counterpart in `surround-bracket-alist`, the corresponding end bracket char is added to the end of the region. Selecting quotes like `'` or `"` will add the same char to the end of the region, while `(`, `[` and `{` will add the corresponding end bracket instead.

* `C-C C-s r` (`surround-replace-wrap`) will replace the `bracket-char` at the ends of the currently selected region with the newly selected `bracket-char`. Counterparts are looked up the same way as for `surround-region`.

* `C-c C-s e` (`surround-expand-region`) will expand the selection region to the next matching `bracket-char`. Repeat to expand further. You can pass `C-u` first to exclude the `bracket-char` pair from the selected area. (Passing the optional `exclusive` parameter to the function).

* `C-c C-s x` (`surround-auto-expand-region`) will automatically expand the selection region to the next matching registered bracket-chars. It will consult `surround-auto-expand-alist` to look for matches and find the opposing bracket by looking up `surround-bracket-alist`. You can pass `C-u` first to exclude the matched bracket pair from the selected area. (Passing the optional `exclusive` parameter to the function). After auto-expanding the region you can keep pressing 'x' to further expand the region.

* `C-c C-s u` (`surround-unwrap`) will unwrap/remove the brackets at the end of the currently selected region. Typically you'd select the region with `surround-expand-region` first, then either unwrap or replace the `bracket-char` pair.

* `C-c C-s l` (`surround-select-line`) is a convenience function to select the whole line. It does the same as `C-a C-SPC C-e`, just nicer on the hands.
