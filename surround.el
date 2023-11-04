;;; surround.el --- Wrap text with brackets and quotes

;; Author: Edvin Syse
;; URL: https://github.com/edvin/surround.el
;; Version: 0.1

;;; License:

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(defun surround-region (bracket-char)
  "Surround region with bracket-char. If bracket-char has a match in
surround-bracket-alist, use that for opposing wrap char"
  (interactive "cSurround region with char: \n")
  (surround--wrap-region bracket-char (surround--get-matching-bracket (char-to-string bracket-char))))

(defun surround--wrap-region (left right)
  (save-excursion
	(narrow-to-region (region-beginning) (region-end))
	(goto-char (point-min))
	(insert left)
	(goto-char (point-max))
	(insert right)
	(widen)))

(defun surround-expand-region (bracket-char &optional exclusive)
  "Expand region to surrounding bracket-char(s)"
  (interactive "cExpand region to char: \nP")
  (let ((prev-beginning (if (region-active-p) (region-beginning) (point)))
		(prev-end (if (region-active-p) (region-end) (point))))
	(goto-char prev-beginning)
	(search-backward (char-to-string bracket-char))
	(set-mark (+ (point) (if exclusive 1 0)))
	(goto-char (1+ prev-end))
	(search-forward (surround--get-matching-bracket (char-to-string bracket-char)))
	(if exclusive (backward-char))))

(defun surround-select-line ()
  "Select the current line"
  (interactive)
  (end-of-line)
  (set-mark (point))
  (beginning-of-line))

(defun surround-replace-wrap (bracket-char)
  "Replace the wrapping chars at the end of the region"
  (interactive "cReplace region wrapper with: \n")
  (save-excursion (surround-unwrap))
  (surround-region bracket-char))
  
(defun surround-unwrap ()
  "Remove the brackets from the edges of the selected region"
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (goto-char (point-min))
  (delete-char 1)
  (goto-char (point-max))
  (delete-char -1)
  (widen))

(defvar surround-bracket-alist '(
  ("(" . ")")
  ("[" . "]")
  ("{" . "}")))

(defun surround--get-matching-bracket (bracket-char)
  "Return the matching bracket char, or the same char if bracket-char
isn't defined in surround-bracket-alist"
  (or (cdr (assoc bracket-char surround-bracket-alist))
	  bracket-char))

;;;###autoload
(defun surround-activate-keymap ()
  (keymap-global-set "C-c C-s" 'surround-region)
  (keymap-global-set "C-c C-r" 'surround-replace-wrap)
  (keymap-global-set "C-c C-x" 'surround-expand-region)
  (keymap-global-set "C-c C-u" 'surround-unwrap)
  (keymap-global-set "C-c C-l" 'surround-select-line))

(provide 'surround)

;;; surround.el ends here
