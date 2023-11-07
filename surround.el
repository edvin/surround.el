;;; surround.el --- Wrap text with brackets and quotes -*- lexical-binding: t; -*-

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

(defcustom surround-bracket-alist '(
  ("(" . ")")
  ("[" . "]")
  ("{" . "}"))
  "Pairs of matching brackets used for surrounding regions"
  :group 'surround
  :type '(alist :value-type (group string)))

(defcustom surround-auto-expand-alist '("\"" "'" "{" "[" "(")
  "Chars to look for when auto expanding regions"
  :group 'surround
  :type '(alist :value-type string))

(defun surround-region (bracket-char)
  "Surround region with bracket-char. If bracket-char has a match in
surround-bracket-alist, use that on right side instead"
  (interactive "cSurround region with char: \n")
  (surround--wrap-region bracket-char (surround--get-matching-bracket (char-to-string bracket-char))))

(defun surround--wrap-region (left right)
  "Wrap region with the left and right char parameters"
  (save-excursion
	(narrow-to-region (region-beginning) (region-end))
	(goto-char (point-min))
	(insert left)
	(goto-char (point-max))
	(insert right)
	(widen)))

(defun surround--expand-region-fn (search-b-fn exclusive)
  "Expand region by searching backwards using search-b-fn, then looking
at the resulting char under cursor and getting the matching bracket
before searching forward"
  (let ((prev-beginning (if (region-active-p) (region-beginning) (point)))
		(prev-end (if (region-active-p) (region-end) (point))))
	(goto-char prev-beginning)
	(funcall search-b-fn)
	(let ((bracket-char (char-after)))
	  (set-mark (+ (point) (if exclusive 1 0)))
	  (goto-char prev-end)
	  (search-forward (surround--get-matching-bracket (char-to-string bracket-char))))
	(if exclusive (backward-char))))

(defun surround-expand-region (bracket-char &optional exclusive)
  "Expand region to surrounding bracket-char"
  (interactive "cExpand region to char: \nP")
  (surround--expand-region-fn (lambda() (search-backward (char-to-string bracket-char))) exclusive))

(defun surround-auto-expand-region (&optional exclusive)
  "Expand region by looking for any char in surround-auto-expand-alist
to the left of the region and then look for the corresponding bracket-char
via lookup in surround-bracket-alist to the right of the region"
  (interactive)
  (surround--expand-region-fn (lambda() (search-backward-regexp
   (concat "[" (string-join surround-auto-expand-alist) "]"))) exclusive))

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

(defun surround--get-matching-bracket (bracket-char)
  "Return the matching bracket char, or the same char if bracket-char
isn't defined in surround-bracket-alist"
  (or (cdr (assoc bracket-char surround-bracket-alist))
	  bracket-char))

;; Define minor mode and activate keymap when surround-mode is toggled
 (defvar surround-mode nil
   "Toggle surround-mode.")

;;;###autoload
(defun surround-mode (&optional ARG)
  "Toggle surround-mode"
  (interactive (list 'toggle))
  (setq surround-mode
		(if (eq ARG 'toggle)
			(not surround-mode)
		  (> ARG 0)))

  (if surround-mode
	  (message "surround-mode activated")
	(message "surround-mode deactivated")))
	
(defvar surround-mode-map (make-sparse-keymap)
  "Keymap for surround keybindings")

(define-key surround-mode-map (kbd "C-C C-s s") 'surround-region)
(define-key surround-mode-map (kbd "C-C C-s r") 'surround-replace-wrap)
(define-key surround-mode-map (kbd "C-C C-s e") 'surround-expand-region)
(define-key surround-mode-map (kbd "C-C C-s x") 'surround-auto-expand-region)
(define-key surround-mode-map (kbd "C-C C-s u") 'surround-unwrap)
(define-key surround-mode-map (kbd "C-C C-s l") 'surround-select-line)

(add-to-list 'minor-mode-alist '(surround-mode " surround"))
(add-to-list 'minor-mode-map-alist (cons 'surround-mode surround-mode-map))

(provide 'surround)

;;; surround.el ends here
