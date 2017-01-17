;;; neotree+.el --- Extra features for neotree to replace dired

;; Copyright (C) 2014 Thibaud Toullier

;; Author: Thibaud Toullier <t.toullier@gmail.com>
;; URL:
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this file, put something like the following in your
;; ~/.emacs:

;;; Code:

;;
;; Constants
;;
(require 'neotree)

(defvar-local neop-list nil)

(defun neop-mark-file()
  "Mark the current selected file for selection"
  (interactive)
  (neo-global--with-buffer
    (setq buffer-read-only nil)
    (beginning-of-line)
    (neop-insert-mark)
    (push (neo-buffer--get-filename-current-line) neop-list)
    (setq buffer-read-only t)))

(defun neop-insert-mark()
  "Insert mark"
  (delete-char 1)
  (insert (all-the-icons-faicon  "check-circle-o")))

(defun neop-unmark-file()
  "Unmark the current selected file for selection"
  (interactive)
  (neo-global--with-buffer
    (setq buffer-read-only nil)
    (beginning-of-line)
    (delete-char 1)
    (insert " ")
    (setq neop-list (remove (neo-buffer--get-filename-current-line) neop-list))
    (setq buffer-read-only t)))

(defun neop-unmark-all-files()
  "Unmark all the selected files"
  (interactive)
  (neo-global--with-buffer
    (setq buffer-read-only nil)
    (setq neop-list nil)
    (neop-update-neotree)
    (setq buffer-read-only t)))

(defun neop-update-neotree ()
  "Update the neotree view."
  (interactive)
  (neotree-refresh) ;; Refresh the view first
  (neo-global--with-buffer
    (goto-char (point-min))
    ;; For every buffer line
    (while (not (eobp))
      (if (neop-find (neo-buffer--get-filename-current-line) neop-list)
	  (neop-insert-mark))
      (forward-line))))

(defun neop-find (current-line list)
  "Find if the CURRENT-LINE is in the LIST."
  (if (null list)
      nil
    (if (eq current-line (car list))
	t
      (neop-find (cdr list)))))

(defun neop-mark-for-deletion()
  "Mark the current selected file for selection"
  (interactive)
  (neo-global--with-buffer
    (setq buffer-read-only nil)
    (beginning-of-line)
    (delete-char 1)
    (insert (all-the-icons-faicon "times-circle-o"))
;;    (insert (all-the-icons-faicon  "times-circle-o"))
    (setq buffer-read-only t)))

(defun neop-copy()
  "Copy selected files"
  (interactive)
  (if (not (null neop-list)) ;; If multiple files are selected
    (let* ((current-path (neo-buffer--get-filename-current-line))
	   msg
	   to-path)
      (setq msg (format "Copy [%d] files to: " (length neop-list)))
      (setq to-path (read-file-name msg (file-name-directory current-path)))
      (dolist (p neop-list)
	(copy-file p to-path))
      (message "Copy successful."))
       ;; Otherwise just ask neotree to do it
    (neo-buffer--copy-node)))
    
(provide 'neotree+)
;;; neotree+.el ends here
