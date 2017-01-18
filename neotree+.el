;;; neotree+.el --- Extra features for neotree to replace dired

;; Copyright (C) 2017 Thibaud Toullier

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

(defvar-local neop-list-mark nil)
(defvar-local neop-list-deletion nil)
(setq neop-list-mark nil)
(setq neop-list-deletion nil)
(defun neop-mark-file()
  "Mark the current selected file for selection"
  (interactive)
  (neo-global--with-buffer
    (setq buffer-read-only nil)
    (beginning-of-line)
    (neop-insert-mark)
    (push (neo-buffer--get-filename-current-line) neop-list-mark)
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
    (setq neop-list-deletion (remove (neo-buffer--get-filename-current-line) neop-list-deletion))
    (setq neop-list-mark (remove (neo-buffer--get-filename-current-line) neop-list-mark))
    ((set )q buffer-read-only t)))

(defun neop-unmark-all-files()
  "Unmark all the selected files"
  (interactive)
  (neo-global--with-buffer
    (setq buffer-read-only nil)
    (setq neop-list-deletion nil)
    (setq neop-list-mark nil)
    (neop-update-neotree)
    (setq buffer-read-only t)))

(defun neop-update-neotree ()
  "Update the neotree view."
  (interactive)
  (neotree-refresh) ;; Refresh the view first
  (neo-global--with-buffer
    (setq buffer-read-only nil)
    (goto-char (point-min))
    ;; For every buffer line
    (while (not (eobp))
      (if (neop-find (neo-buffer--get-filename-current-line) neop-list-mark)
	  (neop-insert-mark))
      (if (neop-find (neo-buffer--get-filename-current-line) neop-list-deletion)
	  (neop-insert-mark-deletion))
      (forward-line))
        (setq buffer-read-only t)))

(defun neop-find (current-line list)
  "Find if the CURRENT-LINE is in the LIST."
  (if (null list)
      nil
    (if (string= current-line (car list))
	t
      (neop-find current-line (cdr list)))))
(defun neop-insert-mark-deletion ()
  "Insert the deletion mark."
    (delete-char 1)
    (insert (all-the-icons-faicon "times-circle-o")))

(defun neop-mark-for-deletion()
  "Mark the current selected file for selection"
  (interactive)
  (neo-global--with-buffer
    (setq buffer-read-only nil)
    (beginning-of-line)
    (push (neo-buffer--get-filename-current-line) neop-list-deletion)
    (neop-insert-mark-deletion)
    (setq buffer-read-only t)))

(defun neop-copy()
  "Copy selected files"
  (interactive)
  (if (not (null neop-list-mark)) ;; If multiple files are selected
    (let* ((current-path (neo-buffer--get-filename-current-line))
	   msg
	   to-path)
      (setq msg (format "Copy [%d] files to: " (length neop-list-mark)))
      (setq to-path (read-file-name msg (file-name-directory current-path)))
      (dolist (p neop-list-mark)
	(copy-file p to-path))
      (message "Copy successful."))
       ;; Otherwise just ask neotree to do it
    (neo-buffer--copy-node)))

(defun neop-make-directory ()
  "Create a new directory."
  (interactive)
  (setq directory-in (file-name-directory (neo-buffer--get-filename-current-line)))
  (setq msg (format "New directory name? %s" directory-in))
  (setq directory-name (read-string msg))
  (make-directory (concat directory-in directory-name))
  (message (format "Directory %s created." directory-name)))
   
(defun neop-delete ()
  "Delete selection."
  (interactive)
  (dolist (p neop-list-deletion)
    (if (file-exists-p p)
	(if (file-directory-p p)
	    (let ((recursive (yes-or-no-p (format "Recursively delete %s?" p))))
	      (delete-directory p recursive))
	  (delete-file p))))
  (setq neop-list-deletion nil)
  (neop-update-neotree))
(provide 'neotree+)
;;; neotree+.el ends here
