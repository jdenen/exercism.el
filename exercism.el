;;; exercism.el --- Convenience functions for exercism.io -*- lexical-binding: t -*-

;; Copyright (C) 2018 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (s "1.12.0"))
;; Homepage: https://github.com/jdenen/exercism.el

;; This file is not part of GNU Emacs.

;; This file is part of exercism.el.

;; exercism.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; exercism.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with exercism.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; exercism.el is an Emacs client for Exercism <http://exercism.io>, a programming
;; language learning platform.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 's)

(defvar exercism-json-file "~/.exercism.json"
  "Filepath to Exercism JSON file.")

(defun exercism-submit-buffer ()
  "Submit function `buffer-file-name' result as a solution."
  (interactive)
  (exercism--run-command (format "submit %s" (buffer-file-name))))

(defun exercism-fetch ()
  "Fetch a new language problem."
  (interactive)
  (let* ((exercism-dir (cdr (assoc 'dir (json-read-file exercism-json-file))))
         (lang-list (directory-files exercism-dir nil "[^\.]+$"))
         (lang (completing-read "Fetch for: " lang-list)))
    (exercism--run-command (format "fetch %s" lang))))

(defun exercism--run-command (cmd)
  "Run exercism command CMD."
  (let* ((process-name (concat "exercism-" (car (split-string cmd " " t))))
         (process-buffer (format "*%s*" process-name)))
    (when (get-buffer process-buffer) (kill-buffer process-buffer))
    (with-output-to-temp-buffer process-buffer
      (start-process-shell-command process-name
                                   process-buffer
                                   (format "exercism %s" cmd)))))

(provide 'exercism)
;;; exercism.el ends here
