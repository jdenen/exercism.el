;;; lint-exercism.el --- Linter for exercism.el

;; Copyright (C) 2018 Johnson Denen
;; Author: Johnson Denen <johnson.denen@gmail.com>

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

;; Lint with checkdoc and package-lint against exercism.el. Kills Emacs
;; with an exit code of 1 if any errors or warnings are reported.

;; Don't run this from inside Emacs (unless you're feeling dangerous).

;;; Code:
(require 'package)
(push '("melpa" . "http://melpa.org/packages/") package-archives)
(package-initialize)
(package-refresh-contents)

(require 'cl-lib)
(require 'checkdoc)
(require 'package-lint)

(defun exercism-lint-print (buffer)
  (with-current-buffer buffer
    (message (buffer-string))))

(with-current-buffer (find-file "exercism.el")
  (checkdoc-current-buffer t))

(with-current-buffer "*Style Warnings*"
  (when (re-search-forward "exercism\\.el:[0-9]+:" nil t)
    (exercism-lint-print "*Style Warnings*")
    (kill-emacs 1)))

(with-current-buffer (find-file "exercism.el")
  (let ((lint (package-lint-buffer)))
    (when lint
      (dolist (item lint)
        (let ((line (car item))
              (type (cl-caddr item))
              (output (cl-cadddr item)))
          (message (format "%s:%s: %s" type line output))))
      (kill-emacs 1))))

(provide 'lint-exercism)
;;; lint-exercism.el ends here
