;;; helm-dict.el --- Helm interface to local dictionary  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 helm-dict authors

;; Author: Raghav Kumar Gautam <rgautam@apache.com>
;; Keywords: Dictionary, WordNet, Emacs, Elisp, Helm

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

;; Enables look up if dictionary through helm-interface.
;; Default configuration works with WordNet on OSX.
;; For other dictionaries configure: helm-dict-prog, helm-dict-pre-arg, helm-dict-post-arg & helm-dict-get-wordlist

;;; Code:
(require 'helm)
(require 'cl)

(defcustom helm-dict-follow-delay 1
  "Delay before Dictionary summary popup."
  :type 'number
  :group 'helm-dict)

(defcustom helm-dict-wordnet-location "/opt/local/share/WordNet-3.0/dict"
  "Delay before Dictionary summary popup."
  :type 'string
  :group 'helm-dict)

(defcustom helm-dict-prog "wn"
  "Name of the Dictionary program."
  :type 'string
  :group 'helm-dict)

(defcustom helm-dict-pre-arg ""
  "Argument to Dictionary program after command and before the word."
  :type 'string
  :group 'helm-dict)

(defcustom helm-dict-post-arg "-over"
  "Argument to Dictionary program after the word."
  :type 'string
  :group 'helm-dict)

(defcustom helm-dict-get-wordlist 'helm-dict-wordnet-wordlist
  "Function for getting list of words in dictionary."
  :type 'symbol-function
  :group 'helm-dict)

(defvar helm-dict-allwords nil
  "List of all the words available in the Dictionary.")

;;(helm-dict-get-candidates)
(defun helm-dict-get-candidates ()
  "Fetch Dictionary suggestions and return them as a list."
  (unless (bound-and-true-p helm-dict-allwords)
    (setq helm-dict-allwords (funcall helm-dict-get-wordlist)))
  helm-dict-allwords)

(defun helm-dict-wordnet-wordlist ()
  "Fetch WordNet suggestions and return them as a list."
  (let* ((all-indexes (directory-files helm-dict-wordnet-location t "index\\..*" ))
	 (word-indexes (remove-if (lambda (x) (string-match-p "index\\.sense$" x)) all-indexes)))
    (mapcan
     (lambda (x)
       (with-temp-buffer
	 (insert-file-contents x)
	 (goto-char (point-min))
	 (while (re-search-forward "^  .*\n\\| .*" nil t)
	   (replace-match ""))
	 (split-string (buffer-string) "\n" t)))
     word-indexes)))

;;(helm-dict-persistent-action "test")
(defun helm-dict-persistent-action (word)
  "Display meaning of WORD."
  (let ((buf (get-buffer-create "*Dictionary*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (setq cursor-type nil)
      (insert (shell-command-to-string (format "%s %s %s %s" helm-dict-prog helm-dict-pre-arg word helm-dict-post-arg)))
      (fill-region (point-min) (point-max))
      (goto-char (point-min))
      (read-only-mode 1)
      (display-buffer buf))))

(defvar helm-dict-suggest-source
  `((name . "Dictionary Suggest")
    (candidates . helm-dict-get-candidates)
    (action . (("Dictionary" . helm-dict-persistent-action)))
    (persistent-action . helm-dict-persistent-action)
    (pattern-transformer . downcase)
    (keymap . ,helm-map)
    (follow . 1)
    (follow-delay . ,helm-dict-follow-delay)
    (requires-pattern . 1)))

;;;###autoload
(defun helm-dict-suggest ()
  "Preconfigured `helm' for Dictionary lookup with Dictionary suggest."
  (interactive)
  (helm :sources 'helm-dict-suggest-source
	:buffer "*helm dictionary*"
	:input (thing-at-point 'word)))

(provide 'helm-dict)
;;; helm-dict.el ends here
