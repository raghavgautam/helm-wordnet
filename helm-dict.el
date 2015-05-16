;;; package --- A helm source for dictionary
;;
;; Copyright (C) 2015 helm-dict authors
;;
;; Author: Raghav Kumar Gautam <raghav@apache.org>
;; Keywords: Dictionary, WordNet, Emacs, Elisp, Helm
;;
;; Released under the GNU General Public License version 3 or later.
;; See <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; Enables helm to look up dictionary words.
;;
;;; Code:

(require 'helm)

(eval-when-compile
  (require 'cl))

(defcustom helm-dict-follow-delay 1
  "Delay before Dictionary summary popup."
  :type 'number
  :group 'helm-dict)

(defcustom helm-dict-dictionary-location "/opt/local/share/WordNet-3.0/dict"
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

(defcustom helm-dict-get-wordlist 'helm-dict-wn-candidates
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

(defun helm-dict-wn-candidates ()
  "Fetch WordNet suggestions and return them as a list."
  (let* ((all-indexes (directory-files helm-dict-dictionary-location t "index\\..*" ))
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
;;; helm-dict ends here
