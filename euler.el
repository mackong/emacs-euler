;;; euler.el --- Fetch euler problem into python file.

;; Copyright (C) 2016-2016 MacKong <mackonghp@gmail.com>

;; Author: MacKong <mackonghp@gmail.com>
;; Version: 1.0
;; Keywords: project euler

;;; Commentary:

;; Fetch project euler problem into python file.
;;
;; the new python file is named with problem number like `problem-1.py`.
;;

;; Usage:
;;
;; run `fetch-euler-problem` with problem number.
;;

;; TODO:
;;
;; - Report error when problem doesn't exist.
;; - Split the long paragraph string.
;; - Post the answer code.
;;

;;; Code:

(defgroup emacs-euler nil
  "Customization group for `emacs-euler' package."
  :group 'applications)

(defcustom emacs-euler-default-file-suffix ".py"
  "File suffix for new file.
Default is python language suffix `.py'."
  :type 'string
  :group 'emacs-euler)

(defcustom emacs-euler-default-file-header "#!/usr/bin/env python
# -*- coding: utf-8 -*-

"
  "File header for new file.
Default is for python language.
It can be empty if doesn't need."
  :type 'string
  :group 'emacs-euler)

(defcustom emacs-euler-default-comment-start "#"
  "Start part for comment.
Default is python comment start.
Example for c language, it should be `/*'."
  :type 'string
  :group 'emacs-euler)

(defcustom emacs-euler-default-comment-middle "#"
  "Middle part for comment.
Default is python comment middle.
Example for c language, it should be ` *'.
It can be empty for language which support multi-line comment."
  :type 'string
  :group 'emacs-euler)

(defcustom emacs-euler-default-comment-end "#"
  "End part for comment.
Default is python comment end.
Example for c language, it should be `*/'."
  :type 'string
  :group 'emacs-euler)


;;;; Variables
(defvar emacs-euler-file-suffix emacs-euler-default-file-suffix
  "Initial file suffix for new file.")

(defvar emacs-euler-file-header emacs-euler-default-file-header
  "Initial file header for new file.")

(defvar emacs-euler-comment-start emacs-euler-default-comment-start
  "Initial comment start.")

(defvar emacs-euler-comment-middle emacs-euler-default-comment-end
  "Initial comment middle.")

(defvar emacs-euler-comment-end emacs-euler-default-comment-end
  "Initial comment end.")

;;;; Functions
(defun get-problem-title ()
  "Get problem title."
  (goto-char (point-min))
  (re-search-forward "<h2>\\([^>]+?\\)</h2>" nil t)
  (match-string 1))

(defun get-problem-info ()
  "Get problem info."
  (goto-char (point-min))
  (re-search-forward "<h3>\\([^>]+?\\)</h3>" nil t)
  (match-string 1))

(defun get-problem-content ()
  "Get problem content into a list."
  (let ((content nil))
    (goto-char (point-min))
    (while (re-search-forward "<p[^>]*>\\(.*\\)</p>" nil t)
      (setq content (append content (list (match-string 1)))))
    content))

(defun build-filename (problem-info)
  "Build a file name with PROBLEM-INFO."
  (concat (replace-regexp-in-string " " "_" (downcase problem-info))
          emacs-euler-file-suffix))

(defun build-problem-string (problem-title problem-info problem-content)
  "Build a string for problem include PROBLEM-TITLE, PROBLEM-INFO, PROBLEM-CONTENT."
  (concat emacs-euler-file-header
          emacs-euler-comment-start "\n"
          emacs-euler-comment-middle " " problem-title "\n"
          (mapconcat 'identity
                     (mapcar (lambda  (s)
                               (concat emacs-euler-comment-middle "\n"
                                       emacs-euler-comment-middle " " s))
                             problem-content)
                     "\n")
          "\n" emacs-euler-comment-end "\n"))

(defun create-euler-file (problem-title problem-info problem-content)
  "Create a euler problem file in current directory with PROBLEM-TITLE, PROBLEM-INFO and PROBLEM-CONTENT."
  (append-to-file (build-problem-string problem-title problem-info problem-content)
                  nil (build-filename problem-info)))

(defun open-euler-file (problem-info)
  "Open euler problem file of PROBLEM-INFO."
  (find-file (build-filename problem-info)))

;;;; Interactive functions
;;;###autoload
(defun fetch-euler-problem (problem-number)
  "Fetch euler problem of PROBLEM-NUMBER."
  (interactive "nProblem Number: ")
  (let ((url (concat "https://projecteuler.net/problem=" (number-to-string problem-number))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let ((problem-info (get-problem-info)))
        (create-euler-file (get-problem-title) problem-info (get-problem-content))
        (open-euler-file problem-info)
        (goto-char (point-max)))
      (message "Fetch Success!"))))

(provide 'euler)
;;; euler.el ends here

