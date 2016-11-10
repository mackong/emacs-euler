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
    (while (re-search-forward "<p[^>]*>\\([^<]+?\\)</p>" nil t)
      (setq content (append content (list (match-string 1)))))
    content))

(defun build-filename (problem-info)
  "Build a file name with PROBLEM-INFO."
  (concat (replace-regexp-in-string " " "-" problem-info) ".py"))

(defun append-dash (problem-content)
  "Append `# ` to each PROBLEM-CONTENT."
  (mapcar (lambda (content) (concat "# " content "\n#")) problem-content))

(defun create-euler-file (problem-title problem-info problem-content)
  "Create a euler problem file in current directory with PROBLEM-TITLE, PROBLEM-INFO and PROBLEM-CONTENT."
  (let ((filename (build-filename problem-info)))
    (append-to-file (mapconcat 'identity
                               (append '("#!/usr/bin/env python" "#-*- coding: utf-8 -*-" "" "#")
                                       (list (concat "# " problem-title))
                                       '("#")
                                       (append-dash problem-content))
                               "\n")
                    nil filename)))

;;;###autoload
(defun fetch-euler-problem (problem-number)
  "Fetch euler problem of PROBLEM-NUMBER."
  (interactive "nProblem Number: ")
  (let ((url (concat "https://projecteuler.net/problem=" (number-to-string problem-number))))
    (with-current-buffer (url-retrieve-synchronously url)
      (create-euler-file (get-problem-title) (get-problem-info) (get-problem-content))
      (message "Fetch Success!"))))

(provide 'euler)
;;; euler.el ends here

