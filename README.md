# emacs-euler

Fetch project euler problem into file. Default is fetch to a python file.

## Configuration
In order to fetch to other language file, you can customize *emacs-euler-\** variables.
For example, with:
```
(setq emacs-euler-file-suffix ".c")
(setq emacs-euler-file-header "")
(setq emacs-euler-comment-start "/*")
(setq emacs-euler-comment-middle " *")
(setq emacs-euler-comment-end " */")
```
we can fetch into a C language file.

## Usage
Type
```
M-x fetch-euler-problem
```
with problem number.

