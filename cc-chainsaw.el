;;; cc-chainsaw.el --- A few tricks to make c++-mode go -*- lexical-binding: t -*-

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/cc-chainsaw
;; Package-Requires: ((emacs "24.1") (function-args "0.5.1"))
;; Version: 0.1.0
;; Keywords: C++

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(require 'function-args)

(defgroup cc-chainsaw nil
  "A few tricks to make C++ go."
  :group 'c
  :prefix "ccc-")

;;* Code and position info
(defun ccc-class-beginning-position ()
  "Return the class start position."
  (interactive)
  (save-excursion
    (if (re-search-backward "\\b\\(?:class\\|struct\\)\\b[^;]+{" nil t)
        (point))))

(defun ccc-get-function-name ()
  "Return the list (RETURN-TYPE FUNCTION-NAME).
The point needs to be on the function name line."
  (save-excursion
    (beginning-of-line)
    (let ((delimiter-position
           (save-excursion (search-forward ";")
                           (point)))
          template return-type function-name)
      (when (or
             ;; first try to match operator
             (re-search-forward
              "^\\s-*\\([^;]*?\\)\\s-*\\(operator\\s-*\\(:?()\\s-*\\|[^;(]*\\)([^)]*)\\(?:\\s-*const\\)?\\)"
              delimiter-position t)
             ;; otherwise, match a regular function
             (re-search-forward
              "^\\s-*\\([^;]*?\\)\\s-+\\([~A-Za-z_0-9]+([^)]*)\\(?:\\s-*const\\)?\\)"
              delimiter-position t))
        (setq return-type
              (cl-reduce
               (lambda (s item) (replace-regexp-in-string item "" s))
               `(,(match-string-no-properties 1) "virtual " "static " "inline ")))
        (setq function-name
              (replace-regexp-in-string " *= *[^,)\n]" ""
                                        (match-string-no-properties 2)))
        (setq template (ccc-get-function-template))
        (list (if template (concat template "\n" return-type) return-type)
              function-name)))))

(defun ccc-get-function-template ()
  "Return the template part of the current function."
  (and (looking-back "template <[^;{}]*" nil)
       (save-excursion
         (let ((beg
                (progn
                  (search-backward "template <" nil t)
                  (point)))
               (end
                (progn
                  (search-forward "<")
                  (backward-char 2)
                  (fa-backward-char-skip<> -1)
                  (point))))
           (replace-regexp-in-string
            "[ \t\n]*$" ""
            (buffer-substring-no-properties beg end))))))

(defun ccc-function-implementation ()
  "Return the fully resolved function name.
The point sould be on the line of a class method declaration."
  (multiple-value-bind (return-type function-name)
      (ccc-get-function-name)
    (when function-name
      (let* ((class-name (moo-c++-class-name))
             (class-template (moo-c++-class-template)))
        (concat
         (and class-template (concat "template <" class-template ">\n"))
         (unless (string= return-type "")
           (concat return-type " "))
         class-name
         (and class-template (concat "<" (ccc-strip-template class-template) ">"))
         "::" function-name)))))

(defun c++-generate-implementation ()
  "Usage: push mark on function definition.
call this one to generate implementation."
  (interactive)
  (let ((pt (point)))
    (exchange-point-and-mark)
    (push-mark)
    (multiple-value-bind
          (return-type function-name)
        (ccc-get-function-name)
      (let* ((class-name (moo-c++-class-name))
             (class-template (moo-c++-class-template))
             (result (concat
                      (and class-template (concat "template <" class-template ">\n"))
                      (unless (string= return-type "")
                        (concat return-type " "))
                      class-name
                      (and class-template (concat "<" (ccc-strip-template class-template) ">"))
                      "::" function-name)))
        (goto-char pt)
        (back-to-indentation)
        (insert (concat "\n" result " {\n\n}"))
        (indent-region pt (point))
        (forward-line -1)
        (indent-for-tab-command)))))

(defun ccc-strip-template (s)
  "Strip whitespace in the function template S."
  (mapconcat (lambda (x)
               (string-match "^.*? \\([^ ]+\\)$" x)
               (match-string 1 x))
             (split-string s ",")
             ","))

(defun ccc-declared-type ()
  "Return the closest declared type."
  (let ((str
         (save-excursion
           (re-search-backward
            "\\(>\\|[^a-zA-Z0-9_]\\([a-zA-Z0-9_]+\\)[& \t]+\\)"
            (point-min) t)
           (let ((s (match-string-no-properties 1))
                 beg end
                 inner
                 outer)
             (if (string= s ">")
                 (progn
                   (setq end (point))
                   (forward-char)
                   (goto-char (scan-lists (point) -1 0))
                   (setq beg (1+ (point)))
                   (re-search-backward "[^a-zA-Z0-9_]\\([a-zA-Z0-9_]+\\)")
                   (setq outer (match-string-no-properties 1))
                   (if (member outer '("vector" "list"))
                       (progn
                         (setq inner (buffer-substring-no-properties beg end))
                         (and (string-match "\\([a-z_A-Z0-9]*\\)" inner)
                              (format "%ss" (match-string 1 inner))))
                     outer))
               (match-string-no-properties 2))))))
    (replace-regexp-in-string
     "\\(?:^ \\| $\\)" ""
     (replace-regexp-in-string "  +" " " str))))

;;* Code generation / mutation
(defun ccc-insert-var-name ()
  "Generate variable name based on type name.
Suitable for use in a snippet."
  (interactive)
  (let ((str (ccc-declared-type))
        (case-fold-search nil)
        (pos 0)
        words)
    (while (string-match ".[^A-Z]*" str pos)
      (let ((word (downcase
                   (match-string-no-properties 0 str))))
        (if (> (length word) 1)
            (push word words)
          (setq words (cons (concat (car words) word)
                            (cdr words)))))
      (setq pos (match-end 0)))
    (insert
     (mapconcat #'identity (nreverse words) "_"))))

(defun ccc-insert-endl ()
  "Insert std::endl with appropriate << and whitespace."
  (interactive)
  (cond ((looking-back "<<\\(\\s-*\\)" nil)
         (delete-region
          (match-beginning 1)
          (match-end 1))
         (insert " std::endl"))

        ((looking-back "<<[ \t\n]+" nil)
         (insert "std::endl"))
        (t
         (and (looking-back "[^ \t]\\(\\s-+\\)" nil)
              (delete-region
               (match-beginning 1)
               (match-end 1)))
         (insert " << std::endl")))
  (c-indent-line)
  nil)

(defun ccc-insert-float ()
  "Insert floating point type i.e. double."
  (interactive)
  "double")

(defun ccc-astyle ()
  "Format C++ code with astyle."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (point-min)
            end (point-max)))
    (shell-command-on-region beg end "astyle -A3 -U -p -k3" nil t)))

(defun ccc-wrap-in-comment (beg end)
  "Wrap the region between BEG and END in C-style comment."
  (interactive "r")
  (let ((str (buffer-substring-no-properties
              beg end)))
    (delete-region beg end)
    (insert "/* "
            str
            " */")))

(defconst ccc-var-regex "[A-Za-z][A-Za-z0-9_]*"
  "The regex for C++ variable name.")

(defalias 'ccc-get-recent-var
    (byte-compile
     `(lambda ()
        (save-excursion
          (when
              (or
               ;; variable dot chain
               (looking-back
                ,(format
                  " \\(%s\\)\\.%s.*\n[\t ]*"
                  ccc-var-regex
                  ccc-var-regex)
                nil)
               ;; variable constructor init
               (looking-back
                ,(format
                  "[\t ]+\\(%s\\)\\(?:([^)]*)\\)?;[\t\n ]*"
                  ccc-var-regex)
                nil)
               ;; variable dot, first on line
               (re-search-backward
                ,(format "^[ \t]*\\(%s\\)\\." ccc-var-regex) nil t))
            (match-string-no-properties 1)))))
  "Return the closest thing that looks like an object.
The search is performed backwards through code.")

(defun ccc-smart-dot ()
  "Insert a dot or an object name plus dot when appropriate."
  (interactive)
  (let (var-name)
    (if (and (looking-back "^[ \t]*" nil)
             (setq var-name (ccc-get-recent-var)))
        (insert var-name ".")
      (insert "."))))

;;* Makefile / run
(defvar-local ccc-compile-cmd "g++ -g -O2 -std=c++11"
  "The command to compile C++ source.")

(defcustom ccc-cmake-export-cmd "export COW_SAYS=MOO &&"
  "Parameters to export before calling cmake.")

(defun ccc-generate-makefile ()
  "Generate a Makefile for the current simple C++ project."
  (interactive)
  (let* ((n-buffer (buffer-file-name))
         (n-file (file-name-nondirectory n-buffer))
         (n-target (file-name-sans-extension n-file))
         (n-makefile (concat (file-name-directory n-buffer) "Makefile"))
         (cmd ccc-compile-cmd))
    (if (file-exists-p n-makefile)
        (when (called-interactively-p 'any)
          (message "Makefile already exists"))
      (with-current-buffer (find-file-noselect n-makefile)
        (insert
         (concat n-target ": " n-file
                 (format "\n\t%s -o $@ $^" cmd)
                 "\n\nclean: \n\trm -f " n-target
                 "\n\nrun: " n-target "\n\t ./" n-target
                 "\n\n.PHONY: clean run\n"))
        (save-buffer)))))

(defun ccc-run ()
  "Compile and run the current simple C++ project."
  (interactive)
  (save-buffer)
  (let* ((n-buffer (buffer-file-name))
         (makefilep (file-exists-p
                     (concat (file-name-directory n-buffer) "Makefile")))
         (cmakelistsp (file-exists-p
                       (concat (file-name-directory n-buffer) "CMakeLists.txt")))
         (builddir (concat (file-name-directory n-buffer) "build/"))
         (builddirp (file-exists-p builddir))
         (out-makefilep (file-exists-p
                         (concat (file-name-directory n-buffer) "build/Makefile"))))
    (cond
      (makefilep)
      (out-makefilep)

      ((not cmakelistsp)
       (ccc-generate-makefile)
       (setq makefilep t))

      (builddirp
       (shell-command
        (concat ccc-cmake-export-cmd
                "cd build && cmake ..")))
      (t
       (async-shell-command "cmake .")))

    (compile (if makefilep
                 "make -j8 run"
               "cd build && make -j8 run"))))

;;* Access modifers
(defun ccc-ensure (modifier)
  "Ensure that the current point is under MODIFIER access."
  (let ((class-beg (ccc-class-beginning-position)))
    (if (null class-beg)
        (user-error "Not in a class")
      (let ((access
             (save-excursion
               (if (re-search-backward
                    "\\(\\(?:public:\\)\\|\\(?:private:\\)\\|\\(?:protected:\\)\\)"
                    class-beg t)
                   (match-string-no-properties 1)
                 "private:"))))
        (unless (equal access modifier)
          (save-excursion
            (let ((prev-thing
                   (save-excursion
                     (back-to-indentation)
                     (when (re-search-backward ":\\|;\\|}" class-beg t)
                       (1+ (point))))))
              (if prev-thing
                  (progn
                    (goto-char prev-thing)
                    (insert "\n" modifier)
                    (indent-region prev-thing (point)))
                (progn
                  (forward-line -1)
                  (if (save-excursion
                        (beginning-of-line)
                        (looking-at "[ \t]*$"))
                      (insert modifier)
                    (progn
                      (end-of-line)
                      (insert "\n" modifier)))
                  (c-indent-line-or-region))))))))))

(defun ccc-ensure-public ()
  "Ensure public access."
  (interactive)
  (ccc-ensure "public:"))

(defun ccc-ensure-private ()
  "Ensure private access."
  (interactive)
  (ccc-ensure "private:"))

(defun ccc-ensure-protected ()
  "Ensure protected access."
  (interactive)
  (ccc-ensure "protected:"))

;;* Alignment
(defun ccc-selected-lines (rbeg rend)
  "Return list of pairs (beginning-of-line-position . string)
for each line selected between RBEG and REND"
  (let ((start (save-excursion (goto-char rbeg)
                               (line-beginning-position)))
        (end (save-excursion (goto-char rend)
                             (line-end-position)))
        lines)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (unless (eolp)
          (push (cons (point)
                      (buffer-substring-no-properties
                       (point)
                       (line-end-position)))
                lines))
        (beginning-of-line 2))
      lines)))

(defun ccc-extract-argument-positions (str)
  "Given a single line C/C++ string STR, return list of arguments in form:
\(start-position end-position string\) for each argument"
  (let ((regex "\\(?: (?[^ (\n]+\\)\\(?:,[\n ]\\|)\\|,$\\|;$\\|[ ]+=[ ]+\\)")
        (args (list))
        (pos 0))
    (while (string-match regex str pos)
      (push (list (match-beginning 0)
                  (match-end 0)
                  (substring (match-string-no-properties 0 str) 0 -1))
            args)
      (setq pos (match-end 0)))
    (reverse args)))

(defun ccc-align-function-arguments (beg end)
  "Align selected C/C++ function arguments list with spaces.
BEG and END are the bounds of the arumetns list."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (let (end)
         (up-list)
         (setq end (point))
         (backward-list)
         (list (point) end)))))
  (let ((lines (ccc-selected-lines beg end)))
    (if (not (string-match "^("
                           (buffer-substring-no-properties beg end)))
        (ccc-align-declarations)
      (if (> (length lines) 1)
          (let* ((pts-begin (mapcar #'car lines))
                 (pts-arg-rel (mapcar (lambda (x)
                                        (caar
                                         (ccc-extract-argument-positions
                                          (cdr x))))
                                      lines))
                 (pt-arg-max (apply #'max pts-arg-rel)))
            (cl-mapcar (lambda (b x)
                         (goto-char (+ b x))
                         (insert (make-string (- pt-arg-max x) ?\ )))
                       pts-begin
                       pts-arg-rel))))))

(defun ccc-align-declarations ()
  "Align C++ declarations at point."
  (interactive)
  (let* ((lines (ccc-selected-lines (region-beginning) (region-end)))
         (semicolon-positions (mapcar (lambda (line) (cl-position ?\; (cdr line))) lines))
         (max-semicolon-position
          (cl-reduce (lambda (a b) (if b (max a b) a)) semicolon-positions :initial-value 0)))
    (cl-mapcar (lambda (line spos)
                 (when spos
                   (let ((ws-pos (string-match "\\(\\s-*\\)[A-Z_a-z0-9<>()]+\\s-*;" (cdr line))))
                     (goto-char (+ ws-pos (car line)))
                     (insert (make-string (- max-semicolon-position spos) ?\ )))))
               lines
               semicolon-positions)))

;;* Navigation
(defun ccc-jump-to-definition ()
  "Jump from declaration to definition.
Both need to be in a single file."
  (interactive)
  (let* ((s1 (ccc-function-implementation))
         (s2 (and (string-match "[^(]+(" s1)
                  (match-string 0 s1)))
         (s3 (and s2 (replace-regexp-in-string "[\n \t]+" "[\n \t]*" s2))))
    (ring-insert find-tag-marker-ring (point-marker))
    (if (and s3 (re-search-forward s3 nil t))
        (backward-char 2)
      (user-error "Couldn't jump to %s" s2))))

(defun ccc-electric-del (&optional arg)
  "Delete backwards one char and some whitespace."
  (interactive "p")
  (dotimes (_i arg)
    (if (looking-back "\n +")
        (progn
          (delete-region
           (match-beginning 0)
           (match-end 0))
          (indent-for-tab-command))
      (delete-char -1))))

(provide 'cc-chainsaw)

;;; cc-chainsaw.el ends here
