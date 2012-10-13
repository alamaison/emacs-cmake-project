;;; cmake-project.el -- integrates C/C++ projects that use CMake with Emacs

;; Copyright (C) 2012 Alexander Lamaison

;; Author:  Alexander Lamaison <alexander.lamaison03@imperial.ac.uk>
;; Maintainer: Alexander Lamaison <alexander.lamaison03@imperial.ac.uk>
;; Version: 0.1
;; Keywords: c cmake languages tools

;; This is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Projects using CMake do not integrate well into the Emacs ecosystem
;; which often assumes the existsnce of Makefiles.  This library
;; improves that situation somewhat.
;;

;;; Bugs/todo:

;; - TODO: Provide command to configure build directory initially.
;; - TODO: Make binary directory confiurable
;; - TODO: Extract Flymake command from compile command to pick up
;;         any user changes to build directory
;; - TODO: Find a better way to support header-only directories with
;;         no CMakeLists.txt.

;;; Code:

; Based on `upward-find-file' at http://emacswiki.org/emacs/CompileCommand
(defun upward-find-last-file (filename &optional startdir)
  "Move up directories until we stop finding a certain
filename. When we stop finding it, return the last directory in
which we found it. If the starting directory doesn't include it,
return nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name
                  (if startdir startdir ".")))
        (found-tip nil) ; set if we stop finding it so we know when to exit loop
        (top nil))  ; top is set when we get
                    ; to / so that we only check it once

    (if (not (file-exists-p (expand-file-name filename dirname)))
        nil ; not even in initial dir!

      ;; While we've still got the file keep looking to find where we lose it
      (while (not (or found-tip top))
        ;; If we're at / set top flag.
        (if (string-match "^\\([a-zA-Z]:\\)?/$" (expand-file-name dirname))
            (setq top t)

          ;; Check for the file in the directory above
          (let ((parent (expand-file-name ".." dirname)))
            (if (not (file-exists-p (expand-file-name filename parent)))
                (setq found-tip t)
              ;; If we found it, keep going till we don't
              (setq dirname parent)))))

      (if (and found-tip (not top)) dirname nil))))

(defun cmake-project-find-root-directory ()
  "Find the top-level CMake directory."
  (cmake-project--upward-find-last-file "CMakeLists.txt"))

(defun cmake-project-find-build-directory ()
  "Return an already-configured CMake build directory based on
current directory."
  (concat (file-name-as-directory (cmake-project-find-root-directory)) "bin"))

(defun cmake-project-current-build-command ()
  "Command line to compile current project as configured in the
build directory."
  (concat "cmake --build "
          (shell-quote-argument (cmake-project-find-build-directory))))

(defun cmake-project-flymake-init ()
  (list (executable-find "cmake")
        (list "--build" (cmake-project-find-build-directory))))

(defadvice flymake-get-file-name-mode-and-masks (around cmake-flymake-advice)
  "Override default flymake initialisers for C/C++ source files."
  (let ((flymake-allowed-file-name-masks
	 (append (list '(".[ch]\\(pp\\)?\\'$" cmake-project-flymake-init))
		 flymake-allowed-file-name-masks)))
    ad-do-it))

(defadvice flymake-post-syntax-check (before cmake-flymake-post-syntax-check)
  "Override the treatment of the make process error code.
Flymake expects the make tool to return an error code only if the
specific file it is checking contains an error, and it thinks
there is a fatal configuration error if this is not the case.
That is because Flymake is designed to syntax check one file at a
time.  We can't do that because CMake doesn't provide a way to
build individual files (or at least we can't find one).
Therefore, this advice converts the normal build failure error
code (2) to a success code (0) to prevent a fatal Flymake
shutdown."
  (if (eq (ad-get-arg 0) 2) (ad-set-arg 0 0)))

;;;###autoload
(define-minor-mode cmake-project-mode
  "Minor mode that integrates a CMake-based project with Emacs
build tools such as the CompileCommand and Flymake."
  :lighter "CMake"

  (cond
   ;; Enabling mode
   (cmake-project-mode
    (set (make-local-variable 'compile-command)
	 (cmake-project-current-build-command))

    (ad-enable-advice
     'flymake-get-file-name-mode-and-masks 'around 'cmake-flymake-advice)
    (ad-enable-advice
     'flymake-post-syntax-check 'before 'cmake-flymake-post-syntax-check)
    (ad-activate 'flymake-get-file-name-mode-and-masks))

   ;; Disabling mode
   (t
    (kill-local-variable 'compile-command)

    (ad-disable-advice
     'flymake-post-syntax-check 'before 'cmake-flymake-post-syntax-check)
    (ad-disable-advice
     'flymake-get-file-name-mode-and-masks 'around 'cmake-flymake-advice)
    (ad-activate 'flymake-get-file-name-mode-and-masks))))

(provide 'cmake-project)
