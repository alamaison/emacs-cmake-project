;;; cmake-project.el --- Integrates CMake build process with Emacs

;; Copyright (C) 2012, 2013 Alexander Lamaison

;; Author:  Alexander Lamaison <alexander.lamaison@gmail>
;; Maintainer: Alexander Lamaison <alexander.lamaison@gmail>
;; URL: http://github.com/alamaison/emacs-cmake-project
;; Version: 0.7
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
;; which often assumes the existence of Makefiles.  This library
;; improves that situation somewhat.
;;

;;; Bugs/todo:

;; - TODO: Make binary directory configurable
;; - TODO: Extract Flymake command from compile command to pick up
;;         any user changes to build directory
;; - TODO: Find a better way to support header-only directories with
;;         no CMakeLists.txt.

;;; History:

;; 0.1 - Initial version with compile command and Flymake support
;; 0.2 - Made compatible with Marmalade
;; 0.3 - Bug fixes
;; 0.4 - Command to configure new CMake build tree
;; 0.5 - Option to choose the generator when configuring
;; 0.6 - Fix bug configuring paths that do not have spaces
;; 0.7 - Pick up build directory from pre-set `cmake-project-build-directory'
;;

;;; Code:

;; Need cl-flet for local functions (somewhere, somehow, flet got deprecated?)
(require 'cl-lib)

(defvar cmake-project-cmake-command "cmake"
  "The (default) cmake command.")

;; NOTE: This variable is NEVER local to a buffer.
(defvar cmake-project-build-config "Debug"
  "The (default) CMake build configuration (Debug, Release, ...)")

(defvar cmake-project-memoize-sources (make-hash-table :test 'equal)
  "Hash table that associates top-level CMake project directory
names to a build directory and code generator.")

(defvar cmake-project-memoize-capabilities (make-hash-table :test 'equal)
  "Hash table that associates cmake-project-cmake-command values to a
'cmake-project--make-cmake-capabilies' record.")

(defcustom cmake-project-default-build-dir-name "cmake-build/"
  "Default name for CMake build tree directories."
  :type 'directory
  :group 'data)

(defconst cmake-project-invalid-src-rootdir 'invalid-source-project-root
  "A value that signals that the CMake project's project source root
directory is invalid.")

(defvar cmake-project-src-rootdir cmake-project-invalid-src-rootdir
  "The (default) CMake project source root directory.")

;; Is the project source root directory valid?
(defun cmake-project--valid-src-rootdir ()
  (and (not (eq cmake-project-src-rootdir cmake-project-invalid-src-rootdir))
       cmake-project-src-rootdir))

;; CMake capabilities record:
(defun cmake-project--make-capability
    (s-flag b-flag a-flag t-flag generators arch-suffix-generators)
  "Make a cmake-project-memoize-capabilities record -- '-S', '-B',
'-A', '-T' command line flags (are they supported?) and the
available generators."
  ;; (aref _ 0): Record type (cmake-project-capabilities
  ;; (aref _ 1): "-S" flag supported
  ;; (aref _ 2): "-B" flag supported
  ;; (aref _ 3): "-A" flag supported
  ;; (aref _ 4): "-T" flag supported
  ;; (aref _ 5): Generator list
  ;; (aref _ 6): Generators that take an architecture (" [arch]") as part
  ;;             of the generator's name.
  (record 'cmake-project-capabilities
          s-flag b-flag a-flag t-flag
          generators
          arch-suffix-generators))

(defun cmake-project--capability-s-flag (capability)
  "Getter for \"-S\" flag capability"
  (or (and (eq 'cmake-project-capabilities (type-of capability)) (aref capability 1))
      (error "cmake-project--capability-s-flag: Expected a cmake-project-capabilities record.")))

(defun cmake-project--capability-b-flag (capability)
  "Getter for \"-B\" flag capability"
  (or (and (eq 'cmake-project-capabilities (type-of capability)) (aref capability 2))
      (error "cmake-project--capability-b-flag: Expected a cmake-project-capabilities record.")))

(defun cmake-project--capability-a-flag (capability)
  "Getter for \"-A\" flag capability"
  (or (and (eq 'cmake-project-capabilities (type-of capability)) (aref capability 3))
      (error "cmake-project--capability-a-flag: Expected a cmake-project-capabilities record.")))

(defun cmake-project--capability-t-flag (capability)
  "Getter for \"-T\" flag capability"
  (or (and (eq 'cmake-project-capabilities (type-of capability)) (aref capability 4))
      (error "cmake-project--capability-t-flag: Expected a cmake-project-capabilities record.")))

(defun cmake-project--capability-generators (capability)
  "Getter for the capability's generator list"
  (or (and (eq 'cmake-project-capabilities (type-of capability)) (aref capability 5))
      (error "cmake-project--capability-generators: Expected a cmake-project-capabilities record.")))

(defun cmake-project--capability-arch-suffix-generators (capability)
  "Getter for the capability's arch-suffix-generator list"
  (or (and (eq 'cmake-project-capabilities (type-of capability)) (aref capability 6))
      (error "cmake-project--capability-arch-suffix-generators: Expected a cmake-project-capabilities record.")))


;; CMake project memos
(defun cmake-project--make-memo (build-dir)
  "Make a cmake-project-memo record, which is stashed in the
cmake-project-memoize-sources hash table."
  ;; (aref _ 0): Record type
  ;; (aref _ 1): Build directory
  ;; (aref _ 2): Architecture argument
  ;; (aref _ 3): Toolset argument
  ;; (aref _ 4): Configure flags
  ;; (aref _ 5): Build configuration (Release, Debug, ...)
  (record 'cmake-project-memo build-dir nil nil nil cmake-project-build-config))

(defun cmake-project--memo-build-dir (memo)
  "'cmake-project-memo' getter function for the build directory."
  (or (and (eq 'cmake-project-memo (type-of memo)) (aref memo 1))
      (error "cmake-project--memo-build-dir: Expected cmake-project-memo record.")))

(defun cmake-project--memo-architecture (memo)
  "'cmake-project-memo' getter function for the architecture
configuration argument."
  (unless (eq 'cmake-project-memo (type-of memo))
    (error "cmake-project--memo-architecture: Expected cmake-project-memo record."))
  (aref memo 2))

(defun cmake-project--memo-toolset (memo)
  "'cmake-project-memo' getter function for the toolset
configuration argument."
  (unless (eq 'cmake-project-memo (type-of memo))
    (error "cmake-project--memo-toolset: Expected cmake-project-memo record."))
  (aref memo 3))

(defun cmake-project--memo-configure-args (memo)
  "'cmake-project-memo' getter function for extra configuration
flags."
  (unless (eq 'cmake-project-memo (type-of memo))
    (error "cmake-project--memo-configure-args: Expected cmake-project-memo record."))
  (aref memo 4))

(defun cmake-project--memo-build-config (memo)
  "'cmake-project-memo' getter function for the build
configuration."
  (or (and (eq 'cmake-project-memo (type-of memo)) (aref memo 5))
      (error "cmake-project--memo-build-config: Expected cmake-project-memo record.")))

(defun cmake-project--set-memo-build-directory (source-dir build-dir)
  (let* ((proper-source-dir (expand-file-name source-dir))
         (memo (gethash proper-source-dir cmake-project-memoize-sources)))
    (if memo
        (aset memo 1 build-dir)
      (error "cmake-project--set-memo-build-directory %s: No source directory? (%s)" build-dir source-dir))))

(defun cmake-project--set-memo-architecture (source-dir arch)
  (let* ((proper-source-dir (expand-file-name source-dir))
         (memo (gethash proper-source-dir cmake-project-memoize-sources)))
    (if memo
        (progn
          (setq arch (string-trim arch))
          (when (string= "" arch)
            (setq arch nil))
          (aset memo 2 arch))
      (error "cmake-project--set-memo-architecture %s: No source directory? (%s)" arch source-dir))))

(defun cmake-project--set-memo-toolset (source-dir toolset)
  "'cmake-project-memo' setter function for the toolset
configuration argument. If TOOLSET is an empty string, then 'nil'
is stored."
  (let* ((proper-source-dir (expand-file-name source-dir))
         (memo (gethash proper-source-dir cmake-project-memoize-sources)))
    (if memo
        (progn
          (setq toolset (string-trim toolset))
          (when (string= "" toolset)
            (setq toolset nil))
          (aset memo 3 toolset))
      (error "cmake-project--set-memo-toolset %s: No source directory? (%s)" toolset source-dir))))

(defun cmake-project--set-memo-configure-args (source-dir flags)
  "'cmake-project-memo' setter function for the additional
configure flags argument. If FLAGS is an empty string, then 'nil'
is stored."
  (let* ((proper-source-dir (expand-file-name source-dir))
         (memo (gethash proper-source-dir cmake-project-memoize-sources)))
    (if memo
        (progn
          (setq flags (string-trim flags))
          (when (string= "" flags)
            (setq flags nil))
          (aset memo 4 flags))
      (error "cmake-project--set-memo-configure-args %s: No source directory? (%s)" flags source-dir))))

(defun cmake-project--set-memo-build-config (source-dir config)
  "'cmake-project-memo' setter function for the build configuration
argument. If CONFIG is an empty string, then 'nil' is stored."
  (let* ((proper-source-dir (expand-file-name source-dir))
         (memo (gethash proper-source-dir cmake-project-memoize-sources)))
    (if memo
        (progn
          (setq config (string-trim config))
          (when (string= "" config)
            (setq config nil))
          (aset memo 5 config))
      (error "cmake-project--set-memo-build-config %s: No source directory? (%s)" config source-dir))))

(defun cmake-project--memoize-source-directory (source-dir build-dir)
  "Associate a new 'cmake-project-memo' with a source directory."
    (puthash (expand-file-name source-dir)
             (cmake-project--make-memo build-dir)
             cmake-project-memoize-sources))

(defun cmake-project--get-source-directory-memo (source-dir)
  (gethash (expand-file-name source-dir) cmake-project-memoize-sources))

(defun cmake-project-find-source-root-directory ()
  "Find the top-level CMake directory. The candidate source
directories to search for the top-level CMakeLists.txt:

(a) The buffer-local 'cmake-project-src-rootdir'
(b) The current project's root (if project.el is loaded)
(c) The directory component of the buffer's file name,
(d) The current dired directory name."

  (or (cmake-project--valid-src-rootdir)
      (cl-flet ((parent-directory (dir)
                  (let ((parent (file-name-directory (directory-file-name dir))))
                    (unless (equal parent dir) parent))))
        (let ((start-dir (or (and (fboundp 'project-current)
                                  (fboundp 'project-root)
                                  (project-root (project-current)))
                             ;; buffer-file-name can be nil! (e.g., being inside a dired
                             ;; buffer.)
                             (let ((buf-file (buffer-file-name)))
                               (and buf-file (file-name-directory buf-file)))
                             (and (eq 'dired-mode major-mode) dired-directory))))
          (or
           ;; Did we already memoize this directory tree?
           (let ((current-dir (expand-file-name start-dir))
                 (found-it nil))
             (while (and (not found-it) current-dir)
               (setq found-it (gethash current-dir cmake-project-memoize-sources))
               (unless found-it
                 (setq current-dir (parent-directory current-dir))))
             current-dir)
           ;; Nope.
           (let (topmost)
             (locate-dominating-file start-dir
                                     (lambda (cand)
                                       (when (file-exists-p (concat cand "CMakeLists.txt"))
                                         (setq topmost cand))))
             topmost))))))


(defun cmake-project--changed-build-directory (new-build-directory build-config)
  (when (eq cmake-project-src-rootdir cmake-project-invalid-src-rootdir)
    (error "cmake-project--changed-build-directory: Source root directory not set."))
  (when (or (not new-build-directory) (string= "" new-build-directory))
    (error "cmake-project--changed-build-directory: Build directory was not set or empty."))
  (cmake-project--set-memo-build-directory cmake-project-src-rootdir new-build-directory)
  (setq-local compile-command (cmake-project-current-build-command new-build-directory build-config))
  ;; Rerun flymake if the mode is enabled
  (when (local-variable-p 'flymake-mode (current-buffer))
    (flymake-mode-off)
    (flymake-mode-on)))

(defun cmake-project-current-build-command (build-directory build-config)
  "Generate the Command line to compile current project in the
build directory BUILD-DIRECTORY."
  (let ((build-dir (shell-quote-argument (expand-file-name build-directory))))
    ;; Quasiquoted list:
    (string-join `(,cmake-project-cmake-command "--build" ,build-dir
                                                "--config" ,build-config)
                 " ")))

;; Build command directory extraction regexp.  Might be useful some day:
;; "cmake\\s-+--build\\s-+\\(?:\"\\([^\"]*\\)\\\"\\|\\(\\S-*\\)\\)"

(defun cmake-project-flymake-init ()
  (when (eq cmake-project-src-rootdir cmake-project-invalid-src-rootdir)
    (error "cmake-project-flymake-init: Source root directory not set."))
  (let* ((memo (or (cmake-project--get-source-directory-memo cmake-project-src-rootdir)
                   (error "cmake-project-flymake-init: Source root directory not found.")))
         (build-dir (shell-quote-argument (expand-file-name (cmake-project--memo-build-dir memo)))))
    ;; Quasiquote list:
    `(,cmake-project-cmake-command "--build" ,build-dir)))

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
code (2 for `make`, 1 for Visual Studio) to a success code (0) to
prevent a fatal Flymake shutdown."
  (if (eq (ad-get-arg 0) 2) (ad-set-arg 0 0)) ; make
  (if (eq (ad-get-arg 0) 1) (ad-set-arg 0 0)) ; Visual Studio
  )

(defun cmake-project--split-directory-path (path)
  (let ((dir-agnostic-path (directory-file-name path)))
    (cons
     (file-name-directory dir-agnostic-path)
     (file-name-as-directory (file-name-nondirectory dir-agnostic-path)))))


(defun cmake-project--get-cmake-capability ()
  "Retrieve the capabilities for the current 'cmake-project-cmake-command'. Run
the command and update 'cmake-project-memoize-capabilities' if not
found."
  (or (gethash cmake-project-cmake-command cmake-project-memoize-capabilities)
      (let* ((help-text (shell-command-to-string (concat cmake-project-cmake-command " --help")))
             ;; has-s-flag: t -> cmake takes a source directory arg
             (has-s-flag (not (eq (string-match "\\s-+-S\\s-?<.* = .*" help-text) nil)))
             ;; has-b-flag: t -> cmake takes a binary directory arg
             (has-b-flag (not (eq (string-match "\\s-+-B\\s-?<.*= .*" help-text) nil)))
             ;; has-a-flag: t -> cmake supports "-A" architecture flag
             (has-a-flag (not (eq (string-match "\\s-+-A\\s-?<.*= .*" help-text) nil)))
             ;; has-t-flag: t -> cmake supports "-T" toolchain flag
             (has-t-flag (not (eq (string-match "\\s-+-A\\s-?<.*= .*" help-text) nil)))
             (regexp (concat
                      "The following generators are available on this platform\\( (\\* marks default)\\)?:\n"
                      "\\([^\\']*\\)\\'"))
             (arch-suffix " [arch]")
             (arch-suffix-len (- (length arch-suffix)))
             generators
             arch-suffixed-generators)
        (string-match regexp help-text)
        (let ((gens-chunk (match-string 2 help-text)))
          (while (string-match
                  "\\s-+\\([^=\n]+?\\)\\s-*=[^\n]+?\n\\([^\\']*\\)\\'" gens-chunk)
            (let ((generator (match-string 1 gens-chunk)))
              (when (string-suffix-p arch-suffix generator)
                ;; strip the architecture suffix
                (setq generator (substring generator 0 arch-suffix-len))
                (add-to-list 'arch-suffixed-generators generator t))
              (setq generators (add-to-list 'generators generator 1)))
            (setq gens-chunk (match-string 2 gens-chunk))))
        (puthash cmake-project-cmake-command
                 (cmake-project--make-capability has-s-flag
                                                 has-b-flag
                                                 has-a-flag
                                                 has-t-flag
                                                 generators
                                                 arch-suffixed-generators)
                 cmake-project-memoize-capabilities))))

(defun cmake-project--available-generators ()
  (cmake-project--capability-generators (cmake-project--get-cmake-capability)))


(defun cmake-project-source-directory (new-source-dir)
  "Set/update the source directory for a file's buffer, propagating
the update to buffers matching the original source directory. The
new source directory MUST EXIST."
  (interactive
   (let* ((cand-new-dir (read-directory-name "New CMake top-level source directory: "
                                             cmake-project-src-rootdir nil t)))
     (list cand-new-dir)))

  (when-let ((original-source-dir (expand-file-name cmake-project-src-rootdir))
             (expanded-new-source-dir (expand-file-name new-source-dir))
             (memo (let ((elt (gethash original-source-dir cmake-project-memoize-sources)))
                     (unless elt
                       (error "cmake-project-source-directory: unknown original dir: %s"
                              original-source-dir))
                     elt))
             ;; A bit cheesy: mapcar across the buffer list, updating the source directory
             ;; and returning 1 for each updated buffer, otherwise 0. Then sum the resulting
             ;; list.
             (total-affected
              (apply '+ (mapcar (lambda (buf)
                                  (with-current-buffer buf
                                    (if (and cmake-project-mode
                                             (cmake-project--valid-src-rootdir)
                                             (string= original-source-dir
                                                      cmake-project-src-rootdir))
                                        (progn
                                          (setq-local cmake-project-src-rootdir expanded-new-source-dir)
                                          1)
                                      0)))
                                (buffer-list)))))

    ;; Re-memoize if more than one buffer affected:
    (when (>= total-affected 1)
      (remhash original-source-dir          cmake-project-memoize-sources)
      (puthash expanded-new-source-dir memo cmake-project-memoize-sources)
      (message "cmake-project updated %d buffer%s"
               total-affected (if (> total-affected 1) "s" "")))))


(defun cmake-project-set-build-directory (new-build-dir)
  (interactive
   (let* ((source-dir (or (cmake-project--valid-src-rootdir)
                          (error "cmake-project-set-build-directory: Source root not set.")))
          (memo (cmake-project--get-source-directory-memo source-dir))
          (old-build-dir (or (and memo (cmake-project--memo-build-dir memo))
                             source-dir))
          (new-build-dir (if (cmake-project--valid-src-rootdir)
                             (read-directory-name "CMake build directory: " old-build-dir nil t nil)
                           (error "cmake-project-set-architecture: Source root not set."))))
     (list new-build-dir)))
  (let* ((memo (cmake-project--get-source-directory-memo cmake-project-src-rootdir))
         (config (cmake-project--memo-build-config memo))
         (new-compile-cmd (cmake-project-current-build-command new-build-dir config)))
    (cmake-project--changed-build-directory new-build-dir config)
    (let ((total-affected
           (apply '+ (mapcar (lambda (buf)
                               (with-current-buffer buf
                                 (if cmake-project-mode
                                     (progn
                                       (setq compile-command new-compile-cmd)
                                       1)
                                   0)))
                             (buffer-list)))))
      (message "cmake-project-set-build-directory: Updated %d buffer%s"
               total-affected (if (> total-affected 1) "s" "")))))

    
(defun cmake-project-build-directory ()
  "Return the project's build directory"
  (let* ((source-dir (or (cmake-project--valid-src-rootdir)
                         (error "cmake-project-set-build-directory: Source root not set.")))
         (memo (cmake-project--get-source-directory-memo source-dir)))
    (cmake-project--memo-build-dir memo)))

(defun cmake-project-set-architecture (arch)
  "Set the project's architecture (\"-A\") configuration argument."
  (interactive
   (let ((new-arch (if (cmake-project--valid-src-rootdir)
                       (read-string "CMake architecture argument: " "" t nil)
                     (error "cmake-project-set-architecture: Source root not set."))))
     (list new-arch)))
  (cmake-project--set-memo-architecture cmake-project-src-rootdir arch))


(defun cmake-project-set-toolset (toolset)
  "Set the project's toolset (\"-T\") configuration argument."
  (interactive
   (let ((new-toolset (if (cmake-project--valid-src-rootdir)
                          (read-string "CMake toolset argument: " "" t nil)
                        (error "cmake-project-set-toolset: Source root not set."))))
     (list new-toolset)))
  (cmake-project--set-memo-toolset cmake-project-src-rootdir toolset))

(defun cmake-project-set-configure-args (flags)
  "Set the CMake project's additional configuration arguments,
e.g., \"-D\" options."
  (interactive
   (let ((new-flags (if (cmake-project--valid-src-rootdir)
                        (read-string "CMake additional configuration flags: " "" t nil)
                     (error "cmake-project-set-configure-args: Source root not set."))))
     (list new-flags)))
  (cmake-project--set-memo-configure-args cmake-project-src-rootdir flags))

(defun cmake-project-set-build-config (config)
  "Set the CMake project's build configuration (Release, Debug, ...)"
  (interactive
   (let* ((source-dir (or (cmake-project--valid-src-rootdir)
                          (error "cmake-project-set-build-config: Source root not set.")))
          (memo (cmake-project--get-source-directory-memo source-dir))
          (old-config (or (and memo (cmake-project--memo-build-config memo))
                          cmake-project-build-config)))
     (list (read-string "CMake build configuration: " old-config t nil))))
  (let* ((memo (cmake-project--get-source-directory-memo cmake-project-src-rootdir))
         (build-dir (cmake-project--memo-build-dir memo))
         (compile-cmd (cmake-project-current-build-command build-dir config)))
    (cmake-project--set-memo-build-config cmake-project-src-rootdir config)
    (let ((total-affected
           (apply '+ (mapcar (lambda (buf)
                               (with-current-buffer buf
                                 (if cmake-project-mode
                                     (progn
                                       (setq compile-command compile-cmd)
                                       1)
                                   0)))
                             (buffer-list)))))
      (message "cmake-project-set-build-config: Updated %d buffer%s"
               total-affected (if (> total-affected 1) "s" "")))))
    
(defun cmake-project-configure-project (build-directory generator &optional flags)
  "Configure a CMake build tree.  BUILD-DIRECTORY is the path to
the build-tree directory.  If the directory does not already
exist, it will be created.  The source directory is found
automatically based on the current buffer. With a prefix argument
additional CMake flags can be specified interactively."
  (interactive
   (let* ((source-directory (cmake-project-find-source-root-directory))
          (memo (cmake-project--get-source-directory-memo source-directory))
          (def-build-dir (or (and memo (cmake-project--memo-build-dir memo))
                             source-directory))
          (directory-parts (cmake-project--split-directory-path def-build-dir))
          (root (car directory-parts))
          (directory-name (cdr directory-parts))
          (cmake-generators (cmake-project--available-generators)))
     (list (read-directory-name "CMake build directory: "
                                root nil nil directory-name)
           (completing-read "Generator: "
                            cmake-generators nil t (car cmake-generators))
           (if current-prefix-arg
               (read-from-minibuffer "Additional CMake flags (optional): ")))))
  (let* ((source-directory (cmake-project-find-source-root-directory))

         ;; Must force `default-directory' here as `compilation-start' has
         ;; a bug in it. It is supposed to notice the `cd` command and
         ;; adjust `default-directory' accordingly but it gets confused by
         ;; spaces in the directory path, even when properly quoted.
         (default-directory build-directory)

         (cmake-capabilities (cmake-project--get-cmake-capability))
         (has-s-flag (cmake-project--capability-s-flag cmake-capabilities))
         (has-b-flag (cmake-project--capability-b-flag cmake-capabilities))
         (has-a-flag (cmake-project--capability-a-flag cmake-capabilities))
         (has-t-flag (cmake-project--capability-t-flag cmake-capabilities))
         (arch-suffix-generators (cmake-project--capability-arch-suffix-generators cmake-capabilities))
         (has-arch-suffix (member generator arch-suffix-generators))
         (quoted-source-directory (shell-quote-argument (expand-file-name source-directory)))
         (quoted-build-directory (shell-quote-argument (expand-file-name build-directory)))

         (memo (cmake-project--get-source-directory-memo source-directory))
         (project-arch (cmake-project--memo-architecture memo))
         (project-toolset (cmake-project--memo-toolset memo))
         (config-args (cmake-project--memo-configure-args memo))
         (build-config (cmake-project--memo-build-config memo))
         
         ;; HACK: force compilation-start to cd to default-directory
         ;; by inserting dummy cd at front.  Without this, the old
         ;; broken version may pick up quoted path without spaces and
         ;; then assume the quotes are part of the path causing an
         ;; error (see https://github.com/alamaison/emacs-cmake-project/issues/1)
         ;;
         ;; Backtick/backquote macro constructs the command list (aka "quasiquoting")
         ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
         ;;
         ;; Also deletes empty strings (flags -- I'm looking at you!) from the result.
         (cmake-configure-cmd (delq ""
                                    `(,@(unless (or has-s-flag has-b-flag)
                                          `("cd ." "&&" "cd" ,quoted-build-directory "&&"))
                                        ,cmake-project-cmake-command
                                        ,(or flags "")
                                        "-G" ,(string-join 
                                               `(,(shell-quote-argument generator)
                                                  ,(and (not has-a-flag)
                                                        has-arch-suffix
                                                        project-arch)))
                                        ,@(when (and has-a-flag project-arch)
                                            `("-A" ,project-arch))
                                        ,(when build-config
                                           (concat "-DCMAKE_BUILD_TYPE=" build-config))
                                        ,@(when (and has-t-flag project-toolset)
                                            `("-T" ,project-toolset))
                                        ,(or config-args "")
                                        ,@(when has-s-flag
                                            `("-S" ,quoted-source-directory))
                                        ,@(when has-b-flag
                                            `("-B" ,quoted-build-directory))
                                        ,@(unless (or has-s-flag has-b-flag) `(,quoted-source-directory))))))
    ;; Create the build directory
    (setq build-directory (expand-file-name (file-name-as-directory build-directory)))
    (unless (file-exists-p build-directory)
      (make-directory build-directory)
      (message "Created build directory %s" build-directory))
    ;; Update the buffer's local variables
    (setq-local compile-command (cmake-project-current-build-command build-directory build-config))
    ;; Update the memoized data:
    (cmake-project--set-memo-build-directory source-directory build-directory)
    (compilation-start (string-join cmake-configure-cmd " "))
    (cmake-project--changed-build-directory build-directory build-config)))

(defun cmake-project-reconfigure ()
  "Reconfigure a CMake project in the build directory. Removes
the build directory's CMakeCache.txt cache file and CMakeFiles
subdirectory, interactively calls cmake-project-configure-project."
  (interactive
   (let* ((memo (or (cmake-project--get-source-directory-memo cmake-project-src-rootdir)
                    (error "cmake-project-reconfigure: CMake source root directory not set.")))
          (build-dir (cmake-project--memo-build-dir memo))
          (cmake-cache-file  (file-name-concat build-dir "CMakeCache.txt"))
          (cmake-makefiles-d (file-name-concat build-dir "CMakeFiles")))
     (when (file-exists-p cmake-cache-file)  (delete-file cmake-cache-file))
     (when (file-exists-p cmake-makefiles-d) (delete-directory cmake-makefiles-d t))
     (call-interactively #'cmake-project-configure-project))))

;;;###autoload
(define-minor-mode cmake-project-mode
    "Minor mode that integrates a CMake-based project with Emacs
build tools such as the CompileCommand and Flymake."
  :lighter " CMakeProject"

  (cond
    ;; Enabling mode
    (cmake-project-mode
     (cl-flet ((prompt-src-root (cand-src-root)
                 (let ((pretty-cand-src-root (abbreviate-file-name cand-src-root)))
                   (call-interactively (lambda (user-src-root)
                                         (interactive (list (read-directory-name "CMake top-level source directory: "
                                                                                 pretty-cand-src-root nil t nil)))
                                         user-src-root))))
               (prompt-build-dir (src-root-dir)
                 (let ((pretty-src-root-dir (abbreviate-file-name src-root-dir)))
                   (call-interactively (lambda (cand-build-dir)
                                         (interactive (list (read-directory-name "CMake build directory: "
                                                                                 pretty-src-root-dir nil t
                                                                                 cmake-project-default-build-dir-name)))
                                         cand-build-dir)))))
       ;; If we've memoized the source directory, pull settings from the
       ;; memo. If we've never memoized this file's CMake source directory,
       ;; prompt for it based on our best guess from
       ;; cmake-project-find-source-root-directory.
       (let* ((source-dir (let* ((cand-src-root (cmake-project-find-source-root-directory))
                                 (maybe-memo (cmake-project--get-source-directory-memo cand-src-root)))
                            (expand-file-name (or (and maybe-memo cand-src-root)
                                                  (prompt-src-root cand-src-root)))))
              (memo (cmake-project--get-source-directory-memo source-dir))
              (build-dir  (or (and memo (cmake-project--memo-build-dir memo))
                              ;; Grab the outer binding for cmake-project-build-directory,
                              ;; if the user provided it.
                              (cmake-project--valid-src-rootdir)
                              ;; before we use the default directory
                              (prompt-build-dir source-dir)))
              (build-config (or (and memo (cmake-project--memo-build-config memo))
                                cmake-project-build-config)))
         (setq-local
          cmake-project-src-rootdir source-dir
          compile-command          (cmake-project-current-build-command build-dir build-config))
         ;; Add memoized source, if needed
         (unless memo
           (cmake-project--memoize-source-directory source-dir build-dir)
           (setq memo (cmake-project--get-source-directory-memo source-dir)))))

     (ad-enable-advice
      'flymake-get-file-name-mode-and-masks 'around 'cmake-flymake-advice)
     (ad-enable-advice
      'flymake-post-syntax-check 'before 'cmake-flymake-post-syntax-check)
     (ad-activate 'flymake-get-file-name-mode-and-masks))

    ;; Disabling mode
    (t
     (kill-local-variable 'compile-command)
     (kill-local-variable 'cmake-project-src-rootdir)

     (ad-disable-advice
      'flymake-post-syntax-check 'before 'cmake-flymake-post-syntax-check)
     (ad-disable-advice
      'flymake-get-file-name-mode-and-masks 'around 'cmake-flymake-advice)
     (ad-activate 'flymake-get-file-name-mode-and-masks))))

(provide 'cmake-project)

;;; cmake-project.el ends here
