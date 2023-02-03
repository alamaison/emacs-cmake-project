# Emacs CMake project mode

A minor-mode integrating the CMake build process with the Emacs ecosystem.

## Why?

There exists an entire ecosystem of projects that use [CMake][2] --
_CMake_ provides better cross-platform build support, in addition to
supporting a wider range of build tools, e.g.,
[Ninja][5]. Unfortunately, Emacs' `compile` command defaults to the
`make` command as the build tool-of-choice and doesn't know anything
about _CMake_. This minor-mode remedies that problem.

This mode is *not* for editing the `CMakeLists.txt` themselves. That
is the major-mode [`cmake-mode`][1]'s function.

## Installing

Copy this file somewhere on your Emacs `load-path` or use Emacs' package
manager `package-list-packages` to install it.

Either auto-load the mode in your Emacs init file (`~/.emacs` or
`~/.emacs.d/init.el`) by adding the line:

  ```elisp
  (autoload 'cmake-project-mode "cmake-project" nil t)
  ```

Or simply require the library:

  ```elisp
  (require 'cmake-project)
  ```
    
## Enabling `cmake-project` minor mode

Use `M-x cmake-project-mode` in any buffer where you wish to use the
mode. This applies to files and `dired` directories.

When you enable `cmake-project-mode`, `cmake-project-mode` locates the
topmost (project-level) `CMakeLists.txt` relative to the visited
file's directory, which may exist in a directory above the file's
directory. If the project-level `CMakeLists.txt` directory was not
previously encountered, `cmake-project-mode` will prompt you for:

  - _CMake top-level source directory:_ The directory where
    `cmake-project-mode` located the project-level
    `CMakeLists.txt`. If `cmake-project-mode` guessed incorrectly, you
    can correct it.
    
    `cmake-project-mode` starts its _CMake_ project-level search in
    one of the following directories:
    
      1. The `(project-root (project-current))` directory, if
         `project.el` is loaded.
      2. The visited file's directory, if the buffer file name is not
         `nil` (the buffer file name can be `nil` if you're using
         `Dired`.)
      3. The `Dired` directory, if you're in a `Dired` buffer.
    
  - _CMake build directory:_ The build directory where _CMake_ will
    output generated build and build output artifacts (executables,
    libraries, ...) This defaults to a subdirectory named
    `cmake-build` under the top-level source directory. Update the
    directory path at the prompt as needed.
    
`cmake-project-mode` remembers (memo-izes) the _CMake_ project-level
source directory so that when you enable the minor mode for files
under the _CMake_ project-level source directory, so it won't prompt
you again.

Alternatively, start the mode automatically for any C/C++ file whose
directory includes a `CMakeLists.txt`:

  ```elisp
  (defun maybe-cmake-project-mode ()
    (if (or (file-exists-p "CMakeLists.txt")
            (file-exists-p (expand-file-name "CMakeLists.txt" (project-root (project-current)))))
      (cmake-project-mode)))

  (add-hook 'c-mode-hook 'maybe-cmake-project-mode)
  (add-hook 'c++-mode-hook 'maybe-cmake-project-mode)
  ;; If you have the cmake-mode package installed:
  (add-hook 'cmake-mode-hook 'maybe-cmake-project-mode)
  ```

## Using `cmake-project-mode` effectively

1. `M-x cmake-project-source-directory`: Changes the _CMake_
   project-level source directory and updates all affected file
   buffers under the previous _CMake_ project level source directory.
   
   NOTE: You will probably never have to use this function.

2. Tweaking the _CMake_ configuration command line arguments.
   
   `cmake-project-mode` will remember these customizations; they are
   memo-ized with the _CMake_ project-level source directory.

   - `M-x cmake-project-set-architecture`: Sets the _"-A"_
     architecture configuration argument. Useful if you need to
     compile for _x86_ on a Windows platform or for _armhf_ to target
     32-bit ARM platforms on Linux or macOS.
     
   - `M-x cmake-project-set-toolset`: Sets the _"-T"_ toolset
     configuration argument. Useful on macOS if you need to target a
     previous OSX release's API or a previous Windows API (e.g.,
     _v141_xp_).
     
   - `M-x cmake-project-set-configure-args`: Set additional
     configuration arguments, such as _"-D"_
     options. `CMAKE_BUILD_TYPE` is automatically added to the _CMake_
     configuration command line, so you don't need to include it.
     
- `M-x cmake-project-set-build-config`: Sets the build
  configuration.
  
  - Single-configuration tools (e.g. `make` and `ninja`):
    `CMAKE_BUILD_TYPE` is passed on the _CMake_ configuration command
    line.
  
  - Multi-configuration tools (e.g., `msbuild` and `xcodebuild`):
  `--config <config>` is passed on the _CMake_ build command line.

- `M-x cmake-project-configure-project`: Configure your _CMake_
  project. You will be prompted to confirm the build directory and the
  _CMake_ build tool generator.
  
- `M-x cmake-project-reconfigure-project`: Reconfigure your _CMake_
  project. This will remove your build directory's `CMakeCache.txt`
  file and `CMakeFiles` subdirectory, and then executes
  `cmake-project-configure-project`.
  
- `M-x compile`: Compile your project using [compile command][3]. The
  `compile-command` variable is set to _CMake_ build command line.
  
- `cmake-project-build-directory`: Returns the _CMake_ build directory
  associated with the file buffer. This is useful if you want to
  programmatically change `cmake-project-mode` settings:

    ```elisp
    (defun maybe-cmake-project-mode ()
      (if (or (file-exists-p "CMakeLists.txt")
              (file-exists-p (expand-file-name "CMakeLists.txt" (project-root (project-current)))))
          ;; Enable the mode first:
          (cmake-project-mode)
          ;; Then apply tweaks:
          (let ((build-dir (directory-file-name (cmake-project-build-directory))))
            ;; The build directory's suffix determines additional configure
            ;; tweaking:
            (cond ((string-suffix-p "arm64" build-dir)
                   (progn
                     (cmake-project-set-architecture "aarch64")
                     (cmake-project-set-configure-args "-DHAVE_AARCH64=On -DHAVE_FEATURE=Off"))
                   (string-suffix-p "x86" build-dir)
                   (progn
                     (cmake-project-set-architecture "x86")
                     (cmake-project-set-toolkit "v141_xp")))))))

    (add-hook 'c-mode-hook 'maybe-cmake-project-mode)
    (add-hook 'c++-mode-hook 'maybe-cmake-project-mode)
    ;; If you have the cmake-mode package installed:
    (add-hook 'cmake-mode-hook 'maybe-cmake-project-mode)
    ```

- `cmake-project-cmake-command`: The _CMake_ command executed to configure and
  compile. If you need to change it:
  
    ```elisp
    (setq cmake-project-cmake-command "cmake-3.14")
    ```
      
- `cmake-project-build-config`: The default _CMake_ build
  configuration. Its initial value it "Debug". If you need to change
  this default, preferably in your Emacs startup file (`~/.emacs` or
  `~/.emacs.d/init.el`):
  
    ```elisp
    (setq cmake-project-build-config "MinSizeRel")
    ```

## Features

### Flymake

This mode integrates with [Flymake][4] so that, when `flymake-mode` is
enabled, the entire project is built whenever the buffer is saved and
any errors are higlighted in the buffer.  This is different from
Flymake's typical behaviour which builds only the file for the buffer
in question.  CMake doesn't provide a way to build one file at a time
(or at least we don't know of a way) so we must build everything.


[1]: https://www.cmake.org/CMakeDocs/cmake-mode.el
[2]: https://cmake.org/
[3]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation.html
[4]: http://www.gnu.org/software/emacs/manual/html_node/flymake/index.html
[5]: https://ninja-build.org/
