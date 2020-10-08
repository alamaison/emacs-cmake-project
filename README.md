Emacs CMake project mode
==========

A minor-mode integrating the CMake build process with the Emacs ecosystem.

Why?
----------

Emacs build tools for C/C++ generally assume a Makefile exists for the
project but this isn't necessarily the case for projects managed by
CMake which can generate many different types of project file.
Integrates the CMake build process with the existing Emacs tools.

This mode is *not* for editing the `CMakeLists.txt` themselves; that
is left to the major-mode [`cmake-mode`][1].

Installing
----------

Copy this file somewhere on your Emacs load path or use [Marmalade][2]
and the package manager to install it.

Either auto-load the mode in your init file by adding the line:

    (autoload 'cmake-project-mode "cmake-project" nil t)

Or simply require the library:

    (require 'cmake-project)

Then use `M-x cmake-project-mode` in any buffer you wish to use the
mode in.  Alternatively, start the mode automatically for any C/C++
file whose directory includes a `CMakeLists.txt`:

    (defun maybe-cmake-project-mode ()
      (if (or (file-exists-p "CMakeLists.txt")
              (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
          (cmake-project-mode)))

    (add-hook 'c-mode-hook 'maybe-cmake-project-mode)
    (add-hook 'c++-mode-hook 'maybe-cmake-project-mode)

Features
----------

# Configuring build tree #

Use `M-x cmake-project-configure-project` to configure or reconfigure
a CMake build tree.  The function finds the source automatically based
on the current buffer.

# The compile command #

This mode makes the [compile command][3], `M-x compile`, build the
project by default via CMake in a `bin` subdirectory of the project
source root directory.

# Flymake #

This mode integrates with [Flymake][4] so that, when `flymake-mode` is
enabled, the entire project is built whenever the buffer is saved and
any errors are higlighted in the buffer.  This is different from
Flymake's typical behaviour which builds only the file for the buffer
in question.  CMake doesn't provide a way to build one file at a time
(or at least we don't know of a way) so we must build everything.

# Changing the architecture of the build in Windows
By default cmake builds for 32bit architecture on windows.  If you are
on windows and you want to set the default architecture of your builds
to something else that can be done like this

`(setq cmake-project-architecture "Win64") ;; Sets the architecture to 64bit`

`(setq cmake-project-architecture "ARM") ;; Sets the architecture for arm`

# Other Customizations #

You can also set the build directory to other directories like so
`(setq cmake-project-default-build-dir-name "build\/")`


[1]: http://www.cmake.org/CMakeDocs/cmake-mode.el
[2]: http://marmalade-repo.org/
[3]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation.html
[4]: http://www.gnu.org/software/emacs/manual/html_node/flymake/index.html
