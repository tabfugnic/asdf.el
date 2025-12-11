;;; asdf.el --- Emacs interface for asdf version manager.

;; Author: Eric J. Collins <eric@tabfugni.cc>
;; Version: 0.1.0
;; Keywords: version manager, asdf
;; URL: https://github.com/tabfugnic/asdf.el/asdf.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; 'asdf.el' is an Emacs integration for asdf version manager.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'subr-x)
(require 'cl-seq)
(require 'compile)

;; Customizable variables
(defgroup asdf nil
  "Settings to interact with asdf."
  :version "0.1.0"
  :group 'applications)

(defcustom asdf-path "$HOME/.asdf"
  "Path to asdf directory."
  :type 'string
  :group 'asdf)

(defcustom asdf-binary "$HOME/.asdf/bin/asdf"
  "Path to asdf binary."
  :type 'string
  :group 'asdf)

(define-compilation-mode
 asdf-compilation-mode
 "asdf compilation"
 "Compilation output for asdf.")

(defun asdf-install (&optional name version)
  "Install tools.
Run this command with no arguments and it will install all tools
based on the .tools-version if available.  Optionally pass NAME
to specify the tool being installed.  Optionally specify
VERSION."
  (interactive (let* ((name
                       (car
                        (string-split
                         (asdf--ask-for-input-from-list
                          "Tool" (asdf--plugin-list-all-list))
                         " ")))
                      (version
                       (unless (string-blank-p name)
                         (asdf--ask-for-input-from-list
                          (format name "Version")
                          (asdf--list-all-list name)))))
                 (list name version)))
  (asdf--compile-command "install" name version))

(defun asdf-current ()
  "Get current versions being used in path."
  (interactive)
  (shell-command (asdf--command "current")))

(defun asdf-plugin-list ()
  "Get currently installed plugin list."
  (interactive)
  (shell-command (asdf--command "plugin" "list")))

(defun asdf-plugin-add (name &optional git-url)
  "Add a new plugin by NAME.
Optionally supply a GIT-URL for git repository to a plugin."
  (interactive (let* ((input
                       (split-string
                        (asdf--ask-for-input-from-list
                         "Plugin" (asdf--plugin-list-all-list))
                        " "))
                      (git-url
                       (unless (string-blank-p (car input))
                         (read-string "Git URL: " (cadr input)))))
                 (list (car input) git-url)))
  (asdf--compile-command "plugin" "add" name git-url))

(defun asdf-plugin-remove (name)
  "Remove plugin by NAME."
  (interactive (let ((name
                      (asdf--ask-for-input-from-list
                       "Plugin" (asdf--plugin-list-list))))
                 (list name)))
  (shell-command (asdf--command "plugin" "remove" name)))

(defun asdf-plugin-update-all ()
  "Update every plugin."
  (interactive)
  (asdf--compile-command "plugin" "update" "--all"))

(defun asdf-plugin-update (name)
  "Update every plugin by NAME."
  (interactive (let ((name
                      (asdf--ask-for-input-from-list
                       "Plugin" (asdf--plugin-list-list))))
                 (list name)))
  (asdf--compile-command "plugin" "update" name))

(defun asdf-latest (name)
  "Get latest version of a package by NAME."
  (interactive (let ((name
                      (asdf--ask-for-input-from-list
                       "Package" (asdf--plugin-list-list))))
                 (list name)))
  (asdf--compile-command "latest" name))

(defun asdf-latest-all ()
  "Get latest version of every package by installed plugin."
  (interactive)
  (asdf-latest "--all"))

(defun asdf-where (name &optional version)
  "Display install path by NAME and optionally VERSION."
  (interactive (let ((name
                      (asdf--ask-for-input-from-list
                       "Package" (asdf--plugin-list-list))))
                 (list name)))
  (asdf--compile-command "where" name))

(defun asdf-which (program)
  "Run which on PROGRAMs installed by asdf."
  (interactive (read-string "Which: "))
  (shell-command (asdf--command "which" program)))

(defun asdf--plugin-list-list ()
  "Get currently installed plugin list as usable strings."
  (asdf--format-output-to-list
   (shell-command-to-string (asdf--command "plugin" "list"))))

(defun asdf--plugin-list-all-list ()
  "Get currently installed plugin list as usable strings."
  (asdf--format-output-to-list
   (shell-command-to-string (asdf--command "plugin" "list" "all"))))

(defun asdf--list-all-list (tool)
  "Get list all versions for specific TOOL."
  (asdf--format-output-to-list
   (shell-command-to-string (asdf--command "list" "all" tool))))

(defun asdf--format-output-to-list (shell-output)
  "Take SHELL-OUTPUT and format it into a usable list to select from."
  (split-string (replace-regexp-in-string
                 (rx (* (any " \t\n")) eos) "" shell-output)
                "\n"))

(defun asdf--ask-for-input-from-list (prompt items)
  "Define PROMPT and list of ITEMS to select from."
  (completing-read (concat prompt ": ") (cons " " items)))

(defun asdf--command (&rest args)
  "Construct command using ARGS and binary for execution."
  (substitute-env-vars
   (string-join (cl-remove-if #'null (append `(,asdf-binary) args))
                " ")))

(defun asdf--compile-command (&rest args)
  "Using ARGS compile the constructed command."
  (compile (apply #'asdf--command args) 'asdf-compilation-mode))

(defun asdf-enable ()
  "Setup asdf for environment."
  (interactive)
  (let ((shims-path (substitute-env-vars (concat asdf-path "/shims")))
        (bin-path
         (directory-file-name
          (file-name-directory (substitute-env-vars asdf-binary)))))
    (setenv "PATH"
            (concat shims-path ":" bin-path ":" (getenv "PATH")))
    (setq exec-path (nconc (list shims-path bin-path) exec-path))))

(provide 'asdf)

;;; asdf.el ends here
