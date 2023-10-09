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
  (interactive
   (let ((name (completing-read "Tool: " (cons " " (asdf--plugin-list-list)))))
     (if (not (string-blank-p name))
         (list name (completing-read "Version: " (cons " " (asdf--list-all-list name))))
       (list name))))
  (compile
   (substitute-env-vars
    (string-join
     (cl-remove-if 'null `(,asdf-binary "install" ,name ,version)) " "))
   'asdf-compilation-mode))

(defun asdf-current ()
  "Get current versions being used in path."
  (interactive)
  (shell-command (asdf--command "current")))

(defun asdf-plugin-list()
  "Get currently installed plugin list."
  (interactive)
  (shell-command (asdf--command "plugin" "list")))

(defun asdf-plugin-add(name &optional git-url)
  "Add a new plugin by NAME.
Optionally supply a GIT-URL for git repository to a plugin."
  (interactive
   (let ((input (split-string
           (completing-read "Plugin: " (cons " " (asdf--plugin-list-all-list)))
          " " t " ")))
     (if (not (string-blank-p (car input)))
         (list (car input) (read-string "Git URL: " (cadr input)))
       (list (car input)))))
  (compile
   (substitute-env-vars
    (string-join
     (cl-remove-if 'null `(,asdf-binary "plugin" ,"add" ,name ,git-url)) " "))
   'asdf-compilation-mode))

(defun asdf-plugin-update-all()
  "Update every plugin."
  (interactive)
  (shell-command (asdf--command "plugin update --all")))

(defun asdf--plugin-list-list()
  "Get currently installed plugin list as usable strings."
  (asdf--format-output-to-list
    (shell-command-to-string (asdf--command "plugin" "list"))))

(defun asdf--plugin-list-all-list()
  "Get currently installed plugin list as usable strings."
  (asdf--format-output-to-list
    (shell-command-to-string (asdf--command "plugin" "list" "all"))))

(defun asdf--list-all-list(tool)
  "Get list all versions for specific TOOL."
  (asdf--format-output-to-list
   (shell-command-to-string (asdf--command "list" "all" tool))))

(defun asdf--format-output-to-list (shell-output)
  "Take SHELL-OUTPUT and format it into a usable list to select from."
  (split-string
   (replace-regexp-in-string
    (rx (* (any " \t\n")) eos)
    ""
    shell-output) "\n"))

(defun asdf--command(&rest args)
  "Construct command using ARGS and binary for execution."
  (substitute-env-vars
   (string-join (cons asdf-binary args) " ")))

(defun asdf-enable ()
  "Setup asdf for environment."
  (interactive)
  (let ((shims-path (substitute-env-vars (concat asdf-path "/shims")))
        (bin-path (directory-file-name (file-name-directory (substitute-env-vars asdf-binary)))))
    (setenv "PATH" (concat shims-path ":" bin-path ":" (getenv "PATH")))
    (setq exec-path (nconc (list shims-path bin-path) exec-path))))

(provide 'asdf)

;;; asdf.el ends here
