;;; -*- lexical-binding: t; -*-
;;; asdf-test.el --- Test suite for asdf

;; Author: Eric J. Collins <eric@tabfugni.cc>
;; Keywords: version manager, asdf, test
;; URL: https://github.com/tabfugnic/asdf.el/asdf-test.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; simple unit test suite for the asdf

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

(load-file "./asdf.el")
(require 'asdf)
(require 'ert)

(ert-deftest asdf-enable-sets-environment-test ()
  (let ((asdf-path "/path-to-asdf-data")
        (asdf-binary "/path-to-asdf-bin/bin/asdf"))
    (asdf-enable)
    (should
     (string-prefix-p
      "/path-to-asdf-data/shims:/path-to-asdf-bin/bin:"
      (getenv "PATH")))
    (should (member "/path-to-asdf-data/shims" exec-path))
    (should (member "/path-to-asdf-bin/bin" exec-path))))

(ert-deftest asdf-current-test ()
  (let ((asdf-binary "./bin/fake-asdf"))
    (asdf-current)
    (with-current-buffer "*Shell Command Output*"
      (should (equal (buffer-string) "asdf current")))))

(ert-deftest asdf-install-no-arguments-test ()
  (let ((asdf-binary "./bin/fake-asdf"))
    (asdf-install)
    (with-current-buffer "*asdf-compilation*"
      (rename-buffer "*no args*")
      (should
       (string-match-p "/bin/fake-asdf install" (buffer-string))))))

(ert-deftest asdf-install-with-tool-test ()
  (let ((asdf-binary "./bin/fake-asdf"))
    (asdf-install "tool")
    (with-current-buffer "*asdf-compilation*"
      (rename-buffer "*tool*")
      (should
       (string-match-p
        "/bin/fake-asdf install tool" (buffer-string))))))

(ert-deftest asdf-install-with-tool-and-version-test ()
  (let ((asdf-binary "./bin/fake-asdf"))
    (asdf-install "tool" "1.0.0")
    (with-current-buffer "*asdf-compilation*"
      (rename-buffer "*tool and version*")
      (should
       (string-match-p
        "/bin/fake-asdf install tool 1.0.0" (buffer-string))))))

(ert-deftest asdf-install-interactive-no-args ()
  (let ((asdf-binary "./bin/fake-asdf")
        (prompt-responses '("tool-thing" "10.0")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection) (pop prompt-responses))))
      (call-interactively 'asdf-install)
      (with-current-buffer "*asdf-compilation*"
        (rename-buffer "*interactive tool and version*")
        (should
         (string-match-p
          "/bin/fake-asdf install tool-thing 10.0"
          (buffer-string)))))))

(ert-deftest asdf-install-interactive-no-input ()
  (let ((asdf-binary "./bin/fake-asdf")
        (prompt-responses '("")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection) (pop prompt-responses))))
      (call-interactively 'asdf-install)
      (with-current-buffer "*asdf-compilation*"
        (rename-buffer "*interactive no input")
        (should (string-match-p "asdf install" (buffer-string)))))))

(ert-deftest asdf-install-interactive-only-input-version ()
  (let ((asdf-binary "./bin/fake-asdf")
        (prompt-responses '("tool-thing" "")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection) (pop prompt-responses))))
      (call-interactively 'asdf-install)
      (with-current-buffer "*asdf-compilation*"
        (rename-buffer "*interactive tool*")
        (should
         (string-match-p
          "asdf install tool-thing" (buffer-string)))))))

(ert-deftest asdf-plugin-list-test ()
  (let ((asdf-binary "./bin/fake-asdf"))
    (asdf-plugin-list)
    (with-current-buffer "*Shell Command Output*"
      (should (string-match-p "asdf plugin list" (buffer-string))))))

(ert-deftest asdf-plugin-add-simple-test ()
  (let ((asdf-binary "./bin/fake-asdf"))
    (asdf-plugin-add "foo")
    (with-current-buffer "*asdf-compilation*"
      (rename-buffer "*plugin add simple*")
      (should
       (string-match-p "asdf plugin add foo" (buffer-string))))))

(ert-deftest asdf-plugin-add-git-url-test ()
  (let ((asdf-binary "./bin/fake-asdf"))
    (asdf-plugin-add "foo" "git@path/to/repo.git")
    (with-current-buffer "*asdf-compilation*"
      (rename-buffer "*plugin add git-url*")
      (should
       (string-match-p
        "asdf plugin add foo git@path/to/repo.git"
        (buffer-string))))))

(ert-deftest asdf-plugin-add-simple-interactive-test ()
  (let ((asdf-binary "./bin/fake-asdf")
        (prompt-responses '("plugin" "")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection) (pop prompt-responses)))
              ((symbol-function 'read-string)
               (lambda (prompt initial-value)
                 (pop prompt-responses))))
      (call-interactively 'asdf-plugin-add)
      (with-current-buffer "*asdf-compilation*"
        (rename-buffer "*interactive plugin add simple*")
        (should
         (string-match-p
          "asdf plugin add plugin" (buffer-string)))))))

(ert-deftest asdf-plugin-add-git-interactive-test ()
  (let ((asdf-binary "./bin/fake-asdf")
        (prompt-responses '("plugin" "path/to/git.git")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection) (pop prompt-responses)))
              ((symbol-function 'read-string)
               (lambda (prompt initial-value)
                 (pop prompt-responses))))
      (call-interactively 'asdf-plugin-add)
      (with-current-buffer "*asdf-compilation*"
        (rename-buffer "*interactive plugin add with git*")
        (should
         (string-match-p
          "asdf plugin add plugin path/to/git.git"
          (buffer-string)))))))

(ert-deftest asdf-plugin-update-all-test ()
  (let ((asdf-binary "./bin/fake-asdf"))
    (asdf-plugin-update-all)
    (with-current-buffer "*Shell Command Output*"
      (should
       (string-match-p "asdf plugin update --all" (buffer-string))))))

(ert-deftest asdf-plugin-remove-test ()
  (let ((asdf-binary "./bin/fake-asdf"))
    (asdf-plugin-remove "foo")
    (with-current-buffer "*Shell Command Output*"
      (should
       (string-match-p "asdf plugin remove foo" (buffer-string))))))
;;; asdf-test.el ends here
