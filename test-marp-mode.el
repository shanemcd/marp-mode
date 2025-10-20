;;; test-marp-mode.el --- Tests for marp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT test suite for marp-mode.
;;
;; Run tests with:
;;   M-x ert RET t RET
;; Or from command line:
;;   emacs -batch -l marp-mode.el -l test-marp-mode.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'marp-mode)

;;; Test Helpers

(defmacro marp-test-with-clean-state (&rest body)
  "Execute BODY with clean marp state."
  `(let ((marp-current-options '())
         (marp-allow-local-files-by-default nil)
         (marp-browser "auto")
         (marp-cli-executable "marp")
         (marp-watch-process nil)
         (marp-server-process nil)
         (marp-preview-process nil))
     ,@body))

;;; Tests for Command Building

(ert-deftest test-marp--build-command-basic ()
  "Test basic command building with no options."
  (marp-test-with-clean-state
   (let ((cmd (marp--build-command "test.md")))
     (should (equal cmd '("marp" "test.md"))))))

(ert-deftest test-marp--build-command-with-output ()
  "Test command building with output file option."
  (marp-test-with-clean-state
   (setq marp-current-options '((output . "slides.pdf")))
   (let ((cmd (marp--build-command "test.md")))
     (should (member "--output" cmd))
     (should (member "slides.pdf" cmd))
     (should (member "test.md" cmd)))))

(ert-deftest test-marp--build-command-with-template ()
  "Test command building with template option."
  (marp-test-with-clean-state
   (setq marp-current-options '((template . "bespoke")))
   (let ((cmd (marp--build-command "test.md")))
     (should (member "--template" cmd))
     (should (member "bespoke" cmd)))))

(ert-deftest test-marp--build-command-with-browser-path ()
  "Test command building with browser-path option."
  (marp-test-with-clean-state
   (setq marp-current-options '((browser-path . "/usr/bin/chrome")))
   (let ((cmd (marp--build-command "test.md")))
     (should (member "--browser-path" cmd))
     (should (member "/usr/bin/chrome" cmd)))))

(ert-deftest test-marp--build-command-with-allow-local-files ()
  "Test command building with allow-local-files option."
  (marp-test-with-clean-state
   (setq marp-current-options '((allow-local-files . t)))
   (let ((cmd (marp--build-command "test.md")))
     (should (member "--allow-local-files" cmd)))))

(ert-deftest test-marp--build-command-with-parallel ()
  "Test command building with parallel option."
  (marp-test-with-clean-state
   (setq marp-current-options '((parallel . 4)))
   (let ((cmd (marp--build-command "test.md")))
     (should (member "--parallel" cmd))
     (should (member "4" cmd)))))

(ert-deftest test-marp--build-command-with-image-scale ()
  "Test command building with image-scale option."
  (marp-test-with-clean-state
   (setq marp-current-options '((image-scale . 2.0)))
   (let ((cmd (marp--build-command "test.md")))
     (should (member "--image-scale" cmd))
     (should (member "2.0" cmd)))))

(ert-deftest test-marp--build-command-with-browser-timeout ()
  "Test command building with browser-timeout option."
  (marp-test-with-clean-state
   (setq marp-current-options '((browser-timeout . 60)))
   (let ((cmd (marp--build-command "test.md")))
     (should (member "--browser-timeout" cmd))
     (should (member "60" cmd)))))

(ert-deftest test-marp--build-command-with-multiple-options ()
  "Test command building with multiple options."
  (marp-test-with-clean-state
   (setq marp-current-options '((output . "slides.pdf")
                                (template . "bespoke")
                                (parallel . 2)
                                (allow-local-files . t)))
   (let ((cmd (marp--build-command "test.md")))
     (should (member "--output" cmd))
     (should (member "slides.pdf" cmd))
     (should (member "--template" cmd))
     (should (member "bespoke" cmd))
     (should (member "--parallel" cmd))
     (should (member "2" cmd))
     (should (member "--allow-local-files" cmd))
     (should (member "test.md" cmd)))))

(ert-deftest test-marp--build-command-with-extra-args ()
  "Test command building with extra arguments."
  (marp-test-with-clean-state
   (let ((cmd (marp--build-command "test.md" '("--pdf" "--watch"))))
     (should (member "--pdf" cmd))
     (should (member "--watch" cmd))
     (should (member "test.md" cmd)))))

(ert-deftest test-marp--build-command-default-allow-local-files ()
  "Test command building with default allow-local-files enabled."
  (marp-test-with-clean-state
   (setq marp-allow-local-files-by-default t)
   (let ((cmd (marp--build-command "test.md")))
     (should (member "--allow-local-files" cmd)))))

(ert-deftest test-marp--build-command-default-allow-local-files-override ()
  "Test that explicit option overrides default allow-local-files."
  (marp-test-with-clean-state
   (setq marp-allow-local-files-by-default t)
   (setq marp-current-options '((allow-local-files . t)))
   (let ((cmd (marp--build-command "test.md")))
     ;; Should only have one instance of --allow-local-files
     (should (= 1 (cl-count "--allow-local-files" cmd :test #'string=))))))

;;; Tests for Browser Configuration

(ert-deftest test-marp--build-command-browser-auto ()
  "Test that 'auto' browser doesn't add browser flags."
  (marp-test-with-clean-state
   (setq marp-browser "auto")
   (let ((cmd (marp--build-command "test.md")))
     (should-not (member "--browser" cmd))
     (should-not (member "--browser-path" cmd)))))

(ert-deftest test-marp--build-command-browser-name ()
  "Test browser name adds --browser flag."
  (marp-test-with-clean-state
   (setq marp-browser "chrome")
   (let ((cmd (marp--build-command "test.md")))
     (should (member "--browser" cmd))
     (should (member "chrome" cmd)))))

(ert-deftest test-marp--build-command-browser-executable-path ()
  "Test browser executable path adds --browser-path flag."
  (marp-test-with-clean-state
   ;; Mock file-executable-p to return t for our test path
   (cl-letf (((symbol-function 'file-executable-p)
              (lambda (path) (string= path "/usr/bin/google-chrome"))))
     (setq marp-browser "/usr/bin/google-chrome")
     (let ((cmd (marp--build-command "test.md")))
       (should (member "--browser-path" cmd))
       (should (member "/usr/bin/google-chrome" cmd))
       (should-not (member "--browser" cmd))))))

(ert-deftest test-marp--build-command-browser-name-vs-path ()
  "Test that browser name doesn't get treated as path."
  (marp-test-with-clean-state
   ;; Mock file-executable-p to return nil for browser name
   (cl-letf (((symbol-function 'file-executable-p)
              (lambda (_path) nil)))
     (setq marp-browser "firefox")
     (let ((cmd (marp--build-command "test.md")))
       (should (member "--browser" cmd))
       (should (member "firefox" cmd))
       (should-not (member "--browser-path" cmd))))))

;;; Tests for Options Management

(ert-deftest test-marp-reset-options ()
  "Test resetting options to defaults."
  (marp-test-with-clean-state
   (setq marp-current-options '((output . "test.pdf")
                                (template . "bespoke")
                                (parallel . 4)))
   (marp-reset-options)
   (should (null marp-current-options))))

(ert-deftest test-marp-toggle-local-files-enable ()
  "Test enabling local files."
  (marp-test-with-clean-state
   (marp-toggle-local-files)
   (should (assoc 'allow-local-files marp-current-options))
   (should (cdr (assoc 'allow-local-files marp-current-options)))))

(ert-deftest test-marp-toggle-local-files-disable ()
  "Test disabling local files."
  (marp-test-with-clean-state
   (setq marp-current-options '((allow-local-files . t)))
   (marp-toggle-local-files)
   (should-not (assoc 'allow-local-files marp-current-options))))

(ert-deftest test-marp-toggle-local-files-toggle-twice ()
  "Test toggling local files twice returns to original state."
  (marp-test-with-clean-state
   (marp-toggle-local-files)
   (should (assoc 'allow-local-files marp-current-options))
   (marp-toggle-local-files)
   (should-not (assoc 'allow-local-files marp-current-options))))

(ert-deftest test-marp-set-parallel-updates-option ()
  "Test setting parallel option."
  (marp-test-with-clean-state
   (cl-letf (((symbol-function 'read-number)
              (lambda (_prompt &optional _default) 4)))
     (marp-set-parallel)
     (should (equal (cdr (assoc 'parallel marp-current-options)) 4)))))

(ert-deftest test-marp-set-parallel-replaces-existing ()
  "Test that setting parallel replaces existing value."
  (marp-test-with-clean-state
   (setq marp-current-options '((parallel . 2)))
   (cl-letf (((symbol-function 'read-number)
              (lambda (_prompt &optional _default) 8)))
     (marp-set-parallel)
     (should (equal (cdr (assoc 'parallel marp-current-options)) 8))
     ;; Should only have one parallel entry
     (should (= 1 (cl-count-if (lambda (opt) (eq (car opt) 'parallel))
                                marp-current-options))))))

(ert-deftest test-marp-set-image-scale-updates-option ()
  "Test setting image scale option."
  (marp-test-with-clean-state
   (cl-letf (((symbol-function 'read-number)
              (lambda (_prompt &optional _default) 2.5)))
     (marp-set-image-scale)
     (should (equal (cdr (assoc 'image-scale marp-current-options)) 2.5)))))

(ert-deftest test-marp-set-browser-timeout-updates-option ()
  "Test setting browser timeout option."
  (marp-test-with-clean-state
   (cl-letf (((symbol-function 'read-number)
              (lambda (_prompt &optional _default) 120)))
     (marp-set-browser-timeout)
     (should (equal (cdr (assoc 'browser-timeout marp-current-options)) 120)))))

;;; Tests for Process Management

(ert-deftest test-marp--kill-process-with-no-process ()
  "Test killing process when none exists."
  (marp-test-with-clean-state
   (setq marp-watch-process nil)
   ;; Should not error
   (marp--kill-process 'marp-watch-process "watch")
   (should (null marp-watch-process))))

(ert-deftest test-marp--kill-process-with-dead-process ()
  "Test killing process that already exited."
  (marp-test-with-clean-state
   ;; Create a mock dead process
   (let ((mock-process (make-symbol "mock-process")))
     (setq marp-watch-process mock-process)
     (cl-letf (((symbol-function 'process-live-p)
                (lambda (_proc) nil)))
       (marp--kill-process 'marp-watch-process "watch")
       ;; Should not attempt to kill, but also not error
       (should (equal marp-watch-process mock-process))))))

(ert-deftest test-marp--kill-process-with-live-process ()
  "Test killing a live process."
  (marp-test-with-clean-state
   (let ((killed nil)
         (mock-process (make-symbol "mock-process")))
     (setq marp-watch-process mock-process)
     (cl-letf (((symbol-function 'process-live-p)
                (lambda (_proc) t))
               ((symbol-function 'kill-process)
                (lambda (_proc) (setq killed t))))
       (marp--kill-process 'marp-watch-process "watch")
       (should killed)
       (should (null marp-watch-process))))))

(ert-deftest test-marp-stop-watch ()
  "Test stopping watch process."
  (marp-test-with-clean-state
   (let ((killed nil)
         (mock-process (make-symbol "mock-process")))
     (setq marp-watch-process mock-process)
     (cl-letf (((symbol-function 'process-live-p)
                (lambda (_proc) t))
               ((symbol-function 'kill-process)
                (lambda (_proc) (setq killed t))))
       (marp-stop-watch)
       (should killed)
       (should (null marp-watch-process))))))

(ert-deftest test-marp-stop-all ()
  "Test stopping all processes."
  (marp-test-with-clean-state
   (let ((watch-killed nil)
         (server-killed nil)
         (preview-killed nil)
         (mock-watch (make-symbol "watch"))
         (mock-server (make-symbol "server"))
         (mock-preview (make-symbol "preview")))
     (setq marp-watch-process mock-watch
           marp-server-process mock-server
           marp-preview-process mock-preview)
     (cl-letf (((symbol-function 'process-live-p)
                (lambda (_proc) t))
               ((symbol-function 'kill-process)
                (lambda (proc)
                  (cond
                   ((eq proc mock-watch) (setq watch-killed t))
                   ((eq proc mock-server) (setq server-killed t))
                   ((eq proc mock-preview) (setq preview-killed t))))))
       (marp-stop-all)
       (should watch-killed)
       (should server-killed)
       (should preview-killed)
       (should (null marp-watch-process))
       (should (null marp-server-process))
       (should (null marp-preview-process))))))

;;; Tests for Utility Functions

(ert-deftest test-marp--executable-exists-p-found ()
  "Test executable detection when marp is found."
  (marp-test-with-clean-state
   (cl-letf (((symbol-function 'executable-find)
              (lambda (exe) (when (string= exe "marp") "/usr/bin/marp"))))
     (should (marp--executable-exists-p)))))

(ert-deftest test-marp--executable-exists-p-not-found ()
  "Test executable detection when marp is not found."
  (marp-test-with-clean-state
   (cl-letf (((symbol-function 'executable-find)
              (lambda (_exe) nil)))
     (should-not (marp--executable-exists-p)))))

(ert-deftest test-marp--executable-exists-p-custom-path ()
  "Test executable detection with custom path."
  (marp-test-with-clean-state
   (setq marp-cli-executable "/opt/marp/bin/marp")
   (cl-letf (((symbol-function 'executable-find)
              (lambda (exe) (when (string= exe "/opt/marp/bin/marp") exe))))
     (should (marp--executable-exists-p)))))

(ert-deftest test-marp--get-input-file-from-buffer ()
  "Test getting input file from buffer."
  (marp-test-with-clean-state
   (with-temp-buffer
     (setq buffer-file-name "/tmp/test.md")
     (should (string= (marp--get-input-file) "/tmp/test.md")))))

(ert-deftest test-marp--get-input-file-prompt ()
  "Test getting input file via prompt when buffer has no file."
  (marp-test-with-clean-state
   (with-temp-buffer
     (setq buffer-file-name nil)
     (cl-letf (((symbol-function 'read-file-name)
                (lambda (&rest _args) "/tmp/prompted.md")))
       (should (string= (marp--get-input-file) "/tmp/prompted.md"))))))

;;; Tests for Edge Cases

(ert-deftest test-marp--build-command-empty-options ()
  "Test command building with explicitly empty options list."
  (marp-test-with-clean-state
   (setq marp-current-options '())
   (let ((cmd (marp--build-command "test.md")))
     (should (equal cmd '("marp" "test.md"))))))

(ert-deftest test-marp--build-command-nil-extra-args ()
  "Test command building with nil extra args."
  (marp-test-with-clean-state
   (let ((cmd (marp--build-command "test.md" nil)))
     (should (equal cmd '("marp" "test.md"))))))

(ert-deftest test-marp--build-command-empty-extra-args ()
  "Test command building with empty extra args list."
  (marp-test-with-clean-state
   (let ((cmd (marp--build-command "test.md" '())))
     (should (equal cmd '("marp" "test.md"))))))

(ert-deftest test-marp--build-command-numeric-conversion ()
  "Test that numeric options are properly converted to strings."
  (marp-test-with-clean-state
   (setq marp-current-options '((parallel . 4)
                                (image-scale . 1.5)
                                (browser-timeout . 30)))
   (let ((cmd (marp--build-command "test.md")))
     (should (member "4" cmd))
     (should (member "1.5" cmd))
     (should (member "30" cmd))
     ;; Should not have numeric values
     (should-not (member 4 cmd))
     (should-not (member 1.5 cmd))
     (should-not (member 30 cmd)))))

(provide 'test-marp-mode)

;;; test-marp-mode.el ends here
