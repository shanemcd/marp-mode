;;; marp-mode.el --- An Emacs minor mode for working with Marp presentations -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Shane McDonald <me@shanemcd.com>
;; URL: https://github.com/shanemcd/marp-mode
;; Version: 0.1.0
;; Package-Requires: ((transient "0.7"))
;; Keywords: marp, markdown, presentations, slides

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; marp-mode is an Emacs minor mode for working with Marp presentations.
;; Marp is a Markdown presentation ecosystem that allows you to create
;; slide decks using Markdown syntax.
;;
;; This mode provides:
;; - A transient interface for easy access to Marp CLI options
;; - Conversion commands for various output formats (HTML, PDF, PPTX, images)
;; - Preview and watch mode functionality
;; - Customizable settings for Marp CLI options
;;
;; Usage:
;; Enable marp-mode in a Markdown buffer and use M-x marp-menu
;; to access the main menu.

;;; Code:

(require 'transient)

;;; Customization

(defgroup marp nil
  "Marp presentation support for Emacs."
  :group 'external
  :prefix "marp-")

(defcustom marp-cli-executable "marp"
  "Path to the Marp CLI executable."
  :type 'string
  :group 'marp)

(defcustom marp-default-output-format 'html
  "Default output format for Marp conversions."
  :type '(choice (const :tag "HTML" html)
                 (const :tag "PDF" pdf)
                 (const :tag "PowerPoint" pptx)
                 (const :tag "Images" images))
  :group 'marp)

(defcustom marp-browser "auto"
  "Browser to use for Marp CLI operations.
Can be:
- \"auto\" (let Marp CLI choose)
- Browser name: \"chrome\", \"chromium\", \"firefox\", \"edge\" (uses --browser)
- Full path: \"/usr/bin/google-chrome\" (uses --browser-path)"
  :type 'string
  :group 'marp)

(defcustom marp-watch-mode-auto-open t
  "Whether to automatically open preview when starting watch mode."
  :type 'boolean
  :group 'marp)

(defcustom marp-allow-local-files-by-default nil
  "Whether to allow local files by default.
When enabled, Marp will be able to access local images and assets."
  :type 'boolean
  :group 'marp)

;;; Variables

(defvar marp-current-options '()
  "Alist of current Marp CLI options.")

(defvar marp-output-buffer "*Marp Output*"
  "Name of the buffer for Marp CLI output.")

(defvar marp-watch-process nil
  "Current Marp watch process.")

(defvar marp-server-process nil
  "Current Marp server process.")

;;; Utility Functions

(defun marp--kill-process (process-var process-name)
  "Kill PROCESS-VAR if it's running and set it to nil."
  (when (and (symbol-value process-var)
             (process-live-p (symbol-value process-var)))
    (message "Stopping existing %s process..." process-name)
    (kill-process (symbol-value process-var))
    (set process-var nil)))

(defun marp-stop-watch ()
  "Stop the current Marp watch process."
  (interactive)
  (marp--kill-process 'marp-watch-process "watch")
  (message "Marp watch mode stopped."))

(defun marp-stop-server ()
  "Stop the current Marp server process."
  (interactive)
  (marp--kill-process 'marp-server-process "server")
  (message "Marp server stopped."))

(defun marp-stop-all ()
  "Stop all running Marp processes."
  (interactive)
  (marp-stop-watch)
  (marp-stop-server))

(defun marp--stop-watch-if-running ()
  "Stop watch process if it's running."
  (interactive)
  (when (and marp-watch-process (process-live-p marp-watch-process))
    (marp-stop-watch)))

(defun marp--stop-server-if-running ()
  "Stop server process if it's running."
  (interactive)
  (when (and marp-server-process (process-live-p marp-server-process))
    (marp-stop-server)))


(defun marp--executable-exists-p ()
  "Check if Marp CLI executable exists."
  (executable-find marp-cli-executable))


(defun marp--get-input-file ()
  "Get the current buffer's file name or prompt for one."
  (or (buffer-file-name)
      (read-file-name "Marp input file: " nil nil t)))

(defun marp--build-command (input-file &optional extra-args)
  "Build Marp CLI command for INPUT-FILE with optional EXTRA-ARGS."
  (let ((cmd (list marp-cli-executable)))
    ;; Add default allow-local-files if enabled
    (when (and marp-allow-local-files-by-default
               (not (assoc 'allow-local-files marp-current-options)))
      (setq cmd (append cmd (list "--allow-local-files"))))
    ;; Add stored options
    (dolist (option marp-current-options)
      (let ((key (car option))
            (value (cdr option)))
        (cond
         ((eq key 'output)
          (setq cmd (append cmd (list "--output" value))))
         ((eq key 'template)
          (setq cmd (append cmd (list "--template" value))))
         ((eq key 'browser-path)
          (setq cmd (append cmd (list "--browser-path" value))))
         ((eq key 'allow-local-files)
          (setq cmd (append cmd (list "--allow-local-files"))))
         ((eq key 'parallel)
          (setq cmd (append cmd (list "--parallel" (number-to-string value)))))
         ((eq key 'image-scale)
          (setq cmd (append cmd (list "--image-scale" (number-to-string value)))))
         ((eq key 'browser-timeout)
          (setq cmd (append cmd (list "--browser-timeout" (number-to-string value))))))))
    ;; Add browser if not auto
    (unless (string= marp-browser "auto")
      (if (file-executable-p marp-browser)
          ;; It's a path to an executable
          (setq cmd (append cmd (list "--browser-path" marp-browser)))
        ;; It's a browser name
        (setq cmd (append cmd (list "--browser" marp-browser)))))
    ;; Add extra args
    (when extra-args
      (setq cmd (append cmd extra-args)))
    ;; Add input file
    (append cmd (list input-file))))

(defun marp--run-command (command)
  "Run COMMAND and display output in dedicated buffer."
  (let ((buffer (get-buffer-create marp-output-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Running: %s\n\n" (mapconcat 'identity command " ")))
      (insert (format "Debug - Command list: %S\n\n" command)))
    (apply 'call-process (car command) nil buffer t (cdr command))
    (display-buffer buffer)))

;;; Core Functions

(defun marp-run-basic ()
  "Run Marp CLI with basic HTML conversion."
  (interactive)
  (unless (marp--executable-exists-p)
    (user-error "Marp CLI not found. Please install it first"))
  (let* ((input-file (marp--get-input-file))
         (command (marp--build-command input-file)))
    (marp--run-command command)))

(defun marp-convert-to-pdf ()
  "Convert current file to PDF using Marp CLI."
  (interactive)
  (unless (marp--executable-exists-p)
    (user-error "Marp CLI not found. Please install it first"))
  (let* ((input-file (marp--get-input-file))
         (command (marp--build-command input-file '("--pdf"))))
    (marp--run-command command)))

(defun marp-convert-to-pptx ()
  "Convert current file to PowerPoint using Marp CLI."
  (interactive)
  (unless (marp--executable-exists-p)
    (user-error "Marp CLI not found. Please install it first"))
  (let* ((input-file (marp--get-input-file))
         (command (marp--build-command input-file '("--pptx"))))
    (marp--run-command command)))

(defun marp-convert-to-images ()
  "Convert current file to images using Marp CLI."
  (interactive)
  (unless (marp--executable-exists-p)
    (user-error "Marp CLI not found. Please install it first"))
  (let* ((input-file (marp--get-input-file))
         (image-format (completing-read "Image format: " '("png" "jpeg") nil t "png"))
         (command (marp--build-command input-file (list (concat "--images=" image-format)))))
    (marp--run-command command)))

(defun marp-preview ()
  "Open preview of current file using Marp CLI."
  (interactive)
  (unless (marp--executable-exists-p)
    (user-error "Marp CLI not found. Please install it first"))
  (let* ((input-file (marp--get-input-file))
         (command (marp--build-command input-file '("--preview"))))
    (marp--run-command command)))

(defun marp-watch-mode ()
  "Start Marp CLI in watch mode."
  (interactive)
  (unless (marp--executable-exists-p)
    (user-error "Marp CLI not found. Please install it first"))

  (let* ((input-file (marp--get-input-file))
         (args (if marp-watch-mode-auto-open
                   '("--watch" "--preview")
                 '("--watch")))
         (command (marp--build-command input-file args)))

    ;; Stop any existing watch process
    (marp--kill-process 'marp-watch-process "watch")

    ;; Start new watch process
    (setq marp-watch-process
          (apply 'start-process "marp-watch" marp-output-buffer (car command) (cdr command)))
    (message "Marp watch mode started. Check %s for output." marp-output-buffer)))

;;; Minor Mode Definition

(defvar marp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m") 'marp-menu)
    map)
  "Keymap for `marp-mode'.")

;;;###autoload
(define-minor-mode marp-mode
  "Minor mode for working with Marp presentations."
  :lighter " Marp"
  :keymap marp-mode-map
  :group 'marp
  (if marp-mode
      (message "Marp mode enabled. Use C-c m for the main menu.")
    (message "Marp mode disabled.")))

;;; Transient Interface

;;;###autoload
(transient-define-prefix marp-menu ()
  "Main menu for Marp operations."
  [:description (lambda ()
                  (format "Marp Mode%s"
                          (let ((running '()))
                            (when (and marp-watch-process (process-live-p marp-watch-process))
                              (push "Watch" running))
                            (when (and marp-server-process (process-live-p marp-server-process))
                              (push "Server" running))
                            (if running
                                (format " (Running: %s)" (string-join running ", "))
                              ""))))
   ["Convert"
    ("h" "HTML (default)" marp-run-basic)
    ("d" "PDF" marp-convert-to-pdf)
    ("x" "PowerPoint" marp-convert-to-pptx)
    ("i" "Images" marp-convert-to-images)]
   ["Preview"
    ("p" "Preview" marp-preview)
    ("w" "Watch mode" marp-watch-mode)
    ("s" "Server mode" marp-server-mode)]
   ["Options"
    ("o" "Output settings" marp-output-menu)
    ("a" "Advanced options" marp-advanced-menu)]]
  [:if (lambda () (or (and marp-watch-process (process-live-p marp-watch-process))
                      (and marp-server-process (process-live-p marp-server-process))))
   ["Process Control"
    ("W" marp--stop-watch-if-running :if (lambda () (and marp-watch-process (process-live-p marp-watch-process))) :description "Stop watch")
    ("S" marp--stop-server-if-running :if (lambda () (and marp-server-process (process-live-p marp-server-process))) :description "Stop server")
    ("Q" "Stop all processes" marp-stop-all)]])


(transient-define-prefix marp-output-menu ()
  "Output settings for Marp."
  ["Output Options"
   ("f" "Set output file" marp-set-output-file)
   ("t" "Choose template" marp-choose-template)
   ("n" "Export notes" marp-export-notes)
   ("c" "Conversion options" marp-conversion-options)]
  ["Navigation"
   ("b" "Back to main menu" marp-menu)
   ("q" "Quit" transient-quit-one)])

(transient-define-prefix marp-advanced-menu ()
  "Advanced options for Marp."
  ["Advanced Settings"
   ("l" "Toggle local files" marp-toggle-local-files)
   ("p" "Set parallel conversion" marp-set-parallel)
   ("s" "Set image scale factor" marp-set-image-scale)
   ("t" "Set browser timeout" marp-set-browser-timeout)]
  ["Reset"
   ("r" "Reset all options" marp-reset-options)]
  ["Navigation"
   ("b" "Back to main menu" marp-menu)
   ("q" "Quit" transient-quit-one)])

;;; Alternative Preview Function




;;; Additional Functions

(defun marp-server-mode ()
  "Start Marp CLI in server mode."
  (interactive)
  (unless (marp--executable-exists-p)
    (user-error "Marp CLI not found. Please install it first"))
  (let* ((input-file (marp--get-input-file))
         (input-dir (file-name-directory input-file))
         (port (read-string "Server port (default 8080): " "8080"))
         (cmd (list marp-cli-executable "--server" "--port" port "--input-dir" input-dir)))
    ;; Add browser if not auto
    (unless (string= marp-browser "auto")
      (if (file-executable-p marp-browser)
          (setq cmd (append cmd (list "--browser-path" marp-browser)))
        (setq cmd (append cmd (list "--browser" marp-browser)))))

    ;; Stop any existing server process
    (marp--kill-process 'marp-server-process "server")

    ;; Start new server process
    (setq marp-server-process
          (apply 'start-process "marp-server" marp-output-buffer (car cmd) (cdr cmd)))
    (message "Marp server started on port %s for directory %s. Check %s for output." port input-dir marp-output-buffer)))


(defun marp-set-output-file ()
  "Set output file."
  (interactive)
  (let ((output-file (read-file-name "Output file: ")))
    (setq marp-current-options
          (cons (cons 'output output-file)
                (assq-delete-all 'output marp-current-options)))
    (message "Output file set to: %s" output-file)))

(defun marp-choose-template ()
  "Choose HTML template."
  (interactive)
  (let ((template (completing-read "Template: " '("bespoke" "bare") nil t "bespoke")))
    (setq marp-current-options
          (cons (cons 'template template)
                (assq-delete-all 'template marp-current-options)))
    (message "Template set to: %s" template)))

(defun marp-export-notes ()
  "Export presenter notes."
  (interactive)
  (unless (marp--executable-exists-p)
    (user-error "Marp CLI not found. Please install it first"))
  (let* ((input-file (marp--get-input-file))
         (command (marp--build-command input-file '("--notes"))))
    (marp--run-command command)))

(defun marp-conversion-options ()
  "Set conversion options."
  (interactive)
  (let ((option (completing-read "Conversion option: "
                                '("allow-local-files" "image-scale" "parallel" "browser-timeout")
                                nil t)))
    (cond
     ((string= option "allow-local-files") (marp-toggle-local-files))
     ((string= option "image-scale") (marp-set-image-scale))
     ((string= option "parallel") (marp-set-parallel))
     ((string= option "browser-timeout") (marp-set-browser-timeout)))))

(defun marp-toggle-local-files ()
  "Toggle allow local files option."
  (interactive)
  (let ((current (assoc 'allow-local-files marp-current-options)))
    (if current
        (progn
          (setq marp-current-options (assq-delete-all 'allow-local-files marp-current-options))
          (message "Allow local files disabled"))
      (progn
        (setq marp-current-options (cons (cons 'allow-local-files t) marp-current-options))
        (message "Allow local files enabled")))))

(defun marp-set-parallel ()
  "Set parallel conversion count."
  (interactive)
  (let ((count (read-number "Parallel conversion count: " 1)))
    (setq marp-current-options
          (cons (cons 'parallel count)
                (assq-delete-all 'parallel marp-current-options)))
    (message "Parallel conversion count set to: %d" count)))

(defun marp-set-image-scale ()
  "Set image scale factor."
  (interactive)
  (let ((scale (read-number "Image scale factor: " 1.0)))
    (setq marp-current-options
          (cons (cons 'image-scale scale)
                (assq-delete-all 'image-scale marp-current-options)))
    (message "Image scale factor set to: %s" scale)))

(defun marp-set-browser-timeout ()
  "Set browser timeout in seconds."
  (interactive)
  (let ((timeout (read-number "Browser timeout (seconds): " 30)))
    (setq marp-current-options
          (cons (cons 'browser-timeout timeout)
                (assq-delete-all 'browser-timeout marp-current-options)))
    (message "Browser timeout set to: %d seconds" timeout)))

(defun marp-reset-options ()
  "Reset all Marp options to defaults."
  (interactive)
  (setq marp-current-options '())
  (message "All Marp options reset to defaults"))

(provide 'marp-mode)

;;; marp-mode.el ends here
