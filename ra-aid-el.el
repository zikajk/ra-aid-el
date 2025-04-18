;;; ra-aid-el.el --- Interface for RA.Aid AI coding assistant -*- lexical-binding: t; -*-

;; Author: Jakub Zika (Akiz) <zikajk@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (transient "0.3.0"))
;; Keywords: ai tools development ra-aid
;; URL: https://github.com/zikajk/re-aid-emacs
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Provides an Emacs Lisp interface for the RA.Aid AI coding assistant.
;; Allows configuring RA.Aid options via a transient menu and running
;; tasks or chat sessions within the current project context, displaying output in a
;; comint buffer.

;;; Code:

(require 'transient)
(require 'comint)
(require 'cl-lib) ;; For cl-letf, cl-mapcan etc. if needed later

(defgroup ra-aid-el nil
  "Emacs Lisp interface for the RA.Aid AI coding assistant."
  :group 'tools
  :group 'convenience)

(defconst ra-aid-el-provider-list
  '("antropic" "deepseek" "fireworks" "gemini" "groq" "ollama" "openrouter" "openai" "openai-compatible"))

(defcustom ra-aid-el-program "ra-aid"
  "The name or path of the ra-aid program."
  :type 'string
  :group 'ra-aid-el)

(defcustom ra-aid-el-provider "anthropic"
  "Default LLM provider to use for RA.Aid."
  :type 'string ;; Later, we can make this a choice
  :group 'ra-aid-el)

(defcustom ra-aid-el-model "claude-3-5-sonnet-20240620"
  "Default LLM model name to use for RA.Aid."
  :type 'string ;; Later, we can make this a choice
  :group 'ra-aid-el)

;; --- Basic Settings ---

(defconst ra-aid-el-temperature-default nil)

(defcustom ra-aid-el-temperature ra-aid-el-temperature-default
  "LLM temperature setting (corresponds to --temperature)."
  :type 'float
  :group 'ra-aid-el)

(defcustom ra-aid-el-log-level "INFO"
  "Logging level for RA.Aid output (corresponds to --log-level)."
  :type '(choice (const :tag "Debug" "DEBUG")
                 (const :tag "Info" "INFO")
                 (const :tag "Warning" "WARNING")
                 (const :tag "Error" "ERROR")
                 (const :tag "Critical" "CRITICAL"))
  :group 'ra-aid-el)

(defcustom ra-aid-el-log-mode "file"
  "Logging mode for RA.Aid output (corresponds to --log-mode)."
  :type '(choice (const :tag "File" "file")
                 (const :tag "Console" "console"))
  :group 'ra-aid-el)

(defcustom ra-aid-el-test-cmd ""
  "Command to run for tests (corresponds to --test-cmd)."
  :type 'string
  :group 'ra-aid-el)

;; --- Per-Agent Provider/Model Settings ---

(defcustom ra-aid-el-research-provider ""
  "Provider for the Research agent (overrides default)."
  :type 'string
  :group 'ra-aid-el)

(defcustom ra-aid-el-research-model ""
  "Model for the Research agent (overrides default)."
  :type 'string
  :group 'ra-aid-el)

(defcustom ra-aid-el-planner-provider ""
  "Provider for the Planner agent (overrides default)."
  :type 'string
  :group 'ra-aid-el)

(defcustom ra-aid-el-planner-model ""
  "Model for the Planner agent (overrides default)."
  :type 'string
  :group 'ra-aid-el)

(defcustom ra-aid-el-expert-provider ""
  "Provider for the Expert agent (overrides default)."
  :type 'string
  :group 'ra-aid-el)

(defcustom ra-aid-el-expert-model ""
  "Model for the Expert agent (overrides default)."
  :type 'string
  :group 'ra-aid-el)

;; --- Numeric Settings ---
(defconst ra-aid-el-recursion-limit-default 100)

(defcustom ra-aid-el-recursion-limit ra-aid-el-recursion-limit-default
  "Maximum recursion depth for the agent (corresponds to --recursion-limit)."
  :type 'integer
  :group 'ra-aid-el)

(defconst ra-aid-el-max-test-cmd-retries-default 3)

(defcustom ra-aid-el-max-test-cmd-retries ra-aid-el-max-test-cmd-retries-default
  "Maximum number of retries for the test command (corresponds to --max-test-cmd-retries)."
  :type 'integer
  :group 'ra-aid-el)

(defconst ra-aid-el-test-cmd-timeout-default 300)

(defcustom ra-aid-el-test-cmd-timeout ra-aid-el-test-cmd-timeout-default
  "Timeout in seconds for the test command (corresponds to --test-cmd-timeout)."
  :type 'integer
  :group 'ra-aid-el)

(defcustom ra-aid-el-custom-tools-path ""
  "Path to custom python tools."
  :type 'string
  :group 'ra-aid-el)

;; --- Boolean Toggles ---

(defcustom ra-aid-el-use-aider nil
  "Use aider's specialized code editing capabilities (corresponds to --use-aider)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-research-only nil
  "Run in research-only mode (corresponds to --research-only)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-cowboy-mode nil
  "Run in cowboy mode, skipping confirmations (corresponds to --cowboy-mode)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-hil nil
  "Enable Human-in-the-Loop mode (corresponds to --hil)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-pretty-logger t
  "Use the pretty logger for output (corresponds to --pretty-logger)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-auto-test nil
  "Automatically run tests when applicable (corresponds to --auto-test)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-wipe-project-memory nil
  "Wipe project memory before starting (corresponds to --wipe-project-memory)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-show-thoughts nil
  "Show agent's internal thoughts/reasoning (corresponds to --show-thoughts)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-show-cost nil
  "Show estimated cost after each interaction (corresponds to --show-cost)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-track-cost nil
  "Track and accumulate cost across the session (corresponds to --track-cost)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-experimental-fallback-handler nil
  "Enable experimental fallback handler (corresponds to --experimental-fallback-handler)."
  :type 'boolean
  :group 'ra-aid-el)

(defcustom ra-aid-el-disable-limit-tokens nil
  "Disable automatic token limiting (corresponds to --disable-limit-tokens)."
  :type 'boolean
  :group 'ra-aid-el)

;; --- End of Defcustom ---

(defun ra-aid-el--project-root ()
  "Get the project root using VC-git, or fallback to default-directory."
  (or (vc-git-root default-directory)
      (when buffer-file-name
        (file-name-directory buffer-file-name))
      default-directory))

(defun ra-aid-el--get-buffer-name (base-name)
  "Generate the ra-aid buffer name based on project root and BASE-NAME."
  (let* ((root (ra-aid-el--project-root))
         (project-name (file-name-nondirectory (directory-file-name root))))
    (format "*ra-aid-el-%s:%s*" base-name (if (string-empty-p project-name) "default" project-name))))

(defun ra-aid-el--build-common-args ()
  "Build the common command-line arguments based on settings."
  (append (list "--provider" ra-aid-el-provider
                "--model" ra-aid-el-model)
          ;; Add temperature if not default 1.0
          (when (numberp ra-aid-el-temperature)
            (list "--temperature" (format "%f" ra-aid-el-temperature)))
          ;; Add log-level if not default "INFO"
          (unless (string= ra-aid-el-log-level "INFO")
            (list "--log-level" ra-aid-el-log-level))
          ;; Add log-mode if not default "file"
          (unless (string= ra-aid-el-log-mode "file")
            (list "--log-mode" ra-aid-el-log-mode))
          ;; Add test-cmd if set and not empty
          (unless (string-empty-p ra-aid-el-test-cmd)
            (list "--test-cmd" ra-aid-el-test-cmd))
	  ;; Add custom path to tools
	  (unless (string-empty-p ra-aid-el-custom-tools-path)
	    (list "--custom-tools" ra-aid-el-custom-tools-path))
          ;; Per-agent provider/model settings
          (unless (string-empty-p ra-aid-el-research-provider)
            (list "--research-provider" ra-aid-el-research-provider))
          (unless (string-empty-p ra-aid-el-research-model)
            (list "--research-model" ra-aid-el-research-model))
          (unless (string-empty-p ra-aid-el-planner-provider)
            (list "--planner-provider" ra-aid-el-planner-provider))
          (unless (string-empty-p ra-aid-el-planner-model)
            (list "--planner-model" ra-aid-el-planner-model))
          (unless (string-empty-p ra-aid-el-expert-provider)
            (list "--expert-provider" ra-aid-el-expert-provider))
          (unless (string-empty-p ra-aid-el-expert-model)
            (list "--expert-model" ra-aid-el-expert-model))
          ;; Numerical settings if not default
          (unless (= ra-aid-el-recursion-limit ra-aid-el-recursion-limit-default)
            (list "--recursion-limit" (format "%d" ra-aid-el-recursion-limit)))
          (unless (= ra-aid-el-max-test-cmd-retries ra-aid-el-max-test-cmd-retries-default)
            (list "--max-test-cmd-retries" (format "%d" ra-aid-el-max-test-cmd-retries)))
          (unless (= ra-aid-el-test-cmd-timeout ra-aid-el-test-cmd-timeout-default)
            (list "--test-cmd-timeout" (format "%d" ra-aid-el-test-cmd-timeout)))
          ;; Boolean toggles
          (when ra-aid-el-use-aider '("--use-aider"))
          (when ra-aid-el-research-only '("--research-only"))
          (when ra-aid-el-cowboy-mode '("--cowboy-mode"))
          (when ra-aid-el-hil '("--hil"))
          (when ra-aid-el-pretty-logger '("--pretty-logger"))
          (when ra-aid-el-auto-test '("--auto-test"))
          (when ra-aid-el-wipe-project-memory '("--wipe-project-memory"))
          (when ra-aid-el-show-thoughts '("--show-thoughts"))
          (when ra-aid-el-show-cost '("--show-cost"))
          (when ra-aid-el-track-cost '("--track-cost"))
          (when ra-aid-el-experimental-fallback-handler '("--experimental-fallback-handler"))
          (when ra-aid-el-disable-limit-tokens '("--disable-limit-tokens"))))

(defun ra-aid-el--run-ra-aid (prompt)
  "Run ra-aid with the given PROMPT and current settings.
Uses `make-comint` to run the process in a dedicated buffer."
  (interactive "sRA.Aid Task: ")
  (let* ((project-root (ra-aid-el--project-root))
         (buffer-name (ra-aid-el--get-buffer-name "task"))
         (command (append (ra-aid-el--build-common-args)
                          (list "-m" prompt)))
         (default-directory project-root))

    (message "Running RA.Aid in %s: %s %s" project-root ra-aid-el-program (string-join command " "))
    (let ((buffer (apply #'make-comint buffer-name ra-aid-el-program nil command)))
      (with-current-buffer buffer
        (setq-local comint-process-echoes t))
      (display-buffer buffer))))

(defun ra-aid-el--run-chat ()
  "Run ra-aid in chat mode with current settings.
Uses `make-comint` to run the process in a dedicated buffer."
  ;; This function is not interactive, called from the transient menu.
  (interactive)
  (let* ((project-root (ra-aid-el--project-root))
         (buffer-name (ra-aid-el--get-buffer-name "chat"))
         ;; Build the command list dynamically based on settings
         (command (append (ra-aid-el--build-common-args)
                          ;; Always add the --chat flag
                          '("--chat")))
         ;; Ensure comint runs in the correct directory
         (default-directory project-root))

    (message "Starting RA.Aid Chat in %s: %s %s" project-root ra-aid-el-program (string-join command " "))
    ;; Create or reuse the comint buffer
    (let ((buffer (apply #'make-comint buffer-name ra-aid-el-program nil command)))
      (with-current-buffer buffer
        ;; Optional: Add mode-specific settings here if needed later
        (setq-local comint-process-echoes t) ; Often useful for interactive CLIs
        )
      (display-buffer buffer))))

;; --- Interactive Setters ---

(defun ra-aid-el-set-provider (provider)
  "Set the RA.Aid provider."
  (interactive
   (list (completing-read (format "Provider (%s): " ra-aid-el-provider)
                          ra-aid-el-provider-list
                          nil t nil nil ra-aid-el-provider)))
  (customize-set-variable 'ra-aid-el-provider provider)
  (message "RA.Aid provider set to: %s" provider))

(defun ra-aid-el-set-model (model)
  "Set the RA.Aid model."
  (interactive
   (list (read-string (format "Model (%s): " ra-aid-el-model)
                      ra-aid-el-model nil nil)))
  (customize-set-variable 'ra-aid-el-model model)
  (message "RA.Aid model set to: %s" model))

(defun ra-aid-el-set-temperature ()
  "Set the RA.Aid temperature. Empty input clears the value."
  (interactive)
  (let* ((current-val ra-aid-el-temperature)
         (prompt (format "Temperature [%s]: "
                         (if current-val (format "%.1f" current-val) "unset")))
         (initial-input (if current-val (format "%.1f" current-val) ""))
         (input-str (read-string prompt initial-input nil nil)))
    (let ((new-val
           (cond
            ((string-empty-p input-str) nil)
            (t (let ((num (ignore-errors (string-to-number input-str))))
                 (unless (numberp num)
                   (error "Invalid numeric input: %s" input-str)
		   nil)
                 num)))))
      (customize-set-variable 'ra-aid-el-temperature new-val)
      (message "RA.Aid temperature set to: %s"
               (if new-val (format "%.1f" new-val) "unset")))))

(defun ra-aid-el-set-log-level (level)
  "Set the RA.Aid log level."
  (interactive
   (list (completing-read (format "Log Level (%s): " ra-aid-el-log-level)
                          '("DEBUG" "INFO" "WARNING" "ERROR" "CRITICAL")
                          nil t nil nil ra-aid-el-log-level)))
  (customize-set-variable 'ra-aid-el-log-level level)
  (message "RA.Aid log level set to: %s" level))

(defun ra-aid-el-set-log-mode (mode)
  "Set the RA.Aid log mode."
  (interactive
   (list (completing-read (format "Log Mode (%s): " ra-aid-el-log-mode)
                          '(("file" . "File") ("console" . "Console")) ; Use alist for clarity
                          nil t nil nil ra-aid-el-log-mode)))
  (customize-set-variable 'ra-aid-el-log-mode mode)
  (message "RA.Aid log mode set to: %s" mode))

(defun ra-aid-el-set-test-cmd (cmd)
  "Set the RA.Aid test command."
  (interactive
   (list (read-string (format "Test Command (%s): " (if (string-empty-p ra-aid-el-test-cmd) "<unset>" ra-aid-el-test-cmd))
                      ra-aid-el-test-cmd nil nil)))
  ;; Ensure empty input results in an empty string, not nil
  (customize-set-variable 'ra-aid-el-test-cmd (if (string-empty-p cmd) "" cmd))
  (message "RA.Aid test command set to: %s" (if (string-empty-p ra-aid-el-test-cmd) "<unset>" ra-aid-el-test-cmd)))

(defun ra-aid-el-set-custom-tools-path (path)
  "Set the RA.Aid custom tools path."
  (interactive
   (list (read-string (format "Path (%s): " (if (string-empty-p ra-aid-el-custom-tools-path) "<unset>" ra-aid-el-custom-tools-path))
		      ra-aid-el-custom-tools-path
		      nil nil)))
  (customize-set-variable 'ra-aid-el-custom-tools-path path)
  (message "RA.Aid custom tools path set to: %s" path))


;; --- Setters for Agent-Specific Models/Providers ---

(defun ra-aid-el-set-research-provider (provider)
  "Set the Research agent provider."
  (interactive
   (list (completing-read (format "Research Provider (%s): " (if (string-empty-p ra-aid-el-research-provider) "<default>" ra-aid-el-research-provider))
                          (cons "<default>" ra-aid-el-provider-list)
                          nil t nil nil (if (string-empty-p ra-aid-el-research-provider) "<default>" ra-aid-el-research-provider))))
  (let ((selected-provider provider))
    (if (string= selected-provider "<default>")
        (customize-set-variable 'ra-aid-el-research-provider "")
      (customize-set-variable 'ra-aid-el-research-provider selected-provider))
    (message "RA.Aid Research provider set to: %s" (if (string-empty-p ra-aid-el-research-provider) "<default>" ra-aid-el-research-provider))))

(defun ra-aid-el-set-research-model (model)
  "Set the Research agent model."
  (interactive
   (list (read-string (format "Research Model (%s): " (if (string-empty-p ra-aid-el-research-model) "<default>" ra-aid-el-research-model))
                      ra-aid-el-research-model nil nil)))
  (customize-set-variable 'ra-aid-el-research-model (if (string-empty-p model) "" model))
  (message "RA.Aid Research model set to: %s" (if (string-empty-p ra-aid-el-research-model) "<default>" ra-aid-el-research-model)))

(defun ra-aid-el-set-planner-provider (provider)
  "Set the Planner agent provider."
  (interactive
   (list (completing-read (format "Planner Provider (%s): " (if (string-empty-p ra-aid-el-planner-provider) "<default>" ra-aid-el-planner-provider))
                          (cons "<default>" ra-aid-el-provider-list)
                          nil t nil nil (if (string-empty-p ra-aid-el-planner-provider) "<default>" ra-aid-el-planner-provider))))
  (let ((selected-provider provider))
    (if (string= selected-provider "<default>")
        (customize-set-variable 'ra-aid-el-planner-provider "")
      (customize-set-variable 'ra-aid-el-planner-provider selected-provider))
    (message "RA.Aid Planner provider set to: %s" (if (string-empty-p ra-aid-el-planner-provider) "<default>" ra-aid-el-planner-provider))))

(defun ra-aid-el-set-planner-model (model)
  "Set the Planner agent model."
  (interactive
   (list (read-string (format "Planner Model (%s): " (if (string-empty-p ra-aid-el-planner-model) "<default>" ra-aid-el-planner-model))
                      ra-aid-el-planner-model nil nil)))
  (customize-set-variable 'ra-aid-el-planner-model (if (string-empty-p model) "" model))
  (message "RA.Aid Planner model set to: %s" (if (string-empty-p ra-aid-el-planner-model) "<default>" ra-aid-el-planner-model)))

(defun ra-aid-el-set-expert-provider (provider)
  "Set the Expert agent provider."
  (interactive
   (list (completing-read (format "Expert Provider (%s): " (if (string-empty-p ra-aid-el-expert-provider) "<default>" ra-aid-el-expert-provider))
                          (cons "<default>" ra-aid-el-provider-list)
                          nil t nil nil (if (string-empty-p ra-aid-el-expert-provider) "<default>" ra-aid-el-expert-provider))))
  (let ((selected-provider provider))
    (if (string= selected-provider "<default>")
        (customize-set-variable 'ra-aid-el-expert-provider "")
      (customize-set-variable 'ra-aid-el-expert-provider selected-provider))
    (message "RA.Aid Expert provider set to: %s" (if (string-empty-p ra-aid-el-expert-provider) "<default>" ra-aid-el-expert-provider))))

(defun ra-aid-el-set-expert-model (model)
  "Set the Expert agent model."
  (interactive
   (list (read-string (format "Expert Model (%s): " (if (string-empty-p ra-aid-el-expert-model) "<default>" ra-aid-el-expert-model))
                      ra-aid-el-expert-model nil nil)))
  (customize-set-variable 'ra-aid-el-expert-model (if (string-empty-p model) "" model))
  (message "RA.Aid Expert model set to: %s" (if (string-empty-p ra-aid-el-expert-model) "<default>" ra-aid-el-expert-model)))

;; --- Setters for Numeric Values ---

(defun ra-aid-el-set-recursion-limit (limit)
  "Set the RA.Aid recursion limit."
  (interactive
   (list (read-number (format "Recursion Limit (current %d): " ra-aid-el-recursion-limit)
                      ra-aid-el-recursion-limit-default)))
  (customize-set-variable 'ra-aid-el-recursion-limit limit)
  (message "RA.Aid recursion limit set to: %d" limit))

(defun ra-aid-el-set-max-test-cmd-retries (retries)
  "Set the RA.Aid max test command retries."
  (interactive
   (list (read-number (format "Max Test Retries (current: %d): " ra-aid-el-max-test-cmd-retries)
                      ra-aid-el-max-test-cmd-retries-default)))
  (customize-set-variable 'ra-aid-el-max-test-cmd-retries retries)
  (message "RA.Aid max test retries set to: %d" retries))

(defun ra-aid-el-set-test-cmd-timeout (timeout)
  "Set the RA.Aid test command timeout."
  (interactive
   (list (read-number (format "Test Timeout (current: %d): " ra-aid-el-test-cmd-timeout)
                      ra-aid-el-test-cmd-timeout-default)))
  (customize-set-variable 'ra-aid-el-test-cmd-timeout timeout)
  (message "RA.Aid test command timeout set to: %d seconds" timeout))

;; --- Interactive Toggles ---

(defun ra-aid-el-use-aider ()
  "Toggle using Aider's editing capabilities."
  (interactive)
  (customize-set-variable 'ra-aid-el-use-aider (not ra-aid-el-use-aider))
  (message "RA.Aid use Aider mode %s" (if ra-aid-el-use-aider "enabled" "disabled")))

(defun ra-aid-el-toggle-research-only ()
  "Toggle research-only mode."
  (interactive)
  (customize-set-variable 'ra-aid-el-research-only (not ra-aid-el-research-only))
  (message "RA.Aid research-only mode %s" (if ra-aid-el-research-only "enabled" "disabled")))

(defun ra-aid-el-toggle-cowboy-mode ()
  "Toggle cowboy mode (skip confirmations)."
  (interactive)
  (customize-set-variable 'ra-aid-el-cowboy-mode (not ra-aid-el-cowboy-mode))
  (message "RA.Aid cowboy mode %s" (if ra-aid-el-cowboy-mode "enabled" "disabled")))

(defun ra-aid-el-toggle-hil ()
  "Toggle Human-in-the-Loop mode."
  (interactive)
  (customize-set-variable 'ra-aid-el-hil (not ra-aid-el-hil))
  (message "RA.Aid Human-in-the-Loop mode %s" (if ra-aid-el-hil "enabled" "disabled")))

(defun ra-aid-el-toggle-pretty-logger ()
  "Toggle the pretty logger."
  (interactive)
  (customize-set-variable 'ra-aid-el-pretty-logger (not ra-aid-el-pretty-logger))
  (message "RA.Aid pretty logger %s" (if ra-aid-el-pretty-logger "enabled" "disabled")))

(defun ra-aid-el-toggle-auto-test ()
  "Toggle automatic testing."
  (interactive)
  (customize-set-variable 'ra-aid-el-auto-test (not ra-aid-el-auto-test))
  (message "RA.Aid automatic testing %s" (if ra-aid-el-auto-test "enabled" "disabled")))

(defun ra-aid-el-toggle-wipe-project-memory ()
  "Toggle wiping project memory on start."
  (interactive)
  (customize-set-variable 'ra-aid-el-wipe-project-memory (not ra-aid-el-wipe-project-memory))
  (message "RA.Aid wipe project memory %s" (if ra-aid-el-wipe-project-memory "enabled" "disabled")))

(defun ra-aid-el-toggle-show-thoughts ()
  "Toggle showing agent thoughts."
  (interactive)
  (customize-set-variable 'ra-aid-el-show-thoughts (not ra-aid-el-show-thoughts))
  (message "RA.Aid show thoughts %s" (if ra-aid-el-show-thoughts "enabled" "disabled")))

(defun ra-aid-el-toggle-show-cost ()
  "Toggle showing estimated cost."
  (interactive)
  (customize-set-variable 'ra-aid-el-show-cost (not ra-aid-el-show-cost))
  (message "RA.Aid show cost %s" (if ra-aid-el-show-cost "enabled" "disabled")))

(defun ra-aid-el-toggle-track-cost ()
  "Toggle tracking accumulated cost."
  (interactive)
  (customize-set-variable 'ra-aid-el-track-cost (not ra-aid-el-track-cost))
  (message "RA.Aid track cost %s" (if ra-aid-el-track-cost "enabled" "disabled")))

(defun ra-aid-el-toggle-experimental-fallback-handler ()
  "Toggle experimental fallback handler."
  (interactive)
  (customize-set-variable 'ra-aid-el-experimental-fallback-handler (not ra-aid-el-experimental-fallback-handler))
  (message "RA.Aid experimental fallback handler %s" (if ra-aid-el-experimental-fallback-handler "enabled" "disabled")))

(defun ra-aid-el-toggle-disable-limit-tokens ()
  "Toggle disabling automatic token limiting."
  (interactive)
  (customize-set-variable 'ra-aid-el-disable-limit-tokens (not ra-aid-el-disable-limit-tokens))
  (message "RA.Aid disable token limiting %s" (if ra-aid-el-disable-limit-tokens "enabled" "disabled")))

;; --- End of interactive functions ---

(transient-define-prefix ra-aid-el-transient-menu ()
  "RA.Aid El Interface"
  ["RA.Aid Task Runner"
   ["Configuration" ;; Group for settings
    ("-p" "Set Provider" ra-aid-el-set-provider
     :transient t
     :description (lambda () (format "Provider: %s" (propertize ra-aid-el-provider 'face 'bold))))
    ("-m" "Set Model" ra-aid-el-set-model
     :transient t
     :description (lambda () (format "Model: %s" (propertize ra-aid-el-model 'face 'bold))))
    ("-e" "Set Temperature" ra-aid-el-set-temperature
     :transient t
     :description (lambda () (format "Temp: %s" (propertize (if (numberp ra-aid-el-temperature)
								(format "%.1f" ra-aid-el-temperature)
							        "None") 'face 'bold))))
    ("-L" "Set Log Level" ra-aid-el-set-log-level
     :transient t
     :description (lambda () (format "Log Lvl: %s" (propertize ra-aid-el-log-level 'face 'bold))))
    ("-g" "Set Log Mode" ra-aid-el-set-log-mode
     :transient t
     :description (lambda () (format "Log Mode: %s" (propertize ra-aid-el-log-mode 'face 'bold))))
    ("-T" "Set Test Cmd" ra-aid-el-set-test-cmd
     :transient t
     :description (lambda () (format "Test Cmd: %s" (propertize (if (string-empty-p ra-aid-el-test-cmd) "None" "Set") 'face 'bold))))
    ("-t" "Set Custom Tools Path" ra-aid-el-set-custom-tools-path
     :transient t
     :description (lambda () (format "Custom Tools Path: %s" (propertize (if (string-empty-p ra-aid-el-custom-tools-path) "None" "Set") 'face 'bold))))
    ;; Agent-specific settings
    ("-r" "Research Provider" ra-aid-el-set-research-provider
     :transient t
     :description (lambda () (format "Research Prov: %s" (propertize (if (string-empty-p ra-aid-el-research-provider) "Default" "Set") 'face 'bold))))
    ("-M" "Research Model" ra-aid-el-set-research-model
     :transient t
     :description (lambda () (format "Research Mode: %s" (propertize (if (string-empty-p ra-aid-el-research-model) "Default" "Set") 'face 'bold))))
    ("-N" "Planner Provider" ra-aid-el-set-planner-provider ;; Changed keybind from -p
     :transient t
     :description (lambda () (format "Planner Prov: %s" (propertize (if (string-empty-p ra-aid-el-planner-provider) "Default" "Set") 'face 'bold))))
    ("-P" "Planner Model" ra-aid-el-set-planner-model
     :transient t
     :description (lambda () (format "Planner Mode: %s" (propertize (if (string-empty-p ra-aid-el-planner-model) "Default" "Set") 'face 'bold))))
    ("-x" "Expert Provider" ra-aid-el-set-expert-provider
     :transient t
     :description (lambda () (format "Expert Prov: %s" (propertize (if (string-empty-p ra-aid-el-expert-provider) "Default" "Set") 'face 'bold))))
    ("-X" "Expert Model" ra-aid-el-set-expert-model
     :transient t
     :description (lambda () (format "Expert Mode: %s" (propertize (if (string-empty-p ra-aid-el-expert-model) "Default" "Set") 'face 'bold))))
    ;; Numeric settings
    ("-l" "Rec Limit" ra-aid-el-set-recursion-limit
     :transient t
     :description (lambda () (format "Recursion Limit: %s" (propertize (format "%d" ra-aid-el-recursion-limit) 'face 'bold))))
    ("-A" "Max Retries" ra-aid-el-set-max-test-cmd-retries ;; Changed keybind from -a
     :transient t
     :description (lambda () (format "Max Retries: %s" (propertize (format "%d" ra-aid-el-max-test-cmd-retries) 'face 'bold))))
    ("-U" "Test Timeout" ra-aid-el-set-test-cmd-timeout ;; Changed keybind from -t
     :transient t
     :description (lambda () (format "Test Timeout: %s" (propertize (format "%d" ra-aid-el-test-cmd-timeout) 'face 'bold))))
    ]
   ["Toggles" ;; Group for boolean flags
    ("a" "Aider Mode" ra-aid-el-use-aider
     :transient t
     :description (lambda () (format "Aider Mode: %s" (propertize (if ra-aid-el-use-aider "ON" "OFF") 'face 'bold))))
    ("R" "Research Only" ra-aid-el-toggle-research-only
     :transient t
     :description (lambda () (format "Research Only: %s" (propertize (if ra-aid-el-research-only "ON" "OFF") 'face 'bold))))
    ("C" "Cowboy Mode" ra-aid-el-toggle-cowboy-mode
     :transient t
     :description (lambda () (format "Cowboy Mode: %s" (propertize (if ra-aid-el-cowboy-mode "ON" "OFF") 'face 'bold))))
    ("H" "Human-in-Loop" ra-aid-el-toggle-hil
     :transient t
     :description (lambda () (format "Human-in-Loop: %s" (propertize (if ra-aid-el-hil "ON" "OFF") 'face 'bold))))
    ("P" "Pretty Logger" ra-aid-el-toggle-pretty-logger
     :transient t
     :description (lambda () (format "Pretty Logger: %s" (propertize (if ra-aid-el-pretty-logger "ON" "OFF") 'face 'bold))))
    ("t" "Auto Test" ra-aid-el-toggle-auto-test
     :transient t
     :description (lambda () (format "Auto Test: %s" (propertize (if ra-aid-el-auto-test "ON" "OFF") 'face 'bold))))
    ("W" "Wipe Memory" ra-aid-el-toggle-wipe-project-memory
     :transient t
     :description (lambda () (format "Wipe Memory: %s" (propertize (if ra-aid-el-wipe-project-memory "ON" "OFF") 'face 'bold))))
    ("o" "Show Thoughts" ra-aid-el-toggle-show-thoughts
     :transient t
     :description (lambda () (format "Show Thoughts: %s" (propertize (if ra-aid-el-show-thoughts "ON" "OFF") 'face 'bold))))
    ("S" "Show Cost" ra-aid-el-toggle-show-cost
     :transient t
     :description (lambda () (format "Show Cost: %s" (propertize (if ra-aid-el-show-cost "ON" "OFF") 'face 'bold))))
    ("k" "Track Cost" ra-aid-el-toggle-track-cost
     :transient t
     :description (lambda () (format "Track Cost: %s" (propertize (if ra-aid-el-track-cost "ON" "OFF") 'face 'bold))))
    ("f" "Exp Fallback" ra-aid-el-toggle-experimental-fallback-handler
     :transient t
     :description (lambda () (format "Exp Fallback: %s" (propertize (if ra-aid-el-experimental-fallback-handler "ON" "OFF") 'face 'bold))))
    ("d" "Disable Tokens" ra-aid-el-toggle-disable-limit-tokens
     :transient t
     :description (lambda () (format "Disable Tokens: %s" (propertize (if ra-aid-el-disable-limit-tokens "ON" "OFF") 'face 'bold))))
    ]
   ["Actions" ;; Group for actions
    ("r" "Run with Prompt" ra-aid-el--run-ra-aid)
    ("c" "Chat" ra-aid-el--run-chat)]]) ;; Added Chat action

;;;###autoload
(defun ra-aid-el-menu ()
  "Open the RA.Aid El transient menu to configure and run a task."
  (interactive)
  (ra-aid-el-transient-menu))

(provide 'ra-aid-el)

;;; ra-aid-el.el ends here
