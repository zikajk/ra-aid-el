# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0] - 2025-04-22

### Added

-   **Region/Buffer Actions:**
    -   New command `ra-aid-el-send-region-or-buffer-as-prompt` ('g' in menu) to start a new task session using the selected text as the initial prompt.
    -   New command `ra-aid-el-send-region-or-buffer-to-chat` ('b' in menu) to send selected text to an existing chat session. Includes fallback to interactive buffer selection if the default chat buffer isn't found.
-   **Optional Vterm Backend:**
    -   New custom variable `ra-aid-el-terminal-backend` ('comint' or 'vterm') to choose the terminal emulator. Requires separate installation of `vterm` package if selected.
    -   Optional loading of `vterm` package added.
    -   New internal helper `ra-aid-el--start-process-in-terminal` to manage backend process creation.
-   **Model/Provider Improvements:**
    -   New custom variable `ra-aid-el-provider-models` (property list) to define available models per provider.
    -   `ra-aid-el-set-model` now offers model completions based on the current provider and the `ra-aid-el-provider-models` variable.
-   **Project Memory Inspection:**
    -   New command `ra-aid-el-inspect-project-memory` ('s' in menu) to open the project's `.ra-aid/pk.db` memory file using `sqlite-mode`.

### Changed

-   **Process Execution:**
    -   Refactored `ra-aid-el--run-ra-aid` and `ra-aid-el--run-chat` to use the new terminal backend helper.
    -   Refactored `ra-aid-el--build-common-args` to accept a `chatp` argument, allowing selective exclusion of arguments (e.g., `--research-only`) in chat mode.
	-   `ra-aid-el--get-buffer-name` now generates base names *without* leading/trailing asterisks. The process starting logic consumes this base name, allowing the chosen backend (`comint` or `vterm`) to apply standard naming conventions (like adding asterisks, which `vterm` does not do by default).
-   **Transient Menu:**
    -   Renamed action 'c' description from "Chat" to "Start Chat".
    -   Added new "Actions" group entries: 'g' ("Use region/buffer as Prompt") and 'b' ("Send region/buffer to Chat").
    -   Added new "Memory" group containing "Show Project Memory" ('s') and the moved "Wipe Memory" ('W') toggle.
-   **User Interaction:**
    -   `ra-aid-el-provider` custom definition now uses `:type choice` for better widget integration.
    -   Improved interactive prompt message in `ra-aid-el--send-to-chat`.
    -   Buffer filtering for `ra-aid-el--send-to-chat` confirmed to work with both `comint` and `vterm` buffers.

### Fixed

-   Corrected provider name typo in internal list ("antropic" -> "anthropic").

## [0.2.0] - 2025-04-18

### Added

- New customization option `ra-aid-el-custom-tools-path` to specify a file for custom Python tools (`--custom-tools` argument).
- Interactive setter (`ra-aid-el-set-custom-tools-path`) and **Transient** menu entry for the new option.

### Changed

- `ra-aid-el-temperature` can now be `nil` (unset). Default changed from `1.0` to `nil`.
- Refactored handling of default values for numeric options (`temperature`, `recursion-limit`, etc.) using dedicated `defconst ...-default` variables. Command arguments are now added only if the value differs from the defined default.
- Improved interactive setter for temperature (`ra-aid-el-set-temperature`) to allow unsetting the value (to `nil`) via empty input. Uses `read-string` and input parsing now instead of `read-number`.
- Improved string-based interactive setters (`set-model`, `set-test-cmd`, agent setters, etc.) to pre-fill the prompt with the current value as initial input for easier editing.
- Improved numeric interactive setters (`set-recursion-limit`, etc.) to show the current value in the prompt.
- Updated **Transient** menu displays for temperature and custom tools path to reflect their current state (set/unset/value).
- Agent-specific provider setters now use `<default>` in `completing-read` to explicitly unset the override.
- Removed unused dependencies.

### Fixed

- Ensure setting string options like `ra-aid-el-test-cmd` via interactive setters correctly handles empty input (sets to `""`, not `nil`).
- Corrected and updated the list of choices when setting agent-specific providers via `completing-read` (e.g., added 'groq', corrected capitalization).

## [0.1.0] - 2025-04-17

### Added

- Initial release of `ra-aid-el`.
- Core functionality to run the `ra-aid` command-line tool from within Emacs.
- Project context detection using **`vc-git.el`** to find the Git project root, falling back to the current directory. Run `ra-aid` commands within this root.
- `ra-aid-el--run-ra-aid`: Function to run `ra-aid` with a user-provided task prompt.
- `ra-aid-el--run-chat`: Function to start `ra-aid` in interactive chat mode.
- Output displayed in a dedicated `comint` buffer per project (`*ra-aid-el-task:...*` or `*ra-aid-el-chat:...*`).
- Extensive customization options (`defcustom`) mirroring `ra-aid` command-line arguments:
    - Basic: `ra-aid-el-program`, `ra-aid-el-provider`, `ra-aid-el-model`, `ra-aid-el-temperature` (default `1.0`), `ra-aid-el-log-level`, `ra-aid-el-log-mode`, `ra-aid-el-test-cmd`.
    - Agent-specific: Overrides for Research, Planner, and Expert agents (`...-provider`, `...-model`).
    - Numeric: `ra-aid-el-recursion-limit`, `ra-aid-el-max-test-cmd-retries`, `ra-aid-el-test-cmd-timeout`.
    - Boolean Toggles: Numerous flags like `--use-aider`, `--research-only`, `--cowboy-mode`, `--hil`, `--pretty-logger`, `--auto-test`, etc.
- Interactive setter functions (`ra-aid-el-set-...`) and toggle functions (`ra-aid-el-toggle-...`) for most customization options.
- **Transient** menu (`ra-aid-el-menu`) defined using `transient-define-prefix`, providing an interface to:
    - View current settings dynamically in descriptions.
    - Quickly modify settings using interactive setters/toggles (marked `:transient t`).
    - Trigger the "Run with Prompt" or "Chat" actions.
