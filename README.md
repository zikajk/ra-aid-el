# ra-aid-el

An Emacs interface for `ra-aid`, an open-source AI assistant that combines research, planning, and implementation to help you build software faster and smarter.

For more information on the `ra-aid` tool itself, see the [official documentation](https://www.ra-aid.ai/).

## Installation

Assuming you use `straight.el` and `use-package`:

```emacs-lisp
(use-package ra-aid-el
  :straight (ra-aid-el :type git :host github :repo "zikajk/ra-aid-el") ;; Replace with your fork/repo if needed
  :bind (("C-c R" . ra-aid-el-menu)) ;; Example keybinding
  :config
  ;; Optional: Set configuration variables here if needed
  ;; (setq ra-aid-el-program "/path/to/your/ra-aid/executable")
  )
```

Ensure the `ra-aid` CLI tool is installed and accessible in your system's PATH or configure the `ra-aid-el-program` variable.

## Usage

1.  Invoke the main interface using the keybinding you set (e.g., `C-c R`) or by running `M-x ra-aid-el-menu`.
2.  This brings up a transient menu where you can configure various `ra-aid` options (provider, model, toggles like `--research-only`, etc.).
3.  From the menu, choose an action:
    *   **Run with Prompt (`r`):** Enter a specific task for `ra-aid` to perform. A new session buffer is created.
    *   **Use region or buffer as Prompt (`g`):** Starts a new task session using the currently selected region (or the whole buffer if no region is active) as the initial input.
    *   **Start Chat (`c`):** Starts a new interactive chat session buffer.
    *   **Send region or buffer to Chat (`b`):** Sends the currently selected region (or the whole buffer) to an existing chat session buffer. If the default chat buffer isn't found, it prompts you to select one.
4.  Output and interaction occur in a dedicated `*ra-aid-el-...*` buffer, using either the `comint` or `vterm` backend based on your configuration.


## Configuration

Options can be configured directly through the transient menu.

Alternatively, use Emacs' customization interface: `M-x customize-group RET ra-aid-el RET`.

Key variables include:
*   `ra-aid-el-program`: Path to the `ra-aid` executable if it's not in your PATH.
*   Variables for default provider, model, temperature, etc.

### Terminal Backend

You can choose the terminal emulator used to run `ra-aid` processes by customizing the `ra-aid-el-terminal-backend` variable.

*   **`comint` (Default):** Uses Emacs' built-in process interaction library. Works out-of-the-box.
*   **`vterm`:** Uses the `vterm` package for a more feature-rich terminal experience. Requires the `vterm` package to be installed separately (`M-x package-install RET vterm RET`).

Set it in your Emacs configuration like this:

```emacs-lisp
(setq ra-aid-el-terminal-backend 'vterm) ;; Or keep it 'comint
```

### Model List Customization

When you use the transient menu to set the model (`-m`), `ra-aid-el` tries to offer completions based on the currently selected provider. These completions are defined in the `ra-aid-el-provider-models` variable.

You can customize this variable to provide specific model lists relevant to your providers. It's an property list where keys are provider names (lowercase strings) and values are lists of model name strings.

Example configuration:

```emacs-lisp
;; In your Emacs config (e.g., init.el)
(setq ra-aid-el-provider-models
      '("openai" ("gpt-4o")
        "openrouter" ("google/gemini-2.5-pro-exp-03-25:free"
                      "google/gemini-2.5-pro-preview-03-25"
					  "deepseek/deepseek-chat-v3-0324")
        ;; Add other providers and their models as needed
        ))
```

If a provider selected via `ra-aid-el-set-provider` is not found in this list, the model prompt will allow free-form input without completion.
