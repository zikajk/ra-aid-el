# ra-aid-el

An Emacs interface for `ra-aid`, an AI-powered pair programming assistant.

For more information on the `ra-aid` tool itself, see the [official documentation](https://github.com/rafayal/ra-aid).

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
    *   **Run with Prompt (`r`):** Enter a specific task for `ra-aid` to perform.
    *   **Chat (`c`):** Start an interactive chat session with `ra-aid`.
4.  Output and interaction occur in a dedicated `*ra-aid-el-...*` buffer.

## Configuration

Options can be configured directly through the transient menu.

Alternatively, use Emacs' customization interface: `M-x customize-group RET ra-aid-el RET`.

Key variables include:
*   `ra-aid-el-program`: Path to the `ra-aid` executable if it's not in your PATH.
*   Variables for default provider, model, temperature, etc.
