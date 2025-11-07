# Emacs Themes for Visual Studio Code

AI coding agent specification for converting Emacs themes to VS Code format.

## Components

### Theme Dumper

- Location: `tools/dumper`
- Implementation: Emacs Lisp (Emacs v30+)
- Execution: Interactive function in Emacs graphical session
- Operation: 
  1. Takes a theme name as input
  2. Loads the theme
  3. Outputs face definitions to `emacs-definitions/{theme-name}.json`
  4. Each entry maps `faceName` to colors: `{"font-lock-property-use-face": {"fg": "#BA36A5", "bg": "cyan"}}`

```elisp
(defun list-theme-face-colors ()
  "List all faces and their foreground/background colors."
  (interactive)
  (with-output-to-temp-buffer "*Theme Faces*"
    (dolist (face (face-list))
      (let ((fg (face-attribute face :foreground nil t))
            (bg (face-attribute face :background nil t)))
        (princ (format "%-30s fg: %-12s bg: %-12s\n" face fg bg))))))
```

### Theme Converter

- Location: `tools/converter`
- Implementation: JavaScript (NodeJS v24)
- Input: Emacs theme face definitions (e.g., `emacs-definitions/leuven.json`)
- Output: VS Code theme (e.g., `samples/output/leuven.json`)

## Main Application

1. Converts all themes from `emacs-definitions/` using Theme Converter
2. Saves converted themes to `/vscode-extension/themes/{theme-name}.json`
3. Validates:
   - VS Code theme format compliance
   - Correct Dark/Light theme type
   - Theme registration in `package.json`
4. Maintains consistency:
   - Removes VS Code theme files when Emacs definition is deleted
   - Updates extension registration accordingly

## VS Code Extension

Publishes the converted themes for VS Code users.