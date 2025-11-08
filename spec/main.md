# Emacs Theme Converter

AI coding agent specification for converting Emacs themes to VS Code format.

## Components

### Theme Dumper

- Path: `tools/dumper`
- Runtime: Emacs v30+
- Mode: Interactive Emacs GUI function
- Process:
  1. Read theme name
  2. Clear themes via `custom-enabled-themes`
  3. Load target theme
  4. Write faces to `emacs-definitions/emacs-{theme-name}.json`

Face definition format:
```json
{
  "font-lock-property-use-face": {
    "fg": "#BA36A5",
    "bg": "cyan"
  }
}
```

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

- Path: `tools/converter`
- Runtime: Node.js v24
- Input: `emacs-definitions/emacs-{theme-name}.json`
- Output: `vscode-extension/themes/{theme-name}.json`
- Reference: `samples/` directory

## Pipeline

1. Read themes from `emacs-definitions/`
2. Convert using Theme Converter
3. Verify:
   - VS Code theme schema
   - Theme type (dark/light)
   - Extension manifest
4. Clean:
   - Remove orphaned theme files
   - Update extension index

## Extension

Package themes for VS Code distribution.