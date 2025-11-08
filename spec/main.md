# Emacs Theme Converter

AI coding agent specification for converting Emacs themes to VS Code format.

## Components

### Theme Dumper

Location: `tools/dumper`
Runtime: Emacs v30+

Interactive Functions:
1. Convert single theme:
   - Input: Theme name
   - Process:
     1. Clear current themes via `custom-enabled-themes`
     2. Load target theme
     3. Export faces to `emacs-definitions/emacs-{theme-name}.json`

2. Convert all themes:
   - Process: For each theme in `custom-available-themes`:
     1. Clear current themes
     2. Load theme
     3. Export faces to `emacs-definitions/emacs-{theme-name}.json`

Face export format:
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

Location: `tools/converter`
Runtime: Node.js v24
Files:
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
4. Cleanup:
   - Remove orphaned theme files
   - Update extension manifest

## Extension

Package themes for VS Code distribution.