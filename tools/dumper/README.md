# Emacs Theme Dumper

This tool extracts face definitions from Emacs themes and saves them as JSON files for VS Code theme conversion.

## Requirements

- Emacs 30 or later
- Graphical Emacs session (not terminal)

## Usage

1. Add to your Emacs init file:
   ```elisp
   (add-to-list 'load-path "/path/to/tools/dumper")
   (require 'dump-theme)
   ```

2. Run Emacs graphically and either:
   - Select Tools > Dump Theme Faces from the menu
   - `M-x dump-theme-faces`

3. Select a theme name from the completion list and confirm
4. Choose output location (defaults to `emacs-definitions/{theme-name}.json`)

## Output Format

The tool creates a JSON file mapping face names to their colors:

```json
{
  "font-lock-comment-face": {
    "fg": "#8D8D84"
  },
  "font-lock-string-face": {
    "fg": "#008000",
    "bg": "#FFFFFF"
  }
}
```

Only faces with explicit foreground or background colors are included.