# Emacs Themes for Visual Studio Code

AI coding agent specification.

## Supporting utilities

### Emacs theme definition dumper

- The dumper tool lives under `tools/dumper`.

- The dumper tool is written in Emacs Lisp runs in Emacs v30 or later.

- The dumper tool is an interactive Emacs Lisp function that runs in an Emacs graphical session.

- It asks for a theme name, loads the theme, lists all face definitions and writes them into a json file under `emacs-definitions/{theme-name}.json`. Each entry should be a mapping of `faceName` and its foreground color and background color.  For example, `"font-lock-property-use-face": { "fg": "#BA36A5", "bg": "cyan" }`. Here's some example code:

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

- The dumper is manually executed by the user.

### Theme Converter

- The converter tool lives under `tools/converter/` directory.

- The converter tool is written in JavaScript and runs on NodeJS v24.

- The converter tool converts Emacs themes to VS Code themes. Given an input file of an Emacs theme face definitions, it converts it into a VS Code theme as close as possible to the original Emacs theme. An example input ELisp file is `emacs-definitions/leuven.json`.  The example output for it is `samples/output/leuven.json`.

## The App

- The app invokes the Theme Converter (described later) on each of the Emacs themes under `emacs-definitions/` and saves the conversion output files under `/vscode-extension/themes` directory. The output file name should have the format of `{theme-name}.json`. Examples of output file names include `/vscode-extension/themes/spacemacs-dark.json`, `/vscode-extension/themes/leuven.json`, and `/vscode-extension/themes/doom-one.json`.

- It then checks the conversion output to ensure the results are proper for Visual Studio Code themes and the theme Dark/Light type is properly set.

- It ensures all converted themes have registration in Visual Studio Code extension's `package.json`. If an Emacs definition file has been deleted, its corresponding Visual Studio Code theme definition file should be removed too, as well as the orphaned registration in the Visual Studio Code extension.

## Visual Studio Code extension

This is the Visual Studio Code extension that provides all the converted themes to Visual Studio Code users.