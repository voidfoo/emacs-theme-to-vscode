# Emacs Themes for Visual Studio Code

AI coding agent specification.

## Supporting utilities

### Theme Converter

This tool converts Emacs themes to VS Code themes. Given an input of Emacs theme defined in Emacs Lisp, it can convert it into a VS Code theme as close as possible to the original Emacs theme. An example input Elisp file is `samples/input/leuven-theme.el`.  The example output for it is `samples/output/leuven.json`. This tool lives under `tools/converter/` directory.


### Emacs theme definition updater

- This tool reads a mapping of Emacs theme packages and their GitHub locations from [sources.json](../emacs-themes/sources.json)
- It fetches raw code of the Emacs theme definitions and other files required by the definitions from their source locations and updates the local copies under `emacs-themes/{theme-name}/`. An example package with a single theme definition is `emacs-themes/leuven/leuven-theme.el`. It is possible that one package includes multiple themes.  For example, `spacemacs-theme` include a dark theme and a light theme (`spacemacs-dark-theme.el` and `spacemacs-light-theme.el`). Their local copy should be saved under `emacs-themes/spacemacs/`.
- It then invokes the Theme Converter on each of the Emacs themes under `emacs-themes` and save the output files under `vscode/` directory. An example is `vscode/leuven/leuven.json`.
- This tool lives under `tools/updater`.

## Visual Studio Code extension

This is the Visual Studio Code extension that provides all the converted themes to Visual Studio Code users.