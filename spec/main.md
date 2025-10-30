# Emacs Themes for Visual Studio Code

AI coding agent specification.

## Supporting utilities

### Theme Converter

- This tool lives under `tools/converter/` directory.
- This tool is written in JavaScript and runs on NodeJS v24.
- This tool converts Emacs themes to VS Code themes. Given an input file of an Emacs theme face definitions, it can convert it into a VS Code theme as close as possible to the original Emacs theme. An example input Elisp file is `samples/input/leuven-theme.json`.  The example output for it is `samples/output/leuven.json`.


### Emacs theme definition dumper

- This tool lives under `tools/dumper`.
- This tool is written in JavaScript and runs on NodeJS v24.
- This tool is an interactive Emacs Lisp function that runs in an Emacs graphical session.
- It asks for a theme name, load the theme, lists all face definitions and writes them into a json file under `emacs-definitions/{theme-name}.json`. Each entry should be a mapping of `faceName` and its foreground color and background color.  For example, `"font-lock-property-use-face": { "fg": "#BA36A5", "bg": "cyan" }`.
- It then invokes the Theme Converter on each of the Emacs themes under `emacs-themes` and save the output files under `/vscode-extension/themes` directory. The output file name should have the format of `{theme-name}.json`. Examples include `/vscode-extension/themes/spacemacs-dark.json`, `/vscode-extension/themes/leuven.json`, and `/vscode-extension/themes/doom-one.json`.
- It then checks the conversion output to ensure the results are correct for Visual Studio Code themes and the theme Dark/Light type is properly set.
- For newly converted themes it updates Visual Studio Code extension's package.json to register them as well. If an Emacs definition file has been deleted, its corresponding Visual Studio Code theme definition should be removed as well as well as its registration in the Visual Studio Code extension.

## Visual Studio Code extension

This is the Visual Studio Code extension that provides all the converted themes to Visual Studio Code users.