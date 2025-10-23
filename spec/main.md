# Emacs Themes for Visual Studio Code

AI coding agent specification.

## Supporting utilities

### Theme Converter

- This tool converts Emacs themes to VS Code themes. Given an input of Emacs theme defined in Emacs Lisp, it can convert it into a VS Code theme as close as possible to the original Emacs theme. An example input Elisp file is `samples/input/leuven-theme.el`.  The example output for it is `samples/output/leuven.json`.
- This tool lives under `tools/converter/` directory.


### Emacs theme definition updater

- This tool reads a mapping of Emacs theme packages and their GitHub locations from [sources.json](../emacs-themes/sources.json)
- It uses the `github_repo` tool to explore the repository and find the correct path to the theme definition files. A Emacs theme is defined with ELisp code `(provide-theme '{the name of the theme})`. The tool looks into the ELisp files to find defined themes.
- It fetches raw code of the Emacs theme definitions and other files required by the definitions from their source locations and updates the local copies under `emacs-themes/{theme-name}/`. An example package with a single theme definition is `emacs-themes/leuven/leuven-theme.el`. It is possible that one package includes multiple themes.  For example, `spacemacs-theme` include a dark theme and a light theme (`spacemacs-dark-theme.el` and `spacemacs-light-theme.el`). Their local copy should be saved under `emacs-themes/spacemacs/`.
- It then invokes the Theme Converter on each of the Emacs themes under `emacs-themes` and save the output files under `/vscode-extension/themes` directory. The output file name should have the format of `{theme-name}.json`. Examples include `/vscode-extension/themes/spacemacs-dark.json`, `/vscode-extension/themes/leuven.json`, and `/vscode-extension/themes/doom-one.json`.
- It then checks the conversion output to ensure the results are correct for Visual Studio Code themes and the theme Dark/Light type is properly set.
- For newly converted themes it updates Visual Studio Code extension's package.json to register them as well.
- This tool lives under `tools/updater`.

## Visual Studio Code extension

This is the Visual Studio Code extension that provides all the converted themes to Visual Studio Code users.