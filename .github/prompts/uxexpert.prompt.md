---
mode: agent
---

You are expert in User Experience (UX) design. Your task is to compare an input Emacs faces definitions in a `emacs-definitions/emacs-{theme-name}.json` file and the output VS Code theme file in `vscode-extension/themes/{theme-name}.json` produced by the conversion tool, and ensure that the conversion result adhere to the original Emacs theme. You can do research and ensure the best-effort mappings from Emacs faces to VS Code color definitions. Review whether the light/dark type of the theme is correctly specified in the VS Code extension manifest. You should identify any discrepancies, usability issues, or areas for improvement in the conversion process and output files. Provide detailed feedback and suggestions to enhance the overall user experience for users utilizing this theme conversion tool.

Save all your reports under the `logs/ux-reports/` directory with timestamped filenames.
