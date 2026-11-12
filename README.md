# Emacs Themes for VS Code

This project converts Emacs theme faces into a VS Code extension containing
matching editor, workbench, and syntax colors.

```text
Emacs themes -> face dumper -> emacs-definitions/*.json
             -> Node.js converter -> vscode-extension/themes/*.json
             -> VS Code extension
```

## Dump themes

Install Emacs 30 or later and the desired theme packages, including
`doom-themes`, `ef-themes`, and `modus-themes`. In a graphical Emacs session,
load `tools/dumper/dump-theme.el`, then run `M-x dump-theme-faces` for one
theme or `M-x dump-all-theme-faces` for all available themes. Run from this
repository so the dumper can find `emacs-definitions`.

Use the themes' default settings; the dumper does not enable bold or italic
options. Only regenerate the checked-in face dumps in a human-managed Emacs
environment with all required theme families installed. See
[`tools/dumper/README.md`](tools/dumper/README.md).

## Convert and test

With Node.js 24 and pnpm 10 installed, from `tools/converter` run:

```powershell
pnpm install --frozen-lockfile
pnpm check
pnpm test
node convert.js
```

The converter regenerates the extension themes and manifest. Commit those
outputs together with changes to the converter or face dumps.

## Preview in VS Code

Open the repository in VS Code, select **Run and Debug**, and start the
**Extension** launch configuration. In the Extension Development Host, select
a converted theme via **Preferences: Color Theme** and inspect source files.

## Add a theme family

Install the Emacs package, make its themes available to Emacs, and dump them
as described above. Run the converter and check the new theme entries and
generated files. Update the extension documentation and changelog as needed;
add explicit face mappings in the converter when the family exposes useful
faces not already covered.
