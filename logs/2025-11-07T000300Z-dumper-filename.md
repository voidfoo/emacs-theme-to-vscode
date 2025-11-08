# Theme Conversion - 2025-11-07T000300Z

Update:
- Modified theme dumper to follow spec's filename format
- Now outputs to `emacs-definitions/emacs-{theme-name}.json`
- Example: theme "leuven" -> `emacs-definitions/emacs-leuven.json`

Changes:
1. Construct default output path with `emacs-` prefix
2. Set as default in file selection prompt
3. Keep full path as initial input for easy acceptance

Testing:
1. Run dumper with theme "leuven"
2. Verify default filename is `emacs-leuven.json`
3. Check file is created in `emacs-definitions/` directory