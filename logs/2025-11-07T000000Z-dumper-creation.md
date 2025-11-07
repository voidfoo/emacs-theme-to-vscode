# Theme Conversion - 2025-11-07T00:00:00Z

Actions:
- Created Emacs theme face dumper tool in `tools/dumper/`
- Added interactive function `dump-theme-faces` for extracting theme colors
- Added menu item under Tools for easy access
- Created README with usage instructions

Files created:
- tools/dumper/dump-theme.el
- tools/dumper/README.md

## Implementation Details

The dumper tool:
1. Takes a theme name as input
2. Loads theme temporarily in session
3. Extracts explicit fg/bg colors for all faces
4. Writes JSON mapping face names to colors
5. Disables theme to restore previous state

## Testing

To test the dumper:
1. Add dumper directory to `load-path`
2. `(require 'dump-theme)`
3. `M-x dump-theme-faces`
4. Select a theme (e.g. "leuven")
5. Verify JSON output matches expected format