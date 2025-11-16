# Gutter Background Color Fix

## Problem

Two themes had visually off gutter (line number panel) backgrounds:
- **deeper-blue**: Gutter was pure black (#000000) instead of matching editor background (#181a26)
- **light-blue**: Gutter was gray85 (#D9D9D9) instead of matching editor background (#ADD8E6)

## Root Cause

The EDITOR_COLORS mapping was incorrectly configured:

1. `fringe` face was mapped to `editorGutter.background` 
   - Fringe is meant for breakpoint/diff indicators in the left margin, NOT the line number gutter
   - When `fringe.bg` was defined in Emacs themes, it would override the proper gutter color

2. `line-number` face had NO background mapping
   - The line-number face should map to `editorGutter.background`
   - When this mapping was missing, the editor default background was used or the fringe color would win

## Solution

**File**: `tools/converter/convert.js` (lines 35-50)

### Changes:

1. **Removed** `fringe.bg` → `editorGutter.background` mapping
   - Fringe now only affects its own visual indicators, not the gutter
   
2. **Added** `line-number.bg` → `editorGutter.background` mapping
   - The line-number face now properly controls gutter styling
   - Themes can define explicit line-number backgrounds if desired

```javascript
// Before
fringe: {
  bg: "editorGutter.background",
},
// ... no line-number background mapping

// After
fringe: {
  // Don't map fringe background to gutter - fringe is for breakpoints/indicators
},
// ...
"line-number": {
  fg: "editorLineNumber.foreground",
  bg: "editorGutter.background",  // ← Added!
},
```

## Results

### Fixed Themes

| Theme | Before | After | Status |
|-------|--------|-------|--------|
| **deeper-blue** | Black (#000000) | #181a26 (matches editor) | ✅ FIXED |
| **light-blue** | Gray85 (#D9D9D9) | #ADD8E6 (matches editor) | ✅ FIXED |

### Preserved Intentional Styling

Some themes like `spacemacs-dark` intentionally define different `line-number.bg` colors:
- Editor background: #292b2e
- Gutter background: #212026 (darker for visual distinction)
- **Result**: Correctly preserved ✓

### Overall Quality

- **Input contrast compliance**: Still 76.5% (78/102 themes pass)
- **No regressions**: All previously passing themes remain unchanged
- **Visual improvement**: Gutters now look correct and intentional

## Files Modified

- `tools/converter/convert.js` - Fixed EDITOR_COLORS mappings
- All 102 theme files regenerated

## Summary

✅ **Fixed** gutter background colors for deeper-blue and light-blue themes
✅ **Preserved** intentional gutter styling for themes that define custom line-number backgrounds
✅ **No impact** on input field contrast or other theme quality metrics
