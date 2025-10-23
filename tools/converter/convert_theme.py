#!/usr/bin/env python3

import json
import re
import os
from pathlib import Path

def convert_emacs_color_to_vscode(color):
    """Convert Emacs color format to VS Code hex format."""
    if not color or not isinstance(color, str):
        return None
        
    # If the color is already in 6-digit hex format, return as is
    if re.match(r'^#[0-9a-fA-F]{6}$', color):
        return color
    # If it's in 3-digit hex format, expand it
    elif re.match(r'^#[0-9a-fA-F]{3}$', color):
        return '#' + ''.join(c*2 for c in color[1:])
        
    # Try to extract color from Emacs face definition
    color_match = re.search(r'"(#[0-9a-fA-F]{6})"', color)
    if color_match:
        return color_match.group(1)
    
    return None

def is_theme_definition(content):
    """Check if this file actually defines a theme by looking for provide-theme."""
    return bool(re.search(r'\(provide-theme\s+\'[^)]+\)', content))

def read_theme_file(theme_path):
    """Read a theme file and all its required dependencies."""
    theme_dir = theme_path.parent
    content = []
    
    with open(theme_path, 'r') as f:
        file_content = f.read()
        content.append(file_content)
        
        # Look for requires
        requires = re.findall(r'\(require\s+\'([^)]+)\)', file_content)
        for req in requires:
            req_path = theme_dir / f"{req}.el"
            if req_path.exists():
                with open(req_path, 'r') as rf:
                    content.append(rf.read())
    
    return '\n'.join(content)

def parse_emacs_theme(content):
    """Parse an Emacs theme file and extract color definitions."""
    # Check if this is actually a theme definition
    if not is_theme_definition(content):
        return None, None
    
    # Determine if it's a dark theme
    is_dark = False
    if 'spacemacs-dark-theme' in content or "create-spacemacs-theme 'dark" in content:
        is_dark = True
    elif 'spacemacs-light-theme' in content or "create-spacemacs-theme 'light" in content:
        is_dark = False
    else:
        is_dark = 'dark' in content.lower() and not 'light' in content.lower()
        
    colors = {
        # Default colors for light theme
        "bg1": "#ffffff" if not is_dark else "#292b2e",
        "bg2": "#f0f0f0" if not is_dark else "#212026",
        "bg3": "#e8e8e8" if not is_dark else "#100a14",
        "bg4": "#d0d0d0" if not is_dark else "#0a0814",
        "base": "#000000" if not is_dark else "#b2b2b2",
        "base-dim": "#666666" if not is_dark else "#686868",
        "cursor": "#000000" if not is_dark else "#ffaa00",
        "const": "#800080" if not is_dark else "#a45bad",
        "comment": "#808080" if not is_dark else "#2aa1ae",
        "func": "#0000ff" if not is_dark else "#bc6ec5",
        "highlight": "#cccccc" if not is_dark else "#444155",
        "highlight-dim": "#dddddd" if not is_dark else "#3b314d",
        "keyword": "#0000ff" if not is_dark else "#4f97d7",
        "lnum": "#999999" if not is_dark else "#44505c",
        "str": "#008000" if not is_dark else "#2d9574",
        "type": "#800080" if not is_dark else "#ce537a",
        "var": "#000080" if not is_dark else "#7590db",
        "err": "#ff0000" if not is_dark else "#e0211d"
    }
    
    # Extract theme type
    is_dark = "dark" in content.lower() and not "light" in content.lower()
    
    # Regular expressions to find color definitions
    color_patterns = {
        # Basic colors
        r'default.*:background\s+"(#[0-9a-fA-F]+)"': "bg1",
        r'default.*:foreground\s+"(#[0-9a-fA-F]+)"': "base",
        r'cursor.*:background\s+"(#[0-9a-fA-F]+)"': "cursor",
        
        # Font-lock faces
        r'font-lock-constant-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "const",
        r'font-lock-comment-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "comment",
        r'font-lock-function-name-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "func",
        r'font-lock-keyword-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "keyword",
        r'font-lock-string-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "str",
        r'font-lock-type-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "type",
        r'font-lock-variable-name-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "var",
        r'font-lock-doc-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "doc",
        
        # UI elements
        r'region.*:background\s+"(#[0-9a-fA-F]+)"': "highlight",
        r'linum.*:foreground\s+"(#[0-9a-fA-F]+)"': "lnum",
        r'error.*:foreground\s+"(#[0-9a-fA-F]+)"': "err",
        r'mode-line.*:background\s+"(#[0-9a-fA-F]+)"': "statusbar-bg",
        r'mode-line.*:foreground\s+"(#[0-9a-fA-F]+)"': "statusbar-fg",
        r'fringe.*:background\s+"(#[0-9a-fA-F]+)"': "gutter-bg",
        r'fringe.*:foreground\s+"(#[0-9a-fA-F]+)"': "gutter-fg",
        r'minibuffer-prompt.*:background\s+"(#[0-9a-fA-F]+)"': "input-bg",
        r'minibuffer-prompt.*:foreground\s+"(#[0-9a-fA-F]+)"': "input-fg",
        
        # Diff faces
        r'diff-added.*:background\s+"(#[0-9a-fA-F]+)"': "diff-add",
        r'diff-removed.*:background\s+"(#[0-9a-fA-F]+)"': "diff-remove",
        r'diff-refine-added.*:background\s+"(#[0-9a-fA-F]+)"': "diff-add-highlight",
        r'diff-refine-removed.*:background\s+"(#[0-9a-fA-F]+)"': "diff-remove-highlight",
        
        # Search/match faces
        r'isearch.*:background\s+"(#[0-9a-fA-F]+)"': "search-bg",
        r'isearch.*:foreground\s+"(#[0-9a-fA-F]+)"': "search-fg",
        r'lazy-highlight.*:background\s+"(#[0-9a-fA-F]+)"': "search-highlight",
        
        # Special elements
        r'paren-matched.*:background\s+"(#[0-9a-fA-F]+)"': "bracket-match",
        r'paren-unmatched.*:background\s+"(#[0-9a-fA-F]+)"': "bracket-unmatch"
    }
    
    # Extract colors using patterns
    for pattern, key in color_patterns.items():
        match = re.search(pattern, content, re.IGNORECASE)
        if match:
            color = convert_emacs_color_to_vscode(match.group(1))
            if color:
                colors[key] = color
    
    # Derive missing colors
    if "bg2" not in colors:
        bg1_hex = colors["bg1"][1:]
        r = int(bg1_hex[0:2], 16)
        g = int(bg1_hex[2:4], 16)
        b = int(bg1_hex[4:6], 16)
        # Slightly darker/lighter than bg1
        delta = -16 if is_dark else 16
        colors["bg2"] = f"#{max(0, min(255, r + delta)):02x}{max(0, min(255, g + delta)):02x}{max(0, min(255, b + delta)):02x}"
    
    return colors, is_dark

def create_vscode_theme(name, type_variant, colors):
    """Create a VS Code theme from the extracted colors."""
    theme = {
        "name": name,
        "type": "dark" if "dark" in name.lower() else "light",
        "colors": {
            # Basic editor colors
            "editor.background": colors.get("bg1", "#ffffff"),
            "editor.foreground": colors.get("base", "#000000"),
            "editorCursor.background": colors.get("cursor", "#000000"),
            
            # Line highlighting and numbers
            "editor.lineHighlightBackground": colors.get("highlight", "#cccccc") + "40",
            "editorLineNumber.foreground": colors.get("lnum", "#999999"),
            "editorLineNumber.activeForeground": colors.get("base", "#000000"),
            
            # Selection and search
            "editor.selectionBackground": colors.get("highlight", "#cccccc"),
            "editor.selectionHighlightBackground": colors.get("highlight-dim", "#dddddd") + "60",
            "editor.findMatchBackground": colors.get("search-bg", "#ffff00"),
            "editor.findMatchHighlightBackground": colors.get("search-highlight", "#ffff00") + "60",
            
            # UI Elements
            "titleBar.activeBackground": colors.get("statusbar-bg", colors.get("bg2", "#f0f0f0")),
            "titleBar.activeForeground": colors.get("statusbar-fg", colors.get("base", "#000000")),
            "activityBar.background": colors.get("bg2", "#f0f0f0"),
            "activityBar.foreground": colors.get("keyword", "#0000ff"),
            "sideBar.background": colors.get("bg2", "#f0f0f0"),
            "sideBar.foreground": colors.get("base", "#000000"),
            "statusBar.background": colors.get("statusbar-bg", colors.get("bg2", "#f0f0f0")),
            "statusBar.foreground": colors.get("statusbar-fg", colors.get("base", "#000000")),
            
            # Input controls
            "input.background": colors.get("input-bg", colors.get("bg2", "#f0f0f0")),
            "input.foreground": colors.get("input-fg", colors.get("base", "#000000")),
            "input.placeholderForeground": colors.get("comment", "#808080"),
            
            # Diff editor colors
            "diffEditor.insertedTextBackground": colors.get("diff-add", "#e6ffed") + "60",
            "diffEditor.removedTextBackground": colors.get("diff-remove", "#ffeef0") + "60",
            "diffEditor.insertedLineBackground": colors.get("diff-add", "#e6ffed") + "40",
            "diffEditor.removedLineBackground": colors.get("diff-remove", "#ffeef0") + "40",
            
            # Editor widget colors
            "editorBracketMatch.background": colors.get("bracket-match", "#c0e8c3"),
            "editorBracketMatch.border": colors.get("bracket-unmatch", "#ff0000"),
            
            # Gutter
            "editorGutter.background": colors.get("gutter-bg", colors.get("bg1", "#ffffff")),
            "editorGutter.modifiedBackground": colors.get("diff-add-highlight", "#97f295"),
            "editorGutter.addedBackground": colors.get("diff-add", "#e6ffed"),
            "editorGutter.deletedBackground": colors.get("diff-remove", "#ffeef0"),
        },
        "tokenColors": [
            {
                "name": "Comments",
                "scope": ["comment", "punctuation.definition.comment"],
                "settings": {
                    "foreground": colors.get("comment", "#808080"),
                    "fontStyle": "italic"
                }
            },
            {
                "name": "Documentation",
                "scope": ["comment.documentation", "comment.block.documentation"],
                "settings": {
                    "foreground": colors.get("doc", "#008000"),
                    "fontStyle": "italic"
                }
            },
            {
                "name": "Constants",
                "scope": [
                    "constant",
                    "constant.numeric",
                    "constant.language",
                    "constant.character",
                    "constant.other"
                ],
                "settings": {
                    "foreground": colors.get("const", "#800080")
                }
            },
            {
                "name": "Strings",
                "scope": ["string", "string.quoted", "string.template"],
                "settings": {
                    "foreground": colors.get("str", "#008000")
                }
            },
            {
                "name": "Keywords",
                "scope": [
                    "keyword",
                    "keyword.control",
                    "storage.type",
                    "storage.modifier"
                ],
                "settings": {
                    "foreground": colors.get("keyword", "#0000ff"),
                    "fontStyle": "bold"
                }
            },
            {
                "name": "Functions",
                "scope": [
                    "entity.name.function",
                    "meta.function-call",
                    "meta.function",
                    "support.function"
                ],
                "settings": {
                    "foreground": colors.get("func", "#0000ff")
                }
            },
            {
                "name": "Classes",
                "scope": [
                    "entity.name.type",
                    "entity.name.class",
                    "entity.name.namespace",
                    "entity.other.inherited-class",
                    "support.class"
                ],
                "settings": {
                    "foreground": colors.get("type", "#800080"),
                    "fontStyle": "bold"
                }
            },
            {
                "name": "Variables",
                "scope": [
                    "variable",
                    "variable.other",
                    "variable.parameter",
                    "variable.language",
                    "support.variable"
                ],
                "settings": {
                    "foreground": colors.get("var", "#000080")
                }
            },
            {
                "name": "Parameters",
                "scope": ["variable.parameter"],
                "settings": {
                    "foreground": colors.get("var", "#000080"),
                    "fontStyle": "italic"
                }
            },
            {
                "name": "Error",
                "scope": ["invalid", "invalid.illegal"],
                "settings": {
                    "foreground": colors.get("err", "#ff0000")
                }
            }
        ],
        "semanticTokenColors": {
            "parameter": colors.get("var", "#000080"),
            "variable": colors.get("var", "#000080"),
            "property": colors.get("var", "#000080"),
            "function": colors.get("func", "#0000ff"),
            "method": colors.get("func", "#0000ff"),
            "class": colors.get("type", "#800080"),
            "enum": colors.get("type", "#800080"),
            "interface": colors.get("type", "#800080"),
            "typeParameter": colors.get("type", "#800080"),
            "type": colors.get("type", "#800080"),
            "comment": colors.get("comment", "#808080")
        }
    }
    return theme

def convert_theme(input_path):
    """Convert an Emacs theme file to VS Code format."""
    # Read the input theme and its dependencies
    input_path = Path(input_path)
    content = read_theme_file(input_path)
    
    # Parse the theme content
    colors, is_dark = parse_emacs_theme(content)
    
    # Skip if this file doesn't actually define a theme
    if colors is None:
        print(f"Skipping {input_path} - not a theme definition")
        return
    
    # Determine theme name from filename
    theme_name = Path(input_path).stem.replace('-theme', '').title()
    theme_type = "dark" if is_dark else "light"
    
    # Create VS Code theme
    theme = create_vscode_theme(theme_name, "dark" if is_dark else "light", colors)
    
    # Determine output path
    # Use the package name (parent directory name) as the output directory
    package_name = input_path.parent.name
    output_dir = Path(__file__).parent.parent.parent / 'vscode' / package_name
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / f"{theme_name.lower()}.json"
    
    # Save the theme
    with open(output_path, 'w') as f:
        json.dump(theme, f, indent=4)
    
    print(f"Converted {input_path} to {output_path}")

def main():
    # Get all theme files from emacs-themes directory
    emacs_themes_dir = Path(__file__).parent.parent.parent / 'emacs-themes'
    
    for theme_dir in emacs_themes_dir.iterdir():
        if not theme_dir.is_dir() or theme_dir.name == 'sources.json':
            continue
            
        # Convert each theme file
        for theme_file in theme_dir.glob('*-theme.el'):
            convert_theme(theme_file)

if __name__ == '__main__':
    main()