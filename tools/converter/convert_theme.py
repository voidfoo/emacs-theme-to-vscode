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

def parse_emacs_theme(content):
    """Parse an Emacs theme file and extract color definitions."""
    colors = {
        # Default colors
        "bg1": "#ffffff",
        "bg2": "#f0f0f0",
        "bg3": "#e8e8e8",
        "bg4": "#d0d0d0",
        "base": "#000000",
        "base-dim": "#666666",
        "cursor": "#000000",
        "const": "#800080",
        "comment": "#808080",
        "func": "#0000ff",
        "highlight": "#cccccc",
        "highlight-dim": "#dddddd",
        "keyword": "#0000ff",
        "lnum": "#999999",
        "str": "#008000",
        "type": "#800080",
        "var": "#000080",
        "err": "#ff0000"
    }
    
    # Extract theme type
    is_dark = "dark" in content.lower() and not "light" in content.lower()
    
    # Regular expressions to find color definitions
    color_patterns = {
        r'default.*:background\s+"(#[0-9a-fA-F]+)"': "bg1",
        r'default.*:foreground\s+"(#[0-9a-fA-F]+)"': "base",
        r'cursor.*:background\s+"(#[0-9a-fA-F]+)"': "cursor",
        r'font-lock-constant-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "const",
        r'font-lock-comment-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "comment",
        r'font-lock-function-name-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "func",
        r'region.*:background\s+"(#[0-9a-fA-F]+)"': "highlight",
        r'font-lock-keyword-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "keyword",
        r'linum.*:foreground\s+"(#[0-9a-fA-F]+)"': "lnum",
        r'font-lock-string-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "str",
        r'font-lock-type-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "type",
        r'font-lock-variable-name-face.*:foreground\s+"(#[0-9a-fA-F]+)"': "var",
        r'error.*:foreground\s+"(#[0-9a-fA-F]+)"': "err"
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
    # Use the same theme structure as in the Spacemacs converter
    theme = {
        "name": name,
        "type": type_variant,
        "colors": {
            # UI Colors
            "editor.background": colors["bg1"],
            "editor.foreground": colors["base"],
            "editor.lineHighlightBackground": colors["highlight"],
            "editorCursor.foreground": colors["cursor"],
            "editorLineNumber.foreground": colors["lnum"],
            "editorLineNumber.activeForeground": colors["base"],
            
            # UI Elements
            "titleBar.activeBackground": colors["bg2"],
            "titleBar.activeForeground": colors["base"],
            "activityBar.background": colors["bg2"],
            "activityBar.foreground": colors["keyword"],
            "sideBar.background": colors["bg2"],
            "sideBar.foreground": colors["base"],
            "statusBar.background": colors["bg2"],
            "statusBar.foreground": colors["base"],
            
            # Selection and highlighting
            "editor.selectionBackground": colors["highlight"],
            "editor.selectionHighlightBackground": colors["highlight-dim"],
            "editor.wordHighlightBackground": colors["highlight-dim"],
        },
        "tokenColors": [
            {
                "name": "Comments",
                "scope": ["comment", "punctuation.definition.comment"],
                "settings": {
                    "foreground": colors["comment"],
                    "fontStyle": "italic"
                }
            },
            {
                "name": "Constants",
                "scope": ["constant", "constant.numeric", "constant.language"],
                "settings": {
                    "foreground": colors["const"]
                }
            },
            {
                "name": "Strings",
                "scope": ["string"],
                "settings": {
                    "foreground": colors["str"]
                }
            },
            {
                "name": "Keywords",
                "scope": ["keyword", "storage.type", "storage.modifier"],
                "settings": {
                    "foreground": colors["keyword"],
                    "fontStyle": "bold"
                }
            },
            {
                "name": "Functions",
                "scope": ["entity.name.function", "meta.function-call"],
                "settings": {
                    "foreground": colors["func"],
                    "fontStyle": "bold"
                }
            },
            {
                "name": "Classes",
                "scope": ["entity.name.type", "entity.name.class", "entity.name.namespace"],
                "settings": {
                    "foreground": colors["type"],
                    "fontStyle": "bold"
                }
            },
            {
                "name": "Variables",
                "scope": ["variable", "variable.other"],
                "settings": {
                    "foreground": colors["var"]
                }
            },
            {
                "name": "Error",
                "scope": ["invalid", "invalid.illegal"],
                "settings": {
                    "foreground": colors["err"]
                }
            }
        ]
    }
    return theme

def convert_theme(input_path):
    """Convert an Emacs theme file to VS Code format."""
    # Read the input theme
    with open(input_path, 'r') as f:
        content = f.read()
    
    # Parse the theme content
    colors, is_dark = parse_emacs_theme(content)
    
    # Determine theme name from filename
    theme_name = Path(input_path).stem.replace('-theme', '').title()
    theme_type = "dark" if is_dark else "light"
    
    # Create VS Code theme
    theme = create_vscode_theme(theme_name, theme_type, colors)
    
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