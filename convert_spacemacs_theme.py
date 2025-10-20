import json
import re

def convert_emacs_color_to_vscode(color_hex):
    # If the color is already in 6-digit hex format, return as is
    if re.match(r'^#[0-9a-fA-F]{6}$', color_hex):
        return color_hex
    # If it's in 3-digit hex format, expand it
    elif re.match(r'^#[0-9a-fA-F]{3}$', color_hex):
        return '#' + ''.join(c*2 for c in color_hex[1:])
    return color_hex

def create_vscode_theme(name, type_variant, colors):
    # Basic VS Code theme structure
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

# Spacemacs Dark Colors
dark_colors = {
    "bg1": "#292b2e",
    "bg2": "#212026",
    "bg3": "#100a14",
    "bg4": "#0a0814",
    "base": "#b2b2b2",
    "base-dim": "#686868",
    "cursor": "#e3dedd",
    "const": "#a45bad",
    "comment": "#2aa1ae",
    "func": "#bc6ec5",
    "highlight": "#444155",
    "highlight-dim": "#3b314d",
    "keyword": "#4f97d7",
    "lnum": "#44505c",
    "str": "#2d9574",
    "type": "#ce537a",
    "var": "#7590db",
    "err": "#e0211d"
}

# Spacemacs Light Colors
light_colors = {
    "bg1": "#fbf8ef",
    "bg2": "#efeae9",
    "bg3": "#e3dedd",
    "bg4": "#d2ceda",
    "base": "#655370",
    "base-dim": "#a094a2",
    "cursor": "#100a14",
    "const": "#4e3163",
    "comment": "#2aa1ae",
    "func": "#6c3163",
    "highlight": "#d3d3e7",
    "highlight-dim": "#e7e7fc",
    "keyword": "#3a81c3",
    "lnum": "#a8a8bf",
    "str": "#2d9574",
    "type": "#ba2f59",
    "var": "#715ab1",
    "err": "#e0211d"
}

# Generate dark theme
dark_theme = create_vscode_theme("Spacemacs Dark", "dark", dark_colors)
with open('samples/output/spacemacs-dark-theme.json', 'w') as f:
    json.dump(dark_theme, f, indent=4)

# Generate light theme
light_theme = create_vscode_theme("Spacemacs Light", "light", light_colors)
with open('samples/output/spacemacs-light-theme.json', 'w') as f:
    json.dump(light_theme, f, indent=4)