const fs = require('fs').promises;
const path = require('path');

const EMACS_DEFS_DIR = path.join(__dirname, '../../emacs-definitions');
const VSCODE_THEMES_DIR = path.join(__dirname, '../../vscode-extension/themes');
const PACKAGE_JSON_PATH = path.join(__dirname, '../../vscode-extension/package.json');

// Map of common Emacs faces to VS Code scopes
// Helper function to calculate luminance
function getLuminance(hexColor) {
  const rgb = hexColor.match(/^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i);
  if (!rgb) return 0;
  const [r, g, b] = rgb.slice(1).map(x => parseInt(x, 16) / 255);
  return 0.2126 * r + 0.7152 * g + 0.0722 * b;
}

function adjustColorLuminance(hexColor, factor) {
  const rgb = hexColor.match(/^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i);
  if (!rgb) return hexColor;
  
  const [r, g, b] = rgb.slice(1).map(x => parseInt(x, 16));
  const adjust = (value) => {
    const newValue = Math.round(value * (1 + factor));
    return Math.min(255, Math.max(0, newValue));
  };
  
  const newR = adjust(r).toString(16).padStart(2, '0');
  const newG = adjust(g).toString(16).padStart(2, '0');
  const newB = adjust(b).toString(16).padStart(2, '0');
  
  return `#${newR}${newG}${newB}`;
}

// Helper function to calculate contrast ratio
function getContrastRatio(color1, color2) {
  const l1 = getLuminance(color1);
  const l2 = getLuminance(color2);
  const lighter = Math.max(l1, l2);
  const darker = Math.min(l1, l2);
  return (lighter + 0.05) / (darker + 0.05);
}

// Helper function to adjust color for minimum contrast
function adjustColorForContrast(foreground, background, minContrast = 4.5) {
  // Implementation of color adjustment logic
  // This is a placeholder - would need actual color manipulation code
  return foreground;
}

const FACE_TO_SCOPE_MAP = {
  // Basic syntax highlighting
  'font-lock-comment-face': ['comment', 'punctuation.definition.comment'],
  'font-lock-comment-delimiter-face': ['comment', 'punctuation.definition.comment'],
  'font-lock-string-face': ['string', 'string.quoted', 'string.template'],
  'font-lock-doc-face': ['comment.documentation', 'comment.block.documentation'],
  'font-lock-keyword-face': ['keyword', 'storage', 'keyword.control', 'storage.modifier'],
  'font-lock-builtin-face': ['support.function', 'support.class', 'support.type'],
  'font-lock-function-name-face': ['entity.name.function', 'support.function'],
  'font-lock-variable-name-face': ['variable', 'variable.other'],
  'font-lock-type-face': ['storage.type', 'support.type', 'entity.name.type'],
  'font-lock-constant-face': ['constant', 'constant.language', 'support.constant'],
  'font-lock-preprocessor-face': ['meta.preprocessor', 'keyword.control.import'],
  'font-lock-warning-face': ['invalid.deprecated'],
  'error': ['invalid.illegal'],
  
  // Editor UI
  'default': ['editor.background', 'editor.foreground'],
  'region': ['editor.selectionBackground', 'editor.selectionForeground'],
  'highlight': ['editor.findMatchBackground'],
  'secondary-selection': ['editor.selectionHighlightBackground'],
  'trailing-whitespace': ['invalid.deprecated'],
  'line-number': ['editorLineNumber.foreground'],
  'line-number-current-line': ['editorLineNumber.activeForeground'],
  'line-number-major-tick': ['editorLineNumber.activeForeground'],
  
  // Git integration
  'diff-added': ['diff.insertedTextBackground'],
  'diff-removed': ['diff.removedTextBackground'],
  'diff-refine-added': ['diffEditor.insertedTextBackground'],
  'diff-refine-removed': ['diffEditor.removedTextBackground'],
  'diff-header': ['meta.diff.header'],
  'diff-file-header': ['meta.diff.header.file'],
  
  // Search and highlights
  'match': ['editor.findMatchBackground'],
  'isearch': ['editor.findMatchBackground', 'editor.findMatchHighlightBackground'],
  'lazy-highlight': ['editor.findMatchHighlightBackground'],
  'show-paren-match': ['editor.wordHighlightStrongBackground'],
  'show-paren-mismatch': ['invalid.illegal'],
  
  // Syntax specific
  'css-selector': ['entity.name.tag.css', 'entity.other.attribute-name.class.css'],
  'css-property': ['support.type.property-name.css'],
  'sgml-namespace': ['entity.name.tag.namespace'],
  
  // Tree and navigation
  'treemacs-root-face': ['breadcrumb.foreground', 'tree.tableColumnHeader.foreground'],
  'treemacs-directory-face': ['explorer.directoryForeground'],
  'treemacs-file-face': ['explorer.fileForeground'],
  'treemacs-git-modified-face': ['gitDecoration.modifiedResourceForeground'],
  'treemacs-git-added-face': ['gitDecoration.addedResourceForeground'],
  'treemacs-git-renamed-face': ['gitDecoration.renamedResourceForeground'],
  'treemacs-git-deleted-face': ['gitDecoration.deletedResourceForeground'],
  'treemacs-git-untracked-face': ['gitDecoration.untrackedResourceForeground'],
  'treemacs-git-ignored-face': ['gitDecoration.ignoredResourceForeground'],
  
  // Markdown and documentation
  'markdown-header-face': ['markup.heading'],
  'markdown-code-face': ['markup.fenced_code', 'markup.inline.raw'],
  'markdown-url-face': ['markup.underline.link'],
  'markdown-list-face': ['markup.list'],
  
  // Interface elements
  'mode-line': ['statusBar.background', 'statusBar.foreground'],
  'mode-line-inactive': ['statusBar.noFolderBackground'],
  'header-line': ['editorGroupHeader.tabsBackground'],
  'vertical-border': ['editorGroup.border'],
  'fringe': ['editorGutter.background'],
  'cursor': ['editorCursor.foreground'],
  
  // Completion and hints
  'company-tooltip': ['editorSuggestWidget.background'],
  'company-scrollbar-bg': ['scrollbarSlider.background'],
  'company-scrollbar-fg': ['scrollbarSlider.hoverBackground'],
  'company-tooltip-selection': ['editorSuggestWidget.selectedBackground'],
  'company-tooltip-common': ['editorSuggestWidget.highlightForeground']
};

async function readEmacsTheme(filePath) {
  const data = await fs.readFile(filePath, 'utf8');
  return JSON.parse(data);
}

// Additional editor color mappings
const NAMED_COLORS = {
  // Standard base colors
  'black': '#000000',
  'red': '#FF0000',
  'green': '#00FF00',
  'yellow': '#FFFF00',
  'blue': '#0000FF',
  'magenta': '#FF00FF',
  'magenta1': '#FF00FF',
  'magenta2': '#EE00EE',
  'magenta3': '#CD00CD',
  'magenta4': '#8B008B',
  'cyan': '#00FFFF',
  'white': '#FFFFFF',

  // Grays with different spellings and variants
  'grey': '#BEBEBE',
  'gray': '#BEBEBE',
  'grey10': '#1A1A1A',
  'grey20': '#333333',
  'grey30': '#4D4D4D',
  'grey40': '#666666',
  'grey50': '#7F7F7F',
  'grey60': '#999999',
  'grey70': '#B3B3B3',
  'grey80': '#CCCCCC',
  'grey90': '#E6E6E6',
  'grey95': '#F2F2F2',
  'gray10': '#1A1A1A',
  'gray20': '#333333',
  'gray30': '#4D4D4D',
  'gray40': '#666666',
  'gray50': '#7F7F7F',
  'gray60': '#999999',
  'gray70': '#B3B3B3',
  'gray80': '#CCCCCC',
  'gray90': '#E6E6E6',
  'gray95': '#F2F2F2',
  'darkgray': '#A9A9A9',
  'darkgrey': '#A9A9A9',
  'dimgray': '#696969',
  'dimgrey': '#696969',
  'lightgray': '#D3D3D3',
  'lightgrey': '#D3D3D3',
  'gainsboro': '#DCDCDC',

  // Blues
  'blue1': '#0000FF',
  'blue2': '#0000EE',
  'blue3': '#0000CD',
  'blue4': '#00008B',
  'darkblue': '#00008B',
  'mediumblue': '#0000CD',
  'cornflowerblue': '#6495ED',
  'cornflower blue': '#6495ED',
  'deepskyblue': '#00BFFF',
  'deep sky blue': '#00BFFF',
  'dodgerblue': '#1E90FF',
  'dodger blue': '#1E90FF',
  'lightblue': '#ADD8E6',
  'light blue': '#ADD8E6',
  'lightskyblue': '#87CEFA',
  'lightsteelblue': '#B0C4DE',
  'light steel blue': '#B0C4DE',
  'midnightblue': '#191970',
  'midnight blue': '#191970',
  'royalblue': '#4169E1',
  'skyblue': '#87CEEB',
  'steelblue': '#4682B4',
  'steel blue': '#4682B4',

  // Greens
  'green1': '#00FF00',
  'green2': '#00EE00',
  'green3': '#00CD00',
  'green4': '#008B00',
  'darkgreen': '#006400',
  'dark green': '#006400',
  'forestgreen': '#228B22',
  'forest green': '#228B22',
  'limegreen': '#32CD32',
  'lime green': '#32CD32',
  'lightgreen': '#90EE90',
  'light green': '#90EE90',
  'palegreen': '#98FB98',
  'pale green': '#98FB98',
  'seagreen': '#2E8B57',
  'sea green': '#2E8B57',
  'springgreen': '#00FF7F',
  'spring green': '#00FF7F',
  'yellowgreen': '#9ACD32',
  'yellow green': '#9ACD32',
  'mediumseagreen': '#3CB371',
  'medium sea green': '#3CB371',

  // Reds and Pinks
  'red1': '#FF0000',
  'red2': '#EE0000',
  'red3': '#CD0000',
  'red4': '#8B0000',
  'darkred': '#8B0000',
  'dark red': '#8B0000',
  'darkorange': '#FF8C00',
  'darkorange1': '#FF7F00',
  'darkorange2': '#EE7600',
  'darkorange3': '#CD6600',
  'darkorange4': '#8B4500',
  'indianred': '#CD5C5C',
  'indian red': '#CD5C5C',
  'lightcoral': '#F08080',
  'hotpink': '#FF69B4',
  'hot pink': '#FF69B4',
  'deeppink': '#FF1493',
  'deep pink': '#FF1493',
  'palevioletred': '#DB7093',
  'lightsalmon': '#FFA07A',
  'light salmon': '#FFA07A',

  // Yellows and Browns
  'yellow1': '#FFFF00',
  'yellow2': '#EEEE00',
  'yellow3': '#CDCD00',
  'yellow4': '#8B8B00',
  'darkgoldenrod': '#B8860B',
  'dark goldenrod': '#B8860B',
  'goldenrod': '#DAA520',
  'lightyellow': '#FFFFE0',
  'light yellow': '#FFFFE0',
  'palegoldenrod': '#EEE8AA',
  'pale goldenrod': '#EEE8AA',
  'brown': '#A52A2A',
  'saddlebrown': '#8B4513',
  'sandybrown': '#F4A460',
  'sandy brown': '#F4A460',

  // Cyans and Turquoises
  'cyan1': '#00FFFF',
  'cyan2': '#00EEEE',
  'cyan3': '#00CDCD',
  'cyan4': '#008B8B',
  'darkcyan': '#008B8B',
  'dark cyan': '#008B8B',
  'lightcyan': '#E0FFFF',
  'paleturquoise': '#AFEEEE',
  'pale turquoise': '#AFEEEE',
  'darkturquoise': '#00CED1',
  'dark turquoise': '#00CED1',
  'turquoise': '#40E0D0',
  'mediumturquoise': '#48D1CC',
  'medium turquoise': '#48D1CC',

  // Purples and Violets
  'purple': '#A020F0',
  'violet': '#EE82EE',
  'darkviolet': '#9400D3',
  'mediumpurple': '#9370DB',
  'medium purple': '#9370DB',
  'blueviolet': '#8A2BE2',

  // Steel Blues
  'steelblue1': '#63B8FF',
  'steelblue2': '#5CACEE',
  'steelblue3': '#4F94CD',
  'steelblue4': '#36648B',
  
  // Sea Greens and Dark Sea Greens
  'seagreen1': '#54FF9F',
  'seagreen2': '#4EEE94',
  'seagreen3': '#43CD80',
  'seagreen4': '#2E8B57',
  'darkseagreen': '#8FBC8F',
  'darkseagreen1': '#C1FFC1',
  'darkseagreen2': '#B4EEB4',
  'darkseagreen3': '#9BCD9B',
  'darkseagreen4': '#698B69',
  
  // Royal Blues
  'royalblue1': '#4876FF',
  'royalblue2': '#436EEE',
  'royalblue3': '#3A5FCD',
  'royalblue4': '#27408B',
  
  // Dodger Blues
  'dodgerblue1': '#1E90FF',
  'dodgerblue2': '#1C86EE',
  'dodgerblue3': '#1874CD',
  'dodgerblue4': '#104E8B',
  
  // Sky Blues
  'skyblue1': '#87CEFF',
  'skyblue2': '#7EC0EE',
  'skyblue3': '#6CA6CD',
  'skyblue4': '#4A708B',
  
  // Deep Sky Blues
  'deepskyblue1': '#00BFFF',
  'deepskyblue2': '#00B2EE',
  'deepskyblue3': '#009ACD',
  'deepskyblue4': '#00688B',
  
  // Dark Orange
  'darkorange1': '#FF7F00',
  'darkorange2': '#EE7600',
  'darkorange3': '#CD6600',
  'darkorange4': '#8B4500',
  
  // More Gray Variants
  'gray12': '#1F1F1F',
  'gray25': '#404040',
  'gray26': '#424242',
  'gray31': '#4F4F4F',
  'gray75': '#BFBFBF',
  'gray85': '#D9D9D9',
  'gray88': '#E0E0E0',
  'grey45': '#737373',
  'grey75': '#BFBFBF',
  'grey85': '#D9D9D9',
  
  // Additional Colors
  'aquamarine': '#7FFFD4',
  'aquamarine1': '#7FFFD4',
  'aquamarine2': '#76EEC6',
  'aquamarine3': '#66CDAA',
  'aquamarine4': '#458B74',
  'cadetblue': '#5F9EA0',
  'cadetblue1': '#98F5FF',
  'cadetblue2': '#8EE5EE',
  'cadetblue3': '#7AC5CD',
  'cadetblue4': '#53868B',
  'cornsilk': '#FFF8DC',
  'coral1': '#FF7256',
  'coral2': '#EE6A50',
  'coral3': '#CD5B45',
  'coral4': '#8B3E2F',
  'darkkhaki': '#BDB76B',
  'dark khaki': '#BDB76B',
  'darkmagenta': '#8B008B',
  'dark magenta': '#8B008B',
  'darkslategray': '#2F4F4F',
  'dark slate gray': '#2F4F4F',
  'khaki1': '#FFF68F',
  'khaki2': '#EEE685',
  'khaki3': '#CDC673',
  'khaki4': '#8B864E',
  'lightseagreen': '#20B2AA',
  'light sea green': '#20B2AA',
  'lightgoldenrod1': '#FFEC8B',
  'lightpink1': '#FFB6C1',
  'lightskyblue1': '#B0E2FF',
  'orange': '#FFA500',
  'gold': '#FFD700',
  'gold1': '#FFD700',
  'gold2': '#EEC900',
  'gold3': '#CDAD00',
  'gold4': '#8B7500',
  'saddle brown': '#8B4513',
  'dim gray': '#696969',
  'pink': '#FFC0CB',
  'salmon1': '#FF8C69',
  'springgreen1': '#00FF7F',
  'springgreen2': '#00EE76',
  'springgreen3': '#00CD66',
  'springgreen4': '#008B45',
  
  // More numbered variants
  'chartreuse1': '#7FFF00',
  'chartreuse2': '#76EE00',
  'chartreuse3': '#66CD00',
  'chartreuse4': '#458B00',
  'burlywood1': '#FFD39B',
  'burlywood2': '#EEC591',
  'burlywood3': '#CDB38B',
  'burlywood4': '#8B7355',
  'paleturquoise1': '#BBFFFF',
  'paleturquoise2': '#AEEEEE',
  'paleturquoise3': '#96CDCD',
  'paleturquoise4': '#668B8B',
  'brown1': '#FF4040',
  'brown2': '#EE3B3B',
  'brown3': '#CD3333',
  'brown4': '#8B2323',
  'palevioletred1': '#FF82AB',
  'palevioletred2': '#EE799F',
  'palevioletred3': '#CD6889',
  'palevioletred4': '#8B475D',
  'darkolivegreen1': '#CAFF70',
  'darkolivegreen2': '#BCEE68',
  'darkolivegreen3': '#A2CD5A',
  'darkolivegreen4': '#6E8B3D',
  'goldenrod1': '#FFB90F',
  'goldenrod2': '#EEAD0E',
  'goldenrod3': '#CD950C',
  'goldenrod4': '#8B6508',

  // Special Colors
  'white smoke': '#F5F5F5',
  'whitesmoke': '#F5F5F5',
  'antiquewhite': '#FAEBD7',
  'beige': '#F5F5DC',
  'bisque': '#FFE4C4',
  'burlywood': '#DEB887',
  'chocolate': '#D2691E',
  'firebrick': '#B22222',
  'ivory': '#FFFFF0',
  'khaki': '#F0E68C',
  'maroon': '#B03060',
  'moccasin': '#FFE4B5',
  'orchid': '#DA70D6',
  'peru': '#CD853F',
  'plum': '#DDA0DD',
  'salmon': '#FA8072',
  'sienna': '#A0522D',
  'tan': '#D2B48C',
  'thistle': '#D8BFD8',
  'wheat': '#F5DEB3',
  // Common theme colors
  'SeaGreen4': '#2E8B57',
  'ForestGreen': '#228B22',
  'RoyalBlue3': '#3A5FCD',
  'light green': '#90EE90',
  'light salmon': '#FFA07A',
  'pale turquoise': '#AFEEEE',
  'sandy brown': '#F4A460',
  'indian red': '#CD5C5C',
  'midnight blue': '#191970',
  'cornflower blue': '#6495ED',
  
  // Additional Colors
  'chocolate1': '#FF7F24',
  'chocolate2': '#EE7621',
  'chocolate3': '#CD661D',
  'chocolate4': '#8B4513',
  'deeppink1': '#FF1493',
  'deeppink2': '#EE1289',
  'deeppink3': '#CD1076',
  'deeppink4': '#8B0A50',
  'mediumaquamarine': '#66CDAA',
  'orangered': '#FF4500',
  'orange red': '#FF4500',
  'mediumspringgreen': '#00FA9A',
  'rosybrown1': '#FFC1C1',
  'rosybrown2': '#EEB4B4',
  'rosybrown3': '#CD9B9B',
  'rosybrown4': '#8B6969',
  'lightslateblue': '#8470FF',
  'ivory1': '#FFFFF0',
  'ivory2': '#EEEEE0',
  'ivory3': '#CDCDC1',
  'ivory4': '#8B8B83',
  'grey65': '#A6A6A6',
  'old lace': '#FDF5E6',
  'dark gray': '#A9A9A9'
};

const EDITOR_COLORS = {
  'default': {
    'bg': 'editor.background',
    'fg': 'editor.foreground'
  },
  'terminal': {
    'bg': 'terminal.background',
    'fg': 'terminal.foreground'
  },
  'fringe': {
    'bg': 'editorGutter.background'
  },
  'sidebar': {
    'bg': ['sideBar.background', 'activityBar.background', 'panel.background']
  },
  'cursor': {
    'bg': 'editorCursor.background',
    'fg': 'editorCursor.foreground'
  },
  'region': {
    'bg': 'editor.selectionBackground',
    'fg': 'editor.selectionForeground'
  },
  'highlight': {
    'bg': 'editor.findMatchBackground'
  },
  'line-number': {
    'fg': 'editorLineNumber.foreground'
  },
  'line-number-current-line': {
    'fg': 'editorLineNumber.activeForeground'
  }
};

function normalizeColor(color) {
  if (!color) return undefined;

  // Convert named colors to hex
  if (typeof color === 'string') {
    const lowerColor = color.toLowerCase();
    
    // Check predefined colors first
    if (NAMED_COLORS[lowerColor]) {
      return NAMED_COLORS[lowerColor];
    }

    // Handle numeric colors (e.g., "color-123")
    if (color.startsWith('color-')) {
      const hexColor = `#${parseInt(color.slice(6)).toString(16).padStart(6, '0')}`;
      return hexColor;
    }

    // Return as-is if it's already a hex color
    if (color.startsWith('#')) {
      return color;
    }

    console.log(`Warning: Unknown color name "${color}"`);
  }

  return color;
}

function detectThemeType(themeData) {
  // Get background color from default face
  const defaultFace = themeData.default || {};
  const bg = normalizeColor(defaultFace.bg || '#ffffff');
  
  // Calculate luminance using more accurate formula
  const luminance = getLuminance(bg);
  
  return luminance < 0.5 ? 'dark' : 'light';
}

function processThemeColors(themeData) {
  const colors = {};
  const themeType = detectThemeType(themeData);
  const defaultBg = normalizeColor(themeData.default?.bg || (themeType === 'dark' ? '#000000' : '#ffffff'));
  const defaultFg = normalizeColor(themeData.default?.fg || (themeType === 'dark' ? '#FFFFFF' : '#000000'));
  
  // Set up base theme colors
  colors['editor.background'] = defaultBg;
  colors['editor.foreground'] = defaultFg;
  colors['terminal.background'] = defaultBg;
  colors['terminal.foreground'] = defaultFg;
  colors['sideBar.background'] = themeType === 'dark' ? adjustColorLuminance(defaultBg, 0.1) : adjustColorLuminance(defaultBg, -0.1);
  colors['activityBar.background'] = colors['sideBar.background'];
  colors['panel.background'] = colors['sideBar.background'];
  
  // Process each face and its mappings
  for (const [face, faceData] of Object.entries(themeData)) {
    if (!faceData) continue;

    // Convert colors and check contrast
    const fg = normalizeColor(faceData.fg);
    const bg = normalizeColor(faceData.bg);

    if (fg && EDITOR_COLORS[face]?.fg) {
      const targets = Array.isArray(EDITOR_COLORS[face].fg) 
        ? EDITOR_COLORS[face].fg 
        : [EDITOR_COLORS[face].fg];
      
      for (const target of targets) {
        // Adjust color if contrast is insufficient
        colors[target] = getContrastRatio(fg, defaultBg) < 4.5
          ? adjustColorForContrast(fg, defaultBg)
          : fg;
      }
    }

    if (bg && EDITOR_COLORS[face]?.bg) {
      const targets = Array.isArray(EDITOR_COLORS[face].bg)
        ? EDITOR_COLORS[face].bg
        : [EDITOR_COLORS[face].bg];
      
      for (const target of targets) {
        colors[target] = bg;
      }
    }
  }

  // Add selection and find match colors with transparency
  if (colors['editor.findMatchBackground']) {
    colors['editor.findMatchBackground'] += '80'; // 50% opacity
  }
  if (colors['editor.selectionBackground']) {
    colors['editor.selectionBackground'] += '40'; // 25% opacity
  }

  // Handle diff backgrounds specially
  const diffMappings = {
    'diff-hl-margin-insert': {
      fg: 'diffEditor.insertedLineBackground',
      bg: 'diffEditor.insertedTextBackground'
    },
    'diff-hl-margin-delete': {
      fg: 'diffEditor.removedLineBackground',
      bg: 'diffEditor.removedTextBackground'
    },
    'diff-hl-margin-change': {
      fg: 'diffEditor.modifiedLineBackground',
      bg: 'diffEditor.modifiedTextBackground'
    }
  };

  for (const [face, mapping] of Object.entries(diffMappings)) {
    if (themeData[face]) {
      const fgColor = normalizeColor(themeData[face].fg);
      if (fgColor) {
        colors[mapping.fg] = fgColor + '30'; // 19% opacity
        colors[mapping.bg] = fgColor + '20'; // 12% opacity
      }
    }
  }

  return colors;
}

function inferScopes(faceName, faceData) {
  const scopes = [];
  const defaultBg = normalizeColor(faceData.default?.bg || '#ffffff');
  
  // Common patterns to infer scopes
  const patterns = [
    { regex: /.*-keyword.*/, scope: 'keyword' },
    { regex: /.*-function.*/, scope: 'entity.name.function' },
    { regex: /.*-type.*/, scope: 'entity.name.type' },
    { regex: /.*-variable.*/, scope: 'variable' },
    { regex: /.*-constant.*/, scope: 'constant' },
    { regex: /.*-string.*/, scope: 'string' },
    { regex: /.*-comment.*/, scope: 'comment' },
    { regex: /.*-doc.*/, scope: 'comment.documentation' },
    { regex: /.*-error.*/, scope: 'invalid' },
    { regex: /.*warning.*/, scope: 'invalid.deprecated' },
    { regex: /.*-diff-add.*/, scope: 'markup.inserted' },
    { regex: /.*-diff-del.*/, scope: 'markup.deleted' },
    { regex: /.*-diff-changed.*/, scope: 'markup.changed' },
    { regex: /.*-html-tag.*/, scope: 'entity.name.tag' },
    { regex: /.*-css-selector.*/, scope: 'entity.other.attribute-name.class' },
    { regex: /.*-css-property.*/, scope: 'support.type.property-name' },
    { regex: /.*-regexp.*/, scope: 'string.regexp' }
  ];

  patterns.forEach(({ regex, scope }) => {
    if (regex.test(faceName)) {
      scopes.push(scope);
    }
  });

  return scopes;
}

function convertToVSCodeTheme(emacsTheme, themeName) {
  const type = detectThemeType(emacsTheme);
  
  const vsCodeTheme = {
    name: themeName,
    type,
    colors: {},
    tokenColors: []
  };

  // Process editor colors
  Object.entries(EDITOR_COLORS).forEach(([faceName, colorMap]) => {
    const face = emacsTheme[faceName] || {};
    Object.entries(colorMap).forEach(([prop, vscodeProp]) => {
      if (face[prop]) {
        vsCodeTheme.colors[vscodeProp] = normalizeColor(face[prop]);
      }
    });
  });

  // Process syntax highlighting and UI colors
  for (const [faceName, faceData] of Object.entries(emacsTheme)) {
    const scopes = FACE_TO_SCOPE_MAP[faceName] || [];
    
    // Handle editor colors
    if (faceName === 'default') {
      if (faceData.fg) vsCodeTheme.colors['editor.foreground'] = normalizeColor(faceData.fg);
      if (faceData.bg) vsCodeTheme.colors['editor.background'] = normalizeColor(faceData.bg);
      continue;
    }

    // Handle token colors
    if (scopes.length > 0) {
      const settings = {};
      if (faceData.fg) settings.foreground = normalizeColor(faceData.fg);
      if (faceData.bg) settings.background = normalizeColor(faceData.bg);
      
      // Convert Emacs face attributes to VS Code settings
      if (faceData.weight === 'bold') settings.fontStyle = 'bold';
      if (faceData.slant === 'italic') {
        settings.fontStyle = settings.fontStyle 
          ? `${settings.fontStyle} italic` 
          : 'italic';
      }
      if (faceData.underline) settings.fontStyle = 'underline';

      if (Object.keys(settings).length > 0) {
        vsCodeTheme.tokenColors.push({
          name: faceName,
          scope: scopes,
          settings
        });
      }
    }

    // Try to infer additional scopes based on face name patterns
    const inferredScopes = inferScopes(faceName, faceData);
    if (inferredScopes.length > 0) {
      const settings = {};
      if (faceData.fg) settings.foreground = normalizeColor(faceData.fg);
      if (faceData.bg) settings.background = normalizeColor(faceData.bg);

      vsCodeTheme.tokenColors.push({
        name: `${faceName} (inferred)`,
        scope: inferredScopes,
        settings
      });
    }
  }

  return vsCodeTheme;
}

async function updatePackageJson(themes) {
  const packageJson = JSON.parse(await fs.readFile(PACKAGE_JSON_PATH, 'utf8'));
  
  // Update contributes.themes section
  packageJson.contributes = packageJson.contributes || {};
  packageJson.contributes.themes = themes.map(theme => ({
    label: theme.name,
    uiTheme: theme.type === 'dark' ? 'vs-dark' : 'vs',
    path: `./themes/${theme.name}.json`
  }));

  await fs.writeFile(PACKAGE_JSON_PATH, JSON.stringify(packageJson, null, 2));
}

async function main() {
  try {
    const files = await fs.readdir(EMACS_DEFS_DIR);
    const themePromises = [];
    const convertedThemes = [];

    // Ensure output directory exists
    await fs.mkdir(VSCODE_THEMES_DIR, { recursive: true });

    for (const file of files) {
      if (!file.endsWith('.json')) continue;

      const filePath = path.join(EMACS_DEFS_DIR, file);
      const themeName = file.replace('emacs-', '').replace('.json', '');
      
      const emacsTheme = await readEmacsTheme(filePath);
      const vsCodeTheme = convertToVSCodeTheme(emacsTheme, themeName);
      convertedThemes.push(vsCodeTheme);

      const outputPath = path.join(VSCODE_THEMES_DIR, `${themeName}.json`);
      await fs.writeFile(outputPath, JSON.stringify(vsCodeTheme, null, 2));
    }

    // Update package.json with theme entries
    await updatePackageJson(convertedThemes);

    console.log('Theme conversion completed successfully');
  } catch (error) {
    console.error('Error during theme conversion:', error);
    process.exit(1);
  }
}

main().catch(console.error);