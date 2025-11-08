const fs = require('fs').promises;
const path = require('path');

const EMACS_DEFS_DIR = path.join(__dirname, '../../emacs-definitions');
const VSCODE_THEMES_DIR = path.join(__dirname, '../../vscode-extension/themes');
const PACKAGE_JSON_PATH = path.join(__dirname, '../../vscode-extension/package.json');

// Map of common Emacs faces to VS Code scopes
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
const EDITOR_COLORS = {
  'default': {
    'bg': 'editor.background',
    'fg': 'editor.foreground'
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
  'fringe': {
    'bg': 'editorGutter.background'
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
  
  // Handle named colors
  const namedColors = {
    'black': '#000000',
    'red': '#FF0000',
    'green': '#00FF00',
    'blue': '#0000FF',
    'yellow': '#FFFF00',
    'magenta': '#FF00FF',
    'cyan': '#00FFFF',
    'white': '#FFFFFF'
  };

  // Convert named colors to hex
  if (namedColors[color.toLowerCase()]) {
    return namedColors[color.toLowerCase()];
  }

  // Handle numeric colors (e.g., "color-123")
  if (color.startsWith('color-')) {
    return `#${parseInt(color.slice(6)).toString(16).padStart(6, '0')}`;
  }

  // Return as-is if it's already a hex color
  if (color.startsWith('#')) {
    return color;
  }

  return color;
}

function detectThemeType(themeData) {
  // Get background color from default face
  const defaultFace = themeData.default || {};
  const bg = defaultFace.bg || '#ffffff';
  
  // Convert hex to RGB and calculate luminance
  const hex = bg.replace('#', '');
  const r = parseInt(hex.substr(0, 2), 16);
  const g = parseInt(hex.substr(2, 2), 16);
  const b = parseInt(hex.substr(4, 2), 16);
  
  // Use perceived brightness formula
  const brightness = (r * 299 + g * 587 + b * 114) / 1000;
  
  return brightness < 128 ? 'dark' : 'light';
}

function inferScopes(faceName, faceData) {
  const scopes = [];
  
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

main().catch(console.err);