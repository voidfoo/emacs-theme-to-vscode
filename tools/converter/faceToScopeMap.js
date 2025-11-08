// @ts-check
/** @type {Record<string, string[]>} */
export const FACE_TO_SCOPE_MAP = {
  // Basic syntax highlighting
  "font-lock-comment-face": ["comment", "punctuation.definition.comment"],
  "font-lock-comment-delimiter-face": [
    "comment",
    "punctuation.definition.comment",
  ],
  "font-lock-string-face": ["string", "string.quoted", "string.template"],
  "font-lock-doc-face": [
    "comment.documentation",
    "comment.block.documentation",
  ],
  "font-lock-keyword-face": [
    "keyword",
    "storage",
    "keyword.control",
    "storage.modifier",
  ],
  "font-lock-builtin-face": [
    "support.function",
    "support.class",
    "support.type",
  ],
  "font-lock-function-name-face": ["entity.name.function", "support.function"],
  "font-lock-variable-name-face": ["variable", "variable.other"],
  "font-lock-type-face": ["storage.type", "support.type", "entity.name.type"],
  "font-lock-constant-face": [
    "constant",
    "constant.language",
    "support.constant",
  ],
  "font-lock-preprocessor-face": [
    "meta.preprocessor",
    "keyword.control.import",
  ],
  "font-lock-warning-face": ["invalid.deprecated"],
  error: ["invalid.illegal"],

  // Editor UI
  default: ["editor.background", "editor.foreground"],
  region: ["editor.selectionBackground", "editor.selectionForeground"],
  highlight: ["editor.findMatchBackground"],
  "secondary-selection": ["editor.selectionHighlightBackground"],
  "trailing-whitespace": ["invalid.deprecated"],
  "line-number": ["editorLineNumber.foreground"],
  "line-number-current-line": ["editorLineNumber.activeForeground"],
  "line-number-major-tick": ["editorLineNumber.activeForeground"],

  // Git integration
  "diff-added": ["diff.insertedTextBackground"],
  "diff-removed": ["diff.removedTextBackground"],
  "diff-refine-added": ["diffEditor.insertedTextBackground"],
  "diff-refine-removed": ["diffEditor.removedTextBackground"],
  "diff-header": ["meta.diff.header"],
  "diff-file-header": ["meta.diff.header.file"],

  // Search and highlights
  match: ["editor.findMatchBackground"],
  isearch: [
    "editor.findMatchBackground",
    "editor.findMatchHighlightBackground",
  ],
  "lazy-highlight": ["editor.findMatchHighlightBackground"],
  "show-paren-match": ["editor.wordHighlightStrongBackground"],
  "show-paren-mismatch": ["invalid.illegal"],

  // Syntax specific
  "css-selector": [
    "entity.name.tag.css",
    "entity.other.attribute-name.class.css",
  ],
  "css-property": ["support.type.property-name.css"],
  "sgml-namespace": ["entity.name.tag.namespace"],

  // Tree and navigation
  "treemacs-root-face": [
    "breadcrumb.foreground",
    "tree.tableColumnHeader.foreground",
  ],
  "treemacs-directory-face": ["explorer.directoryForeground"],
  "treemacs-file-face": ["explorer.fileForeground"],
  "treemacs-git-modified-face": ["gitDecoration.modifiedResourceForeground"],
  "treemacs-git-added-face": ["gitDecoration.addedResourceForeground"],
  "treemacs-git-renamed-face": ["gitDecoration.renamedResourceForeground"],
  "treemacs-git-deleted-face": ["gitDecoration.deletedResourceForeground"],
  "treemacs-git-untracked-face": ["gitDecoration.untrackedResourceForeground"],
  "treemacs-git-ignored-face": ["gitDecoration.ignoredResourceForeground"],

  // Markdown and documentation
  "markdown-header-face": ["markup.heading"],
  "markdown-code-face": ["markup.fenced_code", "markup.inline.raw"],
  "markdown-url-face": ["markup.underline.link"],
  "markdown-list-face": ["markup.list"],

  // Interface elements
  "mode-line": ["statusBar.background", "statusBar.foreground"],
  "mode-line-inactive": ["statusBar.noFolderBackground"],
  "header-line": ["editorGroupHeader.tabsBackground"],
  "vertical-border": ["editorGroup.border"],
  fringe: ["editorGutter.background"],
  cursor: ["editorCursor.foreground"],

  // Completion and hints
  "company-tooltip": ["editorSuggestWidget.background"],
  "company-scrollbar-bg": ["scrollbarSlider.background"],
  "company-scrollbar-fg": ["scrollbarSlider.hoverBackground"],
  "company-tooltip-selection": ["editorSuggestWidget.selectedBackground"],
  "company-tooltip-common": ["editorSuggestWidget.highlightForeground"],
};
