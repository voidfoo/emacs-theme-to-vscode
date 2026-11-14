// @ts-check
/** @type {Record<string, string[]>} */
export const FACE_TO_SCOPE_MAP = {
  // Broad scopes precede more specific mappings.
  "font-lock-punctuation-face": ["punctuation"],
  "font-lock-bracket-face": ["punctuation.section", "meta.brace"],
  "font-lock-delimiter-face": [
    "punctuation.separator",
    "punctuation.terminator",
  ],
  "font-lock-misc-punctuation-face": ["punctuation.accessor"],
  "font-lock-comment-face": ["comment", "punctuation.definition.comment"],
  "font-lock-comment-delimiter-face": ["punctuation.definition.comment"],
  "font-lock-string-face": [
    "string",
    "string.quoted",
    "string.template",
    "punctuation.definition.string",
  ],
  "font-lock-doc-face": [
    "comment.documentation",
    "comment.block.documentation",
  ],
  "font-lock-doc-markup-face": [
    "comment.documentation storage.type",
    "entity.name.type.instance.jsdoc",
  ],
  "font-lock-keyword-face": [
    "keyword",
    "storage",
    "keyword.control",
    "storage.modifier",
  ],
  "font-lock-operator-face": ["keyword.operator"],
  "font-lock-negation-char-face": ["keyword.operator.logical"],
  "font-lock-builtin-face": ["support.function", "support.class"],
  // sgml-mode and html-ts-mode highlight tag names with this face.
  "font-lock-function-name-face": [
    "entity.name.function",
    "support.function",
    "entity.name.tag",
  ],
  "font-lock-function-call-face": [
    "meta.function-call entity.name.function",
    "support.function",
  ],
  "font-lock-variable-name-face": ["variable", "variable.other"],
  "font-lock-variable-use-face": ["variable.other.readwrite"],
  "font-lock-property-name-face": [
    "variable.other.property",
    "support.type.property-name",
    "support.type.vendored.property-name",
  ],
  "font-lock-property-use-face": [
    "variable.other.object.property",
    "meta.property",
  ],
  "font-lock-type-face": ["storage.type", "support.type", "entity.name.type"],
  "font-lock-constant-face": [
    "constant",
    "constant.language",
    "support.constant",
  ],
  "font-lock-number-face": ["constant.numeric"],
  "font-lock-escape-face": ["constant.character.escape"],
  "font-lock-regexp-face": ["string.regexp"],
  "font-lock-regexp-grouping-construct": [
    "punctuation.definition.group.regexp",
  ],
  "font-lock-regexp-grouping-backslash": [
    "constant.character.escape.backslash.regexp",
  ],
  "font-lock-preprocessor-face": [
    "meta.preprocessor",
    "keyword.control.import",
  ],
  "font-lock-warning-face": ["invalid.deprecated"],
  error: ["invalid.illegal"],
  // Trailing whitespace and paren mismatch highlights are not syntax scopes.
  "diff-header": ["meta.diff.header"],
  "diff-file-header": ["meta.diff.header.file"],
  "diff-added": ["markup.inserted"],
  "diff-removed": ["markup.deleted"],
  "diff-changed": ["markup.changed"],
  "css-selector": [
    "entity.name.tag.css",
    "entity.other.attribute-name.class.css",
  ],
  "css-property": ["support.type.property-name.css"],
  "sgml-namespace": ["entity.name.tag.namespace"],
  "markdown-header-face": ["markup.heading"],
  "markdown-code-face": ["markup.fenced_code", "markup.inline.raw"],
  "markdown-url-face": ["markup.underline.link"],
  "markdown-list-face": ["markup.list"],
};

/**
 * Faces whose scopes are nested under broader mapped selectors (for example,
 * `support.type.property-name` under `support.type`). Their rules are always
 * emitted with an explicit foreground and fontStyle so they never inherit the
 * broader rule's color or italics.
 */
export const EXPLICIT_FACES = new Set(["font-lock-property-name-face"]);
