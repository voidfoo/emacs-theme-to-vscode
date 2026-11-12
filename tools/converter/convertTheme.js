// @ts-check

import { FACE_TO_SCOPE_MAP } from "./faceToScopeMap.js";
import {
  adjustColorLuminance,
  getContrastRatio,
  adjustColorForContrast,
  normalizeColor,
  withAlpha,
} from "./utils.js";

/** @type {Record<string, {fg?: string | string[]; bg?: string | string[]; }>} */
const EDITOR_COLORS = {
  default: {
    bg: ["editor.background", "panel.background", "editorGutter.background"],
    fg: "editor.foreground",
  },
  terminal: {
    bg: "terminal.background",
    fg: "terminal.foreground",
  },
  fringe: {
    // Don't map fringe background to gutter - fringe is for breakpoints/indicators,
    // not the line number gutter. Line-number face handles gutter styling.
  },
  sidebar: {
    bg: ["sideBar.background", "activityBar.background", "panel.background"],
  },
  cursor: {
    bg: "editorCursor.background",
    fg: "editorCursor.foreground",
  },
  region: {
    bg: "editor.selectionBackground",
    fg: "editor.selectionForeground",
  },
  highlight: {
    bg: "editor.findMatchBackground",
  },
  "secondary-selection": { bg: "editor.selectionHighlightBackground" },
  match: { bg: "editor.findMatchBackground" },
  isearch: { bg: "editor.findMatchBackground" },
  "lazy-highlight": { bg: "editor.findMatchHighlightBackground" },
  "show-paren-match": { bg: "editor.wordHighlightStrongBackground" },
  "line-number": {
    fg: "editorLineNumber.foreground",
    bg: "editorGutter.background",
  },
  "line-number-current-line": {
    fg: "editorLineNumber.activeForeground",
  },
  "minibuffer-prompt": {
    fg: [
      "input.placeholderForeground",
      "inlineChatInput.placeholderForeground",
    ],
  },
  "mode-line": {
    bg: ["statusBar.background", "statusBarItem.remoteBackground"],
    fg: ["statusBar.foreground", "statusBarItem.remoteForeground"],
  },
  "mode-line-inactive": {
    bg: "statusBar.noFolderBackground",
  },
  "vertical-border": {
    fg: "editorGroup.border",
  },
  "header-line": { bg: "editorGroupHeader.tabsBackground" },
  "diff-added": { bg: "diffEditor.insertedLineBackground" },
  "diff-removed": { bg: "diffEditor.removedLineBackground" },
  "diff-refine-added": { bg: "diffEditor.insertedTextBackground" },
  "diff-refine-removed": { bg: "diffEditor.removedTextBackground" },
  "treemacs-root-face": { fg: "breadcrumb.foreground" },
  "treemacs-git-modified-face": {
    fg: "gitDecoration.modifiedResourceForeground",
  },
  "treemacs-git-added-face": { fg: "gitDecoration.addedResourceForeground" },
  "treemacs-git-renamed-face": {
    fg: "gitDecoration.renamedResourceForeground",
  },
  "treemacs-git-deleted-face": {
    fg: "gitDecoration.deletedResourceForeground",
  },
  "treemacs-git-untracked-face": {
    fg: "gitDecoration.untrackedResourceForeground",
  },
  "treemacs-git-ignored-face": {
    fg: "gitDecoration.ignoredResourceForeground",
  },
  "company-tooltip": {
    bg: "editorSuggestWidget.background",
    fg: "editorSuggestWidget.foreground",
  },
  "company-scrollbar-bg": { bg: "scrollbarSlider.background" },
  "company-scrollbar-fg": { bg: "scrollbarSlider.hoverBackground" },
  "company-tooltip-selection": { bg: "editorSuggestWidget.selectedBackground" },
  "company-tooltip-common": { fg: "editorSuggestWidget.highlightForeground" },
};

/**
 * @param {{ [s: string]: any; "default": { fg: string; bg: string } }} themeData
 */
export function detectThemeType(themeData) {
  // Get background color from default face
  const defaultFace = themeData.default || {};
  const bg = normalizeColor(defaultFace.bg || "#ffffff");

  return getContrastRatio("#ffffff", bg) > getContrastRatio("#000000", bg)
    ? "dark"
    : "light";
}

/** @typedef {{ [s: string]: any; default: { fg: string; bg: string } }} EmacsTheme */
/** @typedef {{name: string, scope: string[], settings: {foreground?: string, background?: string, fontStyle?: string}}} TokenRule */
/** @typedef {{ name: string, type: string, colors: Record<string, string>, tokenColors: TokenRule[] }} VSCodeTheme */

/** @param {EmacsTheme} emacsTheme */
function computeBase(emacsTheme) {
  const type = detectThemeType(emacsTheme);
  return {
    type,
    defaultBg: normalizeColor(
      emacsTheme.default?.bg || (type === "dark" ? "#000000" : "#ffffff"),
    ),
    defaultFg: normalizeColor(
      emacsTheme.default?.fg || (type === "dark" ? "#ffffff" : "#000000"),
    ),
  };
}

/** @param {EmacsTheme} emacsTheme
 * @param {VSCodeTheme} vsCodeTheme */
function applyEditorColors(emacsTheme, vsCodeTheme) {
  Object.entries(EDITOR_COLORS).forEach(([faceName, colorMap]) => {
    const face = emacsTheme[faceName] || {};
    Object.entries(colorMap).forEach(([prop, vscodeProp]) => {
      (Array.isArray(vscodeProp) ? vscodeProp : [vscodeProp]).forEach(
        (vsProp) => {
          if (face[prop]) {
            vsCodeTheme.colors[vsProp] = normalizeColor(face[prop]);
          }
        },
      );
    });
  });

  for (const [face, kind] of Object.entries({
    "diff-hl-margin-insert": "inserted",
    "diff-hl-margin-delete": "removed",
    "diff-hl-margin-change": "modified",
  })) {
    const fg = emacsTheme[face]?.fg;
    if (fg) {
      vsCodeTheme.colors[`diffEditor.${kind}LineBackground`] ??= withAlpha(
        fg,
        0x30 / 255,
      );
      vsCodeTheme.colors[`diffEditor.${kind}TextBackground`] ??= withAlpha(
        fg,
        0x20 / 255,
      );
    }
  }
  /** @type {[string, number][]} */
  const transparentColors = [
    ["editor.selectionBackground", 0.25],
    ["editor.findMatchBackground", 0.5],
  ];
  for (const [key, alpha] of transparentColors) {
    if (vsCodeTheme.colors[key]) {
      vsCodeTheme.colors[key] = withAlpha(vsCodeTheme.colors[key], alpha);
    }
  }
}

/** @param {EmacsTheme} emacsTheme
 * @param {VSCodeTheme} vsCodeTheme
 * @param {ReturnType<typeof computeBase>} base */
function applyFallbacks(emacsTheme, vsCodeTheme, base) {
  const { type: effectiveType, defaultBg, defaultFg } = base;
  // Ensure sidebar/activityBar/panel backgrounds are always present.
  // If the Emacs theme doesn't define corresponding faces, compute a
  // reasonable fallback based on the theme's default background.
  const computedSidebarBg =
    effectiveType === "dark"
      ? adjustColorLuminance(defaultBg, 0.1)
      : adjustColorLuminance(defaultBg, -0.1);

  if (!vsCodeTheme.colors["sideBar.background"]) {
    vsCodeTheme.colors["sideBar.background"] = computedSidebarBg;
  }
  if (!vsCodeTheme.colors["activityBar.background"]) {
    vsCodeTheme.colors["activityBar.background"] = computedSidebarBg;
  }
  if (!vsCodeTheme.colors["panel.background"]) {
    vsCodeTheme.colors["panel.background"] = computedSidebarBg;
  }

  // Set activity bar foreground colors based on theme type
  if (!vsCodeTheme.colors["activityBar.foreground"]) {
    vsCodeTheme.colors["activityBar.foreground"] =
      effectiveType === "dark" ? "#cccccc" : "#333333";
  }
  if (!vsCodeTheme.colors["activityBar.inactiveForeground"]) {
    vsCodeTheme.colors["activityBar.inactiveForeground"] =
      effectiveType === "dark" ? "#888888" : "#666666";
  }

  // Ensure input/chat colors have reasonable fallbacks based on computed luminance
  // Don't use mode-line colors as they're designed for status bars, not input fields
  if (!vsCodeTheme.colors["input.background"]) {
    vsCodeTheme.colors["input.background"] = adjustColorLuminance(
      defaultBg,
      effectiveType === "dark" ? 0.08 : -0.12,
    );
  }
  if (!vsCodeTheme.colors["input.foreground"]) {
    vsCodeTheme.colors["input.foreground"] =
      getContrastRatio(defaultFg, vsCodeTheme.colors["input.background"]) >= 4.5
        ? defaultFg
        : adjustColorForContrast(
            defaultFg,
            vsCodeTheme.colors["input.background"],
          );
  }
  if (!vsCodeTheme.colors["input.placeholderForeground"]) {
    vsCodeTheme.colors["input.placeholderForeground"] = emacsTheme[
      "minibuffer-prompt"
    ]?.fg
      ? normalizeColor(emacsTheme["minibuffer-prompt"].fg)
      : adjustColorForContrast(defaultFg, defaultBg);
  }
  if (!vsCodeTheme.colors["inlineChatInput.background"]) {
    vsCodeTheme.colors["inlineChatInput.background"] =
      vsCodeTheme.colors["input.background"];
  }
  if (!vsCodeTheme.colors["inlineChatInput.foreground"]) {
    vsCodeTheme.colors["inlineChatInput.foreground"] =
      vsCodeTheme.colors["input.foreground"];
  }
  if (!vsCodeTheme.colors["inlineChatInput.placeholderForeground"]) {
    vsCodeTheme.colors["inlineChatInput.placeholderForeground"] =
      vsCodeTheme.colors["input.placeholderForeground"];
  }

  // Ensure cursor color has a foreground (for visibility)
  if (!vsCodeTheme.colors["editorCursor.foreground"]) {
    vsCodeTheme.colors["editorCursor.foreground"] = defaultFg;
  }

  // Ensure statusBar colors have appropriate defaults (use mode-line if available, otherwise compute)
  if (!vsCodeTheme.colors["statusBar.background"]) {
    vsCodeTheme.colors["statusBar.background"] = emacsTheme["mode-line"]?.bg
      ? normalizeColor(emacsTheme["mode-line"].bg)
      : adjustColorLuminance(
          defaultBg,
          effectiveType === "dark" ? 0.08 : -0.08,
        );
  }
  if (!vsCodeTheme.colors["statusBar.foreground"]) {
    vsCodeTheme.colors["statusBar.foreground"] = emacsTheme["mode-line"]?.fg
      ? normalizeColor(emacsTheme["mode-line"].fg)
      : getContrastRatio(
            defaultFg,
            vsCodeTheme.colors["statusBar.background"],
          ) >= 4.5
        ? defaultFg
        : adjustColorForContrast(
            defaultFg,
            vsCodeTheme.colors["statusBar.background"],
          );
  }
  if (!vsCodeTheme.colors["statusBar.noFolderBackground"]) {
    const statusBg = vsCodeTheme.colors["statusBar.background"];
    if (statusBg) {
      vsCodeTheme.colors["statusBar.noFolderBackground"] = withAlpha(
        statusBg,
        0.25,
      );
    }
  }

  // Ensure remote indicator button has appropriate colors (fallback to statusBar colors)
  if (
    !vsCodeTheme.colors["statusBarItem.remoteForeground"] &&
    vsCodeTheme.colors["statusBar.foreground"]
  ) {
    vsCodeTheme.colors["statusBarItem.remoteForeground"] =
      vsCodeTheme.colors["statusBar.foreground"];
  }
  if (
    !vsCodeTheme.colors["statusBarItem.remoteBackground"] &&
    vsCodeTheme.colors["statusBar.background"]
  ) {
    vsCodeTheme.colors["statusBarItem.remoteBackground"] =
      vsCodeTheme.colors["statusBar.background"];
  }
}

/** @param {EmacsTheme} emacsTheme
 * @param {VSCodeTheme} vsCodeTheme */
function buildTokenColors(emacsTheme, vsCodeTheme) {
  const defaultFg = normalizeColor(
    emacsTheme.default?.fg ||
      (vsCodeTheme.type === "dark" ? "#ffffff" : "#000000"),
  );
  for (const [faceName, scopes] of Object.entries(FACE_TO_SCOPE_MAP)) {
    const faceData = emacsTheme[faceName];
    if (!faceData) continue;

    // Handle token colors
    if (scopes.length > 0) {
      const settings = {};
      if (faceData.fg) settings.foreground = normalizeColor(faceData.fg);
      if (faceData.bg) settings.background = normalizeColor(faceData.bg);

      // Convert Emacs face attributes to VS Code settings
      if (faceData.weight === "bold") settings.fontStyle = "bold";
      if (faceData.slant === "italic") {
        settings.fontStyle = settings.fontStyle
          ? `${settings.fontStyle} italic`
          : "italic";
      }
      if (faceData.underline) settings.fontStyle = "underline";

      if (
        settings.foreground === defaultFg &&
        !settings.background &&
        !settings.fontStyle
      )
        continue;
      if (Object.keys(settings).length > 0) {
        vsCodeTheme.tokenColors.push({
          name: faceName,
          scope: scopes,
          settings,
        });
      }
    }
  }
}

/**
 * @param {EmacsTheme} emacsTheme
 * @param {string} themeName
 */
export function convertToVSCodeTheme(emacsTheme, themeName) {
  const base = computeBase(emacsTheme);
  /** @type {VSCodeTheme} */
  const vsCodeTheme = {
    name: themeName,
    type: base.type,
    colors: {},
    tokenColors: [],
  };
  applyEditorColors(emacsTheme, vsCodeTheme);
  applyFallbacks(emacsTheme, vsCodeTheme, base);
  buildTokenColors(emacsTheme, vsCodeTheme);
  return vsCodeTheme;
}
