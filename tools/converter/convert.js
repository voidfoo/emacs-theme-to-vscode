// @ts-check

import { promises as fs } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

import { FACE_TO_SCOPE_MAP } from "./faceToScopeMap.js";
import { NAMED_COLORS } from "./namedColors.js";
import {
  getLuminance,
  adjustColorLuminance,
  getContrastRatio,
  adjustColorForContrast,
  readEmacsTheme,
} from "./utils.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const EMACS_DEFS_DIR = join(__dirname, "../../emacs-definitions");
const VSCODE_THEMES_DIR = join(__dirname, "../../vscode-extension/themes");
const PACKAGE_JSON_PATH = join(
  __dirname,
  "../../vscode-extension/package.json",
);

/** @type {Record<string, {fg?: string | string[]; bg?: string | string[]; }>} */
const EDITOR_COLORS = {
  default: {
    bg: [
      "editor.background",
      "panel.background",
      "editorGutter.background",
    ],
    fg: "editor.foreground",
  },
  terminal: {
    bg: "terminal.background",
    fg: "terminal.foreground",
  },
  fringe: {
    bg: "editorGutter.background",
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
  "line-number": {
    fg: "editorLineNumber.foreground",
  },
  "line-number-current-line": {
    fg: "editorLineNumber.activeForeground",
  },
  "minibuffer-prompt": {
    fg: [
      "input.placeholderForeground",
      "inlineChatInput.placeholderForeground",
    ],
    bg: ["input.background", "inlineChatInput.background"],
  },
  "mode-line": {
    bg: "statusBar.background",
    fg: "statusBar.foreground",
  },
  "mode-line-inactive": {
    bg: "statusBar.noFolderBackground",
  },
  "vertical-border": {
    fg: "editorGroup.border",
  },
};

/**
 * @param {string} color
 */
function normalizeColor(color) {
  if (!color) {
    throw new Error("Color is invalid");
  }

  // Convert named colors to hex
  if (typeof color === "string") {
    const lowerColor = color.toLowerCase();

    // Check predefined colors first
    if (NAMED_COLORS[lowerColor]) {
      return NAMED_COLORS[lowerColor];
    }

    // Handle numeric colors (e.g., "color-123")
    if (color.startsWith("color-")) {
      const hexColor = `#${parseInt(color.slice(6)).toString(16).padStart(6, "0")}`;
      return hexColor;
    }

    // Return as-is if it's already a hex color
    if (color.startsWith("#")) {
      return color;
    }

    console.log(`Warning: Unknown color name "${color}"`);
  }

  return color;
}

/**
 * @param {{ [s: string]: any; "default": { fg: string; bg: string } }} themeData
 */
function detectThemeType(themeData) {
  // Get background color from default face
  const defaultFace = themeData.default || {};
  const bg = normalizeColor(defaultFace.bg || "#ffffff");

  // Calculate luminance using more accurate formula
  const luminance = getLuminance(bg);

  return luminance < 0.5 ? "dark" : "light";
}

/**
 * @param {{ [s: string]: any; "default": { fg: string; bg: string } }} themeData
 */
function processThemeColors(themeData) {
  /** @type {Record<string, string | undefined >} */
  const colors = {};
  const themeType = detectThemeType(themeData);
  const defaultBg = normalizeColor(
    themeData.default?.bg || (themeType === "dark" ? "#000000" : "#ffffff"),
  );
  const defaultFg = normalizeColor(
    themeData.default?.fg || (themeType === "dark" ? "#FFFFFF" : "#000000"),
  );

  // Set up base theme colors
  colors["editor.background"] = defaultBg;
  colors["editor.foreground"] = defaultFg;
  colors["terminal.background"] = defaultBg;
  colors["terminal.foreground"] = defaultFg;

  // Always set sidebar colors with appropriate contrast
  const sidebarBg =
    themeType === "dark"
      ? adjustColorLuminance(defaultBg, 0.1) // Make slightly lighter in dark themes
      : adjustColorLuminance(defaultBg, -0.1); // Make slightly darker in light themes
  colors["sideBar.background"] = sidebarBg;
  colors["activityBar.background"] = sidebarBg;
  colors["panel.background"] = sidebarBg;

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
        colors[target] =
          getContrastRatio(fg, defaultBg) < 4.5
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
  if (colors["editor.findMatchBackground"]) {
    colors["editor.findMatchBackground"] += "80"; // 50% opacity
  }
  if (colors["editor.selectionBackground"]) {
    colors["editor.selectionBackground"] += "40"; // 25% opacity
  }

  // Ensure input/chat colors have reasonable fallbacks based on computed luminance
  // Don't use mode-line colors as they're designed for status bars, not input fields
  if (!colors["input.background"]) {
    colors["input.background"] = adjustColorLuminance(
      defaultBg,
      themeType === "dark" ? 0.08 : -0.12
    );
  }
  if (!colors["input.foreground"]) {
    colors["input.foreground"] = getContrastRatio(defaultFg, colors["input.background"]) >= 4.5
      ? defaultFg
      : adjustColorForContrast(defaultFg, colors["input.background"]);
  }
  if (!colors["input.placeholderForeground"]) {
    colors["input.placeholderForeground"] = themeData["minibuffer-prompt"]?.fg
      ? normalizeColor(themeData["minibuffer-prompt"].fg)
      : adjustColorForContrast(defaultFg, defaultBg);
  }
  if (!colors["inlineChatInput.background"]) {
    colors["inlineChatInput.background"] = colors["input.background"];
  }
  if (!colors["inlineChatInput.foreground"]) {
    colors["inlineChatInput.foreground"] = colors["input.foreground"];
  }
  if (!colors["inlineChatInput.placeholderForeground"]) {
    colors["inlineChatInput.placeholderForeground"] = colors["input.placeholderForeground"];
  }

  // Ensure cursor color has a foreground (for visibility)
  if (!colors["editorCursor.foreground"]) {
    colors["editorCursor.foreground"] = defaultFg;
  }

  // Ensure statusBar colors have appropriate defaults (use mode-line if available, otherwise compute)
  if (!colors["statusBar.background"]) {
    colors["statusBar.background"] = themeData["mode-line"]?.bg
      ? normalizeColor(themeData["mode-line"].bg)
      : adjustColorLuminance(defaultBg, themeType === "dark" ? 0.08 : -0.08);
  }
  if (!colors["statusBar.foreground"]) {
    colors["statusBar.foreground"] = themeData["mode-line"]?.fg
      ? normalizeColor(themeData["mode-line"].fg)
      : getContrastRatio(defaultFg, colors["statusBar.background"]) >= 4.5
      ? defaultFg
      : adjustColorForContrast(defaultFg, colors["statusBar.background"]);
  }
  if (!colors["statusBar.noFolderBackground"]) {
    const statusBg = colors["statusBar.background"];
    if (statusBg) {
      colors["statusBar.noFolderBackground"] = statusBg + "40"; // 25% opacity
    }
  }

  // Handle diff backgrounds specially
  const diffMappings = {
    "diff-hl-margin-insert": {
      fg: "diffEditor.insertedLineBackground",
      bg: "diffEditor.insertedTextBackground",
    },
    "diff-hl-margin-delete": {
      fg: "diffEditor.removedLineBackground",
      bg: "diffEditor.removedTextBackground",
    },
    "diff-hl-margin-change": {
      fg: "diffEditor.modifiedLineBackground",
      bg: "diffEditor.modifiedTextBackground",
    },
  };

  for (const [face, mapping] of Object.entries(diffMappings)) {
    if (themeData[face]) {
      const fgColor = normalizeColor(themeData[face].fg);
      if (fgColor) {
        colors[mapping.fg] = fgColor + "30"; // 19% opacity
        colors[mapping.bg] = fgColor + "20"; // 12% opacity
      }
    }
  }

  return colors;
}

/**
 * @param {string} faceName
 * @param {{ default: { bg: any; }; }} faceData
 */
function inferScopes(faceName, faceData) {
  /**
   * @type {string[]}
   */
  const scopes = [];
  const defaultBg = normalizeColor(faceData.default?.bg || "#ffffff");

  // Common patterns to infer scopes
  const patterns = [
    { regex: /.*-keyword.*/, scope: "keyword" },
    { regex: /.*-function.*/, scope: "entity.name.function" },
    { regex: /.*-type.*/, scope: "entity.name.type" },
    { regex: /.*-variable.*/, scope: "variable" },
    { regex: /.*-constant.*/, scope: "constant" },
    { regex: /.*-string.*/, scope: "string" },
    { regex: /.*-comment.*/, scope: "comment" },
    { regex: /.*-doc.*/, scope: "comment.documentation" },
    { regex: /.*-error.*/, scope: "invalid" },
    { regex: /.*warning.*/, scope: "invalid.deprecated" },
    { regex: /.*-diff-add.*/, scope: "markup.inserted" },
    { regex: /.*-diff-del.*/, scope: "markup.deleted" },
    { regex: /.*-diff-changed.*/, scope: "markup.changed" },
    { regex: /.*-html-tag.*/, scope: "entity.name.tag" },
    { regex: /.*-css-selector.*/, scope: "entity.other.attribute-name.class" },
    { regex: /.*-css-property.*/, scope: "support.type.property-name" },
    { regex: /.*-regexp.*/, scope: "string.regexp" },
  ];

  patterns.forEach(({ regex, scope }) => {
    if (regex.test(faceName)) {
      scopes.push(scope);
    }
  });

  return scopes;
}

/**
 * @param {{ [s: string]: any; "default": { fg: string; bg: string } }}  emacsTheme
 * @param {string} themeName
 */
function convertToVSCodeTheme(emacsTheme, themeName) {
  const type = detectThemeType(emacsTheme);

  /** @type {{ name: string, type: string; colors: Record<string, string>; tokenColors: any[] }} */
  const vsCodeTheme = {
    name: themeName,
    type,
    colors: {},
    tokenColors: [],
  };

  // Process editor colors
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

  // Ensure sidebar/activityBar/panel backgrounds are always present.
  // If the Emacs theme doesn't define corresponding faces, compute a
  // reasonable fallback based on the theme's default background.
  const effectiveType = detectThemeType(emacsTheme);
  const effectiveDefaultBg = normalizeColor(
    emacsTheme.default?.bg ||
      (effectiveType === "dark" ? "#000000" : "#ffffff"),
  );
  const computedSidebarBg =
    effectiveType === "dark"
      ? adjustColorLuminance(effectiveDefaultBg, 0.1)
      : adjustColorLuminance(effectiveDefaultBg, -0.1);

  if (!vsCodeTheme.colors["sideBar.background"]) {
    vsCodeTheme.colors["sideBar.background"] = computedSidebarBg;
  }
  if (!vsCodeTheme.colors["activityBar.background"]) {
    vsCodeTheme.colors["activityBar.background"] = computedSidebarBg;
  }
  if (!vsCodeTheme.colors["panel.background"]) {
    vsCodeTheme.colors["panel.background"] = computedSidebarBg;
  }

  // Get default theme values for fallbacks
  const defaultBg = normalizeColor(
    emacsTheme.default?.bg ||
      (effectiveType === "dark" ? "#000000" : "#ffffff"),
  );
  const defaultFg = normalizeColor(
    emacsTheme.default?.fg ||
      (effectiveType === "dark" ? "#FFFFFF" : "#000000"),
  );

  // Ensure input/chat colors have reasonable fallbacks based on computed luminance
  // Don't use mode-line colors as they're designed for status bars, not input fields
  if (!vsCodeTheme.colors["input.background"]) {
    vsCodeTheme.colors["input.background"] = adjustColorLuminance(
      defaultBg,
      effectiveType === "dark" ? 0.08 : -0.12
    );
  }
  if (!vsCodeTheme.colors["input.foreground"]) {
    vsCodeTheme.colors["input.foreground"] = getContrastRatio(
      defaultFg,
      vsCodeTheme.colors["input.background"]
    ) >= 4.5
      ? defaultFg
      : adjustColorForContrast(defaultFg, vsCodeTheme.colors["input.background"]);
  }
  if (!vsCodeTheme.colors["input.placeholderForeground"]) {
    vsCodeTheme.colors["input.placeholderForeground"] = emacsTheme["minibuffer-prompt"]?.fg
      ? normalizeColor(emacsTheme["minibuffer-prompt"].fg)
      : adjustColorForContrast(defaultFg, defaultBg);
  }
  if (!vsCodeTheme.colors["inlineChatInput.background"]) {
    vsCodeTheme.colors["inlineChatInput.background"] = vsCodeTheme.colors["input.background"];
  }
  if (!vsCodeTheme.colors["inlineChatInput.foreground"]) {
    vsCodeTheme.colors["inlineChatInput.foreground"] = vsCodeTheme.colors["input.foreground"];
  }
  if (!vsCodeTheme.colors["inlineChatInput.placeholderForeground"]) {
    vsCodeTheme.colors["inlineChatInput.placeholderForeground"] = vsCodeTheme.colors["input.placeholderForeground"];
  }

  // Ensure cursor color has a foreground (for visibility)
  if (!vsCodeTheme.colors["editorCursor.foreground"]) {
    vsCodeTheme.colors["editorCursor.foreground"] = defaultFg;
  }

  // Ensure statusBar colors have appropriate defaults (use mode-line if available, otherwise compute)
  if (!vsCodeTheme.colors["statusBar.background"]) {
    vsCodeTheme.colors["statusBar.background"] = emacsTheme["mode-line"]?.bg
      ? normalizeColor(emacsTheme["mode-line"].bg)
      : adjustColorLuminance(defaultBg, effectiveType === "dark" ? 0.08 : -0.08);
  }
  if (!vsCodeTheme.colors["statusBar.foreground"]) {
    vsCodeTheme.colors["statusBar.foreground"] = emacsTheme["mode-line"]?.fg
      ? normalizeColor(emacsTheme["mode-line"].fg)
      : getContrastRatio(defaultFg, vsCodeTheme.colors["statusBar.background"]) >= 4.5
      ? defaultFg
      : adjustColorForContrast(defaultFg, vsCodeTheme.colors["statusBar.background"]);
  }
  if (!vsCodeTheme.colors["statusBar.noFolderBackground"]) {
    const statusBg = vsCodeTheme.colors["statusBar.background"];
    if (statusBg) {
      vsCodeTheme.colors["statusBar.noFolderBackground"] = statusBg + "40"; // 25% opacity
    }
  }
  if (!vsCodeTheme.colors["statusBar.noFolderBackground"]) {
    const statusBg = vsCodeTheme.colors["statusBar.background"];
    if (statusBg) {
      vsCodeTheme.colors["statusBar.noFolderBackground"] = statusBg + "40"; // 25% opacity
    }
  }
  for (const [faceName, faceData] of Object.entries(emacsTheme)) {
    const scopes = FACE_TO_SCOPE_MAP[faceName] || [];

    // Handle editor colors
    if (faceName === "default") {
      if (faceData.fg)
        vsCodeTheme.colors["editor.foreground"] = normalizeColor(faceData.fg);
      if (faceData.bg)
        vsCodeTheme.colors["editor.background"] = normalizeColor(faceData.bg);
      continue;
    }

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

      if (Object.keys(settings).length > 0) {
        vsCodeTheme.tokenColors.push({
          name: faceName,
          scope: scopes,
          settings,
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
        settings,
      });
    }
  }

  return vsCodeTheme;
}

/**
 * @param {{ name: string; type: string }[]} themes
 */
async function updatePackageJson(themes) {
  const packageJson = JSON.parse(await fs.readFile(PACKAGE_JSON_PATH, "utf8"));

  // Update contributes.themes section
  packageJson.contributes = packageJson.contributes || {};
  packageJson.contributes.themes = themes.map((theme) => ({
    label: theme.name,
    uiTheme: theme.type === "dark" ? "vs-dark" : "vs",
    path: `./themes/${theme.name}.json`,
  }));

  await fs.writeFile(PACKAGE_JSON_PATH, JSON.stringify(packageJson, null, 2));
}

async function main() {
  try {
    const files = await fs.readdir(EMACS_DEFS_DIR);
    /** @type {{ name: string; type: string; colors: {} }[]} */
    const convertedThemes = [];

    // Ensure output directory exists
    await fs.mkdir(VSCODE_THEMES_DIR, { recursive: true });

    for (const file of files) {
      if (!file.endsWith(".json")) continue;

      const filePath = join(EMACS_DEFS_DIR, file);
      const themeName = file.replace("emacs-", "").replace(".json", "");

      const emacsTheme = await readEmacsTheme(filePath);
      const vsCodeTheme = convertToVSCodeTheme(emacsTheme, themeName);
      convertedThemes.push(vsCodeTheme);

      const outputPath = join(VSCODE_THEMES_DIR, `${themeName}.json`);
      await fs.writeFile(outputPath, JSON.stringify(vsCodeTheme, null, 2));
    }

    // Update package.json with theme entries
    await updatePackageJson(convertedThemes);

    console.log("Theme conversion completed successfully");
  } catch (error) {
    console.error("Error during theme conversion:", error);
    process.exit(1);
  }
}

main().catch(console.error);
