// @ts-check

import { promises as fs } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { convertToVSCodeTheme } from "./convertTheme.js";
import { readEmacsTheme } from "./utils.js";

const __dirname = dirname(fileURLToPath(import.meta.url));
const EMACS_DEFS_DIR = join(__dirname, "../../emacs-definitions");
const VSCODE_THEMES_DIR = join(__dirname, "../../vscode-extension/themes");
const PACKAGE_JSON_PATH = join(
  __dirname,
  "../../vscode-extension/package.json",
);

/** @param {{ name: string; type: string }[]} themes */
async function updatePackageJson(themes) {
  const packageJson = JSON.parse(await fs.readFile(PACKAGE_JSON_PATH, "utf8"));
  packageJson.contributes = packageJson.contributes || {};
  packageJson.contributes.themes = themes.map((theme) => ({
    label: theme.name,
    uiTheme: theme.type === "dark" ? "vs-dark" : "vs",
    path: `./themes/${theme.name}.json`,
  }));
  await fs.writeFile(
    PACKAGE_JSON_PATH,
    JSON.stringify(packageJson, null, 2) + "\n",
  );
}

async function main() {
  const files = (await fs.readdir(EMACS_DEFS_DIR)).sort();
  /** @type {{ name: string; type: string }[]} */
  const convertedThemes = [];
  await fs.mkdir(VSCODE_THEMES_DIR, { recursive: true });

  for (const file of files) {
    if (!file.endsWith(".json")) continue;
    const filePath = join(EMACS_DEFS_DIR, file);
    const themeName = file.replace("emacs-", "").replace(".json", "");
    const emacsTheme = await readEmacsTheme(filePath);
    const vsCodeTheme = convertToVSCodeTheme(emacsTheme, themeName);
    convertedThemes.push(vsCodeTheme);
    const outputPath = join(VSCODE_THEMES_DIR, `${themeName}.json`);
    await fs.writeFile(outputPath, JSON.stringify(vsCodeTheme, null, 2) + "\n");
  }

  await updatePackageJson(convertedThemes);
  console.log("Theme conversion completed successfully");
}

main().catch((error) => {
  console.error("Error during theme conversion:", error);
  process.exitCode = 1;
});
