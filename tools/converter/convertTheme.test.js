// @ts-check

import assert from "node:assert/strict";
import { test } from "node:test";
import { readFile } from "node:fs/promises";
import { convertToVSCodeTheme, detectThemeType } from "./convertTheme.js";

test("converts dark and light themes without running the CLI", () => {
  const dark = { default: { fg: "#ffffff", bg: "#000000" } };
  const light = { default: { fg: "#000000", bg: "#ffffff" } };
  assert.equal(detectThemeType(dark), "dark");
  assert.equal(detectThemeType(light), "light");
  const result = convertToVSCodeTheme(dark, "example");
  assert.equal(result.name, "example");
  assert.equal(result.colors["editor.background"], "#000000");
});

test("classifies representative real theme dumps", async () => {
  for (const [name, expected] of [
    ["modus-vivendi", "dark"],
    ["modus-operandi", "light"],
    ["doom-one", "dark"],
    ["leuven", "light"],
  ]) {
    const data = await readFile(
      new URL(`../../emacs-definitions/emacs-${name}.json`, import.meta.url),
      "utf8",
    );
    assert.equal(detectThemeType(JSON.parse(data)), expected);
  }
});

test("applies translucent selection, find and diff-hl colors", () => {
  const theme = {
    default: { fg: "#fff", bg: "#000" },
    region: { bg: "#abc" },
    highlight: { bg: "#12345678" },
    "diff-hl-margin-insert": { fg: "#0f0" },
    "diff-hl-margin-delete": { bg: "#f00" },
  };
  const { colors } = convertToVSCodeTheme(theme, "example");
  assert.equal(colors["editor.selectionBackground"], "#aabbcc40");
  assert.equal(colors["editor.findMatchBackground"], "#12345680");
  assert.equal(colors["diffEditor.insertedLineBackground"], "#00ff0030");
  assert.equal(colors["diffEditor.insertedTextBackground"], "#00ff0020");
  assert.equal(colors["diffEditor.removedLineBackground"], undefined);
});

test("uses explicit ordered token mappings and workbench color targets", () => {
  const theme = {
    default: { fg: "#fff", bg: "#000" },
    "font-lock-string-face": { fg: "#f00" },
    "font-lock-punctuation-face": { fg: "#0f0" },
    "font-lock-number-face": { fg: "#fff" },
    "font-lock-function-call-face": { fg: "#abc" },
    "next-error": { fg: "#123" },
    "company-tooltip": { bg: "#456" },
    "treemacs-git-modified-face": { fg: "#789" },
    "diff-refine-added": { bg: "#012" },
    "diff-hl-margin-insert": { fg: "#0f0" },
  };
  const { colors, tokenColors } = convertToVSCodeTheme(theme, "example");
  assert.deepEqual(
    tokenColors.map((rule) => rule.name),
    [
      "font-lock-punctuation-face",
      "font-lock-string-face",
      "font-lock-function-call-face",
    ],
  );
  assert.ok(tokenColors[1].scope.includes("punctuation.definition.string"));
  assert.equal(colors["editorSuggestWidget.background"], "#445566");
  assert.equal(colors["gitDecoration.modifiedResourceForeground"], "#778899");
  assert.equal(colors["diffEditor.insertedTextBackground"], "#001122");
  const workbench =
    /^(editor|statusBar|diff|diffEditor|gitDecoration|editorSuggestWidget|editorLineNumber|editorGutter|scrollbarSlider|breadcrumb|explorer|tree)\./;
  assert.ok(
    tokenColors.every(
      (rule) =>
        !rule.name.includes("(inferred)") &&
        rule.scope.every((scope) => !workbench.test(scope)),
    ),
  );
});
