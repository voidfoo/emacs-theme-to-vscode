// @ts-check

import assert from "node:assert/strict";
import { test } from "node:test";
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
