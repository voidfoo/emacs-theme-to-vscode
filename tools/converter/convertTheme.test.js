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
