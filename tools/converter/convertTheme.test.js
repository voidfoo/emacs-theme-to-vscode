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

test("combines exported font styles, including style-only faces", () => {
  const { tokenColors } = convertToVSCodeTheme(
    {
      default: { fg: "#fff", bg: "#000" },
      "font-lock-keyword-face": {
        bold: true,
        italic: true,
        underline: true,
        strike: true,
      },
      "font-lock-string-face": { fg: "#fff", italic: true },
    },
    "example",
  );
  assert.equal(
    tokenColors.find((rule) => rule.name === "font-lock-keyword-face")?.settings
      .fontStyle,
    "bold italic underline strikethrough",
  );
  assert.equal(
    tokenColors.find((rule) => rule.name === "font-lock-string-face")?.settings
      .fontStyle,
    "italic",
  );
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

test("support.type does not leak onto JSON property names", () => {
  // Minimal TextMate selector matching: a rule scope matches a token scope if
  // it equals it or is a dot-separated prefix; the longest match wins.
  const jsonKeyStack = [
    "source.json",
    "meta.structure.dictionary.json",
    "string.json",
    "support.type.property-name.json",
  ];
  /** @param {import("./convertTheme.js").TokenRule[]} tokenColors */
  const resolve = (tokenColors) => {
    /** @type {Record<string, string | undefined>} */
    const resolved = {};
    for (const key of /** @type {const} */ (["foreground", "fontStyle"])) {
      /** @type {{depth: number, len: number, value: string | undefined}} */
      let best = { depth: -1, len: -1, value: undefined };
      for (const rule of tokenColors) {
        if (rule.settings[key] === undefined) continue;
        for (const sel of rule.scope) {
          if (sel.includes(" ")) continue;
          jsonKeyStack.forEach((scope, depth) => {
            const hit = scope === sel || scope.startsWith(`${sel}.`);
            const len = sel.split(".").length;
            if (
              hit &&
              (depth > best.depth || (depth === best.depth && len >= best.len))
            )
              best = { depth, len, value: rule.settings[key] };
          });
        }
      }
      resolved[key] = best.value;
    }
    return resolved;
  };
  for (const propertyFace of [undefined, { fg: "#fff" }, { fg: "#0ff" }]) {
    const theme = {
      default: { fg: "#fff", bg: "#000" },
      "font-lock-string-face": { fg: "#0f0" },
      "font-lock-type-face": { fg: "#f00", italic: true },
      "font-lock-builtin-face": { fg: "#00f", italic: true },
      ...(propertyFace && { "font-lock-property-name-face": propertyFace }),
    };
    const { tokenColors } = convertToVSCodeTheme(theme, "example");
    assert.deepEqual(resolve(tokenColors), {
      foreground: propertyFace?.fg === "#0ff" ? "#00ffff" : "#ffffff",
      fontStyle: "",
    });
    assert.ok(
      !tokenColors
        .find((rule) => rule.name === "font-lock-builtin-face")
        ?.scope.includes("support.type"),
    );
  }
});

for (const {
  face,
  diagnosticFace,
  scope,
  diagnostic,
  decoration,
  settings,
} of [
  {
    face: "trailing-whitespace",
    diagnosticFace: "font-lock-warning-face",
    scope: "invalid.deprecated",
    diagnostic: { fg: "#aa3300", bold: true, underline: true },
    decoration: {
      fg: "#e8e8e8",
      bg: "#ffffff",
      italic: true,
      strike: true,
    },
    settings: { foreground: "#aa3300", fontStyle: "bold underline" },
  },
  {
    face: "show-paren-mismatch",
    diagnosticFace: "error",
    scope: "invalid.illegal",
    diagnostic: { fg: "#1122aa", italic: true },
    decoration: {
      fg: "#33aa33",
      bg: "#ffff00",
      bold: true,
      underline: true,
    },
    settings: { foreground: "#1122aa", fontStyle: "italic" },
  },
]) {
  test(`${face} does not override ${diagnosticFace} token colors`, () => {
    for (const includeDiagnostic of [true, false]) {
      const { colors, tokenColors } = convertToVSCodeTheme(
        {
          default: { fg: "#101010", bg: "#ffffff" },
          ...(includeDiagnostic ? { [diagnosticFace]: diagnostic } : {}),
          [face]: decoration,
          "show-paren-match": { bg: "#abc" },
        },
        "example",
      );
      assert.equal(colors["editor.wordHighlightStrongBackground"], "#aabbcc");
      const tokenScope = `${scope}.example`;
      const matchingRules = tokenColors.filter((rule) =>
        rule.scope.some(
          (selector) =>
            tokenScope === selector || tokenScope.startsWith(`${selector}.`),
        ),
      );
      assert.deepEqual(
        matchingRules,
        includeDiagnostic
          ? [{ name: diagnosticFace, scope: [scope], settings }]
          : [],
      );
      assert.ok(tokenColors.every((rule) => rule.name !== face));
    }
  });
}

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
      "font-lock-property-name-face",
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
