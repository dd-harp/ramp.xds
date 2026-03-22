# NA

This folder contains an R package. It implements a mathematical
framework for building systems of differential equations that describe
mosquito-borne pathogen dynamics.

## Proofreading Guidelines

### Roxygen documentation

- Be conservative — flag changes for approval, do not auto-apply.
- Show before/after for each proposed change and wait for confirmation.
- Fix only clear errors: punctuation, double words, grammar.
- Do not restructure or rephrase sentences.

### Vignettes

- More latitude — auto-fix minor grammar, punctuation, double words, and
  awkward phrasing.
- Provide a before/after change list to review after edits are applied.

### Both

- Do not touch technical terms (function names, parameter names,
  mathematical notation, etc.).
- Flag any suspicious technical terms for the author to decide.
- Change list format: always show before → after.
- Clear grammar errors (wrong articles, double words, punctuation):
  auto-fix without asking.
- Punctuation inside backtick code spans (e.g. `` `ramp.library.` `` →
  `` `ramp.library` ``): auto-fix without asking.
- Plotting function titles: `xds_plot_Z` / `xds_lines_Z` describe
  infective mosquitoes (not “infected and infective”) — auto-fix to
  “infective” without asking.

## Title Alignment Rules

- `MEffectSizes` module titles: `Apply effect sizes for \`module\`
  (**MY**)\` — apply without asking.
- `LEffectSizes` module titles: `Apply effect sizes for \`module\`
  (**L**)\` — apply without asking.
- `MBionomics` module titles: `Mosquito bionomics for \`module\`
  (**MY**)\` — apply without asking.
- `LBionomics` module titles: `Mosquito bionomics for \`module\`
  (**L**)\` — apply without asking.
- Interface generic titles follow the same pattern without “for
  `module`”: e.g., `Apply effect sizes (**MY**)` or
  `Mosquito bionomics (**MY**)`.
