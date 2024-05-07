## Release v0.17.0

 * The binary used is now the one in `PATH` instead of `/usr/bin/fzf`
 * Added support for the `--expect` flag via `Fzf.Expect`
 * Support for output with large stdout buffer sizes

## Release v0.16.0

  * `Fzf.of_escaped_strings` and `Fzf.of_strings_raise_on_newlines` now return `string Fzf.t`
  * `Fzf.of_escaped_strings_assoc` was added
    - Summarizes/display items in a human-friendly way
    - Maintains a reverse map
    - Handles duplicate items or ambiguous string mappings with `on_collision` parameter (`Raise`, `Update`, or `Ignore`)
  * `Fzf.Streaming.lookup_selection` was added
    - Allows for a decoded value to be extracted
  * `Fzf.complete_enumerable_sexpable`
    - Generates auto-complete options for a given `Command.Enumerable_sexpable` module
  * `Fzf.key_with_hidden_part` was added
    - Creates a key with a hidden part. Fzf will show only the visible part but will search in both visible and hidden parts.
  * Fzf pick functions gained:
   - `fzf_path` allowing one to specifying a custom path for the Fzf executable
   - Optional `case_match` parameter allowing a user to  specifying case matching behavior: `case_insensitive`, `case_sensitive`, or `smart_case`
