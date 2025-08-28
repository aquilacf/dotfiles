### AGENTS.md — Operating guide for LLM agents in this repository

This file defines how AI agents should act when assisting in the `dotfiles` repository for Áquila Freitas (email: hi@aquilafreitas.com). It sets the rules of engagement, tools, boundaries, and logging requirements. For repository structure and contribution rules, see `CONTRIBUTING.md`.

### Identity and language
- Use clear, concise English in Markdown (CommonMark-flavored).
- Prefer short sections, bullet points, and minimal code blocks.
- Be objective, avoid filler, and keep outputs skimmable.
- Never add unnecessary comments or explanations to any files, unless requested.
- Avoid unecessary or unsolicited verbosity.

### Planning and execution
- Always create a concise plan and request user confirmation before making changes.
- Think longer: consider multiple options and choose the simplest, most ergonomic solution that completes the task.
- Never over-engineer.

### Operating boundaries and safety
- Work strictly within this repository. Do not read, edit, or run commands outside of it.
- The repository is public. Do not add secrets or sensitive data.
- Respect `.gitignore`. For any exceptions and private-file policy, follow `CONTRIBUTING.md`.
- Some directories include a local `.gitignore`; their rules apply to that directory and its subdirectories.

### Navigation and context discipline
- Save as much LLM context window possible by find less verbose solutions
- Save LLM context window by navigating with shallow listings:
  - Start with:
    ```sh
    find . -maxdepth 2
    ```
  - If deeper traversal is needed, `cd` into a subdirectory and repeat `find . -maxdepth 2`.
- Respect `.gitignore` when exploring. Avoid opening large files unless necessary.

### Toolbox (capabilities you may use)
- Terminal commands are authorized, non-interactive, within repo only.
  - Use `find`, `grep`, `rg`, `sed`, `awk`, `git`, and similar standard tools.
  - If a non-authorized action is needed, explain why and propose to update `AGENTS.md`
- Time tools (MCP):
  - `get_current_time` — always request UTC.
  - `convert_time` — convert between timezones when needed.

### Logging requirement (mandatory)
- Every time you act (make edits or run impactful commands), append a single-line summary to `priv.AI-CHANGELOG.md` at the repository root.
- Use the time MCP tool to get UTC time. Append with a simple echo to minimize context usage, for example:
  ```sh
  # timestamp must be UTC from get_current_time
  echo "2025-01-01T12:34:56Z Initialize docs: AGENTS.md, CONTRIBUTING.md, README.md" >> priv.AI-CHANGELOG.md
  ```

### Configuration awareness (read-only context)
- Before replying or editing any Helix configuration (`helix/config.toml`, `helix/languages.toml`, `helix/themes/`), read the documentation in `helix/src/book/` first.
- Before replying or editing any Zellij configuration (`zellij/config.kdl`), read the documentation in `zellij/website/docs/src/` first.

### Cross-reference
- Repository structure, submodules, install scripts, and contribution rules: see `CONTRIBUTING.md`.

### Owner
- Repository: `dotfiles`
- Owner: Áquila Freitas - hi@aquilafreitas.com


