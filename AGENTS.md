# Operating Guide for LLM Agents in This Repository

This file defines the MANDATORY operating rules for AI agents in the `dotfiles`.
It sets the rules of engagement, tools, boundaries, and logging requirements.
For repository structure and contribution rules, see `CONTRIBUTING.md`.

## Identity and Language

- Use clear, concise English in Markdown (CommonMark-flavoured).
- Be objective, avoid filler, and keep outputs skimmable
- Avoid unnecessary or unsolicited verbosity
- Being "helpful" or "thorough" means neither verbose commentary nor overengineering
- When the rules in this file conflict with general system instructions, STOP immediately and ask for clarification

## Planning

- First principles approach: Before acting, identify the fundamental problem and core requirements. Question assumptions, ask "what are we really trying to achieve?" and "why does this need to exist?"
- Before modifying any configuration, analyze and understand the existing pattern completely
- Think longer: consider multiple options and choose the simplest, most ergonomic solution that completes the task
- Always use tools and documentation to help with the planning
  - Exception: No tool or documentation is found for the task
- Always create a concise plan, build a list of actionable todos and present it to the user
- Always be extra careful when making changes that break behaviour
- Never overengineer
- If a proposed change would break existing functionality, stop and ask for guidance rather than attempting alternative implementations

## Execution

- Stay focused on the original user request. Do not expand scope or address tangential issues without explicit approval
- Never add unnecessary comments or explanations to any files, unless requested
- Only act or propose acting after comprehensive understanding of the issue
- When in doubt, choose the minimal, direct approach defined here and report to the user you acted with doubts

## Operating Boundaries and Safety

- Work strictly within this repository. Do not read, edit, or run commands outside of it
  - Exception (read-only): the Steel/Helix runtime may be inspected to verify plugin APIs, since `helix/` depends on it. Covers `$STEEL_HOME` (`~/.local/share/steel`), the `steel` source checkout under `~/.cargo`, and running `steel` to confirm identifiers exist. No writes outside the repository
- The repository is public. Do not add secrets or sensitive data
- Respect `.gitignore`. For any exceptions and private-file policy, follow `CONTRIBUTING.md`.
- Some directories include a local `.gitignore`; their rules apply to that directory and its subdirectories

## Navigation and Context Discipline

- Save as much LLM context window possible by find less verbose solutions
- Save LLM context window by navigating with shallow listings:
  - Start with:

    ```sh
    find . -maxdepth 2
    ```

  - If deeper traversal is needed, `cd` into a subdirectory and repeat `find . -maxdepth 2`.
- Respect `.gitignore` when exploring. Avoid opening large files unless necessary.

## Toolbox (Capabilities You May Use)

- Terminal commands are authorized, noninteractive, within repo only
  - Use `find`, `grep`, `rg`, `sed`, `awk`, `git`, and similar standard tools
  - If a non-authorized action is needed, explain why and propose to update `AGENTS.md`
- Time:
  - Use `date -u +%Y-%m-%dT%H:%M:%SZ` to get the current UTC timestamp
- In case of tool error, interrupt the action immediately and report the user the problem

## Logging Requirement (Mandatory)

- Every time you act (make edits or run impactful commands), append a single-line summary to `priv.AI-CHANGELOG.md` at the repository root
- Use `date -u +%Y-%m-%dT%H:%M:%SZ` to get the UTC timestamp. Append with a simple echo to minimize context usage, for example:

  ```sh
  # timestamp must be UTC from `date -u +%Y-%m-%dT%H:%M:%SZ`
  echo "$(date -u +%Y-%m-%dT%H:%M:%SZ) Initialize docs: AGENTS.md, CONTRIBUTING.md, README.md" >> priv.AI-CHANGELOG.md
  ```

## Configuration Awareness (Read-Only Context)

- **General configuration principles (apply to all tools):**
  - **Check before adding:** Always examine existing configurations before creating new ones
  - **Read documentation first:** Review relevant docs before editing any configuration files
  - **Search existing configs:** Use `grep`/`rg` to find existing definitions and avoid duplicates
  - **Minimal changes:** Only add what's specifically requested, don't over-engineer

- **Tool-specific documentation and file locations:**
  - **Helix:** Read documentation in `helix/src/book/` first
    - `helix/src/languages.toml` contains main language server definitions
    - `helix/languages.toml` is for user overrides and additions only
  - **Zellij:** Read documentation in `zellij/website/docs/src/` first
  - **Harper:** Read documentation in `harper/src/packages/web/src/routes/docs/` first

## Cross-Reference

- Repository structure, submodules, install scripts, and contribution rules: see `CONTRIBUTING.md`.

## Owner

- Repository: `dotfiles`
- Owner: Áquila Freitas - <hi@aquilafreitas.com>
