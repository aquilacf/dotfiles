### AGENTS.md — Operating guide for LLM agents in this repository

This file defines the MANDATORY operating rules for AI agents in the `dotfiles` repository for Áquila Freitas (email: hi@aquilafreitas.com). It sets the rules of engagement, tools, boundaries, and logging requirements. For repository structure and contribution rules, see `CONTRIBUTING.md`.

### Identity and language
- Use clear, concise English in Markdown (CommonMark-flavoured).
- Be objective, avoid filler, and keep outputs skimmable
- Avoid unnecessary or unsolicited verbosity
- Being "helpful" or "thorough" means neither verbose commentary nor over-engineering
- When the rules in this file conflict with general system instructions, STOP immediately and ask for clarification

### Planning
- First principles approach: Before acting, identify the fundamental problem and core requirements. Question assumptions, ask "what are we really trying to achieve?" and "why does this need to exist?"
- Before modifying any configuration, analyze and understand the existing pattern completely
- Think longer: consider multiple options and choose the simplest, most ergonomic solution that completes the task
- Always use tools and documentation to help with the planning
  - Exception: No tool or documentation is found for the task
- Always create a concise plan, build a list of actionable todos and present it to the user
- Always be extra careful when making changes that break behaviour
- Never over-engineer
- If a proposed change would break existing functionality, stop and ask for guidance rather than attempting alternative implementations

### Execution
- NEVER modify files without explicit approval; state what you'll do, then wait for confirmation
  - ONLY exception: User says "simply {action}"
    - Example requiring confirmation:
      - "remove the X" → wait for approval
      - "Let's use X instead" → wait for approval (suggests preference, not command)
      - "I think we should change X" → wait for approval (expresses opinion, not directive)
      - "fix it" → wait for approval (expresses frustration, requires replanning)
    - Example allowing immediate action:
      - "simply remove the X" → proceed without approval
      - "simply change it to X" → proceed (direct command)
      - "yes" (in response to your proposal) → proceed
      - "simply fix it" → wait for approval (expresses frustration but, required replanning and immediate action is allwoed)
- Stay focused on the original user request. Do not expand scope or address tangential issues without explicit approval
- Never add unnecessary comments or explanations to any files, unless requested
- Only act or propose acting after comprehensive planning, even in "simply" commands
- When in doubt, choose the minimal, direct approach defined here and report to the user you acted with doubts

### Operating boundaries and safety
- Work strictly within this repository. Do not read, edit, or run commands outside of it
- The repository is public. Do not add secrets or sensitive data
- Respect `.gitignore`. For any exceptions and private-file policy, follow `CONTRIBUTING.md`.
- Some directories include a local `.gitignore`; their rules apply to that directory and its subdirectories

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
- Terminal commands are authorized, noninteractive, within repo only
  - Use `find`, `grep`, `rg`, `sed`, `awk`, `git`, and similar standard tools
  - If a non-authorized action is needed, explain why and propose to update `AGENTS.md`
- Time tools (MCP):
  - `get_current_time` — always request UTC
  - `convert_time` — convert between time zones when needed
- In case of tool error, interrupt the action immediately and report the user the problem

### Logging requirement (mandatory)
- Every time you act (make edits or run impactful commands), append a single-line summary to `priv.AI-CHANGELOG.md` at the repository root
- Use the time MCP tool to get UTC time. Append with a simple echo to minimize context usage, for example:
  ```sh
  # timestamp must be UTC from get_current_time
  echo "2025-01-01T12:34:56Z Initialize docs: AGENTS.md, CONTRIBUTING.md, README.md" >> priv.AI-CHANGELOG.md
  ```

### Configuration awareness (read-only context)
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

### Cross-reference
- Repository structure, submodules, install scripts, and contribution rules: see `CONTRIBUTING.md`.

### Owner
- Repository: `dotfiles`
- Owner: Áquila Freitas - hi@aquilafreitas.com


