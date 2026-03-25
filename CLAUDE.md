# Agent Instructions

## Executive Summary

The MCPR package aims to bridge a critical gap in the current LLM-R integration landscape. While existing AI coding assistants like Gemini CLI, Claude Code, and GitHub Copilot can execute R scripts via `Rscript -e "..."`, they lack the ability to **interactively join and persist within R sessions**. This limitation forces a stateless, script-based interaction model that prevents iterative data analysis, workspace persistence, and real-time collaboration between AI agents and R users.

### The MCPR Vision

MCPR envisions a paradigm shift toward **interactive AI-R collaboration** where:

- **Session Persistence**: AI agents join existing R sessions, maintaining full workspace context
- **Dynamic Introspection**: Real-time exploration of variables, objects, and session state
- **Visual Integration**: Automatic plot rendering and multimodal AI analysis of visualizations
- **Documentation Access**: Seamless integration with R's help system and function documentation
- **Iterative Workflows**: Support for complex, multi-step analyses that build upon previous work

### Codebase at a Glance

- **`aaa.R` / `mcpr-base.R`** — `the` environment holds all global state (sockets, registries, processes); `BaseMCPR` R6 class provides shared logging, cleanup registration, and controlled state access for all components
- **`mcpr-server.R` / `mcpr-server-http.R`** — `mcprServer` handles JSON-RPC 2.0 dispatch and tool routing over stdio; `mcprServerHTTP` wraps it via composition, adding an httpuv layer with per-client sessions and CORS for MCP App support
- **`mcpr-session.R` / `mcpr-client.R`** — `mcprSession` manages the R-side nanonext socket listener that receives forwarded tool calls and executes them in the live session; `mcprClient` is the symmetric counterpart for programmatic server connections
- **`tool-definition.R` / `tool-register.R` / `tool-registry-helpers.R`** — `ToolDef` R6 class defines tools with typed arguments; `ToolRegistry` auto-discovers tools from roxygen2 tags and converts them to JSON Schema for MCP `tools/list`
- **`protocol.R` / `type-conversion-*.R`** — JSON-RPC 2.0 message construction and bidirectional R ↔ JSON serialization with type restoration (factors, dates, matrices, S3/S4/R6)
- **`inst/tool-*.R`** — built-in MCP tools shipped with the package: `execute_r_code`, `show_plot`, `view`, `inspect_object`, `manage_r_sessions`, `read_instructions`

## R Library Contexts

This project commonly runs against two different R library contexts, and debugging often depends on knowing which one is active:

- **Project-local `renv` library** — used when working inside `/Users/santiago/projects/MCPR` with `renv` active; local tests and `pkgload::load_all()` may resolve MCPR from here
- **Global R library used by Claude Desktop** — `/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library`; Claude Desktop launches `MCPR::mcpr_server()` from this installed package, not from the project checkout

When changing runtime behavior, verify both contexts as needed. A fix visible in the project `renv` is not enough if Claude Desktop is still loading an older global install.

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Messaging & Error Style

`cli` is in `Imports` — always available. Use `cli::` everywhere. Never use base R messaging functions.

| Instead of | Use |
|---|---|
| `stop("msg ", x)` | `cli::cli_abort("msg {x}")` |
| `stop("Internal error: ...")` | `cli::cli_abort("...", .internal = TRUE)` |
| `warning("msg")` | `cli::cli_warn("msg")` |
| `message("msg")` | `cli::cli_inform("msg")` |
| `cat("msg\n")` | `cli::cli_text("msg")` *(or `cat()` only for file I/O)* |

`cat()` is acceptable **only** when writing to a file connection (e.g. `MCPRLogger`). All other uses should be `cli::`.

## Module Maintenance Conventions

When creating or editing modules in `R/` or `inst/`, agents must preserve the file-level structure conventions below.

### Required 3-line module header

Every code module in `R/` and every built-in tool module in `inst/` must begin with a plain 3-line header using regular `#` comments, not roxygen comments.

```r
# Module Title
# Brief description of the module's primary responsibility.
# Key implementation detail, scope boundary, or important dependency context.
```

Rules:

- Keep this header at the very top of the file.
- Update it when the module's role changes materially.
- Do not replace the header with a roxygen block.

### `@include` policy for `R/`

`@include` is the package-level dependency declaration mechanism for files in `R/`.

Rules:

- Use `@include` only in `R/` files, never in `inst/` tool files.
- Add `@include` entries whenever a file depends on objects that must be defined earlier during package collation.
- Keep `@include` entries minimal and real; do not add blanket includes.
- If you add, remove, or change `@include` relationships, regenerate roxygen so `DESCRIPTION` `Collate` stays in sync.

### `inst/` tool discovery convention

Built-in tools in `inst/tool-*.R` are discovered through roxygen metadata, not decorator comments.

Rules:

- Use `#' @keywords mcpr_tool` on tool functions that should be discoverable.
- Do not introduce `#* @mcp_tool` or other decorator-style markers.
- If a tool file needs helpers, keep them documented normally, but preserve the 3-line file header at the top.

## Debug Logging

Three log files are relevant when debugging MCPR:

| Log | Path | Description |
|---|---|---|
| **MCPR debug** | `/Users/santiago/mcpr_execute_r_code_debug.log` | Internal MCPR server/session communication |
| **Claude MCP** | `~/Library/Logs/Claude/mcp.log` | Claude Code's MCP client ↔ server messages (all MCP servers) |
| **Claude MCPR stderr** | `~/Library/Logs/Claude/mcp-server-mcpr.log` | Stderr capture from the MCPR MCP server process spawned by Claude Code |

**MCPR debug log format:** `[YYYY-MM-DD HH:MM:SS] [LEVEL] [COMPONENT] MESSAGE`
Levels: `DEBUG | INFO | WARNING | COMMUNICATION | ERROR` — Components: `SERVER | SESSION`

**Claude MCP log format:** `ISO_TIMESTAMP [level] [server_name] MESSAGE`
Levels: `info | warn | error` — Server names: `mcpr | anytype | …`

### Log Inspector (`scripts/inspect_logs/mcpr_log.py`)

Base Python (3.6+), no dependencies. Supports both the MCPR debug log and Claude MCP log via the `--log` flag:

- `--log mcpr` (default) → MCPR debug log
- `--log claude` → Claude MCP log (`~/Library/Logs/Claude/mcp.log`)
- `--log <path>` → any explicit file path (e.g. `~/Library/Logs/Claude/mcp-server-mcpr.log`)

`tail` prints last N entries and exits (agent-safe). Add `-f` to live-follow. Use `--last` or `--since`/`--until` to narrow the time window:

```bash
# Show last 30 MCPR entries and exit (default, agent-safe)
python scripts/inspect_logs/mcpr_log.py tail -n 30 --json

# Last 200 entries from the last hour only (best for agents — reduces context)
python scripts/inspect_logs/mcpr_log.py tail -n 200 --last 1h --json

# Live-follow new entries (like tail -f), with optional timeout
python scripts/inspect_logs/mcpr_log.py tail -n 10 -f --json
python scripts/inspect_logs/mcpr_log.py tail -f --timeout 2m --json

# Read the Claude MCP log or MCPR stderr log
python scripts/inspect_logs/mcpr_log.py tail --log claude --json
python scripts/inspect_logs/mcpr_log.py tail --log ~/Library/Logs/Claude/mcp-server-mcpr.log

# Filter Claude MCP log to only MCPR server entries
python scripts/inspect_logs/mcpr_log.py filter --log claude --server mcpr --json

# Filter only COMMUNICATION entries for a specific MCP method
python scripts/inspect_logs/mcpr_log.py comms --method tools/call --json

# Comms from the last 30 minutes only
python scripts/inspect_logs/mcpr_log.py comms --last 30m --json

# Trace a full request round-trip by JSON-RPC id
python scripts/inspect_logs/mcpr_log.py trace --id 6

# Summary stats (counts by level, component, method)
python scripts/inspect_logs/mcpr_log.py summary

# Offline filter: errors since a date, or grep for a keyword
python scripts/inspect_logs/mcpr_log.py filter --level ERROR --since "2026-02-19"
python scripts/inspect_logs/mcpr_log.py filter --level ERROR --last 4h
python scripts/inspect_logs/mcpr_log.py filter --grep "SOCKET_URL"
```

`--last` accepts durations: `30s`, `5m`, `1h`, `2d`, `1h30m`, `2d12h`.

## Session Completion

When ending a work session:

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Commit and sync**:
   ```bash
   bd sync
   git add <files> && git commit -m "..."
   ```
5. **Hand off** - Provide context for next session

Pushing to remote is the user's responsibility.


<!-- BEGIN BEADS INTEGRATION -->
## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Dolt-powered version control with native sync
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**

```bash
bd ready --json
```

**Create new issues:**

```bash
bd create "Issue title" --description="Detailed context" -t bug|feature|task -p 0-4 --json
bd create "Issue title" --description="What this issue is about" -p 1 --deps discovered-from:bd-123 --json
```

**Claim and update:**

```bash
bd update <id> --claim --json
bd update bd-42 --priority 1 --json
```

**Complete work:**

```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task atomically**: `bd update <id> --claim`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" --description="Details about what was found" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`

### Auto-Sync

bd automatically syncs via Dolt:

- Each write auto-commits to Dolt history
- Use `bd dolt push`/`bd dolt pull` for remote sync
- No manual export/import needed!

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems

For more details, see README.md and docs/QUICKSTART.md.

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

<!-- END BEADS INTEGRATION -->
