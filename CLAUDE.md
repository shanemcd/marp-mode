# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

marp-mode is an Emacs minor mode for working with Marp presentations. It provides a transient-based interface for accessing Marp CLI functionality directly from Emacs. The project is built entirely in Emacs Lisp and wraps the Marp CLI tool.

## Architecture

### Core Components

**marp-mode.el** (main file, ~460 lines):
- Minor mode definition with keymap (`C-c m` to open main menu)
- Transient interface definitions using the transient package (version 0.7+ required)
- Process management for watch and server modes
- Command builders that construct Marp CLI invocations with various flags
- Customization group for user-configurable variables

### Key Design Patterns

**Process Management**: The mode maintains three process variables (`marp-watch-process`, `marp-server-process`, and `marp-preview-process`) to track long-running Marp CLI processes. These use `start-process` for asynchronous execution to avoid blocking Emacs. Functions like `marp--kill-process` handle cleanup when starting new processes or when user stops them. Conversion commands (PDF, PPTX, images) use synchronous `call-process` since they complete quickly.

**Options State**: `marp-current-options` is an alist that stores user-selected options (output file, template, browser-path, allow-local-files, etc.). The `marp--build-command` function iterates this alist to construct CLI arguments.

**Browser Configuration**: The `marp-browser` variable has special handling:
- "auto" = no flags passed to Marp CLI
- Browser name (chrome, firefox, etc.) = uses `--browser` flag
- Full executable path = uses `--browser-path` flag

This logic is in `marp--build-command` (lines 178-183) and `marp-server-mode` (lines 362-365).

**Transient Menus**: Three main transient prefixes:
- `marp-menu` - main entry point with convert/preview actions
- `marp-output-menu` - output settings submenu
- `marp-advanced-menu` - advanced options submenu

The main menu dynamically shows running processes in its description and conditionally displays process control commands.

## Development

### Automated Testing

**test-marp-mode.el** contains a comprehensive ERT test suite with 38 tests covering:
- Command building logic (`marp--build-command`)
- Browser configuration (auto/name/path detection)
- Options management (setting, toggling, resetting)
- Process management (killing processes, lifecycle)
- Utility functions (executable detection, input file handling)
- Edge cases and numeric conversions

Tests use mocking extensively to avoid requiring Marp CLI installation or spawning actual processes.

**Running tests:**

Interactive (in Emacs):
```elisp
M-x load-file RET test-marp-mode.el RET
M-x ert RET t RET
```

Batch mode (command line):
```bash
emacs -Q -batch -l marp-mode.el -l test-marp-mode.el -f ert-run-tests-batch-and-exit
```

See TESTING.md for detailed documentation.

**Continuous Integration:**

GitHub Actions automatically runs the test suite on:
- Pull requests to `main`
- Pushes to `main`
- Manual workflow dispatch

Tests run in parallel across Emacs versions 28.1, 28.2, 29.1, 29.2, and snapshot. See `.github/workflows/test.yml`.

**IMPORTANT**: Always run tests before submitting PRs. Add new tests when adding features or fixing bugs.

### Manual Testing

For integration testing with actual Marp CLI:

1. Load the modified file in Emacs: `M-x eval-buffer` or restart Emacs
2. Open a markdown file with Marp frontmatter (must have `marp: true` in YAML frontmatter)
3. Enable the mode: `M-x marp-mode`
4. Test the transient menu: `C-c m`

### Prerequisites for Manual Testing

- Marp CLI installed and available in PATH (download from https://github.com/marp-team/marp-cli/releases)
- A browser (Chrome, Chromium, Firefox, or Edge) for preview/watch/server modes
- Emacs 28+ (includes transient package) or transient 0.7+ installed separately

Note: Unit tests do NOT require Marp CLI as they use mocked functions.

### Common Development Scenarios

**Adding a new Marp CLI flag**:
1. Add to `marp-current-options` handling in `marp--build-command` (lines 159-176)
2. Create a function to set the option (follow pattern of `marp-set-parallel`, `marp-set-image-scale`, etc.)
3. Add to appropriate transient menu
4. **Write tests** in test-marp-mode.el:
   - Test command building with the new option
   - Test the setter function updates `marp-current-options`
   - Test that the option replaces existing values (if applicable)

**Adding a new conversion format**:
1. Create a function following the pattern of `marp-convert-to-pdf`, `marp-convert-to-pptx`
2. Add to the "Convert" section of `marp-menu` transient
3. **Write tests** for command building with the new format flags

**Modifying process handling**:
- Preview mode uses `--preview` flag and runs asynchronously with `start-process`
- Watch mode uses `--watch` flag and optionally `--preview` (controlled by `marp-watch-mode-auto-open`)
- Server mode uses `--server`, `--port`, and `--input-dir` flags
- All three store process objects in global variables for lifecycle management
- Long-running commands that launch browsers must use `start-process` (async) not `call-process` (sync) to avoid blocking Emacs
- **Write tests** using mocked `start-process`/`kill-process` functions to verify process lifecycle

**General testing guidance**:
- Use the `marp-test-with-clean-state` macro for test isolation
- Mock external functions with `cl-letf` (e.g., `executable-find`, `process-live-p`, `read-number`)
- Test both success cases and edge cases (nil values, empty lists, numeric conversions)
- Follow existing test naming: `test-marp-<function>-<scenario>`

### Customization Variables

Key variables users typically configure:
- `marp-cli-executable` - path to Marp CLI (default: "marp")
- `marp-browser` - browser selection (default: "auto")
- `marp-watch-mode-auto-open` - auto-open preview in watch mode (default: t)
- `marp-allow-local-files-by-default` - allow local file access (default: nil)

## Code Style

Follow standard Emacs Lisp conventions:
- Use `marp-` prefix for public functions, `marp--` for internal functions
- Docstrings for all interactive functions
- Line length ~80 characters where practical
- Lexical binding enabled in file header
