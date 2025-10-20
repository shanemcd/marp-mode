# Testing marp-mode

This document describes how to run the test suite for marp-mode.

## Test Framework

marp-mode uses **ERT** (Emacs Lisp Regression Testing), which is built into Emacs 24+.

## Running Tests

### Interactive Mode (from within Emacs)

1. Load both the main file and test file:
   ```elisp
   M-x load-file RET marp-mode.el RET
   M-x load-file RET test-marp-mode.el RET
   ```

2. Run all tests:
   ```elisp
   M-x ert RET t RET
   ```

3. Run a specific test:
   ```elisp
   M-x ert RET test-marp--build-command-basic RET
   ```

4. Run tests matching a pattern:
   ```elisp
   M-x ert RET "^test-marp--build-command" RET
   ```

### Batch Mode (from command line)

Run all tests non-interactively:

```bash
emacs -batch -l marp-mode.el -l test-marp-mode.el -f ert-run-tests-batch-and-exit
```

Add `-Q` to skip loading user configuration:

```bash
emacs -Q -batch -l marp-mode.el -l test-marp-mode.el -f ert-run-tests-batch-and-exit
```

### Using Makefile

You can create a simple Makefile for convenience:

```makefile
EMACS ?= emacs
BATCH = $(EMACS) -Q -batch -l marp-mode.el -l test-marp-mode.el

test:
\t$(BATCH) -f ert-run-tests-batch-and-exit

.PHONY: test
```

Then run:
```bash
make test
```

## Test Coverage

The test suite covers:

### Command Building (`marp--build-command`)
- Basic command with no options
- All individual option types (output, template, browser-path, etc.)
- Multiple options together
- Extra arguments
- Default allow-local-files behavior
- Numeric option conversion to strings

### Browser Configuration
- Auto browser (no flags added)
- Browser name (uses --browser)
- Browser executable path (uses --browser-path)
- Distinction between name and path

### Options Management
- Setting individual options (parallel, image-scale, browser-timeout)
- Toggling local files on/off
- Resetting all options
- Replacing existing option values

### Process Management
- Killing non-existent processes (no error)
- Killing dead processes (no error)
- Killing live processes
- Stopping individual processes (watch, server, preview)
- Stopping all processes at once

### Utility Functions
- Executable detection (found/not found/custom path)
- Input file retrieval (from buffer/via prompt)

### Edge Cases
- Empty options list
- Nil/empty extra arguments
- Numeric to string conversion

## Test Organization

Tests use the `marp-test-with-clean-state` macro to ensure each test runs with:
- Clean `marp-current-options`
- Default customization values
- No running processes

Tests use `cl-letf` to mock functions like:
- `file-executable-p` - for testing browser path detection
- `executable-find` - for testing Marp CLI detection
- `read-number` / `read-file-name` - for testing interactive input
- `process-live-p` / `kill-process` - for testing process management

## Writing New Tests

Follow this pattern:

```elisp
(ert-deftest test-my-new-feature ()
  "Test description of what this tests."
  (marp-test-with-clean-state
   ;; Setup
   (setq marp-current-options '((some-option . value)))

   ;; Mock functions if needed
   (cl-letf (((symbol-function 'some-function)
              (lambda (&rest args) 'mocked-result)))

     ;; Execute
     (let ((result (my-function)))

       ;; Assert
       (should (equal result expected-value))))))
```

### Assertion Macros

- `should` - Assert expression is non-nil
- `should-not` - Assert expression is nil
- `should-error` - Assert expression signals an error
- `should-error ... :type` - Assert specific error type

### Running Single Test During Development

While editing test-marp-mode.el:

1. Position cursor inside test definition
2. `M-x eval-defun` (C-M-x)
3. `M-x ert RET test-name RET`

## Continuous Integration

### GitHub Actions

The repository includes a GitHub Actions workflow (`.github/workflows/test.yml`) that automatically runs tests on:
- Pull requests to `main`
- Pushes to `main`
- Manual workflow dispatch

The workflow tests against multiple Emacs versions:
- Emacs 28.1, 28.2
- Emacs 29.1, 29.2
- Emacs snapshot (latest development version)

Tests run in parallel across all versions using a matrix strategy. The workflow will:
1. Checkout the code
2. Install the specified Emacs version
3. Run the test suite in batch mode
4. Report results with exit code (0 = pass, 1 = fail)

### Custom CI/CD Pipelines

For other CI/CD systems, use the batch mode command with exit codes:

```bash
#!/bin/bash
emacs -Q -batch \
  -l marp-mode.el \
  -l test-marp-mode.el \
  -f ert-run-tests-batch-and-exit

# Exit code will be:
# 0 - all tests passed
# 1 - some tests failed
```

### Testing Specific Emacs Versions

To test against a specific Emacs version locally, you can use Docker:

```bash
docker run -v $(pwd):/workspace -w /workspace silex/emacs:28.2 \
  emacs -Q -batch -l marp-mode.el -l test-marp-mode.el \
  -f ert-run-tests-batch-and-exit
```

## Debugging Failed Tests

When a test fails interactively:

1. ERT shows which test failed and the assertion that failed
2. Press `b` to view backtrace
3. Press `l` to view messages buffer
4. Press `r` to re-run failed test
5. Press `.` to jump to test definition

In batch mode, failures are printed with full backtraces.

## Test Isolation

Each test runs in isolation with:
- Clean variable state (via `marp-test-with-clean-state`)
- Mocked external dependencies
- No side effects between tests

This ensures tests are:
- **Repeatable** - Same result every time
- **Independent** - Order doesn't matter
- **Fast** - No actual process execution or file I/O
