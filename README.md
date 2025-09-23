# marp-mode

An Emacs minor mode for working with [Marp](https://marp.app/) presentations.

Marp is a Markdown presentation ecosystem that allows you to create slide decks using Markdown syntax. This mode provides an intuitive transient-based interface for accessing Marp CLI functionality directly from Emacs.

## Features

- **Transient Interface**: Modern, discoverable menu system using transient
- **Multiple Output Formats**: Convert to HTML, PDF, PowerPoint, and images
- **Live Preview**: Watch mode and server mode for real-time updates
- **Configurable Options**: Comprehensive settings for browser choice, templates, and conversion options
- **Seamless Integration**: Works with any Markdown buffer

## Installation

### Prerequisites

First, install [Marp CLI](https://github.com/marp-team/marp-cli):

```bash
npm install -g @marp-team/marp-cli
```

### Package Installation

#### Manual Installation

1. Download `marp-mode.el`
2. Add it to your load path
3. Require the package in your Emacs configuration:

```elisp
(require 'marp-mode)
```

#### Using quelpa and use-package (Recommended)

```elisp
(use-package marp-mode
  :quelpa (marp-mode :fetcher github :repo "shanemcd/marp-mode")
  :hook (markdown-mode . marp-mode))
```

#### Using use-package with local path

```elisp
(use-package marp-mode
  :load-path "path/to/marp-mode"
  :hook (markdown-mode . marp-mode))
```

#### From GitHub

```bash
git clone https://github.com/shanemcd/marp-mode.git
```

## Usage

1. Open a Markdown file containing your Marp presentation
2. Enable marp-mode: `M-x marp-mode`
3. Access the main menu: `C-c m` or `M-x marp-menu`

### Main Menu Options

- **Convert**: Generate output in various formats (HTML, PDF, PowerPoint, images)
- **Preview**: Open live preview or start watch mode
- **Options**: Configure browser settings, output options, and advanced settings

### Transient Interface

The transient interface is organized into several sub-menus:

#### Main Menu (`C-c m`)
- `h`: Convert to HTML (default)
- `d`: Convert to PDF
- `x`: Convert to PowerPoint
- `i`: Convert to images
- `p`: Preview presentation
- `w`: Start watch mode
- `s`: Start server mode
- `b`: Browser settings
- `o`: Output settings
- `a`: Advanced options

#### Browser Settings
- Choose browser for PDF conversion (Chrome, Edge, Firefox)
- Set custom browser path
- Configure browser-specific options

#### Output Settings
- Set custom output file
- Choose HTML template
- Export presenter notes
- Configure conversion options

#### Advanced Options
- Enable/disable local file access
- Set parallel conversion count
- Configure image scale factor
- Set browser timeout
- Reset all options

## Configuration

### Customizable Variables

```elisp
;; Path to Marp CLI executable
(setq marp-cli-executable "marp")

;; Default output format
(setq marp-default-output-format 'html)

;; Browser choice for PDF conversion
(setq marp-browser-choice 'auto)  ; or 'chrome, 'edge, 'firefox

;; Auto-open preview in watch mode
(setq marp-watch-mode-auto-open t)

;; Allow local files by default (needed for local images)
(setq marp-allow-local-files-by-default t)
```

### Key Bindings

By default, marp-mode binds `C-c m` to open the main transient menu. You can customize this:

```elisp
(define-key marp-mode-map (kbd "C-c C-m") 'marp-menu)
```

## Dependencies

- [transient](https://github.com/magit/transient) package (built into Emacs 28+)
- [Marp CLI](https://github.com/marp-team/marp-cli) installed and available in PATH

## Examples

### Basic Usage

```markdown
---
marp: true
theme: default
---

# My Presentation

Welcome to my Marp presentation!

---

## Slide 2

- Point 1
- Point 2
- Point 3
```

1. Open the file in Emacs
2. Enable marp-mode: `M-x marp-mode`
3. Press `C-c m` to open the transient menu
4. Press `h` to convert to HTML, or `p` to preview

### Watch Mode

For live editing:

1. Press `C-c m` then `w` to start watch mode
2. Edit your Markdown file
3. Changes will automatically update the preview

## Contributing

Contributions are welcome! Please feel free to submit issues, feature requests, or pull requests.

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.

## Acknowledgments

- Inspired by [pandoc-mode](https://github.com/joostkremers/pandoc-mode) for the menu interface design
- Built for the [Marp](https://marp.app/) presentation ecosystem
- Uses the [transient](https://github.com/magit/transient) package for the menu interface
- Initial code generated with assistance from [Claude](https://claude.ai)