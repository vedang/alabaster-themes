# Alabaster Theme for Emacs

A minimal color theme for Emacs based on the [Alabaster Sublime Text theme](https://github.com/tonsky/sublime-scheme-alabaster).

## Features

- All 5 Alabaster variants:
  - `alabaster` - Light theme with foreground highlighting
  - `alabaster-bg` - Light theme with background highlighting
  - `alabaster-dark` - Dark theme with foreground highlighting
  - `alabaster-mono` - Light monochromatic theme
  - `alabaster-dark-mono` - Dark monochromatic theme

- Perfect color fidelity to the original Sublime scheme
- Minimal 4-class highlighting system (strings, constants, comments, definitions)
- No highlighting of language keywords or types
- Full MELPA package structure

## Installation

### From MELPA (when approved)

```elisp
(use-package alabaster-theme
  :config
  (load-theme 'alabaster t))
```

### Manual Installation

Clone this repository and add to your init:

```elisp
(add-to-list 'load-path "path/to/alabaster-emacs")
(require 'alabaster-theme)
(load-theme 'alabaster t) ; or any other variant
```

## Available Themes

- `(load-theme 'alabaster t)` - Main light theme
- `(load-theme 'alabaster-bg t)` - Light theme with background highlights
- `(load-theme 'alabaster-dark t)` - Dark theme
- `(load-theme 'alabaster-mono t)` - Light monochromatic
- `(load-theme 'alabaster-dark-mono t)` - Dark monochromatic

## Theme Functions

### Interactive Theme Selection
```elisp
;; Load a specific theme
(alabaster-load-theme 'alabaster-dark)

;; Cycle through all variants
(alabaster-switch-theme)
```

### Automatic Loading
```elisp
;; Set default theme variant
(setq alabaster-theme 'alabaster-bg)
(alabaster-load-theme) ; loads the default variant
```

## Customization

All colors are customizable via the `alabaster-*` variables:

```elisp
(customize-group 'alabaster)
```

Or programmatically:

```elisp
(setq alabaster-green "#00ff00") ; Make strings neon green
(load-theme 'alabaster t)       ; Reload theme
```

## Theme Philosophy

Alabaster follows the original design philosophy:

1. **Minimal highlighting** - Only 4 classes are highlighted
2. **No font variations** - Consistent normal weight throughout
3. **Reliable parsing** - Only highlights what parsers can identify correctly
4. **Readable comments** - Comments are highlighted, not dimmed
5. **Scannable code** - Strings, constants, and definitions visually distinct

This differs from most themes that highlight everything possible, creating a "fireworks show" that's hard to read and scan.

## Color Palette

### Light Themes
- **Background**: #F7F7F7
- **Foreground**: #000000
- **Strings**: #448C27 (green)
- **Constants**: #7A3E9D (magenta)
- **Comments**: #AA3731 (red)
- **Definitions**: #325CC0 (blue)
- **Cursor/Active**: #007ACC
- **Selection**: #BFDBFE

### Dark Themes
- **Background**: #0E1415
- **Foreground**: #CECECE
- **Comments**: #DFDF8E
- **Strings**: #95CB82
- **Constants**: #CC8BC9
- **Definitions**: #8AB1F0 (light blue)
- **Cursor/Active**: #CD974B

## Variants Explained

### Standard Themes
- **Alabaster**: Uses foreground colors for syntax highlighting
- **Alabaster Dark**: Inverted palette for dark mode
- **Alabaster BG**: Uses background colors instead of text colors

### Mono Themes
- **Alabaster Mono**: Only highlights errors, search, and cursor
- **Alabaster Dark Mono**: Dark version with minimal highlighting

The mono variants follow the maximalist minimalist philosophy - only show color when absolutely necessary for comprehension.

## Requirements

- Emacs 26.1 or later
- No external dependencies

## License

MIT License - same as the original Alabaster theme by Nikita Prokopov.