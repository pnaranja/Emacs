# WARP.md

This file provides guidance to WARP (warp.dev) when working with code in this repository.

## Repository Overview

This is a personal Emacs configuration repository containing a modern Emacs setup focused on development productivity. The configuration uses `use-package` for package management and includes comprehensive language support, AI integration, and workflow enhancements.

## Key Files Structure

- `init.el` - Main configuration file with all package configurations and settings
- `early-init.el` - Early initialization settings for performance and basic UI
- `custom-vars.el` - Custom variables set by Emacs (automatically managed)
- `archive/` - Contains older 2016 configuration (legacy, not currently used)
- `backups/` - Automatic backup files directory
- `autosaves/` - Auto-save files directory

## Development Commands

### Starting and Managing Emacs

```bash
# Start Emacs (GUI)
emacs

# Start Emacs in terminal mode
emacs -nw

# Start with specific configuration file
emacs -q -l init.el

# Byte-compile configuration files
emacs --batch -f batch-byte-compile *.el
```

### Package Management

Inside Emacs, use these commands:
- `M-x package-refresh-contents` - Refresh package archives
- `M-x package-install` - Install a specific package
- `M-x package-list-packages` - Browse available packages
- `M-x auto-compile-on-save-mode` - Enable automatic compilation

### Configuration Management

```bash
# View startup time and performance metrics (automatically shown on startup)
# Check for configuration errors
emacs --debug-init

# Test configuration in clean environment
emacs -q --eval "(load-file \"init.el\")"
```

## Configuration Architecture

### Package Management System
- Uses `use-package` for declarative package configuration
- Packages are automatically installed from MELPA and GNU archives
- Configuration is modular with each package having its own `use-package` block

### Key Configuration Sections

1. **Performance Optimization**
   - Garbage collection tuning in `early-init.el`
   - LSP booster integration for faster language servers
   - Startup time measurement and optimization

2. **Development Environment**
   - LSP Mode for multiple languages (TypeScript, Python, Rust, Swift, C/C++)
   - Company mode for auto-completion
   - Flycheck for syntax checking
   - Projectile for project management

3. **AI Integration**
   - GPTel integration with Google Gemini API
   - AI-powered commit message generation
   - Aidermacs for AI-assisted development

4. **Language-Specific Support**
   - TypeScript/JavaScript with Tide and LSP
   - Python with LSP-Pyright
   - Rust with Rustic mode
   - Swift with SourceKit-LSP
   - Dart/Flutter development
   - Web development with web-mode

5. **Productivity Features**
   - Org mode with journal and roam integration
   - Magit for Git operations
   - Ivy/Counsel for enhanced minibuffer completion
   - Avy for rapid navigation

### Key Bindings Philosophy
- Extensive use of `C-c` prefix for custom commands
- Super key (`s-`) combinations for macOS integration  
- Modal editing preparation (some Evil mode references in archive)
- Consistent navigation patterns with avy-goto-char

### File Organization Strategy
- Single `init.el` file for all configuration (monolithic approach)
- External `custom-vars.el` for Emacs customization variables
- Backup and autosave files stored in dedicated directories
- Archive directory preserves historical configurations

## Important Configuration Details

### API Keys and Security
- **WARNING**: Gemini API key is currently hardcoded in `init.el` (line 145)
- Should be moved to environment variables or secure storage

### Language Server Configuration
- Uses `lsp-use-plists` for better performance
- Emacs LSP Booster integration for faster JSON parsing
- Multiple language servers configured with specific settings

### Performance Considerations
- Garbage collection threshold increased during startup
- File name handler optimizations
- Package loading is deferred where possible using `:defer t`

### Directory Dependencies
- Requires `~/journal/emacs_journal` for org-journal
- Requires `~/journal/org-roam` for org-roam notes
- Creates backup directories automatically

## Troubleshooting Common Issues

### Package Installation Issues
```elisp
;; Refresh package contents and reinstall
(package-refresh-contents)
(package-install 'package-name)
```

### LSP Server Issues
- Check if language servers are installed system-wide
- Verify `lsp-describe-session` for active connections
- Restart LSP with `lsp-restart-workspace`

### Performance Issues
- Check startup time with built-in measurement
- Use `esup` package for detailed profiling: `M-x esup`
- Consider increasing GC thresholds if needed

## Development Workflow

1. **Making Changes**: Edit `init.el` directly for configuration changes
2. **Testing Changes**: Use `M-x eval-buffer` or restart Emacs
3. **Package Updates**: Use `M-x package-list-packages` → `U` → `x`
4. **Backup**: Configuration is automatically backed up in `backups/` directory

## External Dependencies

- **System Requirements**: Various language servers for LSP functionality
- **Font Requirements**: Nerd Icons font family for proper icon display  
- **macOS Specific**: Uses `mdfind` for locate, `pngpaste` for screenshots
- **Optional**: `emacs-lsp-booster` binary for enhanced LSP performance
