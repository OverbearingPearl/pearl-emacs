# Pearl Emacs

[中文文档](README_zh.md) | [English Documentation](README.md)

My personal Emacs configuration built with `use-package` for efficient and organized package management.

## ✨ Features

- **Modular Design**: Configuration is split into modules for better organization
- **Package Management**: Uses `use-package` for declarative package configuration
- **AI Integration**: Support for AI programming assistance via `aidermacs`
- **Git Integration**: Magit for powerful version control operations
- **Customizable**: Easy to extend with custom modules and configurations
- **Prerequisite Checking**: Automatically checks for required executables

## 📁 Project Structure

```
pearl-emacs/
├── init.el                 # Main initialization file
├── modules/                # Modular configuration files
│   ├── my-ai.el            # AI programming assistance configuration
│   └── my-git.el           # Git integration settings
├── custom/                 # Custom templates and user configurations
│   ├── features.el.template
│   └── secrets-plain.el.template
├── infra/                  # Infrastructure and utility files
│   └── my-preq.el
├── README.md               # English documentation
├── README_zh.md            # Chinese documentation
└── LICENSE                 # License file
```

## 🚀 Installation

1. **Backup your existing Emacs configuration** (if any):
   ```bash
   mv ~/.emacs.d ~/.emacs.d.backup
   mv ~/.emacs ~/.emacs.backup
   ```

2. **Clone this repository**:
   ```bash
   git clone https://github.com/your-username/pearl-emacs.git ~/.emacs.d
   ```

3. **Start Emacs** - the configuration will load automatically:
   ```bash
   emacs
   ```

4. **Configure modules**:
   On first run, copy and customize the template files:
   ```bash
   cp ~/.emacs.d/custom/features.el.template ~/.emacs.d/custom/features.el
   cp ~/.emacs.d/custom/secrets-plain.el.template ~/.emacs.d/custom/secrets-plain.el
   ```

5. **Edit `custom/features.el`** to enable desired modules by uncommenting them

## ⚙️ Configuration

### Enabling Modules

Edit `custom/features.el` to uncomment the modules you want to use:
```elisp
;; Uncomment the modules you want to use
(require 'my-ai)
(require 'my-git)
```

### Secrets and API Keys

Add your private secrets and API keys to `custom/secrets-plain.el`:
```elisp
;; Add your private secrets and API keys here
;; This file is gitignored, so your secrets won't be committed
(setq deepseek-api-key "your-api-key-here")
```

### Adding New Modules

1. Create a new `.el` file in the `modules/` directory
2. Add your configuration using `use-package`
3. Add a `(provide 'module-name)` at the end
4. Add `(require 'module-name)` to `custom/features.el`

## 🧩 Available Modules

### AI (`modules/my-ai.el`)
- Integration with `aidermacs` for AI-assisted programming
- Requires `aider` CLI tool to be installed
- Configures DeepSeek API for code generation
- Module name: `my-ai`

### Git (`modules/my-git.el`)
- Magit integration for powerful Git operations
- Requires Git to be installed on the system
- Module name: `my-git`

### Completion (`modules/my-completion.el`)
- Uses `company` for code auto-completion
- Module name: `my-completion`

### Clojure Development (`modules/my-clojure.el`)
- Provides Clojure development environment support
- Requires Java to be installed
- Module name: `my-clojure`

## 📋 Prerequisites

- Emacs 25.1 or newer
- Git (for version control features)
- `aider` CLI tool (for AI features) - install via pip:
  ```bash
  pip install aider-chat
  ```

## 🔧 Customization

To add your personal configurations:

1. **For general settings**: Add them to appropriate module files or create new modules
2. **For sensitive data**: Use `custom/secrets-plain.el` (be careful with this file)
3. **For feature toggling**: Use `custom/features.el` to enable/disable modules

## ❓ Troubleshooting

### Common Issues

1. **Package installation failures**: Ensure you have internet connectivity
2. **Missing executables**: Install required tools like Git or aider
3. **Module not loading**: Check if it's enabled in `custom/features.el`

### Debugging

Start Emacs in debug mode to see initialization errors:
```bash
emacs --debug-init
```

## 🤝 Contributing

While this is primarily a personal configuration, suggestions are welcome:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## 📄 License

This configuration is distributed under the GNU General Public License v3.0. Please refer to the [LICENSE](LICENSE) file for detailed terms and conditions.

## 🙏 Acknowledgments

- `use-package` team for the excellent package declaration system
- Magit developers for the incredible Git interface
- The Emacs community for countless packages and resources
