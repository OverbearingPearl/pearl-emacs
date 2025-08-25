# Pearl Emacs

[中文文档](README_zh.md) | [English Documentation](README.md)

我的个人 Emacs 配置，使用 `use-package` 构建，用于高效和有序的包管理。

## ✨ 特性

- **模块化设计**：配置拆分为模块以便更好地组织
- **包管理**：使用 `use-package` 进行声明式包配置
- **AI 集成**：通过 `aidermacs` 支持 AI 编程辅助
- **Git 集成**：使用 Magit 进行强大的版本控制操作
- **可定制性**：易于使用自定义模块和配置进行扩展
- **前提条件检查**：自动检查所需的可执行文件

## 📁 项目结构

```
pearl-emacs/
├── init.el                 # 主初始化文件
├── modules/                # 模块化配置文件
│   ├── my-ai.el           # AI 编程辅助配置
│   └── my-git.el          # Git 集成设置
├── custom/                 # 自定义模板和用户配置
│   ├── features.el.template
│   └── secrets-plain.el.template
├── infra/                  # 基础设施和实用文件
│   └── my-preq.el
└── README.md              # 英文说明文件
└── README_zh.md           # 中文说明文件
```

## 🚀 安装

1. **备份现有的 Emacs 配置**（如果有）：
   ```bash
   mv ~/.emacs.d ~/.emacs.d.backup
   mv ~/.emacs ~/.emacs.backup
   ```

2. **克隆此仓库**：
   ```bash
   git clone https://github.com/your-username/pearl-emacs.git ~/.emacs.d
   ```

3. **启动 Emacs** - 配置将自动加载：
   ```bash
   emacs
   ```

4. **配置模块**：
   首次运行时，复制并自定义模板文件：
   ```bash
   cp ~/.emacs.d/custom/features.el.template ~/.emacs.d/custom/features.el
   cp ~/.emacs.d/custom/secrets-plain.el.template ~/.emacs.d/custom/secrets-plain.el
   ```

5. **编辑 `custom/features.el`** 通过取消注释来启用所需的模块

## ⚙️ 配置

### 启用模块

编辑 `custom/features.el` 取消注释要使用的模块：
```elisp
;; 取消注释要使用的模块
(require 'ai)
(require 'git)
```

### 机密信息和 API 密钥

添加您的私人机密信息和 API 密钥到 `custom/secrets-plain.el`：
```elisp
;; 添加您的私人机密信息和 API 密钥到这里
;; 此文件被 git 忽略，因此您的机密信息不会被提交
(setq deepseek-api-key "your-api-key-here")
```

### 添加新模块

1. 在 `modules/` 目录中创建新的 `.el` 文件
2. 使用 `use-package` 添加配置
3. 在末尾添加 `(provide 'module-name)`
4. 在 `custom/features.el` 中添加 `(require 'module-name)`

## 🧩 可用模块

### AI (`modules/my-ai.el`)
- 集成 `aidermacs` 用于 AI 辅助编程
- 需要安装 `aider` CLI 工具
- 配置 DeepSeek API 用于代码生成

### Git (`modules/my-git.el`)
- Magit 集成用于强大的 Git 操作
- 需要系统上安装 Git

## 📋 前提条件

- Emacs 25.1 或更新版本
- Git（用于版本控制功能）
- `aider` CLI 工具（用于 AI 功能）- 通过 pip 安装：
  ```bash
  pip install aider-chat
  ```

## 🔧 自定义

要添加个人配置：

1. **常规设置**：添加到适当的模块文件或创建新模块
2. **敏感数据**：使用 `custom/secrets-plain.el`（请小心处理此文件）
3. **功能切换**：使用 `custom/features.el` 启用/禁用模块

## ❓ 故障排除

### 常见问题

1. **包安装失败**：确保网络连接正常
2. **缺少可执行文件**：安装所需的工具如 Git 或 aider
3. **模块未加载**：检查是否在 `custom/features.el` 中启用

### 调试

在调试模式下启动 Emacs 查看初始化错误：
```bash
emacs --debug-init
```

## 🤝 贡献

虽然这主要是一个个人配置，但欢迎建议：

1. Fork 仓库
2. 创建功能分支
3. 进行更改
4. 提交拉取请求

## 📄 许可证

此配置根据 GNU 通用公共许可证 v3.0 分发。请参阅 [LICENSE](LICENSE) 文件了解详细条款和条件。

## 🙏 致谢

- `use-package` 团队提供优秀的包声明系统
- Magit 开发者提供出色的 Git 界面
- Emacs 社区提供无数的包和资源
