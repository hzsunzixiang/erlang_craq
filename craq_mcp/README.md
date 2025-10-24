# CRAQ MCP Source Code Analysis Server

一个基于 FastMCP 2.0 的 MCP 服务器，专门用于分析 CRAQ 协议的 Erlang 实现源码并生成技术文档。

## 功能特性

- 🚀 **FastMCP 2.0**: 使用最新的 FastMCP 框架构建
- 📡 **Stdio 模式**: 通过标准输入输出进行通信
- 📝 **源码分析**: 专注于 CRAQ 协议的 Erlang 实现源码分析
- 🎯 **模板驱动**: 基于专业的技术文档分析模板
- 📦 **uv 包管理**: 使用 uv 进行依赖管理和打包
- ⚡ **uvx 支持**: 支持通过 uvx 直接调用

## 安装

### 本地开发安装（当前推荐）

```bash
# 克隆或进入项目目录
cd /Users/shuaizi/Documents/06_AI/craq_mcp

# 安装依赖
uv sync

# 直接运行
uv run craq-mcp

# 或者本地可编辑安装
uv pip install -e .
```

### 使用 uvx (需要发布到PyPI后)

```bash
uvx craq-mcp
```

### 使用 pip (需要发布到PyPI后)

```bash
pip install craq-mcp
```

### 从源码安装

```bash
git clone <repository-url>
cd craq-mcp
uv sync
uv run craq-mcp
```

## 使用方法

### 作为 MCP 服务器运行

```bash
# 直接运行
craq-mcp

# 使用 uvx 运行
uvx craq-mcp

# 调试模式
craq-mcp --debug
```

### MCP 客户端配置

#### 本地开发配置（推荐）

如果包还未发布到PyPI，使用以下配置：

```json
{
  "mcpServers": {
    "craq-mcp": {
      "command": "uv",
      "args": ["run", "--directory", "/Users/shuaizi/Documents/06_AI/craq_mcp", "craq-mcp"]
    }
  }
}
```

或者使用Python直接运行：

```json
{
  "mcpServers": {
    "craq-mcp": {
      "command": "uv",
      "args": ["run", "--directory", "/Users/shuaizi/Documents/06_AI/craq_mcp", "python", "-m", "craq_mcp.main"]
    }
  }
}
```

#### 发布后配置

包发布到PyPI后，可以使用：

```json
{
  "mcpServers": {
    "craq-mcp": {
      "timeout": 60,
      "type": "stdio",
      "command": "uvx",
      "args": [
        "--index-url",
        "https://mirrors.cloud.tencent.com/pypi/simple/",
        "craq-mcp"
      ]
    }
  }
}
```

## 可用工具

### 1. generate_craq

生成 CRAQ 协议源码分析的提示词。

**参数:**
- `craq_analysis_detail` (string): CRAQ 源码分析的详细信息，包括：
  - 分析目标：CRAQ 协议的 Erlang 实现
  - 技术重点：链式复制、消息传递、数据持久化
  - 分析深度：源码级别的深度分析
  - 输出格式：完整的技术文档

**示例:**
```json
{
  "craq_analysis_detail": "分析目标：基于 CRAQ 协议的分布式存储系统 Erlang 实现\n技术重点：链式复制机制、前驱后继节点消息传递、数据持久化落地\n分析深度：结合论文和 trace 日志进行源码级分析\n输出格式：包含架构图和代码分析的完整技术文档"
}
```

## CRAQ 源码分析模板

服务器使用专业的 CRAQ 源码分析模板，包含以下章节：

1. **第一章**: 程序启动流程分析
2. **第二章**: 消息的链式传递机制
3. **第三章**: 消息的反向传递和 Commit 过程
4. **第四章**: 数据持久化落地实现
5. **第五章**: CRAQ 协议优缺点分析
6. **技术架构图**: 使用 Mermaid 绘制系统架构
7. **源码解析**: 结合具体代码行号进行分析
8. **性能评估**: 基于论文和实现的性能分析
## 输出格式

生成的 CRAQ 源码分析文档将：
- 包含完整的HTML技术文档结构
- 集成Mermaid架构图和流程图支持
- 提供Word文档导出功能
- 包含源码高亮和代码分析
- 结合论文理论和实际实现进行对比分析

## 开发

### 环境要求

- Python 3.13+
- uv

### 开发设置

```bash
# 克隆仓库
git clone <repository-url>
cd craq-mcp

# 安装依赖
uv sync

# 运行测试
uv run pytest

# 代码格式化
uv run black src/
uv run ruff check src/
```

### 构建和发布

```bash
# pyproject中需要包含：
[[tool.uv.index]]
name = "tencent-pypi"
url = "https://mirrors.tencent.com/repository/pypi/tencent_pypi/simple"
publish-url = "https://mirrors.tencent.com/repository/pypi/tencent_pypi/simple"

# 构建包
uv build

# 本地测试
uv run craq-mcp

# 发布到 PyPI
uv publish
uv publish --index tencent-pypi --username ${USER} --password ${PASSWORD}
```

## 项目结构

```
craq-mcp/
├── pyproject.toml          # 项目配置
├── README.md              # 项目文档
├── src/
│   └── craq_mcp/
│       ├── __init__.py    # 包初始化
│       ├── main.py        # 主程序入口
│       ├── server.py      # MCP 服务器实现
│       └── craq_analysis_template.md  # CRAQ源码分析模板
└── tests/                 # 测试文件
```

## 技术栈

- **Python 3.13**: 最新 Python 版本
- **FastMCP 2.0**: MCP 服务器框架
- **Pydantic**: 数据验证和序列化
- **uv**: 包管理和构建工具

## 许可证

MIT License

## 贡献

欢迎提交 Issue 和 Pull Request！

## 更新日志

### v0.1.0

- 初始版本发布
- 支持CRAQ协议源码分析
- 包含专业的源码分析模板
- 支持HTML技术文档输出和Word导出功能
- 集成Mermaid架构图和流程图支持
