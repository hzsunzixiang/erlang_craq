# CRAQ MCP Patent Prompt Server

一个基于 FastMCP 2.0 的 MCP 服务器，专门用于生成云计算领域的专利技术交底书提示词。

## 功能特性

- 🚀 **FastMCP 2.0**: 使用最新的 FastMCP 框架构建
- 📡 **Stdio 模式**: 通过标准输入输出进行通信
- 📝 **专利专业**: 专注于云计算领域的专利技术交底书生成
- 🎯 **模板驱动**: 基于专业的专利编写模板
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

生成编写专利的提示词。

**参数:**
- `patent_detail` (string): 专利的详细信息，包括：
  - 专利题目
  - 现有技术痛点
  - 专利创新点
  - 关键概念介绍

**示例:**
```json
{
  "patent_detail": "专利题目：基于云计算的虚拟机资源调度优化方法\n现有技术痛点：传统虚拟机资源调度效率低下，资源利用率不高\n专利创新点：提出了一种基于机器学习的智能调度算法\n关键概念介绍：云计算、虚拟机调度、资源优化、机器学习"
}
```

## 专利技术交底书模板

服务器使用专业的专利技术交底书模板，包含以下章节：

1. **第一章**: 关键术语和定义
2. **第二章**: 技术关键点（概述+分点说明）
3. **第三章**: 技术背景和时代背景
4. **第四章**: 现有技术方案（描述+Mermaid架构图）
5. **第五章**: 现有技术缺点
6. **第六章**: 本专利解决的问题
7. **第七章**: 产品层面方案
8. **第八章**: 技术层面方案（总体描述+Mermaid架构图+关键流程）
9. **第九章**: 有益效果

## 输出格式

生成的提示词将：
- 包含完整的HTML文档结构
- 集成Mermaid图表支持
- 提供Word文档导出功能
- 包含必要的JavaScript库支持

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
│       └── patent_prompt_template.md  # 专利提示词模板
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
- 支持专利技术交底书提示词生成
- 包含专业的专利编写模板
- 支持HTML输出和Word导出功能
- 集成Mermaid图表支持
