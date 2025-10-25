# CRAQ Paper Reading MCP Server

一个基于 FastMCP 2.0 的 MCP 服务器，专门用于阅读和分析 CRAQ 相关论文，提供英文原文复述、中文翻译和图表绘制功能。

## 功能特性

- 🚀 **FastMCP 2.0**: 使用最新的 FastMCP 框架构建
- 📡 **Stdio 模式**: 通过标准输入输出进行通信
- 📚 **论文阅读**: 专注于 CRAQ 相关论文的深度阅读和分析
- 🌐 **双语支持**: 原文复述 + 中文翻译，逐段对照
- 📊 **图表绘制**: 智能识别并绘制论文中的图表和架构图
- 📦 **uv 包管理**: 使用 uv 进行依赖管理和打包
- ⚡ **uvx 支持**: 支持通过 uvx 直接调用

## 支持的论文

1. **Chain Replication for Supporting High Throughput and Availability**
   - 经典链式复制论文
   - 高吞吐量和可用性保证
   
2. **Object Storage on CRAQ High-throughput chain replication for read-mostly workloads**
   - CRAQ 协议详细描述
   - 读密集型工作负载优化

## 安装

### 本地开发安装（当前推荐）

```bash
# 克隆或进入项目目录
cd /Users/shuaizi/Documents/06_AI/craq_paper_mcp

# 安装依赖
uv sync

# 直接运行
uv run craq-paper-mcp

# 或者本地可编辑安装
uv pip install -e .
```

### 使用 uvx (需要发布到PyPI后)

```bash
uvx craq-paper-mcp
```

### 使用 pip (需要发布到PyPI后)

```bash
pip install craq-paper-mcp
```

### 从源码安装

```bash
git clone <repository-url>
cd craq-paper-mcp
uv sync
uv run craq-paper-mcp
```

## 使用方法

### 作为 MCP 服务器运行

```bash
# 直接运行
craq-paper-mcp

# 使用 uvx 运行
uvx craq-paper-mcp

# 调试模式
craq-paper-mcp --debug
```

### MCP 客户端配置

#### 本地开发配置（推荐）

如果包还未发布到PyPI，使用以下配置：

```json
{
  "mcpServers": {
    "craq-paper-mcp": {
      "command": "uv",
      "args": ["run", "--directory", "/Users/shuaizi/Documents/06_AI/craq_paper_mcp", "craq-paper-mcp"]
    }
  }
}
```

或者使用Python直接运行：

```json
{
  "mcpServers": {
    "craq-paper-mcp": {
      "command": "uv",
      "args": ["run", "--directory", "/Users/shuaizi/Documents/06_AI/craq_paper_mcp", "python", "-m", "craq_mcp.main"]
    }
  }
}
```

#### 发布后配置

包发布到PyPI后，可以使用：

```json
{
  "mcpServers": {
    "craq-paper-mcp": {
      "timeout": 60,
      "type": "stdio",
      "command": "uvx",
      "args": [
        "--index-url",
        "https://mirrors.cloud.tencent.com/pypi/simple/",
        "craq-paper-mcp"
      ]
    }
  }
}
```

## 可用工具

### 1. read_paper

阅读和分析 CRAQ 相关论文，提供英文原文复述和中文翻译。

**参数:**
- `paper_content` (string): 论文内容或段落，支持以下论文：
  - Chain Replication for Supporting High Throughput and Availability
  - Object Storage on CRAQ High-throughput chain replication for read-mostly workloads
- `section_type` (string): 段落类型，可选值：
  - "abstract" - 摘要
  - "introduction" - 引言
  - "methodology" - 方法论
  - "results" - 结果
  - "conclusion" - 结论
  - "figure" - 图表描述

**示例:**
```json
{
  "paper_content": "Chain replication provides strong consistency guarantees and has been shown to support large throughput when clients exhibit read-mostly workloads...",
  "section_type": "abstract"
}
```

### 2. analyze_figure

分析和绘制论文中的图表。

**参数:**
- `figure_description` (string): 图表的描述信息
- `figure_type` (string): 图表类型：
  - "architecture" - 系统架构图
  - "flowchart" - 流程图
  - "sequence" - 时序图
  - "performance" - 性能图表

**示例:**
```json
{
  "figure_description": "Figure 1: Chain replication architecture showing head, middle nodes, and tail",
  "figure_type": "architecture"
}
```

## 论文阅读模板

服务器使用专业的论文阅读模板，包含以下功能：

1. **英文原文复述**: 原原本本复述英文内容
2. **中文翻译**: 每段英文对应的中文翻译
3. **段落分析**: 按段落进行结构化分析
4. **图表绘制**: 使用 Mermaid 绘制论文中的图表
5. **关键概念**: 提取和解释关键技术概念
6. **架构分析**: 深入分析系统架构和设计思路

## 输出格式

生成的论文阅读文档将：
- 包含完整的HTML文档结构
- 英文原文和中文翻译对照显示
- 集成Mermaid图表支持
- 提供Word文档导出功能
- 包含论文结构化分析
- 支持图表转换为静态图片

## 开发

### 环境要求

- Python 3.13+
- uv

### 开发设置

```bash
# 克隆仓库
git clone <repository-url>
cd craq-paper-mcp

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
uv run craq-paper-mcp

# 发布到 PyPI
uv publish
uv publish --index tencent-pypi --username ${USER} --password ${PASSWORD}
```

## 项目结构

```
craq-paper-mcp/
├── pyproject.toml          # 项目配置
├── README.md              # 项目文档
├── src/
│   └── craq_mcp/
│       ├── __init__.py    # 包初始化
│       ├── main.py        # 主程序入口
│       ├── server.py      # MCP 服务器实现
│       └── paper_reading_template.md  # 论文阅读模板
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
- 支持CRAQ相关论文阅读
- 包含英文原文复述和中文翻译功能
- 支持HTML文档输出和Word导出功能
- 集成Mermaid图表绘制支持
- 支持论文图表分析和绘制