# MCP Prompt服务器

## Core Features

- FastMCP 2.0服务器实现

- Stdio模式通信

- 智能Prompt生成

- uv包管理

- uvx命令行调用

## Tech Stack

{
  "language": "Python 3.13",
  "framework": "FastMCP 2.0",
  "package_manager": "uv",
  "deployment": "pip package with uvx support"
}

## Design

模块化架构设计，包含服务器核心、Prompt生成器和命令行入口，支持标准MCP协议通信

## Plan

Note: 

- [ ] is holding
- [/] is doing
- [X] is done

---

[X] 创建项目目录结构和基础文件

[X] 配置pyproject.toml文件，设置项目元数据和依赖

[X] 实现prompts.py模块，创建Prompt生成器类

[X] 实现server.py模块，创建FastMCP服务器

[X] 实现main.py入口文件，处理stdio模式启动

[X] 创建__init__.py文件，设置包导出

[X] 编写README.md文档，说明安装和使用方法

[X] 使用uv构建项目并测试uvx调用
