"""
FastMCP server implementation for CRAQ MCP.

This module contains the MCP server implementation using FastMCP 2.0
that provides CRAQ protocol source code analysis tools.
"""

import asyncio
from fastmcp import FastMCP
from pydantic import Field


def create_server() -> FastMCP:
    """
    Create and configure the FastMCP server.

    Returns:
        Configured FastMCP server instance
    """
    # Initialize the MCP server
    mcp = FastMCP("CRAQ MCP Source Code Analysis Server")

    @mcp.tool()
    async def generate_craq(
        craq_analysis_detail: str = Field(description="""CRAQ源码分析的详细信息，例如：
- 分析目标：CRAQ协议的Erlang实现
- 技术重点：链式复制、消息传递、数据持久化
- 分析深度：源码级别的深度分析
- 输出格式：完整的技术文档""")
    ) -> str:
        """分析CRAQ协议的Erlang实现源码并生成技术文档"""
        # 从文件读取CRAQ分析模板
        try:
            import os
            # 获取当前文件所在目录的绝对路径
            current_dir = os.path.dirname(os.path.abspath(__file__))
            template_path = os.path.join(current_dir, "craq_analysis_template.md")
            
            # 如果新模板不存在，尝试使用旧的模板文件
            if not os.path.exists(template_path):
                template_path = os.path.join(current_dir, "patent_prompt_template.md")
            
            with open(template_path, "r", encoding="utf-8") as f:
                template = f.read()
            
            # 直接替换占位符
            result = template.replace("{craq_analysis_detail}", craq_analysis_detail)
            # 兼容旧的占位符
            result = result.replace("{patent_detail}", craq_analysis_detail)

            # 压缩处理：移除多余的空行和空白字符
            lines = result.split('\n')
            compressed_lines = []
            for line in lines:
                stripped_line = line.strip()
                if stripped_line:  # 只保留非空行
                    compressed_lines.append(stripped_line)

            # 重新组合成压缩后的内容
            compressed_result = '\\n'.join(compressed_lines)

            return compressed_result
        except FileNotFoundError:
            return f"错误：CRAQ分析模板文件未找到，请确保 craq_analysis_template.md 或 patent_prompt_template.md 文件存在"
        except Exception as e:
            return f"错误：读取CRAQ分析模板时发生异常: {str(e)}"

    return mcp


async def run_server() -> None:
    """
    Run the MCP server in stdio mode.

    This function creates and runs the FastMCP server using stdio
    transport for communication with MCP clients.
    """
    # Create the server
    server = create_server()

    # Use the async stdio method directly
    await server.run_stdio_async()


def main() -> None:
    """
    Main entry point for the server.

    This function is called when the package is executed as a script
    or through the console script entry point.
    """
    try:
        asyncio.run(run_server())
    except KeyboardInterrupt:
        print("\nServer stopped by user")
    except Exception as e:
        print(f"Server error: {e}")
        raise


if __name__ == "__main__":
    main()