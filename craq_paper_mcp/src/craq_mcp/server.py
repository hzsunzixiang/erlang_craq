"""
FastMCP server implementation for CRAQ Paper Reading MCP.

This module contains the MCP server implementation using FastMCP 2.0
that provides CRAQ paper reading and analysis tools.
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
    mcp = FastMCP("CRAQ Paper Reading MCP Server")

    @mcp.tool()
    async def read_paper(
        paper_content: str = Field(description="""论文内容或段落，支持以下论文：
- Chain Replication for Supporting High Throughput and Availability
- Object Storage on CRAQ High-throughput chain replication for read-mostly workloads"""),
        section_type: str = Field(description="""段落类型，可选值：
- abstract - 摘要
- introduction - 引言  
- methodology - 方法论
- results - 结果
- conclusion - 结论
- figure - 图表描述""", default="general")
    ) -> str:
        """阅读和分析CRAQ相关论文，提供英文原文复述和中文翻译"""
        # 从文件读取论文阅读模板
        try:
            import os
            # 获取当前文件所在目录的绝对路径
            current_dir = os.path.dirname(os.path.abspath(__file__))
            template_path = os.path.join(current_dir, "paper_reading_template.md")
            
            # 如果新模板不存在，尝试使用旧的模板文件
            if not os.path.exists(template_path):
                template_path = os.path.join(current_dir, "craq_analysis_template.md")
            
            with open(template_path, "r", encoding="utf-8") as f:
                template = f.read()
            
            # 替换占位符
            result = template.replace("{paper_content}", paper_content)
            result = result.replace("{section_type}", section_type)

            return result
        except FileNotFoundError:
            return f"错误：论文阅读模板文件未找到，请确保 paper_reading_template.md 文件存在"
        except Exception as e:
            return f"错误：读取论文阅读模板时发生异常: {str(e)}"

    @mcp.tool()
    async def analyze_figure(
        figure_description: str = Field(description="""图表的描述信息，例如：
- Figure 1: Chain replication architecture
- Figure 2: Performance comparison chart
- Table 1: Experimental results"""),
        figure_type: str = Field(description="""图表类型，可选值：
- architecture - 系统架构图
- flowchart - 流程图
- sequence - 时序图
- performance - 性能图表
- table - 数据表格""", default="architecture")
    ) -> str:
        """分析和绘制论文中的图表"""
        try:
            # 根据图表类型生成相应的Mermaid图表代码
            if figure_type == "architecture":
                mermaid_code = f"""
```mermaid
graph TD
    A[Client] --> B[Head Node]
    B --> C[Middle Node 1]
    C --> D[Middle Node 2]
    D --> E[Tail Node]
    E --> F[Response to Client]
    
    style A fill:#e1f5fe
    style B fill:#f3e5f5
    style C fill:#f3e5f5
    style D fill:#f3e5f5
    style E fill:#e8f5e8
    style F fill:#fff3e0
```

**图表说明**: {figure_description}

**架构分析**:
1. **客户端 (Client)**: 发起读写请求
2. **头节点 (Head Node)**: 处理写请求，转发给下一个节点
3. **中间节点 (Middle Nodes)**: 传递更新，维护数据副本
4. **尾节点 (Tail Node)**: 确认提交，响应读请求
"""
            elif figure_type == "sequence":
                mermaid_code = f"""
```mermaid
sequenceDiagram
    participant C as Client
    participant H as Head
    participant M as Middle
    participant T as Tail
    
    C->>H: Write Request
    H->>M: Forward Update
    M->>T: Forward Update
    T->>M: Ack
    M->>H: Ack
    H->>C: Write Complete
    
    C->>T: Read Request
    T->>C: Read Response
```

**时序图说明**: {figure_description}

**流程分析**:
1. 写操作从头节点开始，沿链传播到尾节点
2. 确认从尾节点返回到头节点
3. 读操作直接从尾节点获取最新数据
"""
            elif figure_type == "performance":
                mermaid_code = f"""
```mermaid
xychart-beta
    title "CRAQ vs Chain Replication Performance"
    x-axis [1, 2, 4, 8, 16, 32]
    y-axis "Throughput (ops/sec)" 0 --> 10000
    line [1000, 2000, 3500, 6000, 8500, 9500]
    line [1000, 1800, 2800, 3200, 3500, 3600]
```

**性能图表说明**: {figure_description}

**性能分析**:
- 蓝线: CRAQ协议性能
- 红线: 传统链式复制性能
- CRAQ在读密集型工作负载下表现更优
"""
            else:
                mermaid_code = f"""
**图表描述**: {figure_description}

**图表类型**: {figure_type}

**分析说明**: 
由于图表类型为 {figure_type}，暂时无法自动生成对应的Mermaid图表。
建议提供更详细的图表描述信息，或选择支持的图表类型：
- architecture (架构图)
- sequence (时序图) 
- performance (性能图表)

**手动绘制提示**:
请根据论文中的具体描述，手动绘制此图表。
"""
            
            return mermaid_code
            
        except Exception as e:
            return f"错误：分析图表时发生异常: {str(e)}"

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