<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>CRAQ论文阅读分析</title>
    <script src="https://cdn.jsdelivr.net/npm/mermaid@10.6.1/dist/mermaid.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js"></script>
    <script src="https://unpkg.com/docx@8.2.2/build/index.js"></script>
    <style>
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            line-height: 1.6;
            color: #333;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            background-color: #f8f9fa;
        }
        
        .container {
            background: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        
        h1 {
            color: #2c3e50;
            text-align: center;
            border-bottom: 3px solid #3498db;
            padding-bottom: 10px;
            margin-bottom: 30px;
        }
        
        h2 {
            color: #34495e;
            border-left: 4px solid #3498db;
            padding-left: 15px;
            margin-top: 30px;
        }
        
        h3 {
            color: #2980b9;
            margin-top: 25px;
        }
        
        .section {
            margin-bottom: 30px;
            padding: 20px;
            border: 1px solid #e0e0e0;
            border-radius: 8px;
            background-color: #fafafa;
        }
        
        .english-text {
            background-color: #e8f4fd;
            padding: 15px;
            border-left: 4px solid #2196F3;
            margin: 10px 0;
            font-style: italic;
            border-radius: 5px;
        }
        
        .chinese-text {
            background-color: #f0f8e8;
            padding: 15px;
            border-left: 4px solid #4CAF50;
            margin: 10px 0;
            border-radius: 5px;
        }
        
        .section-type {
            display: inline-block;
            background: #3498db;
            color: white;
            padding: 5px 15px;
            border-radius: 20px;
            font-size: 0.9em;
            margin-bottom: 15px;
        }
        
        .mermaid {
            text-align: center;
            margin: 20px 0;
            background: white;
            padding: 20px;
            border-radius: 8px;
            border: 1px solid #ddd;
        }
        
        .export-buttons {
            text-align: center;
            margin: 20px 0;
        }
        
        .export-btn {
            background: #3498db;
            color: white;
            border: none;
            padding: 10px 20px;
            margin: 0 10px;
            border-radius: 5px;
            cursor: pointer;
            font-size: 16px;
        }
        
        .export-btn:hover {
            background: #2980b9;
        }
        
        .analysis-box {
            background: #fff3cd;
            border: 1px solid #ffeaa7;
            padding: 15px;
            border-radius: 5px;
            margin: 15px 0;
        }
        
        .key-concepts {
            background: #f8f9fa;
            border: 1px solid #dee2e6;
            padding: 15px;
            border-radius: 5px;
            margin: 15px 0;
        }
        
        .figure-placeholder {
            background: #e9ecef;
            border: 2px dashed #6c757d;
            padding: 30px;
            text-align: center;
            margin: 20px 0;
            border-radius: 8px;
            color: #6c757d;
        }
        
        @media print {
            body { background: white; }
            .container { box-shadow: none; }
            .export-buttons { display: none; }
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🔬 CRAQ论文深度阅读分析</h1>
        
        <div class="export-buttons">
            <button class="export-btn" onclick="exportToWord()">📄 导出Word文档</button>
            <button class="export-btn" onclick="window.print()">🖨️ 打印PDF</button>
        </div>

        <div class="section">
            <div class="section-type">{section_type}</div>
            
            <h2>📖 英文原文复述</h2>
            <div class="english-text">
                <strong>Original English Text:</strong><br>
                {paper_content}
            </div>
            
            <h2>🌐 中文翻译</h2>
            <div class="chinese-text">
                <strong>中文翻译:</strong><br>
                <!-- 这里需要根据具体的英文内容进行翻译 -->
                根据提供的英文段落内容，这里将显示对应的中文翻译。请提供具体的英文段落以获得准确的翻译。
            </div>
            
            <h2>📊 段落分析</h2>
            <div class="analysis-box">
                <h3>🔍 内容要点</h3>
                <ul>
                    <li><strong>主要概念</strong>: 根据段落内容提取的核心概念</li>
                    <li><strong>技术细节</strong>: 涉及的技术实现和方法</li>
                    <li><strong>创新点</strong>: 论文提出的新思路或改进</li>
                    <li><strong>实验结果</strong>: 相关的实验数据和结论</li>
                </ul>
            </div>
            
            <div class="key-concepts">
                <h3>🎯 关键概念解释</h3>
                <p><strong>Chain Replication (链式复制)</strong>: 一种分布式系统中的数据复制策略，通过链式结构保证数据一致性和高可用性。</p>
                <p><strong>CRAQ协议</strong>: Chain Replication with Apportioned Queries的缩写，是对传统链式复制的改进，支持从任意节点读取数据。</p>
                <p><strong>Read-mostly Workloads (读密集型负载)</strong>: 指系统中读操作远多于写操作的工作负载模式。</p>
            </div>
        </div>

        <div class="section">
            <h2>📈 相关图表分析</h2>
            
            <div class="mermaid">
                graph TD
                    A[Client] -->|Write Request| B[Head Node]
                    B -->|Forward| C[Middle Node 1]
                    C -->|Forward| D[Middle Node 2]
                    D -->|Forward| E[Tail Node]
                    E -->|Ack| D
                    D -->|Ack| C
                    C -->|Ack| B
                    B -->|Response| A
                    
                    F[Client] -->|Read Request| E
                    E -->|Read Response| F
                    
                    style A fill:#e1f5fe
                    style B fill:#f3e5f5
                    style C fill:#f3e5f5
                    style D fill:#f3e5f5
                    style E fill:#e8f5e8
                    style F fill:#fff3e0
            </div>
            
            <div class="analysis-box">
                <h3>🔧 架构说明</h3>
                <p><strong>写操作流程</strong>: 客户端向头节点发送写请求，请求沿链传播到尾节点，然后确认信息反向传播回头节点。</p>
                <p><strong>读操作流程</strong>: 在CRAQ协议中，客户端可以直接从尾节点读取最新的已提交数据，提高了读操作的性能。</p>
            </div>
        </div>

        <div class="section">
            <h2>🎨 图表绘制说明</h2>
            
            <div class="figure-placeholder">
                <h3>📊 论文图表重现</h3>
                <p>如果论文中包含以下类型的图表，系统将尝试使用Mermaid重新绘制：</p>
                <ul style="text-align: left; display: inline-block;">
                    <li>✅ 系统架构图 (Architecture Diagrams)</li>
                    <li>✅ 流程图 (Flowcharts)</li>
                    <li>✅ 时序图 (Sequence Diagrams)</li>
                    <li>✅ 性能对比图 (Performance Charts)</li>
                    <li>⚠️ 复杂数学公式图表 (需要手动处理)</li>
                    <li>⚠️ 实验环境照片 (无法重现)</li>
                </ul>
                <p><strong>提示</strong>: 如果遇到无法自动绘制的图表，请提供详细描述，我们将给出绘制建议。</p>
            </div>
        </div>

        <div class="section">
            <h2>📝 阅读总结</h2>
            <div class="analysis-box">
                <h3>🎯 本段落核心观点</h3>
                <p>根据提供的论文内容，本段落的核心观点和贡献将在这里总结。</p>
                
                <h3>🔗 与其他章节的联系</h3>
                <p>分析本段落与论文其他部分的逻辑关系和承接关系。</p>
                
                <h3>💡 技术启发</h3>
                <p>从技术角度分析本段落提供的启发和可借鉴的思路。</p>
            </div>
        </div>
    </div>

    <script>
        // 初始化Mermaid
        mermaid.initialize({
            startOnLoad: true,
            theme: 'default',
            flowchart: {
                useMaxWidth: true,
                htmlLabels: true
            }
        });

        // 导出Word文档功能
        async function exportToWord() {
            try {
                // 将Mermaid图表转换为图片
                const mermaidElements = document.querySelectorAll('.mermaid');
                const images = [];
                
                for (let element of mermaidElements) {
                    const canvas = await html2canvas(element);
                    const imgData = canvas.toDataURL('image/png');
                    images.push(imgData);
                }

                // 创建Word文档内容
                const doc = new docx.Document({
                    sections: [{
                        properties: {},
                        children: [
                            new docx.Paragraph({
                                text: "CRAQ论文阅读分析",
                                heading: docx.HeadingLevel.TITLE,
                                alignment: docx.AlignmentType.CENTER,
                            }),
                            new docx.Paragraph({
                                text: "英文原文:",
                                heading: docx.HeadingLevel.HEADING_1,
                            }),
                            new docx.Paragraph({
                                text: "{paper_content}",
                            }),
                            new docx.Paragraph({
                                text: "中文翻译:",
                                heading: docx.HeadingLevel.HEADING_1,
                            }),
                            new docx.Paragraph({
                                text: "根据提供的英文段落内容进行的中文翻译。",
                            }),
                        ],
                    }],
                });

                // 生成并下载Word文档
                const blob = await docx.Packer.toBlob(doc);
                saveAs(blob, 'CRAQ论文阅读分析-包含图表.docx');
                
            } catch (error) {
                console.error('导出Word文档时出错:', error);
                alert('导出失败，请检查浏览器兼容性');
            }
        }

        // 页面加载完成后的处理
        document.addEventListener('DOMContentLoaded', function() {
            console.log('CRAQ论文阅读分析页面加载完成');
        });
    </script>
</body>
</html>