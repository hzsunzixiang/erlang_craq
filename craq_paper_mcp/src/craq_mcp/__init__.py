"""
CRAQ Paper MCP - A FastMCP 2.0 based MCP server for reading CRAQ papers.

This package provides a Model Context Protocol (MCP) server that reads
and analyzes CRAQ related academic papers with English recitation and Chinese translation.
"""

__version__ = "0.1.0"
__author__ = "CRAQ Paper Team"
__email__ = "craq-paper@example.com"

from .server import create_server

__all__ = ["create_server", "__version__"]
