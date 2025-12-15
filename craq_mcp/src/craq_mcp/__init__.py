"""
CRAQ MCP - A FastMCP 2.0 based MCP server for CRAQ source code analysis.

This package provides a Model Context Protocol (MCP) server that analyzes
CRAQ protocol Erlang implementation and generates technical documentation.
"""

__version__ = "0.1.0"
__author__ = "CRAQ"
__email__ = "craq@example.com"

from .server import create_server

__all__ = ["create_server", "__version__"]