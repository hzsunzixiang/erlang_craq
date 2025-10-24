"""
CRAQ MCP - A FastMCP 2.0 based MCP server for generating patent prompts.

This package provides a Model Context Protocol (MCP) server that generates
patent prompts using the FastMCP 2.0 framework.
"""

__version__ = "0.1.0"
__author__ = "CRAQ"
__email__ = "craq@example.com"

from .server import create_server

__all__ = ["create_server", "__version__"]
