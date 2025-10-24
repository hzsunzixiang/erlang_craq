"""
Main entry point for CRAQ MCP Prompt Server.

This module provides the main entry point for running the MCP server
in stdio mode, handling command line arguments and server lifecycle.
"""

import argparse
import asyncio
import sys
import signal
import os
from typing import NoReturn

from .server import run_server
from . import __version__


def create_parser() -> argparse.ArgumentParser:
    """
    Create command line argument parser.

    Returns:
        Configured ArgumentParser instance
    """
    parser = argparse.ArgumentParser(
        prog="craq-mcp",
        description="CRAQ MCP Prompt Server - A FastMCP 2.0 based server for generating prompts",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  craq-mcp                    # Run server in stdio mode
  craq-mcp --version          # Show version information
  uvx craq-mcp                # Run via uvx

For more information, visit: https://github.com/craq/craq-mcp
        """
    )

    parser.add_argument(
        "--version",
        action="version",
        version=f"%(prog)s {__version__}"
    )

    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug mode with verbose logging"
    )

    return parser


def setup_signal_handlers():
    """Setup signal handlers for graceful shutdown."""
    def signal_handler(signum, frame):
        sys.exit(0)

    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)


async def async_main(args) -> None:
    """
    Async main function that runs the server.

    This function handles the server startup and runs the MCP server
    in stdio mode for communication with MCP clients.
    """
    if args.debug:
        print(f"Starting CRAQ MCP Prompt Server v{__version__} in debug mode", file=sys.stderr)
        print("Server ready for MCP client connections via stdio", file=sys.stderr)

    try:
        # Run the server
        await run_server()
    except KeyboardInterrupt:
        if args.debug:
            print("\nServer stopped by user", file=sys.stderr)
    except Exception as e:
        if args.debug:
            print(f"Server error: {e}", file=sys.stderr)
        raise


def main() -> NoReturn:
    """
    Main entry point for the CRAQ MCP server.

    This function is called when the package is executed as a script
    or through the console script entry point defined in pyproject.toml.

    The server runs in stdio mode and communicates with MCP clients
    through standard input/output streams.
    """
    # Parse arguments first to handle --version and --help synchronously
    parser = create_parser()
    args = parser.parse_args()

    # Setup signal handlers
    setup_signal_handlers()

    # Set asyncio policy to avoid conflicts
    if os.name == 'nt':  # Windows
        asyncio.set_event_loop_policy(asyncio.WindowsProactorEventLoopPolicy())
    else:  # Unix/Linux/macOS
        try:
            import uvloop
            asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
        except ImportError:
            pass

    # Apply nest_asyncio to handle potential event loop conflicts
    try:
        import nest_asyncio
        nest_asyncio.apply()
    except ImportError:
        pass

    # Create a new event loop to avoid conflicts
    try:
        # Close any existing loop
        try:
            loop = asyncio.get_event_loop()
            if not loop.is_closed():
                loop.close()
        except RuntimeError:
            pass

        # Create and set a new event loop
        new_loop = asyncio.new_event_loop()
        asyncio.set_event_loop(new_loop)

        try:
            new_loop.run_until_complete(async_main(args))
        finally:
            new_loop.close()

    except KeyboardInterrupt:
        sys.exit(0)
    except Exception as e:
        print(f"Fatal error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()