#!/usr/bin/env python3
"""
Test script to verify MCP server connection.
"""

import json
import subprocess
import sys
import time

def test_mcp_server():
    """Test the MCP server connection."""
    print("🧪 Testing CRAQ MCP Source Code Analysis Server connection...")

    # MCP initialize message
    init_message = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {
                "name": "test-client",
                "version": "1.0.0"
            }
        }
    }

    try:
        # Start the MCP server process
        process = subprocess.Popen(
            ["uv", "run", "craq-mcp"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )

        # Send initialize message
        init_json = json.dumps(init_message) + "\n"
        process.stdin.write(init_json)
        process.stdin.flush()

        # Wait for response
        time.sleep(2)

        # Read response
        response = process.stdout.readline()
        if response:
            print("✅ Server responded:")
            try:
                response_data = json.loads(response)
                print(json.dumps(response_data, indent=2))
            except json.JSONDecodeError:
                print(response)
        else:
            print("❌ No response from server")

        # Check for errors
        error_output = process.stderr.read()
        if error_output:
            print("⚠️  Server stderr:")
            print(error_output)

        # Terminate process
        process.terminate()
        process.wait()

        return True

    except Exception as e:
        print(f"❌ Error testing server: {e}")
        return False

if __name__ == "__main__":
    success = test_mcp_server()
    sys.exit(0 if success else 1)