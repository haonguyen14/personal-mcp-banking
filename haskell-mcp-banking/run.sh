#!/usr/bin/env bash
set -a; source .env; set +a
stack exec haskell-mcp-banking-exe
