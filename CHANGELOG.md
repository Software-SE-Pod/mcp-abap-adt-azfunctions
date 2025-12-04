# Changelog

## [0.2.0] - Azure Functions Migration
- Migrated from MCP stdio server to Azure Functions HTTP endpoint
- Added single `/api/mcp` endpoint following Gateway Routing pattern
- Integrated Azure Functions Core Tools for local development
- Added `host.json` and `local.settings.json` for Azure Functions configuration
- Updated scripts in `package.json` for Azure Functions workflow
- All existing handlers now accessible via single HTTP endpoint

## [0.1.1] - Better unified response structure
- Improved and unified the response structure.

## [0.1.0] - Initial Commit
- Initial project setup.
