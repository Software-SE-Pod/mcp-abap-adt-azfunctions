import { McpError, ErrorCode } from "@modelcontextprotocol/sdk/types.js";
import { BaseHandler } from './BaseHandler.js';
import type { ToolDefinition } from '../types/tools.js';
import { getBestPractices, getBestPracticesByCategory, searchBestPractices } from '../lib/best-practices.js';

export class BestPracticesHandlers extends BaseHandler {
  getTools(): ToolDefinition[] {
    return [
      {
        name: 'getABAPBestPractices',
        description: 'Retrieve comprehensive enterprise-grade ABAP development best practices including coding standards, naming conventions, performance guidelines, security practices, architectural patterns, error handling, documentation, testing, code review processes, and transport management',
        inputSchema: {
          type: 'object',
          properties: {
            category: {
              type: 'string',
              description: 'Optional: Filter by specific category (codingStandards, namingConventions, performanceGuidelines, securityPractices, architecturalPatterns, errorHandling, documentation, testing, codeReview, transportManagement)',
              optional: true
            },
            search: {
              type: 'string',
              description: 'Optional: Search best practices by keyword',
              optional: true
            }
          }
        }
      }
    ];
  }

  async handle(toolName: string, args: any): Promise<any> {
    switch (toolName) {
      case 'getABAPBestPractices':
        return this.handleGetBestPractices(args);
      default:
        throw new McpError(ErrorCode.MethodNotFound, `Unknown best practices tool: ${toolName}`);
    }
  }

  private async handleGetBestPractices(args: any) {
    const startTime = performance.now();
    try {
      let result: any;

      if (args.search) {
        // Search mode
        result = {
          type: 'search_results',
          keyword: args.search,
          results: searchBestPractices(args.search)
        };
      } else if (args.category) {
        // Category filter mode
        result = {
          type: 'category',
          category: args.category,
          practices: getBestPracticesByCategory(args.category as any)
        };
      } else {
        // Full best practices
        result = getBestPractices();
      }

      this.trackRequest(startTime, true);
      return {
        content: [
          {
            type: 'text',
            text: JSON.stringify(result, null, 2)
          }
        ]
      };
    } catch (error: any) {
      this.trackRequest(startTime, false);
      throw new McpError(
        ErrorCode.InternalError,
        `Failed to retrieve best practices: ${error.message || 'Unknown error'}`
      );
    }
  }
}
