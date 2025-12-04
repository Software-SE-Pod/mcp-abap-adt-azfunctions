#!/usr/bin/env node

import { config } from 'dotenv';
import { app, HttpRequest, HttpResponseInit, InvocationContext } from '@azure/functions';
import { McpError, ErrorCode } from "@modelcontextprotocol/sdk/types.js";
import { ADTClient, session_types } from "abap-adt-api";
import path from 'path';
import { AuthHandlers } from './handlers/AuthHandlers.js';
import { TransportHandlers } from './handlers/TransportHandlers.js';
import { ObjectHandlers } from './handlers/ObjectHandlers.js';
import { ClassHandlers } from './handlers/ClassHandlers.js';
import { CodeAnalysisHandlers } from './handlers/CodeAnalysisHandlers.js';
import { ObjectLockHandlers } from './handlers/ObjectLockHandlers.js';
import { ObjectSourceHandlers } from './handlers/ObjectSourceHandlers.js';
import { ObjectDeletionHandlers } from './handlers/ObjectDeletionHandlers.js';
import { ObjectManagementHandlers } from './handlers/ObjectManagementHandlers.js';
import { ObjectRegistrationHandlers } from './handlers/ObjectRegistrationHandlers.js';
import { NodeHandlers } from './handlers/NodeHandlers.js';
import { DiscoveryHandlers } from './handlers/DiscoveryHandlers.js';
import { UnitTestHandlers } from './handlers/UnitTestHandlers.js';
import { PrettyPrinterHandlers } from './handlers/PrettyPrinterHandlers.js';
import { GitHandlers } from './handlers/GitHandlers.js';
import { DdicHandlers } from './handlers/DdicHandlers.js';
import { ServiceBindingHandlers } from './handlers/ServiceBindingHandlers.js';
import { QueryHandlers } from './handlers/QueryHandlers.js';
import { FeedHandlers } from './handlers/FeedHandlers.js';
import { DebugHandlers } from './handlers/DebugHandlers.js';
import { RenameHandlers } from './handlers/RenameHandlers.js';
import { AtcHandlers } from './handlers/AtcHandlers.js';
import { TraceHandlers } from './handlers/TraceHandlers.js';
import { RefactorHandlers } from './handlers/RefactorHandlers.js';
import { RevisionHandlers } from './handlers/RevisionHandlers.js';
import { BestPracticesHandlers } from './handlers/BestPracticesHandlers.js';

config({ path: path.resolve(__dirname, '../.env') });

export class AbapAdtService {
  private adtClient: ADTClient;
  private authHandlers: AuthHandlers;
  private transportHandlers: TransportHandlers;
  private objectHandlers: ObjectHandlers;
  private classHandlers: ClassHandlers;
  private codeAnalysisHandlers: CodeAnalysisHandlers;
  private objectLockHandlers: ObjectLockHandlers;
  private objectSourceHandlers: ObjectSourceHandlers;
  private objectDeletionHandlers: ObjectDeletionHandlers;
  private objectManagementHandlers: ObjectManagementHandlers;
  private objectRegistrationHandlers: ObjectRegistrationHandlers;
  private nodeHandlers: NodeHandlers;
  private discoveryHandlers: DiscoveryHandlers;
  private unitTestHandlers: UnitTestHandlers;
  private prettyPrinterHandlers: PrettyPrinterHandlers;
  private gitHandlers: GitHandlers;
  private ddicHandlers: DdicHandlers;
  private serviceBindingHandlers: ServiceBindingHandlers;
  private queryHandlers: QueryHandlers;
  private feedHandlers: FeedHandlers;
  private debugHandlers: DebugHandlers;
  private renameHandlers: RenameHandlers;
  private atcHandlers: AtcHandlers;
  private traceHandlers: TraceHandlers;
  private refactorHandlers: RefactorHandlers;
  private revisionHandlers: RevisionHandlers;
  private bestPracticesHandlers: BestPracticesHandlers;

  constructor() {
    const missingVars = ['SAP_URL', 'SAP_USER', 'SAP_PASSWORD'].filter(v => !process.env[v]);
    if (missingVars.length > 0) {
      throw new Error(`Missing required environment variables: ${missingVars.join(', ')}`);
    }
    
    this.adtClient = new ADTClient(
      process.env.SAP_URL as string,
      process.env.SAP_USER as string,
      process.env.SAP_PASSWORD as string,
      process.env.SAP_CLIENT as string,
      process.env.SAP_LANGUAGE as string
    );
    this.adtClient.stateful = session_types.stateful
    
    // Initialize handlers
    this.authHandlers = new AuthHandlers(this.adtClient);
    this.transportHandlers = new TransportHandlers(this.adtClient);
    this.objectHandlers = new ObjectHandlers(this.adtClient);
    this.classHandlers = new ClassHandlers(this.adtClient);
    this.codeAnalysisHandlers = new CodeAnalysisHandlers(this.adtClient);
    this.objectLockHandlers = new ObjectLockHandlers(this.adtClient);
    this.objectSourceHandlers = new ObjectSourceHandlers(this.adtClient);
    this.objectDeletionHandlers = new ObjectDeletionHandlers(this.adtClient);
    this.objectManagementHandlers = new ObjectManagementHandlers(this.adtClient);
    this.objectRegistrationHandlers = new ObjectRegistrationHandlers(this.adtClient);
    this.nodeHandlers = new NodeHandlers(this.adtClient);
    this.discoveryHandlers = new DiscoveryHandlers(this.adtClient);
    this.unitTestHandlers = new UnitTestHandlers(this.adtClient);
    this.prettyPrinterHandlers = new PrettyPrinterHandlers(this.adtClient);
    this.gitHandlers = new GitHandlers(this.adtClient);
    this.ddicHandlers = new DdicHandlers(this.adtClient);
    this.serviceBindingHandlers = new ServiceBindingHandlers(this.adtClient);
    this.queryHandlers = new QueryHandlers(this.adtClient);
    this.feedHandlers = new FeedHandlers(this.adtClient);
    this.debugHandlers = new DebugHandlers(this.adtClient);
    this.renameHandlers = new RenameHandlers(this.adtClient);
    this.atcHandlers = new AtcHandlers(this.adtClient);
    this.traceHandlers = new TraceHandlers(this.adtClient);
    this.refactorHandlers = new RefactorHandlers(this.adtClient);
    this.revisionHandlers = new RevisionHandlers(this.adtClient);
    this.bestPracticesHandlers = new BestPracticesHandlers(this.adtClient);
  }

  private serializeResult(result: any) {
    return JSON.stringify(result, (key, value) => 
      typeof value === 'bigint' ? value.toString() : value
    );
  }

  getTools() {
    return [
      ...this.authHandlers.getTools(),
      ...this.transportHandlers.getTools(),
      ...this.objectHandlers.getTools(),
      ...this.classHandlers.getTools(),
      ...this.codeAnalysisHandlers.getTools(),
      ...this.objectLockHandlers.getTools(),
      ...this.objectSourceHandlers.getTools(),
      ...this.objectDeletionHandlers.getTools(),
      ...this.objectManagementHandlers.getTools(),
      ...this.objectRegistrationHandlers.getTools(),
      ...this.nodeHandlers.getTools(),
      ...this.discoveryHandlers.getTools(),
      ...this.unitTestHandlers.getTools(),
      ...this.prettyPrinterHandlers.getTools(),
      ...this.gitHandlers.getTools(),
      ...this.ddicHandlers.getTools(),
      ...this.serviceBindingHandlers.getTools(),
      ...this.queryHandlers.getTools(),
      ...this.feedHandlers.getTools(),
      ...this.debugHandlers.getTools(),
      ...this.renameHandlers.getTools(),
      ...this.atcHandlers.getTools(),
      ...this.traceHandlers.getTools(),
      ...this.refactorHandlers.getTools(),
      ...this.revisionHandlers.getTools(),
      ...this.bestPracticesHandlers.getTools(),
      {
        name: 'healthcheck',
        description: 'Check server health and connectivity',
        inputSchema: {
          type: 'object',
          properties: {}
        }
      }
    ];
  }

  async callTool(toolName: string, args: any): Promise<string> {
    let result: any;

    switch (toolName) {
      case 'login':
      case 'logout':
      case 'dropSession':
        result = await this.authHandlers.handle(toolName, args);
        break;
      case 'transportInfo':
      case 'createTransport':
      case 'hasTransportConfig':
      case 'transportConfigurations':
      case 'getTransportConfiguration':
      case 'setTransportsConfig':
      case 'createTransportsConfig':
      case 'userTransports':
      case 'transportsByConfig':
      case 'transportDelete':
      case 'transportRelease':
      case 'transportSetOwner':
      case 'transportAddUser':
      case 'systemUsers':
      case 'transportReference':
        result = await this.transportHandlers.handle(toolName, args);
        break;
      case 'lock':
      case 'unLock':
        result = await this.objectLockHandlers.handle(toolName, args);
        break;
      case 'objectStructure':
      case 'searchObject':
      case 'findObjectPath':
      case 'objectTypes':
      case 'reentranceTicket':
        result = await this.objectHandlers.handle(toolName, args);
        break;
      case 'classIncludes':
      case 'classComponents':
        result = await this.classHandlers.handle(toolName, args);
        break;
      case 'syntaxCheckCode':
      case 'syntaxCheckCdsUrl':
      case 'codeCompletion':
      case 'findDefinition':
      case 'usageReferences':
      case 'syntaxCheckTypes':
      case 'codeCompletionFull':
      case 'runClass':
      case 'codeCompletionElement':
      case 'usageReferenceSnippets':
      case 'fixProposals':
      case 'fixEdits':
      case 'fragmentMappings':
      case 'abapDocumentation':
        result = await this.codeAnalysisHandlers.handle(toolName, args);
        break;
      case 'getObjectSource':
      case 'setObjectSource':
        result = await this.objectSourceHandlers.handle(toolName, args);
        break;
      case 'deleteObject':
        result = await this.objectDeletionHandlers.handle(toolName, args);
        break;
      case 'activateObjects':
      case 'activateByName':
      case 'inactiveObjects':
        result = await this.objectManagementHandlers.handle(toolName, args);
        break;
      case 'objectRegistrationInfo':
      case 'validateNewObject':
      case 'createObject':
        result = await this.objectRegistrationHandlers.handle(toolName, args);
        break;
      case 'nodeContents':
      case 'mainPrograms':
        result = await this.nodeHandlers.handle(toolName, args);
        break;
      case 'featureDetails':
      case 'collectionFeatureDetails':
      case 'findCollectionByUrl':
      case 'loadTypes':
      case 'adtDiscovery':
      case 'adtCoreDiscovery':
      case 'adtCompatibiliyGraph':
        result = await this.discoveryHandlers.handle(toolName, args);
        break;
      case 'unitTestRun':
      case 'unitTestEvaluation':
      case 'unitTestOccurrenceMarkers':
      case 'createTestInclude':
        result = await this.unitTestHandlers.handle(toolName, args);
        break;
      case 'prettyPrinterSetting':
      case 'setPrettyPrinterSetting':
      case 'prettyPrinter':
        result = await this.prettyPrinterHandlers.handle(toolName, args);
        break;
      case 'gitRepos':
      case 'gitExternalRepoInfo':
      case 'gitCreateRepo':
      case 'gitPullRepo':
      case 'gitUnlinkRepo':
      case 'stageRepo':
      case 'pushRepo':
      case 'checkRepo':
      case 'remoteRepoInfo':
      case 'switchRepoBranch':
        result = await this.gitHandlers.handle(toolName, args);
        break;
      case 'annotationDefinitions':
      case 'ddicElement':
      case 'ddicRepositoryAccess':
      case 'packageSearchHelp':
        result = await this.ddicHandlers.handle(toolName, args);
        break;
      case 'publishServiceBinding':
      case 'unPublishServiceBinding':
      case 'bindingDetails':
        result = await this.serviceBindingHandlers.handle(toolName, args);
        break;
      case 'tableContents':
      case 'runQuery':
        result = await this.queryHandlers.handle(toolName, args);
        break;
      case 'feeds':
      case 'dumps':
        result = await this.feedHandlers.handle(toolName, args);
        break;
      case 'debuggerListeners':
      case 'debuggerListen':
      case 'debuggerDeleteListener':
      case 'debuggerSetBreakpoints':
      case 'debuggerDeleteBreakpoints':
      case 'debuggerAttach':
      case 'debuggerSaveSettings':
      case 'debuggerStackTrace':
      case 'debuggerVariables':
      case 'debuggerChildVariables':
      case 'debuggerStep':
      case 'debuggerGoToStack':
      case 'debuggerSetVariableValue':
        result = await this.debugHandlers.handle(toolName, args);
        break;
      case 'renameEvaluate':
      case 'renamePreview':
      case 'renameExecute':
        result = await this.renameHandlers.handle(toolName, args);
        break;
      case 'atcCustomizing':
      case 'atcCheckVariant':
      case 'createAtcRun':
      case 'atcWorklists':
      case 'atcUsers':
      case 'atcExemptProposal':
      case 'atcRequestExemption':
      case 'isProposalMessage':
      case 'atcContactUri':
      case 'atcChangeContact':
        result = await this.atcHandlers.handle(toolName, args);
        break;
      case 'tracesList':
      case 'tracesListRequests':
      case 'tracesHitList':
      case 'tracesDbAccess':
      case 'tracesStatements':
      case 'tracesSetParameters':
      case 'tracesCreateConfiguration':
      case 'tracesDeleteConfiguration':
      case 'tracesDelete':
        result = await this.traceHandlers.handle(toolName, args);
        break;
      case 'extractMethodEvaluate':
      case 'extractMethodPreview':
      case 'extractMethodExecute':
        result = await this.refactorHandlers.handle(toolName, args);
        break;
      case 'revisions':
        result = await this.revisionHandlers.handle(toolName, args);
        break;
      case 'getABAPBestPractices':
        result = await this.bestPracticesHandlers.handle(toolName, args);
        break;
      case 'healthcheck':
        result = { status: 'healthy', timestamp: new Date().toISOString() };
        break;
      default:
        throw new McpError(ErrorCode.MethodNotFound, `Unknown tool: ${toolName}`);
    }

    return this.serializeResult(result);
  }
}

// Lazy-load service instance
let adtService: AbapAdtService | null = null;

function getAdtService(): AbapAdtService {
  if (!adtService) {
    adtService = new AbapAdtService();
  }
  return adtService;
}

// Azure Function HTTP endpoint with MCP SSE support
export async function mcpHandler(
  request: HttpRequest,
  context: InvocationContext
): Promise<HttpResponseInit> {
  context.log(`MCP request received: ${request.method} ${request.url}`);

  // Handle SSE endpoint (GET request)
  if (request.method === 'GET') {
    context.log('SSE endpoint requested');
    return {
      status: 200,
      headers: {
        'Content-Type': 'text/event-stream',
        'Cache-Control': 'no-cache',
        'Connection': 'keep-alive',
        'Access-Control-Allow-Origin': '*'
      },
      body: 'data: {"jsonrpc":"2.0","method":"endpoint"}\n\n'
    };
  }

  // Handle MCP JSON-RPC requests (POST)
  try {
    const service = getAdtService();
    const body = await request.json() as { 
      jsonrpc?: string;
      id?: number | string;
      method: string; 
      params?: any 
    };
    
    const method = body.method;
    const params = body.params || {};
    const id = body.id;

    context.log(`Processing MCP method: ${method}`);

    let result: any;

    // Handle MCP protocol methods
    if (method === 'initialize') {
      result = {
        protocolVersion: '2024-11-05',
        capabilities: {
          tools: {}
        },
        serverInfo: {
          name: 'abap-adt-api',
          version: '0.1.1'
        }
      };
    } else if (method === 'tools/list') {
      result = {
        tools: service.getTools()
      };
    } else if (method === 'tools/call') {
      const toolName = params.name;
      const args = params.arguments || {};
      
      const toolResult = await service.callTool(toolName, args);
      result = JSON.parse(toolResult);
    } else {
      return {
        status: 200,
        jsonBody: {
          jsonrpc: '2.0',
          id: id,
          error: {
            code: -32601,
            message: `Method not found: ${method}`
          }
        }
      };
    }

    return {
      status: 200,
      jsonBody: {
        jsonrpc: '2.0',
        id: id,
        result: result
      }
    };
  } catch (error: any) {
    context.error('Error processing request:', error);
    
    return {
      status: 200,
      jsonBody: {
        jsonrpc: '2.0',
        id: (await request.json() as any).id,
        error: {
          code: error instanceof McpError ? error.code : -32603,
          message: error.message || 'Internal server error'
        }
      }
    };
  }
}

// Register Azure Function
app.http('mcp', {
  methods: ['GET', 'POST'],
  authLevel: 'anonymous',
  route: 'mcp',
  handler: mcpHandler
});