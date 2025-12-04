/**
 * Enterprise-grade ABAP Development Best Practices
 * 
 * This module defines comprehensive coding standards, architectural patterns,
 * and organizational guidelines for professional ABAP development.
 */

export interface ABAPBestPractices {
  version: string;
  lastUpdated: string;
  categories: {
    codingStandards: CodingStandards;
    namingConventions: NamingConventions;
    performanceGuidelines: PerformanceGuidelines;
    securityPractices: SecurityPractices;
    architecturalPatterns: ArchitecturalPatterns;
    errorHandling: ErrorHandling;
    documentation: Documentation;
    testing: Testing;
    codeReview: CodeReview;
    transportManagement: TransportManagement;
  };
}

export interface CodingStandards {
  general: string[];
  formatting: {
    indentation: string;
    lineLength: number;
    whitespace: string[];
    comments: string[];
  };
  forbidden: string[];
  deprecated: string[];
  modernABAP: string[];
}

export interface NamingConventions {
  development: {
    prefix: string;
    description: string;
  };
  objects: {
    classes: string[];
    interfaces: string[];
    methods: string[];
    attributes: string[];
    constants: string[];
    tables: string[];
    structures: string[];
    dataElements: string[];
    domains: string[];
  };
  variables: {
    local: string[];
    importing: string[];
    exporting: string[];
    changing: string[];
    returning: string[];
  };
}

export interface PerformanceGuidelines {
  database: string[];
  internal: string[];
  loopOptimization: string[];
  memory: string[];
  parallelProcessing: string[];
}

export interface SecurityPractices {
  authorization: string[];
  inputValidation: string[];
  sqlInjection: string[];
  sensitiveData: string[];
  cryptography: string[];
}

export interface ArchitecturalPatterns {
  objectOriented: string[];
  designPatterns: string[];
  separation: string[];
  cleanCode: string[];
  solid: string[];
}

export interface ErrorHandling {
  exceptions: string[];
  logging: string[];
  messageHandling: string[];
  recovery: string[];
}

export interface Documentation {
  code: string[];
  technical: string[];
  functional: string[];
  api: string[];
}

export interface Testing {
  unitTests: string[];
  integration: string[];
  coverage: string[];
  testData: string[];
}

export interface CodeReview {
  checklist: string[];
  process: string[];
  tools: string[];
}

export interface TransportManagement {
  strategy: string[];
  dependencies: string[];
  documentation: string[];
}

export const ABAP_BEST_PRACTICES: ABAPBestPractices = {
  version: "1.0.0",
  lastUpdated: "2025-12-04",
  categories: {
    codingStandards: {
      general: [
        "Use ABAP Objects (OO) approach for new development",
        "Follow Clean ABAP principles (https://github.com/SAP/styleguides)",
        "Keep methods small and focused (max 50 lines, ideally < 20)",
        "One responsibility per method (Single Responsibility Principle)",
        "Prefer composition over inheritance",
        "Use immutable objects where possible",
        "Avoid global variables and side effects",
        "Write self-documenting code with meaningful names",
        "Use inline declarations (DATA(lv_var)) for local variables",
        "Leverage modern ABAP syntax (7.40+, 7.50+, 7.54+)"
      ],
      formatting: {
        indentation: "2 spaces (no tabs)",
        lineLength: 120,
        whitespace: [
          "One blank line between methods",
          "Blank line after variable declarations",
          "Space around operators (=, +, -, etc.)",
          "No trailing whitespace",
          "Consistent spacing in method signatures"
        ],
        comments: [
          "Use '\"' for inline comments",
          "Use '*' for full-line comments sparingly",
          "Prefer self-documenting code over comments",
          "Comment 'why', not 'what'",
          "Keep comments up-to-date with code changes"
        ]
      },
      forbidden: [
        "SUBMIT program (use function modules or classes)",
        "SELECT * (always specify field list)",
        "Nested SELECT statements (use JOIN or FOR ALL ENTRIES)",
        "MOVE-CORRESPONDING without strict typing",
        "Dynamic ASSIGN without field symbols typed",
        "Modification of standard SAP objects",
        "Hard-coded values (use constants or customizing)",
        "OCCURS clause (use TYPE STANDARD TABLE)",
        "Use of obsolete commands (REFRESH, FREE, etc.)"
      ],
      deprecated: [
        "FIELD-SYMBOLS without typing: FIELD-SYMBOLS <fs> TYPE ty",
        "Old-style field symbols without angle brackets",
        "Header lines in internal tables",
        "TYPE POOL (use interfaces or classes)",
        "PERFORM routines (use methods instead)",
        "PARAMETERS and SELECT-OPTIONS in classes",
        "TABLES statement (declare explicitly)"
      ],
      modernABAP: [
        "Use inline declarations: DATA(lv_var) = value",
        "Constructor expressions: VALUE, CORRESPONDING, NEW, CONV, EXACT, REF",
        "Table expressions: lt_table[ key = value ]",
        "String templates: |Text { variable }|",
        "LOOP AT ... ASSIGNING FIELD-SYMBOL(<fs>)",
        "METHOD chaining: obj->method1( )->method2( )",
        "Optional parameters with DEFAULT",
        "COND and SWITCH for conditional logic",
        "FILTER for filtering internal tables",
        "REDUCE for aggregations"
      ]
    },
    namingConventions: {
      development: {
        prefix: "Z or Y namespace",
        description: "All custom objects must start with Z or Y, or use registered namespace (/COMPANY/)"
      },
      objects: {
        classes: [
          "Global classes: ZCL_<MODULE>_<PURPOSE>",
          "Local classes: lcl_<purpose>",
          "Test classes: ltcl_<test_name>",
          "Exception classes: ZCX_<MODULE>_<EXCEPTION>",
          "Interface implementations: ZCL_<MODULE>_<INTERFACE>_IMPL"
        ],
        interfaces: [
          "Global interfaces: ZIF_<MODULE>_<PURPOSE>",
          "Local interfaces: lif_<purpose>",
          "Use verb phrases for behavioral interfaces"
        ],
        methods: [
          "Use verb phrases: get_data, calculate_total, validate_input",
          "Boolean methods: is_valid, has_data, can_execute",
          "Private methods: lowercase with underscores",
          "Public methods: descriptive and intention-revealing",
          "Constructor: constructor (for instance) or class_constructor (for static)"
        ],
        attributes: [
          "Instance attributes: mv_<name> (member variable)",
          "Static attributes: gv_<name> (global variable) or cv_<name> (class variable)",
          "Constants: c_<name> or gc_<name> (global constant)",
          "Private: prefix with underscore for true private: _mv_name"
        ],
        constants: [
          "Constants in uppercase: lc_max_items TYPE i VALUE 100",
          "Class constants: gc_status_active TYPE char1 VALUE 'A'",
          "Group related constants in interfaces or constant classes"
        ],
        tables: [
          "Custom tables: ZTAB_<MODULE>_<PURPOSE>",
          "Customizing tables: ZTBC_<MODULE>_<PURPOSE>",
          "Use semantic names reflecting business purpose"
        ],
        structures: [
          "Structures: ZSTR_<MODULE>_<PURPOSE>",
          "Types: ZTY_<MODULE>_<TYPE_NAME>",
          "Deep structures: indicate with _DEEP suffix"
        ],
        dataElements: [
          "Data elements: ZDE_<MODULE>_<FIELD>",
          "Use semantic names, not technical abbreviations",
          "Include domain information in name if relevant"
        ],
        domains: [
          "Domains: ZDOM_<MODULE>_<VALUE_RANGE>",
          "Use descriptive names for value ranges",
          "Consider reusability across modules"
        ]
      },
      variables: {
        local: [
          "lv_<name> - local variable",
          "lt_<name> - local table",
          "ls_<name> - local structure",
          "lr_<name> - local reference",
          "lo_<name> - local object",
          "lx_<name> - local exception"
        ],
        importing: [
          "iv_<name> - importing variable",
          "it_<name> - importing table",
          "is_<name> - importing structure",
          "ir_<name> - importing reference",
          "io_<name> - importing object"
        ],
        exporting: [
          "ev_<name> - exporting variable",
          "et_<name> - exporting table",
          "es_<name> - exporting structure",
          "er_<name> - exporting reference",
          "eo_<name> - exporting object"
        ],
        changing: [
          "cv_<name> - changing variable",
          "ct_<name> - changing table",
          "cs_<name> - changing structure",
          "cr_<name> - changing reference",
          "co_<name> - changing object"
        ],
        returning: [
          "rv_<name> - returning variable",
          "rt_<name> - returning table",
          "rs_<name> - returning structure",
          "rr_<name> - returning reference",
          "ro_<name> - returning object"
        ]
      }
    },
    performanceGuidelines: {
      database: [
        "Use SELECT SINGLE for single record retrieval with complete key",
        "Use UP TO 1 ROWS for single record without complete key",
        "Specify field list explicitly (never SELECT *)",
        "Use WHERE clause to minimize data retrieval",
        "Use database indexes effectively (check with ST05)",
        "Prefer JOIN over nested SELECT or FOR ALL ENTRIES for large datasets",
        "Use FOR ALL ENTRIES only when necessary, check for empty driver table",
        "Implement database buffering where appropriate",
        "Use aggregate functions (COUNT, SUM, AVG) at database level",
        "Avoid SELECT in loops (array fetch instead)",
        "Use SELECT ... PACKAGE SIZE for very large result sets",
        "Leverage secondary indexes for non-key field searches",
        "Use database views for complex joins when reusable",
        "Implement strict WHERE conditions to use indexes",
        "Consider materialized views for complex aggregations"
      ],
      internal: [
        "Use HASHED tables for key-based lookups (O(1) complexity)",
        "Use SORTED tables for range queries and sorted iterations",
        "Use STANDARD tables only for append operations",
        "Avoid nested loops with large tables (use COLLECT, SUM, etc.)",
        "Use READ TABLE with KEY for better performance",
        "Use BINARY SEARCH with SORTED tables",
        "Implement parallel cursor technique for multiple table processing",
        "Use ASSIGN instead of INTO for large structures",
        "Clear internal tables explicitly when no longer needed",
        "Use shared memory (SHM) for frequently accessed data"
      ],
      loopOptimization: [
        "Avoid SELECT in LOOP (fetch all data before loop)",
        "Use LOOP AT ... WHERE for filtering instead of IF inside loop",
        "Use GROUP BY in LOOP AT for grouping operations",
        "Minimize operations inside loops",
        "Use parallel processing (ABAP Channels, aRFC) for independent iterations",
        "Prefer table expressions over READ TABLE in simple cases",
        "Use REDUCE for aggregations instead of explicit loops",
        "Consider breaking large loops into smaller batches"
      ],
      memory: [
        "Release large internal tables as soon as possible (CLEAR, FREE)",
        "Use table declarations without header lines",
        "Avoid deep copies with CORRESPONDING, use direct field assignment",
        "Use references for large objects instead of value passing",
        "Implement garbage collection by clearing object references",
        "Monitor memory consumption with tools (ST22, ST04)",
        "Use FIELD-SYMBOLS for in-place modifications",
        "Avoid string concatenation in loops (use string templates)"
      ],
      parallelProcessing: [
        "Use parallel RFC (pRFC) for asynchronous processing",
        "Implement background jobs for long-running processes",
        "Use ABAP Channels for parallel data processing",
        "Consider ABAP Daemon for continuously running processes",
        "Implement checkpoints for restartability",
        "Use application server groups for load distribution"
      ]
    },
    securityPractices: {
      authorization: [
        "Implement authorization checks using AUTHORITY-CHECK",
        "Define custom authorization objects for business-specific access",
        "Never bypass authorization checks in production code",
        "Use S_DEVELOP for development object access control",
        "Implement field-level authorization where needed",
        "Log authorization failures for audit trails",
        "Use transaction codes with proper authorization objects",
        "Implement role-based access control (RBAC)",
        "Regular review and update of authorization roles"
      ],
      inputValidation: [
        "Validate all user inputs before processing",
        "Use domain and data element checks for type validation",
        "Implement length and format checks for string inputs",
        "Sanitize inputs to prevent injection attacks",
        "Use allowed value ranges (domains, check tables)",
        "Validate numeric inputs for reasonable ranges",
        "Check for null/initial values before use",
        "Implement business rule validation in separate methods"
      ],
      sqlInjection: [
        "Never use dynamic SQL with unvalidated user input",
        "Use ABAP SQL with static field lists",
        "If dynamic SQL is necessary, use strict whitelisting",
        "Validate and escape special characters in dynamic WHERE clauses",
        "Use parameter binding for dynamic SQL",
        "Avoid EXEC SQL (use Open SQL instead)",
        "Log all dynamic SQL executions for audit"
      ],
      sensitiveData: [
        "Never log sensitive data (passwords, credit cards, personal info)",
        "Use encryption for sensitive data storage (SSF, SECSTORE)",
        "Implement data masking in logs and traces",
        "Avoid displaying sensitive data in GUI fields without authorization",
        "Use secure channels (HTTPS, SSL) for data transmission",
        "Implement data retention and purging policies",
        "Comply with GDPR and other data privacy regulations",
        "Use personal data management framework (PMDM) where applicable"
      ],
      cryptography: [
        "Use SAP SSF (Secure Store and Forward) for encryption",
        "Never implement custom encryption algorithms",
        "Use strong encryption standards (AES-256)",
        "Implement proper key management (never hardcode keys)",
        "Use SAP Secure Storage for credential management",
        "Implement digital signatures for critical transactions",
        "Use SSL/TLS for all external communications"
      ]
    },
    architecturalPatterns: {
      objectOriented: [
        "Encapsulate data with private attributes and public methods",
        "Use inheritance to model IS-A relationships",
        "Prefer composition over inheritance for HAS-A relationships",
        "Program to interfaces, not implementations",
        "Use polymorphism to avoid type checks and CASE statements",
        "Apply dependency injection for loose coupling",
        "Implement factory patterns for object creation",
        "Use singleton pattern sparingly (prefer dependency injection)"
      ],
      designPatterns: [
        "Singleton: for single instance classes (use with caution)",
        "Factory: for complex object creation logic",
        "Strategy: for interchangeable algorithms",
        "Observer: for event-driven architectures",
        "Decorator: for adding responsibilities dynamically",
        "Adapter: for integrating incompatible interfaces",
        "Command: for encapsulating requests",
        "Template Method: for defining algorithm skeletons",
        "Repository: for data access abstraction",
        "Unit of Work: for transactional consistency"
      ],
      separation: [
        "Separate presentation, business logic, and data access layers",
        "Use Model-View-Controller (MVC) for UI applications",
        "Implement business logic in dedicated classes, not in UI code",
        "Use business objects (BO) for core business entity logic",
        "Separate read and write operations (CQRS) for complex scenarios",
        "Use API/service layers for system integration",
        "Implement clear boundaries between modules"
      ],
      cleanCode: [
        "Functions should do one thing and do it well",
        "Keep abstraction levels consistent within a method",
        "Minimize method parameters (ideally ≤3, max 5)",
        "Avoid output parameters (use returning instead)",
        "Don't return null (use optional patterns or exceptions)",
        "Fail fast with early returns and guard clauses",
        "Extract complex conditions into well-named methods",
        "Use meaningful names that reveal intent"
      ],
      solid: [
        "S - Single Responsibility: One class, one responsibility",
        "O - Open/Closed: Open for extension, closed for modification",
        "L - Liskov Substitution: Subtypes must be substitutable for base types",
        "I - Interface Segregation: Many specific interfaces better than one general",
        "D - Dependency Inversion: Depend on abstractions, not concretions"
      ]
    },
    errorHandling: {
      exceptions: [
        "Use class-based exceptions (CX_*), not message-based",
        "Create custom exception classes inheriting from appropriate base",
        "Use CX_STATIC_CHECK for recoverable errors",
        "Use CX_DYNAMIC_CHECK for programming errors",
        "Use CX_NO_CHECK sparingly (runtime system errors only)",
        "Always document exceptions in method signatures (RAISING clause)",
        "Catch specific exceptions, not generic ones",
        "Don't catch and ignore exceptions without good reason",
        "Clean up resources in CLEANUP section",
        "Re-raise exceptions if cannot handle appropriately"
      ],
      logging: [
        "Use application log (BAL) for structured logging",
        "Implement different log levels (error, warning, info, debug)",
        "Include context information in log messages",
        "Use message classes for consistent messaging",
        "Log at appropriate abstraction level",
        "Avoid logging in loops (aggregate and log once)",
        "Include correlation IDs for distributed transactions",
        "Implement log retention and archiving policies"
      ],
      messageHandling: [
        "Use message classes for all user-facing messages",
        "Create custom message classes per module (Z<MODULE>)",
        "Use message IDs consistently and meaningfully",
        "Include variable placeholders (&1, &2, etc.) for dynamic content",
        "Maintain message texts in multiple languages",
        "Use appropriate message types (E, W, I, S, A, X)",
        "Document message resolution in system documentation"
      ],
      recovery: [
        "Implement retry logic for transient failures",
        "Use exponential backoff for retry mechanisms",
        "Implement circuit breaker pattern for external service calls",
        "Provide graceful degradation when dependencies fail",
        "Implement compensating transactions for distributed operations",
        "Use database savepoints for partial rollback scenarios",
        "Log all recovery attempts for diagnostics"
      ]
    },
    documentation: {
      code: [
        "Write self-documenting code with clear names",
        "Use ABAP Doc comments for public APIs",
        "Document complex algorithms and business rules",
        "Include examples in ABAP Doc for public methods",
        "Document parameters with @parameter in ABAP Doc",
        "Document exceptions with @raising in ABAP Doc",
        "Keep documentation synchronized with code",
        "Document workarounds and technical debt with TODO/FIXME comments"
      ],
      technical: [
        "Maintain design documentation for complex components",
        "Document architecture decisions (ADR format)",
        "Create sequence diagrams for complex workflows",
        "Document database schema and relationships",
        "Maintain API documentation for interfaces",
        "Document deployment and configuration procedures",
        "Create troubleshooting guides for common issues"
      ],
      functional: [
        "Document business processes and rules",
        "Create user guides for custom transactions",
        "Document system configuration steps",
        "Maintain functional specifications for enhancements",
        "Document integration points and dependencies",
        "Create test scenarios and expected results",
        "Document authorization requirements"
      ],
      api: [
        "Document all public methods with ABAP Doc",
        "Provide usage examples for complex APIs",
        "Document parameter constraints and valid ranges",
        "Specify pre-conditions and post-conditions",
        "Document side effects and state changes",
        "Version your APIs and document breaking changes",
        "Provide migration guides for deprecated APIs"
      ]
    },
    testing: {
      unitTests: [
        "Write unit tests for all business logic",
        "Use ABAP Unit framework for automated testing",
        "Follow AAA pattern: Arrange, Act, Assert",
        "Test one behavior per test method",
        "Use descriptive test method names (should_do_x_when_y)",
        "Mock external dependencies (database, RFC, etc.)",
        "Aim for high code coverage (>80% for critical code)",
        "Run tests frequently during development",
        "Include tests for error conditions and edge cases",
        "Use test doubles (mocks, stubs, fakes) appropriately"
      ],
      integration: [
        "Test integration points separately from unit tests",
        "Use test data that represents real scenarios",
        "Implement end-to-end tests for critical business processes",
        "Test error handling in integration scenarios",
        "Use separate test systems/clients for integration testing",
        "Automate integration tests where possible",
        "Document test data setup and teardown procedures"
      ],
      coverage: [
        "Measure code coverage with ABAP Unit coverage tools",
        "Aim for 80%+ coverage for business-critical code",
        "Focus on branch coverage, not just line coverage",
        "Don't write tests just to increase coverage metrics",
        "Identify and test critical paths through code",
        "Use coverage reports to find untested edge cases"
      ],
      testData: [
        "Use meaningful test data that represents real scenarios",
        "Create reusable test data builders",
        "Avoid dependencies on production data in tests",
        "Clean up test data after test execution",
        "Use constants for test data values",
        "Document test data requirements and setup"
      ]
    },
    codeReview: {
      checklist: [
        "Verify adherence to coding standards and naming conventions",
        "Check for proper error handling and exception usage",
        "Validate performance considerations (database access, loops)",
        "Review security aspects (authorization, input validation)",
        "Ensure adequate test coverage",
        "Verify documentation completeness",
        "Check for code duplication (DRY principle)",
        "Validate proper use of ABAP OO principles",
        "Review transaction management and data consistency",
        "Check for proper resource cleanup",
        "Verify transport request organization"
      ],
      process: [
        "All code changes require peer review before transport",
        "Use code review tools (ABAP Test Cockpit, Code Inspector)",
        "Provide constructive feedback focused on code quality",
        "Review small changes frequently rather than large batches",
        "Author should provide context and test evidence",
        "Resolve all critical findings before approval",
        "Document review decisions and rationale",
        "Follow up on technical debt items identified in reviews"
      ],
      tools: [
        "Use ABAP Test Cockpit (ATC) for automated checks",
        "Configure custom ATC check variants for your standards",
        "Use Code Inspector (SCI) for static code analysis",
        "Implement custom checks for organization-specific rules",
        "Use Extended Program Check (SLIN) for syntax and semantics",
        "Leverage ABAP Development Tools (ADT) built-in validators",
        "Use third-party tools where applicable (abapGit, abaplint)"
      ]
    },
    transportManagement: {
      strategy: [
        "Use separate transport requests for different features/fixes",
        "Include only related objects in a single transport",
        "Use task-based development (one task per developer)",
        "Document transport purpose in description field",
        "Follow change request workflow (dev -> test -> prod)",
        "Use transport of copies (ToC) for emergency fixes",
        "Implement transport dependency management",
        "Release transports in logical sequence",
        "Use transport groups for related changes",
        "Archive old transports according to retention policy"
      ],
      dependencies: [
        "Document dependencies between transports",
        "Ensure prerequisite transports are released first",
        "Use transport layers appropriately",
        "Avoid circular dependencies between transports",
        "Test transport import order in non-production systems",
        "Maintain transport dependency documentation",
        "Use Modification Adjustment for conflicts"
      ],
      documentation: [
        "Provide detailed transport descriptions",
        "Link transports to change tickets/user stories",
        "Document test evidence for transport approval",
        "Maintain transport import logs and results",
        "Document any manual steps required post-import",
        "Create rollback procedures for critical changes",
        "Maintain transport calendar for production imports"
      ]
    }
  }
};

/**
 * Get complete ABAP best practices as JSON
 */
export function getBestPractices(): ABAPBestPractices {
  return ABAP_BEST_PRACTICES;
}

/**
 * Get specific category of best practices
 */
export function getBestPracticesByCategory(category: keyof ABAPBestPractices['categories']): any {
  return ABAP_BEST_PRACTICES.categories[category];
}

/**
 * Search best practices by keyword
 */
export function searchBestPractices(keyword: string): any[] {
  const results: any[] = [];
  const searchTerm = keyword.toLowerCase();
  
  Object.entries(ABAP_BEST_PRACTICES.categories).forEach(([category, content]) => {
    const matches = searchInObject(content, searchTerm);
    if (matches.length > 0) {
      results.push({
        category,
        matches
      });
    }
  });
  
  return results;
}

function searchInObject(obj: any, term: string, path: string = ''): string[] {
  const results: string[] = [];
  
  if (typeof obj === 'string') {
    if (obj.toLowerCase().includes(term)) {
      results.push(`${path}: ${obj}`);
    }
  } else if (Array.isArray(obj)) {
    obj.forEach((item, index) => {
      results.push(...searchInObject(item, term, `${path}[${index}]`));
    });
  } else if (typeof obj === 'object' && obj !== null) {
    Object.entries(obj).forEach(([key, value]) => {
      const newPath = path ? `${path}.${key}` : key;
      results.push(...searchInObject(value, term, newPath));
    });
  }
  
  return results;
}
