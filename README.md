DISCLAIMER: This server is still in experimental status! Use it with caution!

# ABAP-ADT-API MCP-Server

[![smithery badge](https://smithery.ai/badge/@mario-andreschak/mcp-abap-abap-adt-api)](https://smithery.ai/server/@mario-andreschak/mcp-abap-abap-adt-api)

## Description

The MCP-Server `mcp-abap-abap-adt-api` is an Azure Function that exposes ABAP Development Tools (ADT) capabilities through a single HTTP endpoint. It is a wrapper for [abap-adt-api](https://github.com/marcellourbani/abap-adt-api/) and provides a suite of tools for managing ABAP objects, handling transport requests, performing code analysis, and more, enhancing the efficiency and effectiveness of ABAP development workflows.

The server can be run locally using Azure Functions Core Tools or deployed to Azure Functions for cloud-based access.

## Features

- **Authentication**: Securely authenticate with ABAP systems using the `login` tool.
- **Object Management**: Create, read, update, and delete ABAP objects seamlessly.
- **Transport Handling**: Manage transport requests with tools like `createTransport` and `transportInfo`.
- **Code Analysis**: Perform syntax checks and retrieve code completion suggestions.
- **Extensibility**: Easily extend the server with additional tools and resources as needed.
- **Session Management**: Handle session caching and termination using `dropSession` and `logout`.

## Installation

### Installing via Smithery

To install ABAP-ADT-API MCP-Server for Claude Desktop automatically via [Smithery](https://smithery.ai/server/@mario-andreschak/mcp-abap-abap-adt-api):

```bash
npx -y @smithery/cli install @mario-andreschak/mcp-abap-abap-adt-api --client claude
```

### Prerequisites

- **Node.js**: Ensure you have Node.js v18+ installed. You can download it from [here](https://nodejs.org/).
- **Azure Functions Core Tools**: Required for local development. Will be installed via npm.
- **ABAP System Access**: Credentials and URL to access the ABAP system.

### Steps

1. **Clone the Repository**

   ```cmd
   git clone https://github.com/mario-andreschak/mcp-abap-abap-adt-api.git
   cd mcp-abap-abap-adt-api
   ```

2. **Install Dependencies**

   ```cmd
   npm install
   ```

3. **Configure Local Settings**

   Open the `local.settings.json` file and replace the placeholder values with your actual SAP connection details:

   ```json
   {
     "IsEncrypted": false,
     "Values": {
       "FUNCTIONS_WORKER_RUNTIME": "node",
       "AzureWebJobsFeatureFlags": "EnableWorkerIndexing",
       "SAP_URL": "https://your-sap-server.com:44300",
       "SAP_USER": "YOUR_SAP_USERNAME",
       "SAP_PASSWORD": "YOUR_SAP_PASSWORD",
       "SAP_CLIENT": "100",
       "SAP_LANGUAGE": "EN"
     }
   }
   ```

   Note: The SAP_CLIENT and SAP_LANGUAGE variables are optional but recommended. Default is "EN" for language.

   **IMPORTANT**: Never commit your `local.settings.json` file to version control. It's already included in `.gitignore` to prevent accidental commits.

4. **Build the Project**

   ```cmd
   npm run build
   ```

5. **Run the Server Locally**

   ```cmd
   npm start
   ```

   The Azure Function will start at `http://localhost:7071/api/mcp`

   You can also run in watch mode during development:
   ```cmd
   npm run watch
   ```

## Usage

### Local Development

Once the server is running locally, you can test it using HTTP requests:

**List available tools:**
```bash
curl --request POST -H "Content-Type:application/json" --data '{"method":"tools/list"}' http://localhost:7071/api/mcp
```

**Call a tool:**
```bash
curl --request POST -H "Content-Type:application/json" --data '{"method":"tools/call","params":{"name":"healthcheck","arguments":{}}}' http://localhost:7071/api/mcp
```

### Request Format

The endpoint accepts POST requests with the following JSON structure:

```json
{
  "method": "tools/list" | "tools/call",
  "params": {
    "name": "tool-name",
    "arguments": {}
  }
}
```

### Azure Deployment

To deploy to Azure Functions:

1. Install Azure Functions extension for VS Code
2. Use the Azure Functions extension to deploy
3. Configure application settings in Azure Portal with your SAP credentials

For detailed deployment instructions, see the [Azure Functions documentation](https://learn.microsoft.com/en-us/azure/azure-functions/functions-develop-vs-code).

## Custom Instruction
Use this Custom Instruction to explain the tool to your model:
```
## mcp-abap-abap-adt-api Server

This server provides tools for interacting with an SAP system via ADT (ABAP Development Tools) APIs. It allows you to retrieve information about ABAP objects, modify source code, and manage transports.

**Key Tools and Usage:**

*   **`searchObject`:** Finds ABAP objects based on a query string (e.g., class name).
    *   `query`: (string, required) The search term.
    *   Returns the object's URI.  Example: `/sap/bc/adt/oo/classes/zcl_invoice_xml_gen_model`

*   **`transportInfo`:** Retrieves transport information for a given object.
    *   `objSourceUrl`: (string, required) The object's URI (obtained from `searchObject`).
    *   Returns transport details, including the transport request number (`TRKORR` or `transportInfo.LOCKS.HEADER.TRKORR` in the JSON response).

*   **`lock`:** Locks an ABAP object for editing.
    *   `objectUrl`: (string, required) The object's URI.
    *   Returns a `lockHandle`, which is required for subsequent modifications.

*   **`unLock`:** Unlocks a previously locked ABAP object.
    *   `objectUrl`: (string, required) The object's URI.
    *   `lockHandle`: (string, required) The lock handle obtained from the `lock` operation.

*   **`setObjectSource`:** Modifies the source code of an ABAP object.
    *   `objectSourceUrl`: (string, required) The object's URI *with the suffix `/source/main`*.  Example: `/sap/bc/adt/oo/classes/zcl_invoice_xml_gen_model/source/main`
    *   `lockHandle`: (string, required) The lock handle obtained from the `lock` operation.
    *   `source`: (string, required) The complete, modified ABAP source code.
    *   `transport`: (string, optional) The transport request number.

*   **`syntaxCheckCode`:** Performs a syntax check on a given ABAP source code.
    *   `code`: (string, required) The ABAP source code to check.
    *   `url`: (string, optional) The URL of the object.
    *   `mainUrl`: (string, optional) The main URL.
    *   `mainProgram`: (string, optional) The main program.
    *   `version`: (string, optional) The version.
    *   Returns syntax check results, including any errors.

*   **`activate`:** Activates an ABAP object. (See notes below on activation/unlocking.)
    *    `object`: The object to be activated.

*   **`getObjectSource`:** Retrieves the source code of an ABAP object.
    *   `objectSourceUrl`: (string, required) The object's URI *with the suffix `/source/main`*.

**Workflow for Modifying ABAP Code:**

1.  **Find the object URI:** Use `searchObject`.
2.  **Read the original source code:** Use `getObjectSource` (with the `/source/main` suffix).
3.  **Clone and Modify the source code locally:** (e.g., `write_to_file` for creating a local copy, and using `read_file`, `replace_in_file` for modifying this local copy).
4.  **Get transport information:** Use `transportInfo`.
5.  **Lock the object:** Use `lock`.
6.  **Set the modified source code:** Use `setObjectSource` (with the `/source/main` suffix).
7.  **Perform a syntax check:** Use `syntaxCheckCode`.
8.  **Activate** the object, Use `activate`..
9.  **unLock the object:** Use `unLock`.

**Important Notes:**
*   **File Handling:** SAP is completly de-coupled from the local file system. Reading source code will only return the code as tool result - it has no effect on file. Files are not synchronized with SAP but merely a local copy for our reference. FYI: It's not strictly necessary for you to create local copies of source codes, as they have no effect on SAP, but it helps us track changes. 
*   **File Handling:** The local filenames you will use will not contain any paths, but only a filename! It's preferable to use a pattern like "[ObjectName].[ObjectType].abap". (e.g., SAPMV45A.prog.abap for a ABAP Program SAPMV45A, CL_IXML.clas.abap for a Class CL_IXML)
*   **URL Suffix:**  Remember to add `/source/main` to the object URI when using `setObjectSource` and `getObjectSource`.
*   **Transport Request:** Obtain the transport request number (e.g., from `transportInfo` or from the user) and include it in relevant operations.
*   **Lock Handle:**  The `lockHandle` obtained from the `lock` operation is crucial for `setObjectSource` and `unLock`. Ensure you are using a valid `lockHandle`. If a lock fails, you may need to re-acquire the lock. Locks can expire or be released by other users.
*   **Activation/Unlocking Order:** The exact order of `activate` and `unLock` operations might need clarification. Refer to the tool descriptions or ask the user. It appears `activate` can be used without unlocking first.
* **Error Handling:** The tools return JSON responses. Check for error messages within these responses.

## Efficient Database Access

SAP systems contain vast amounts of data.  It's crucial to write ABAP code that accesses the database efficiently to minimize performance impact and network traffic.  Avoid selecting entire tables or using broad `WHERE` clauses when you only need specific data.

*   **Use `WHERE` clauses:** Always use `WHERE` clauses in your `SELECT` statements to filter the data retrieved from the database.  Select only the specific rows you need.
*   **`UP TO 1 ROWS`:** If you only need a single record, use the `SELECT SINGLE` statement, if you can guarantee that you can provide ALL the key fields for the `SELECT SINGLE` statement. Otherwise, use the `SELECT` statement with the `UP TO 1 ROWS` addition. This tells the database to stop searching after finding the first matching record, improving performance. Example:

    ```abap
    SELECT vgbel FROM vbrp WHERE vbeln = @me->lv_vbeln INTO @DATA(lv_vgbel) UP TO 1 ROWS.
      EXIT. " Exit any loop after this.
    ENDSELECT.
    ```
## Checking Table and Structure Definitions

When working with ABAP objects, you may encounter errors related to unknown field names or incorrect table usage.  You can use the following tools to inspect table and structure definitions:

*   **`GetTable`:** Use this tool to retrieve the structure of an ABAP Dictionary table, including its field names and data types. This is helpful for verifying the correct fields to use in your `SELECT` statements.
*    If you need to inspect an include structure, you may need to use `searchObject` to find the include and then use `GetTypeInfo` or `GetStructure`. You may get a 404 error and try again with `GetStructure`
*   **`GetStructure`:** Use this tool to retrieve the structure of an ABAP Dictionary structure, including its field names and data types. This is helpful for verifying the correct fields to use in your `SELECT` statements.
*    If you need to inspect an include structure, you may need to use `searchObject` to find the include and then use `GetTypeInfo` or `GetStructure`.

```

## Contributing

Contributions are welcome! Please follow these steps to contribute:

1. **Fork the Repository**
2. **Create a New Branch**

   ```cmd
   git checkout -b feature/your-feature-name
   ```

3. **Commit Your Changes**

   ```cmd
   git commit -m "Add some feature"
   ```

4. **Push to the Branch**

   ```cmd
   git push origin feature/your-feature-name
   ```

5. **Open a Pull Request**

## License

This project is licensed under the [MIT License](LICENSE).
