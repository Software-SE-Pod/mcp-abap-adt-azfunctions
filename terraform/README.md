# Terraform Deployment for MCP ABAP ADT Azure Functions

This directory contains Terraform configuration for deploying the MCP ABAP ADT server to Azure Functions with full infrastructure automation, including secure credential storage in Azure Key Vault.

## Architecture

The Terraform configuration deploys:

- **Azure Function App** (Linux, Node.js 18) - Hosts the MCP server
- **Azure Key Vault** - Securely stores SAP credentials
- **Storage Account** - Required for Azure Functions + stores deployment packages
- **Application Insights** - Monitors function performance and logs
- **App Service Plan** - Compute resources for the function app
- **Managed Identity** - Enables secure Key Vault access without credentials

## Prerequisites

### Required Tools
- [Terraform](https://www.terraform.io/downloads) >= 1.5.0
- [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/install-azure-cli) >= 2.40.0
- [Node.js](https://nodejs.org/) >= 18.x
- npm (comes with Node.js)

### Azure Requirements
- Active Azure subscription
- Appropriate permissions to create resources
- Azure CLI authenticated (`az login`)

## Quick Start

### 1. Build the Application

From the project root directory:

```bash
npm install
npm run build
```

This creates the `dist/` directory that Terraform will package and deploy.

### 2. Configure Environment Variables

Set your environment variables using the template:

```bash
# Required SAP Configuration
export TF_VAR_sap_url="https://your-sap-server.com:44300"
export TF_VAR_sap_user="YOUR_SAP_USERNAME"
export TF_VAR_sap_password="YOUR_SAP_PASSWORD"

# Optional Configuration
export TF_VAR_sap_client="100"
export TF_VAR_sap_language="EN"
export TF_VAR_environment="dev"
export TF_VAR_location="eastus"
```

**Tip:** You can create a `terraform.tfvars.sh` file with these exports and source it: `source terraform.tfvars.sh`

**IMPORTANT:** Never commit files containing credentials to version control!

### 3. Deploy with Terraform

```bash
cd terraform
terraform init
terraform plan
terraform apply
```

Terraform will automatically:
1. Create all Azure infrastructure
2. Package your built application from `dist/`
3. Upload it to blob storage
4. Configure the Function App to run from the package
5. Store SAP credentials securely in Key Vault

### 4. Get Deployment Information

After deployment, view the outputs:

```bash
terraform output
```

Key outputs:
- `function_app_url` - Your MCP endpoint URL
- `function_app_name` - Name of the deployed function app
- `key_vault_name` - Name of the Key Vault storing credentials
- `deployment_summary` - Complete deployment information

## File Structure
## File Structure

```
terraform/
├── README.md                    # This file
├── provider.tf                  # Azure provider configuration
├── variables.tf                 # Input variable definitions
├── locals.tf                    # Local values and naming conventions
├── main.tf                      # Function App and infrastructure
├── keyvault.tf                  # Key Vault and secrets
├── outputs.tf                   # Output values
├── terraform.tfvars.template    # Environment variables template
└── .gitignore                   # Ignore sensitive files
```
## Security Best Practices

### Credentials Management

**What we do:**
- Store SAP credentials in Azure Key Vault
- Use Managed Identity for Key Vault access (no passwords in code)
- Reference secrets via `@Microsoft.KeyVault()` syntax
- Mark sensitive variables as `sensitive = true`
- Use environment variables (never hardcode)

**What to avoid:**
- Committing `terraform.tfvars.sh` or any file with credentials
- Storing secrets in `terraform.tfstate` (handled automatically)
- Using hardcoded passwords in any configuration file

### Production Recommendations

For production deployments:

1. **Enable Key Vault Network Security:**
   ```hcl
   # In keyvault.tf, change:
   network_acls {
     default_action = "Deny"
     bypass         = "AzureServices"
     ip_rules       = ["YOUR_IP_RANGE"]
   }
   ```

2. **Change Function App Authentication:**
   Edit `src/index.ts` to change `authLevel: 'anonymous'` to `authLevel: 'function'`

3. **Use Premium V3 SKU (if available in your subscription):**
   ```bash
   export TF_VAR_function_app_sku="P0v3"  # Premium V3 Small
   ```

4. **Enable Remote State:**
   Uncomment the backend configuration in `provider.tf`:
   ```hcl
   backend "azurerm" {
     resource_group_name  = "rg-terraform-state"
     storage_account_name = "sttfstate<unique>"
     container_name       = "tfstate"
     key                  = "mcp-abap-adt.terraform.tfstate"
   }
   ```

5. **Use Private Endpoints:**
   Consider adding private endpoints for Storage and Key Vault

## Manual Deployment Steps

If you prefer manual control over the deployment process:
## Destroy Infrastructure

To remove all deployed resources:

```bash
cd terraform
terraform destroy
```

**WARNING:** This will permanently delete all resources including the Key Vault and all stored secrets.
### Required Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `sap_url` | SAP server URL | `https://sap.example.com:44300` |
| `sap_user` | SAP username | `DEVELOPER01` |
| `sap_password` | SAP password | `SecurePass123!` |

### Optional Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `environment` | `dev` | Environment name |
| `location` | `eastus` | Azure region |
| `project_name` | `mcp-abap-adt` | Project identifier |
| `sap_client` | `""` | SAP client number |
| `sap_language` | `EN` | SAP language code |
| `node_tls_reject_unauthorized` | `1` | TLS certificate validation |
| `function_app_sku` | `P0v3` | Function App pricing tier |
| `enable_app_insights` | `true` | Enable monitoring |

### Function App SKU Options

- **P0v3** (Premium V3 Small): 1 vCPU, 4GB RAM, high-performance (default - available in your subscription)
- **P1v3** (Premium V3 Medium): 2 vCPU, 8GB RAM
- **P2v3** (Premium V3 Large): 4 vCPU, 16GB RAM
- **B1** (Basic): Low-cost option (requires quota approval)
- **S1** (Standard): More resources, auto-scaling (requires quota approval)
- **Y1** (Consumption): Pay-per-execution (requires quota approval)

## Updating the Deployment

To update the function code or infrastructure:

1. Make changes to your code or Terraform configuration
2. Build the application: `npm run build`
3. Run deployment: `./deploy.sh`

Terraform will automatically detect and apply only the necessary changes.

## Troubleshooting

### "Key Vault access denied"

**Cause:** Terraform service principal doesn't have Key Vault permissions.

**Solution:** The access policy is automatically created, but may take a few seconds. Wait and retry.

### "Function App not found" when deploying code

**Cause:** Terraform apply hasn't completed yet.

**Solution:** Ensure `terraform apply` completes successfully before the function code deployment.

### "dist directory not found"

**Cause:** Application hasn't been built.

**Solution:** Run `npm install && npm run build` from the project root.

### "Quota limit" or "Unauthorized" errors

**Cause:** Your Azure subscription doesn't have quota for the selected SKU.

**Solution:** 
- Check available quota in Azure Portal: Subscriptions → Usage + quotas → App Service
- Use a SKU you have quota for (e.g., P0v3, P1v3): `export TF_VAR_function_app_sku="P0v3"`
- Or request quota increase: Support → New support request → Service and subscription limits (quotas)
- Try a different region: `export TF_VAR_location="eastus2"`

### "Cannot create storage account - name taken"

**Cause:** Storage account names must be globally unique.

**Solution:** The random suffix should handle this automatically. If it persists, modify the `random_string` resource in `locals.tf`.

### Check Function App Logs

```bash
# Stream logs in real-time
func azure functionapp logstream <function-app-name>

# Or view in Azure Portal
az functionapp log tail --name <function-app-name> --resource-group <resource-group-name>
```

### Verify Key Vault Secrets
## Updating the Deployment

To update the function code or infrastructure:

1. Make changes to your code or Terraform configuration
2. Build the application: `npm run build` (from project root)
3. Run Terraform: `terraform apply` (from terraform directory)

Terraform will automatically detect and apply only the necessary changes. When you rebuild the app, the ZIP file hash changes and Terraform will upload the new package.
## CI/CD Integration

### GitHub Actions Example

Create `.github/workflows/deploy.yml`:

```yaml
name: Deploy to Azure

on:
  push:
    branches: [main]
  workflow_dispatch:

jobs:
### "dist directory not found"

**Cause:** Application hasn't been built before running Terraform.

**Solution:** Run `npm install && npm run build` from the project root before running `terraform apply`.
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'
      
      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v2
        with:
          terraform_version: 1.5.0
      
      - name: Azure Login
        uses: azure/login@v1
        with:
          creds: ${{ secrets.AZURE_CREDENTIALS }}
      
      - name: Set Environment Variables
        run: |
          echo "TF_VAR_sap_url=${{ secrets.SAP_URL }}" >> $GITHUB_ENV
          echo "TF_VAR_sap_user=${{ secrets.SAP_USER }}" >> $GITHUB_ENV
          echo "TF_VAR_sap_password=${{ secrets.SAP_PASSWORD }}" >> $GITHUB_ENV
      
      - name: Deploy
        run: |
          chmod +x terraform/deploy.sh
          terraform/deploy.sh
```
## CI/CD Integration

### GitHub Actions Example

Create `.github/workflows/deploy.yml`:

```yaml
name: Deploy to Azure

on:
  push:
    branches: [main]
  workflow_dispatch:

jobs:
  deploy:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'
      
      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v2
        with:
          terraform_version: 1.5.0
      
      - name: Azure Login
        uses: azure/login@v1
        with:
          creds: ${{ secrets.AZURE_CREDENTIALS }}
      
      - name: Build Application
        run: |
          npm install
          npm run build
      
      - name: Terraform Init
        working-directory: ./terraform
        run: terraform init
      
      - name: Terraform Apply
        working-directory: ./terraform
        env:
### Azure DevOps Pipeline Example

Create `azure-pipelines.yml`:

```yaml
trigger:
  - main

pool:
  vmImage: 'ubuntu-latest'

variables:
  - group: 'mcp-abap-adt-secrets'  # Variable group with secrets

steps:
  - task: NodeTool@0
    inputs:
      versionSpec: '18.x'
    displayName: 'Install Node.js'

  - task: TerraformInstaller@0
    inputs:
      terraformVersion: '1.5.0'
    displayName: 'Install Terraform'

  - script: |
      npm install
      npm run build
    displayName: 'Build Application'

  - task: AzureCLI@2
    inputs:
      azureSubscription: 'Azure-Service-Connection'
      scriptType: 'bash'
      scriptLocation: 'inlineScript'
      inlineScript: |
        cd terraform
        terraform init
        terraform apply -auto-approve
      workingDirectory: '$(System.DefaultWorkingDirectory)'
    env:
      TF_VAR_sap_url: $(SAP_URL)
      TF_VAR_sap_user: $(SAP_USER)
      TF_VAR_sap_password: $(SAP_PASSWORD)
    displayName: 'Deploy Infrastructure and Code'
```