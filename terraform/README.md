# Terraform Deployment

Deploy Azure Functions infrastructure with Key Vault for secure SAP credentials.

## Prerequisites

- [Terraform](https://www.terraform.io/downloads) >= 1.5.0
- [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/install-azure-cli)
- Azure CLI authenticated: `az login`

## Deploy Infrastructure

```bash
# Set required variables
export TF_VAR_sap_url="https://your-sap-server.com:44300"
export TF_VAR_sap_user="YOUR_USERNAME"
export TF_VAR_sap_password="YOUR_PASSWORD"

# Deploy
cd terraform
terraform init
terraform apply
```

## Deploy Function Code

```bash
# From project root
npm run build

# Prepare deployment directory
rm -rf deploy-temp
mkdir deploy-temp
cp -r dist/* deploy-temp/
cp host.json package.json deploy-temp/

# Fix package.json in deploy directory
cd deploy-temp
sed -i '' 's/"main": "dist\/index.js"/"main": "index.js"/g' package.json

# Install production dependencies
npm install --omit=dev

# Verify files before zipping
ls -la

# Create zip
zip -r ../function-app.zip .
cd ..

# Deploy
az functionapp deployment source config-zip \
  --resource-group $(cd terraform && terraform output -raw resource_group_name) \
  --name $(cd terraform && terraform output -raw function_app_name) \
  --src function-app.zip

# Wait a moment then restart
az functionapp restart \
  --name $(cd terraform && terraform output -raw function_app_name) \
  --resource-group $(cd terraform && terraform output -raw resource_group_name)
```

## Destroy Infrastructure

```bash
cd terraform
terraform destroy
```

## Configuration Variables

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
```
