# Resource Group
output "resource_group_name" {
  description = "Name of the resource group"
  value       = azurerm_resource_group.rg.name
}

output "resource_group_location" {
  description = "Location of the resource group"
  value       = azurerm_resource_group.rg.location
}

# Key Vault
output "key_vault_name" {
  description = "Name of the Key Vault"
  value       = azurerm_key_vault.kv.name
}

output "key_vault_uri" {
  description = "URI of the Key Vault"
  value       = azurerm_key_vault.kv.vault_uri
}

# Storage Account
output "storage_account_name" {
  description = "Name of the storage account"
  value       = azurerm_storage_account.storage.name
}

# Function App
output "function_app_name" {
  description = "Name of the Function App"
  value       = azurerm_linux_function_app.function.name
}

output "function_app_id" {
  description = "ID of the Function App"
  value       = azurerm_linux_function_app.function.id
}

output "function_app_default_hostname" {
  description = "Default hostname of the Function App"
  value       = azurerm_linux_function_app.function.default_hostname
}

output "function_app_url" {
  description = "Full URL of the MCP endpoint"
  value       = "https://${azurerm_linux_function_app.function.default_hostname}/api/mcp"
}

output "function_app_identity_principal_id" {
  description = "Principal ID of the Function App managed identity"
  value       = azurerm_linux_function_app.function.identity[0].principal_id
}

# Application Insights
output "app_insights_instrumentation_key" {
  description = "Application Insights instrumentation key"
  value       = var.enable_app_insights ? azurerm_application_insights.insights[0].instrumentation_key : ""
  sensitive   = true
}

output "app_insights_connection_string" {
  description = "Application Insights connection string"
  value       = var.enable_app_insights ? azurerm_application_insights.insights[0].connection_string : ""
  sensitive   = true
}

# Deployment Information
output "deployment_summary" {
  description = "Summary of deployed resources"
  value = {
    environment          = var.environment
    location             = var.location
    function_app_name    = azurerm_linux_function_app.function.name
    function_app_url     = "https://${azurerm_linux_function_app.function.default_hostname}/api/mcp"
    key_vault_name       = azurerm_key_vault.kv.name
    app_insights_enabled = var.enable_app_insights
  }
}
