# Resource Group
resource "azurerm_resource_group" "rg" {
  name     = local.resource_group_name
  location = var.location

  tags = local.common_tags
}

# Storage Account (required by Azure Functions)
resource "azurerm_storage_account" "storage" {
  name                     = local.storage_account_name
  resource_group_name      = azurerm_resource_group.rg.name
  location                 = azurerm_resource_group.rg.location
  account_tier             = "Standard"
  account_replication_type = "LRS"
  min_tls_version          = "TLS1_2"

  tags = local.common_tags
}

# Log Analytics Workspace for Application Insights
resource "azurerm_log_analytics_workspace" "workspace" {
  count               = var.enable_app_insights ? 1 : 0
  name                = local.log_analytics_name
  resource_group_name = azurerm_resource_group.rg.name
  location            = azurerm_resource_group.rg.location
  sku                 = "PerGB2018"
  retention_in_days   = 30

  tags = local.common_tags
}

# Application Insights for monitoring
resource "azurerm_application_insights" "insights" {
  count               = var.enable_app_insights ? 1 : 0
  name                = local.app_insights_name
  resource_group_name = azurerm_resource_group.rg.name
  location            = azurerm_resource_group.rg.location
  application_type    = "Node.JS"
  workspace_id        = azurerm_log_analytics_workspace.workspace[0].id

  tags = local.common_tags
}

# App Service Plan for Function App
resource "azurerm_service_plan" "plan" {
  name                = local.app_service_plan_name
  resource_group_name = azurerm_resource_group.rg.name
  location            = azurerm_resource_group.rg.location
  os_type             = "Linux"
  sku_name            = var.function_app_sku

  tags = local.common_tags
}

# Linux Function App
resource "azurerm_linux_function_app" "function" {
  name                       = local.function_app_name
  resource_group_name        = azurerm_resource_group.rg.name
  location                   = azurerm_resource_group.rg.location
  storage_account_name       = azurerm_storage_account.storage.name
  storage_account_access_key = azurerm_storage_account.storage.primary_access_key
  service_plan_id            = azurerm_service_plan.plan.id
  https_only                 = true

  # Enable system-assigned managed identity for Key Vault access
  identity {
    type = "SystemAssigned"
  }

  site_config {
    application_stack {
      node_version = "18"
    }

    # Enable Application Insights if configured
    application_insights_connection_string = var.enable_app_insights ? azurerm_application_insights.insights[0].connection_string : null

    # CORS configuration (adjust for production)
    cors {
      allowed_origins = ["*"]
    }

    # Security headers
    ftps_state = "Disabled"
  }

  # Application settings with Key Vault references
  app_settings = {
    # Function runtime settings
    "FUNCTIONS_WORKER_RUNTIME"     = "node"
    "WEBSITE_NODE_DEFAULT_VERSION" = "~18"
    "AzureWebJobsFeatureFlags"     = "EnableWorkerIndexing"
    
    # Application Insights
    "APPINSIGHTS_INSTRUMENTATIONKEY" = var.enable_app_insights ? azurerm_application_insights.insights[0].instrumentation_key : ""
    
    # SAP Configuration - Reference Key Vault secrets
    "SAP_URL"                      = "@Microsoft.KeyVault(SecretUri=${azurerm_key_vault_secret.sap_url.id})"
    "SAP_USER"                     = "@Microsoft.KeyVault(SecretUri=${azurerm_key_vault_secret.sap_user.id})"
    "SAP_PASSWORD"                 = "@Microsoft.KeyVault(SecretUri=${azurerm_key_vault_secret.sap_password.id})"
    "SAP_CLIENT"                   = var.sap_client
    "SAP_LANGUAGE"                 = var.sap_language
    "NODE_TLS_REJECT_UNAUTHORIZED" = var.node_tls_reject_unauthorized
  }

  tags = local.common_tags
}
