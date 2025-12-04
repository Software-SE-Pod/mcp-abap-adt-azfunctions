# Key Vault for storing sensitive SAP credentials
resource "azurerm_key_vault" "kv" {
  name                       = local.key_vault_name
  resource_group_name        = azurerm_resource_group.rg.name
  location                   = azurerm_resource_group.rg.location
  tenant_id                  = data.azurerm_client_config.current.tenant_id
  sku_name                   = "standard"
  soft_delete_retention_days = 7
  purge_protection_enabled   = false

  # Network ACLs - Configure for production security
  network_acls {
    default_action = "Allow" # Change to "Deny" for production with specific IP allowlists
    bypass         = "AzureServices"
  }

  tags = local.common_tags
}

# Access policy for Terraform/deployment principal to manage secrets
resource "azurerm_key_vault_access_policy" "terraform" {
  key_vault_id = azurerm_key_vault.kv.id
  tenant_id    = data.azurerm_client_config.current.tenant_id
  object_id    = data.azurerm_client_config.current.object_id

  secret_permissions = [
    "Get",
    "List",
    "Set",
    "Delete",
    "Purge",
    "Recover"
  ]
}

# Access policy for Function App to read secrets
# Note: This is created after the function app to avoid circular dependency
resource "azurerm_key_vault_access_policy" "function_app" {
  key_vault_id = azurerm_key_vault.kv.id
  tenant_id    = data.azurerm_client_config.current.tenant_id
  object_id    = azurerm_linux_function_app.function.identity[0].principal_id

  secret_permissions = [
    "Get",
    "List"
  ]

  depends_on = [
    azurerm_linux_function_app.function
  ]
}

# Store SAP credentials in Key Vault
resource "azurerm_key_vault_secret" "sap_url" {
  name         = "sap-url"
  value        = var.sap_url
  key_vault_id = azurerm_key_vault.kv.id

  tags = local.common_tags

  depends_on = [
    azurerm_key_vault_access_policy.terraform
  ]
}

resource "azurerm_key_vault_secret" "sap_user" {
  name         = "sap-user"
  value        = var.sap_user
  key_vault_id = azurerm_key_vault.kv.id

  tags = local.common_tags

  depends_on = [
    azurerm_key_vault_access_policy.terraform
  ]
}

resource "azurerm_key_vault_secret" "sap_password" {
  name         = "sap-password"
  value        = var.sap_password
  key_vault_id = azurerm_key_vault.kv.id

  tags = local.common_tags

  depends_on = [
    azurerm_key_vault_access_policy.terraform
  ]
}
