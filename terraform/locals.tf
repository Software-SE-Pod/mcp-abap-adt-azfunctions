locals {
  # Generate unique suffix for globally unique resource names
  suffix = random_string.suffix.result

  # Resource naming conventions
  resource_group_name      = "rg-${var.project_name}-${var.environment}"
  storage_account_name     = "st${replace(var.project_name, "-", "")}${local.suffix}"
  key_vault_name           = "kv-${substr(var.project_name, 0, 15)}-${local.suffix}"
  function_app_name        = "func-${var.project_name}-${var.environment}-${local.suffix}"
  app_service_plan_name    = "asp-${var.project_name}-${var.environment}"
  app_insights_name        = "appi-${var.project_name}-${var.environment}"

  # Common tags for all resources
  common_tags = merge(
    {
      Environment = var.environment
      ManagedBy   = "Terraform"
      Project     = var.project_name
      DeployedAt  = timestamp()
    },
    var.tags
  )
}

resource "random_string" "suffix" {
  length  = 6
  special = false
  upper   = false
}
