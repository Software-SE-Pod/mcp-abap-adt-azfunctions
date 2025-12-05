variable "environment" {
  type        = string
  description = "Environment name (dev, staging, prod)"
  default     = "dev"
}

variable "location" {
  type        = string
  description = "Azure region for resources"
  default     = "eastus"
}

variable "project_name" {
  type        = string
  description = "Project name used for resource naming"
  default     = "mcp-abap-adt"
}

# SAP Configuration - Read from environment variables
variable "sap_url" {
  type        = string
  description = "SAP server URL (set via TF_VAR_sap_url)"
  sensitive   = true
}

variable "sap_user" {
  type        = string
  description = "SAP username (set via TF_VAR_sap_user)"
  sensitive   = true
}

variable "sap_password" {
  type        = string
  description = "SAP password (set via TF_VAR_sap_password)"
  sensitive   = true
}

variable "sap_client" {
  type        = string
  description = "SAP client number (set via TF_VAR_sap_client)"
  default     = ""
}

variable "sap_language" {
  type        = string
  description = "SAP language code (set via TF_VAR_sap_language)"
  default     = "EN"
}

variable "node_tls_reject_unauthorized" {
  type        = string
  description = "Set to '0' to allow self-signed certificates (set via TF_VAR_node_tls_reject_unauthorized)"
  default     = "1"
}

# Function App Configuration
variable "function_app_sku" {
  type        = string
  description = "SKU for Function App Service Plan (P0v3=Premium V3 Small, P1v3=Premium V3 Medium, B1=Basic, S1=Standard, Y1=Consumption)"
  default     = "B1"
}

variable "enable_app_insights" {
  type        = bool
  description = "Enable Application Insights for monitoring"
  default     = true
}

variable "tags" {
  type        = map(string)
  description = "Additional tags for all resources"
  default     = {}
}
