#' 117,790 line items associated with 23,339 shipments.
#'
#' A dataset containing the line items, mostly parts, asssociated
#' with 23,339 shipments from a US-based warehouse.
#'
#' @format A data frame (tibble) with 117,790 rows and 5 variables:
#' \describe{
#'   \item{shipID}{unique ID for each shipment}
#'   \item{plannedShipDate}{shipment date promised to customer}
#'   \item{actualShipDate}{date the shipment was actually shipped}
#'   \item{partID}{unique part identifier}
#'   \item{quantity}{quantity of partID in shipment}
#' }
#' @source Adam Fleischhacker
"delivDF"

#' Product line and product category assignments for 12,026 partID's.
#'
#' A dataset containing partID attributes.
#'
#' @format A data frame (tibble) with 117,790 rows and 5 variables:
#' \describe{
#'   \item{partID}{unique part identifier}
#'   \item{productLine}{a product line associated with the partID}
#'   \item{prodCategory}{a product category associated with the partID}
#'   #' }
#' @source Adam Fleischhacker
"prodLineDF"
