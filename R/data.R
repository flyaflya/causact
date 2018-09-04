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

#' Dataframe of 12,145 observations of baseball games in 2010
#'
#'
#' @format A data frame with 12145 rows and 5 variables:
#' \describe{
#'   \item{Date}{date game was played}
#'   \item{Home}{abbreviation for home team (i.e. stadium where game played)}
#'   \item{Visitor}{abbreviation for visiting team}
#'   \item{HomeScore}{Runs scored by the home team}
#'   \item{VisitorScore}{Runs scored by the visiting team}
#' }
"baseballData"


#' Dataframe of 55,167 observations of the number of tickets
#' written by NYC precincts each day
#' Data modified from https://github.com/stan-dev/stancon_talks/tree/master/2018/Contributed-Talks/01_auerbach which originally sourced data from https://opendata.cityofnewyork.us/
#' @format A data frame with 55167 rows and 4 variables:
#' \describe{
#'   \item{precinct}{unique precinct identifier representing precinct of issuing officer}
#'   \item{date}{the date on which ticket violations occurred}
#'   \item{month_year}{the month_year extracted from date column}
#'   \item{daily_tickets}{Number of tickets issued out of precinct on this day}
#' }
"ticketsDF"

