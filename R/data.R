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

#' Dataframe of 44 observations of free crossfit classes data
#' Each observation indicates how many students that participated in the free
#' month of crossfit signed up for the monthly membership afterwards
#' @format A data frame with 44 rows and 5 variables:
#' \describe{
#'   \item{gymID}{unique gym identifier}
#'   \item{nTrialCustomers}{number of unique customers taking free trial classes}
#'   \item{nSigned}{number of customers from trial that sign up for membership}
#'   \item{yogaStretch}{whether trial classes included a yoga type stretch}
#'   \item{timePeriod}{month number, since inception of company, for which trial period was offered}
#' }
"gymDF"

#' Dataframe of 174 observations where information on the human developmet index (HDI)
#' and the corruption perceptions index (CPI) both exist.
#' Each observation is a country.
#' @format A data frame with 174 rows and 7 variables:
#' \describe{
#'   \item{country}{country name}
#'   \item{region}{region name as given with CPI rating}
#'   \item{countryCode}{three letter abbreviation for country}
#'   \item{regionCode}{four letter or less abbreviation for country}
#'   \item{population}{2017 country population}
#'   \item{CPI2017}{The Corruption Perceptions Index score for 2017: A country/territory’s score indicates the perceived level of public sector corruption on a scale of 0-100, where 0 means that a country is perceived as highly corrupt and a 100 means that a country is perceived as very clean. }
#'   \item{HDI2017}{The human development index score for 2017: the Human Development Index (HDI) is a measure of achievement in the basic dimensions of human development across countries.  It is an index made from a simple unweighted average of a nation’s longevity, education and income and is widely accepted in development discourse.}
#' }
#' @source \url{http://www.transparency.org/cpi} CPI data available from www.transparency.org/cpi.  Accessed Oct 1, 2018. Consumer Perception Index 2017 by Transparency International is licensed under CC-BY- ND 4.0.
#' @source \url{http://hdr.undp.org/en/content/human-development-index-hdi} HDA data accessed on Oct 1, 2018.
#' @source \url{https://data.worldbank.org/indicator/sp.pop.totl} Population data accessed on Oct 1, 2018.
"corruptDF"

