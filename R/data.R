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
#' }
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
#' @source <https://www.transparency.org/cpi> CPI data available from www.transparency.org/cpi.  Accessed Oct 1, 2018. Consumer Perception Index 2017 by Transparency International is licensed under CC-BY- ND 4.0.
#' @source <http://hdr.undp.org/en/content/human-development-index-hdi> HDA data accessed on Oct 1, 2018.
#' @source <https://data.worldbank.org/> Population data accessed on Oct 1, 2018.
"corruptDF"


#' Dataframe of 1,460 observations of home sales in Ames, Iowa.  Known as The Ames Housing dataset, it was compiled by Dean De Cock for use in data science education.
#' Each observation is a home sale.  See `houseDFDescr` for more info.
#' @format A data frame with 1,460 rows and 37 variables:
#' \describe{
#'   \item{SalePrice}{the property's sale price in dollars. This is the target variable}
#'   \item{MSSubClass}{The building class}
#'   \item{MSZoning}{The general zoning classification}
#'   \item{LotFrontage}{Linear feet of street connected to property}
#'   \item{LotArea}{Lot size in square feet}
#'   \item{Street}{Type of road access}
#'   \item{LotShape}{General shape of property}
#'   \item{Utilities}{Type of utilities available}
#'   \item{LotConfig}{Lot configuration}
#'   \item{Neighborhood}{Physical locations within Ames city limits}
#'   \item{BldgType}{Type of dwelling}
#'   \item{HouseStyle}{Style of dwelling}
#'   \item{OverallQual}{Overall material and finish quality}
#'   \item{OverallCond}{Overall condition rating}
#'   \item{YearBuilt}{Original construction date}
#'   \item{YearRemodAdd}{Remodel date}
#'   \item{ExterQual}{Exterior material quality}
#'   \item{ExterCond}{Present condition of the material on the exterior}
#'   \item{BsmtQual}{Height of the basement}
#'   \item{BsmtCond}{General condition of the basement}
#'   \item{BsmtExposure}{Walkout or garden level basement walls}
#'   \item{BsmtUnfSF}{Unfinished square feet of basement area}
#'   \item{TotalBsmtSF}{Total square feet of basement area}
#'   \item{1stFlrSF}{First Floor square feet}
#'   \item{2ndFlrSF}{Second floor square feet}
#'   \item{LowQualFinSF}{Low quality finished square feet (all floors)}
#'   \item{GrLivArea}{Above grade (ground) living area square feet}
#'   \item{FullBath}{Full bathrooms above grade}
#'   \item{HalfBath}{Half baths above grade}
#'   \item{BedroomAbvGr}{Number of bedrooms above basement level}
#'   \item{TotRmsAbvGrd}{Total rooms above grade (does not include bathrooms)}
#'   \item{Functional}{Home functionality rating}
#'   \item{GarageCars}{Size of garage in car capacity}
#'   \item{MoSold}{Month Sold}
#'   \item{YrSold}{Year Sold}
#'   \item{SaleType}{Type of sale}
#'   \item{SaleCondition}{Condition of sale}
#' }
#' @source <https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data> Accessed Jan 22, 2019. Kaggle dataset on "House Prices: Advanced Regression Techniques".
"houseDF"

#' Dataframe of 523 descriptions of data values from "The Ames Housing dataset", compiled by Dean De Cock for use in data science education.
#' Each observation is a possible value from a varaible in the `houseDF` dataset.
#' @format A data frame with 260 rows and 2 variables:
#' \describe{
#'   \item{varName}{the name and description of a variable stored in the `houseDF` dataset}
#'   \item{varValueDescr}{The value and accompanying interpretation for values in the `houseDF` dataset}
#' }
#' @source <https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data> Accessed Jan 22, 2019. Kaggle dataset on "House Prices: Advanced Regression Techniques".
"houseDFDescr"

#' Dataframe of 1000 (fake) observations of whether certain car buyers were willing to get information on a credit card speciailizing in rewards for adventure travellers.
#' @format A data frame with 1000 rows and 3 variables:
#' \describe{
#'   \item{customerID}{a unique id of a potential credit card customer.  They just bought a car and are asked if they want information on the credit card.}
#'   \item{carModel}{The model of car purchased.}
#'   \item{getCard}{Whether the customer expressed interest in hearing more about the card.}
#' }
"carModelDF"

#' A representative sample from a random variable that represents the annual number of beach goers to Ocean City, MD beaches on hot days.  Think of this representative sample as coming from either a prior or posterior distribution.  An example using this sample is can be found in The Business Analyst's Guide To Business Analytics at https://www.causact.com/.
#' @format A 4,000 element vector.
#' \describe{
#'   \item{totalBeachgoersRepSample}{a draw from a representative sample of total beachgoers to Ocean City, MD.}
#' }
"totalBeachgoersRepSample"

#' Dataframe where each row represents data about one of the 26 mile markers (fake) from mile 0 to mile 2.5 along the Ocean City, MD beach/boardwalk.
#' @format A data frame with 26 rows and 3 variables:
#' \describe{
#'   \item{mileMarker}{a number representing a location on the Ocean City beach/boardwalk.}
#'   \item{beachgoerProb }{The probability of any Ocean City, MD beachgoer (during the hot swimming days) exiting the beach at that mile marker.}
#'   \item{expenseEst}{The estimated annual expenses of running a business at that location on the beach.  It is assumed a large portion of the expense is based on commercial rental rates at that location.  More populated locations tend to have higher expenses.}
#' }
"beachLocDF"

#' Data from behavior trials in a captive group of chimpanzees, housed in Lousiana. From Silk et al. 2005. Nature 437:1357-1359 and further popularized in McElreath, Richard. Statistical rethinking: A Bayesian course with examples in R and Stan. CRC press, 2020.  Experiment
#' @format A data frame with 504 rows and 9 variables:
#' \describe{
#'   \item{actor}{name of actor}
#'   \item{recipient}{name of recipient (NA for partner absent condition)}
#'   \item{condition}{partner absent (0), partner present (1)}
#'   \item{block}{block of trials (each actor x each recipient 1 time)}
#'   \item{trial}{trial number (by chimp = ordinal sequence of trials for each chimp, ranges from 1-72; partner present trials were interspersed with partner absent trials)}
#'   \item{prosoc_left}{prosocial_left : 1 if prosocial (1/1) option was on left}
#'   \item{chose_prosoc}{choice chimp made (0 = 1/0 option, 1 = 1/1 option)}
#'   \item{pulled_left}{which side did chimp pull (1 = left, 0 = right)}
#'   \item{treatment}{narrative description combining condition and prosoc_left that describes the side the prosical food option was on and whether a partner was present}
#' }
#' @source Silk et al. 2005. Nature 437:1357-1359..
"chimpanzeesDF"

#' This example, often referred to as 8-schools, was popularized by its inclusion in Bayesian Data Analysis (Gelman, Carlin, & Rubin 1997).
#' @format A data frame with 8 rows and 3 variables:
#' \describe{
#'   \item{y}{estimated treatment effect at a particular school}
#'   \item{sigma}{standard error of the treamtment effect estimate}
#'   \item{schoolName}{an identifier for the school represented by this row}
#'   }
"schoolsDF"

