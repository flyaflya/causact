#' Generate posterior check
#'
#' The input should be the graph created using \code{dag_create()} and results generated from \code{dag_greta(mcmc=TRUE)}
#'
#' @param
#' @return return a named list with class c("psis_loo", "loo") and components. Please see the loo function in a package \code{rstanarm} for more details.
#'
#' @example
#'carModelDF$carModel[1:4]

#'graph <- dag_create() %>%
#'  dag_node("Bernoulli","y",
#'           rhs = bernoulli(theta),
#'           data = carModelDF$getCard) %>%
#'  dag_node("Probability","theta",
#'           rhs = beta(2,2),
#'           child = "y")
#'graph %>% dag_render()
#'graph %>% dag_greta(mcmc = T)
#'post_check <- dag_posterior()
#'
#' @importFrom dplyr tibble as_tibble filter left_join mutate slice pull
#' @importFrom rlang parse_expr
#' @importFrom stringr str_split
#' @export

dag_posterior = function(){
  ### Find whether variables are estimated for posterior check
  colInNeed = graph$arg_df %>%
    dplyr::filter(rhsID==1,argType=='param')

  colInDraws = tibble::enframe(unlist(stringr::str_split(colnames(drawsDF),boundary("word"))),name="DrawCol")

  if(exists("draws_extraDF")){
    colInExtra = tibble::enframe(unlist(stringr::str_split(colnames(draws_extraDF),boundary("word"))),name="ExtraCol")
  } else{
    colInExtra = tibble(value="InCasecolInExtraDoesNotExist",ExtraCol="InCasecolInExtraDoesNotExist")
  }

  colComp =
    colInNeed %>%
    dplyr::left_join(colInDraws,by=c('argValue'='value')) %>%
    dplyr::left_join(colInExtra,by=c('argValue'='value')) %>%
    dplyr::mutate(foundInDraws = ifelse( (is.na(DrawCol) & is.na(ExtraCol)),0,1)) %>%
    dplyr::filter(foundInDraws==0)

  ### Identify whether a plate node exists
  plateDimDF <-  graph$plate_index_df %>% dplyr::filter(!is.na(dataNode))
  plate_flag <- ifelse(nrow(plateDimDF)>0,1,0)

  ### Find data length and the likelihood distrubution in use
  nodeDF = graph$nodes_df
  plateDF = graph$plate_index_df

  dataLabel = nodeDF %>%
    dplyr::filter(obs == TRUE | !is.na(data)) %>%
    dplyr::filter(!(label %in% plateDF$indexLabel)) %>%
    dplyr::pull(auto_label)

  datalength = length(eval(rlang::parse_expr(dataLabel)))

  dataLikelihood = nodeDF %>%
    dplyr::filter(obs == TRUE | !is.na(data)) %>%
    dplyr::filter(!(label %in% plateDF$indexLabel)) %>%
    dplyr::pull(rhs)

  ### Generate data frames for paramaters in the likelihood distribution
  argValue_1 <-
    colInNeed %>%
    dplyr::slice(1) %>%
    dplyr::pull(argValue)
  if(identical(argValue_1,character(0))){
    DF1 <- NULL
  } else if(exists("draws_extraDF")){
    DF1 <- drawsDF_all[,grepl(argValue_1,names(drawsDF_all))]
  } else {
    DF1 <- drawsDF[,grepl(argValue_1,names(drawsDF))]
  }

  argValue_2 <-
    colInNeed %>%
    dplyr::slice(2) %>%
    dplyr::pull(argValue)
  if(identical(argValue_2,character(0))){
    DF2 <- NULL
  } else if(exists("draws_extraDF")){
    DF2 <- drawsDF_all[,grepl(argValue_2,names(drawsDF_all))]
  } else {
    DF2 <- drawsDF[,grepl(argValue_2,names(drawsDF))]
  }

  argValue_3 <-
    colInNeed %>%
    dplyr::slice(3) %>%
    dplyr::pull(argValue)
  if(identical(argValue_3,character(0))){
    DF3 <- NULL
  } else if(exists("draws_extraDF")){
    DF3 <- drawsDF_all[,grepl(argValue_3,names(drawsDF_all))]
  } else {
    DF3 <- drawsDF[,grepl(argValue_3,names(drawsDF))]
  }

  #names(drawsDF_all) <- make.names(names(drawsDF_all))

  ### Generate a matrix for posterior check
  postCheck <- matrix(0,nrow=4000,ncol=datalength)


  if(plate_flag==1){
    print("Posterior check for a model with plate is under development")
  } else if(nrow(colComp)>0){
    print("More parameters need to be estimated. Extra parameters can be specified using extraPara in dag_greta")
  } else if(dataLikelihood == "normal") {
    if(ncol(DF1)==1 & ncol(DF2)==1){
      for(i in 1:datalength){
        postCheck[,i] = rnorm(4000,unlist(DF1[,1]),unlist(DF2[,1]))
      }
    } else if(ncol(DF1)>1 & ncol(DF2)==1){
      for(i in 1:datalength){
        postCheck[,i] = rnorm(4000,unlist(DF1[,i]),unlist(DF2[,1]))
      }
    } else if(ncol(DF1)==1 & ncol(DF2)>1){
      for(i in 1:datalength){
        postCheck[,i] = rnorm(4000,unlist(DF1[,1]),unlist(DF2[,i]))
      }
    } else{
      for(i in 1:datalength){
        postCheck[,i] = rnorm(4000,unlist(DF1[,i]),unlist(DF2[,i]))
      }
    }
  } else if(dataLikelihood == "bernoulli") {
    if(ncol(DF1)==1 ){
      for(i in 1:datalength){
        postCheck[,i] = rbinom(4000,1,unlist(DF1[,1]))
      }
    } else{
      for(i in 1:datalength){
        postCheck[,i] = rbinom(4000,1,unlist(DF1[,i]))
      }
    }
  } else if(dataLikelihood == "poisson") {
    if(ncol(DF1)==1 ){
      for(i in 1:datalength){
        postCheck[,i] = rpois(4000,unlist(DF1[,1]))
      }
    } else{
      for(i in 1:datalength){
        postCheck[,i] = rpois(4000,unlist(DF1[,i]))
      }
    }
  } else{
    print("Posterior check for this likelihood distribution is under development")
  }
  return(postCheck)
}


###

# ### Example 1 Bernoulli_simple
# carModelDF$carModel[1:4]
#
# graph <- dag_create() %>%
#   dag_node("Bernoulli","y",
#            rhs = bernoulli(theta),
#            data = carModelDF$getCard) %>%
#   dag_node("Probability","theta",
#            rhs = beta(2,2),
#            child = "y")
#
# graph %>% dag_render()
#
# graph %>% dag_greta(mcmc = T)
#
# post_check <- dag_posterior()
# summary(drawsDF)
# mean(colMeans(post_check))

### Example 2 linear regression
# graph = dag_create() %>%
#   dag_node("Cherry Tree Height","X",
#            rhs = normal(mu,sigma),
#            data = trees$Height) %>%
#   dag_node("Exp Height of Tree","mu",
#            rhs = normal(50,24.5),
#            child = 'X') %>%
#   dag_node("Sd of Tree","sigma",
#            rhs = uniform (0,50),
#            child = 'X')
# graph %>% dag_render()
# graph %>% dag_greta(mcmc = TRUE)
#
# post_check <- dag_posterior()
# colMeans(post_check)

# ### Example 3 Poisson regression
# nycTicketsDF = ticketsDF %>%
#   group_by(date) %>%
#   summarize(numTickets = sum(daily_tickets)) %>%
#   mutate(dayOfWeek = lubridate::wday(date, label = TRUE))
#
# graph = dag_create() %>%
#   dag_node("# of tickets","K",
#            rhs = poisson(lambda),
#            data = nycTicketsDF$numTickets) %>%
#   dag_node("Exp Number of Tickets","lambda",
#            rhs = normal(4500,2000,truncation = c(0,Inf)),
#            child = "K") %>%
#   dag_plate("Day of The Week","day",
#             nodeLabels = "lambda",
#             data = nycTicketsDF$dayOfWeek,
#             addDataNode = TRUE)
# graph %>% dag_render()
# graph %>% dag_greta(mcmc = TRUE)
#
# post_check <- dag_posterior()

# ### Example 4 linear regression with formular
# data(attitude)
# design <- as.matrix(attitude[, 2:7])
#
# graph <- dag_create() %>%
#   dag_node("normal","y",
#            rhs = normal(mu,sd),
#            data = attitude$rating) %>%
#   dag_node("Mean","mu",
#            rhs = int+design %*% coefs,
#            child = "y") %>%
#   dag_node("Intercept","int",
#            rhs = normal(0,10),
#            child = "mu") %>%
#   dag_node("Coefficient","coefs",
#            rhs = normal(0,10,dim=ncol(design)),
#            child = "mu") %>%
#   dag_node("Standard deviation","sd",
#            rhs = cauchy(0, 3, truncation = c(0, Inf)),
#            child = "y")
#
# graph %>% dag_render()

# graph %>% dag_greta(mcmc = T)
# post_check <- dag_posterior()
#
# graph %>% dag_greta(mcmc = T,extraPara=c('mu'))
# post_check <- dag_posterior()


# graph %>% dag_greta
# graph %>% dag_greta(extraPara=c('mu'))
#
#

#

