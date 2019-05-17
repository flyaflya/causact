#' Generate efficient approximate leave-one-out cross-validation
#'
#' The input should be the graph created using \code{dag_create()} and results generated from \code{dag_greta(mcmc=TRUE)}
#'
#'
#'
#' @param
#' @return return a named list with class c("psis_loo", "loo") and components. Please see the loo function in a package \code{rstanarm} for more details.
#'
#' @example
#'graph <- dag_create() %>%
#'  dag_node("Bernoulli","y",
#'           rhs = bernoulli(theta),
#'           data = carModelDF$getCard) %>%
#'  dag_node("Probability","theta",
#'           rhs = beta(2,2),
#'           child = "y")
#'
#' graph %>% dag_render()
#'
#' graph %>% dag_greta(mcmc = T)
#'
#' loo <- dag_loo()
#' print(loo)
#' plot(loo)
#' @importFrom dplyr tibble as_tibble filter
#' @importFrom rlang parse_expr
#' @importFrom stringr str_split
#' @importFrom rstanarm relative_eff loo
#' @export


dag_loo <- function(){
  y_train <- parse_expr(graph$nodes_df$data[1])
  likelihood_distribution <- graph$nodes_df$rhs[1]
  data_length <- eval(parse_expr(paste0("length(",graph$nodes_df$data[1],")")))

  plateDimDF <-  graph$plate_index_df %>% dplyr::filter(!is.na(dataNode))
  plate_flag <- ifelse(nrow(plateDimDF)>0,1,0)
  if(plate_flag==1){
    a_label <- parse_expr(plateDimDF$indexLabel)
    a_dim <- parse_expr(paste0(plateDimDF$indexLabel,"_dim"))
  }

  if(plate_flag==1){
    a_post <- drawsDF[,(1:eval(a_dim))]
    a_array <- array(as.matrix(a_post),c(4000,eval(a_dim)))
  } else{
    a_post <- drawsDF[,1]
    a_array <-array(as.matrix(a_post),c(4000,1))
  }

  Prop_fit <- matrix(0,nrow = 4000, ncol = data_length)
  for(i in (1:data_length)) {
    if(likelihood_distribution == "bernoulli"){
      if(plate_flag==1){
        Prop_fit[,i] <- dbinom(eval(y_train)[i], 1, a_array[,as.numeric(eval(a_label))[i]])
      } else{Prop_fit[,i] <- dbinom(eval(y_train)[i], 1, a_array[,1]) }

    } else if(likelihood_distribution == "normal"){

      if(plate_flag==1){
        int_fit <- drawsDF[,1:eval(a_dim)]
        coefs_fit <- t(cbind(drawsDF[,(eval(a_dim)+1):(ncol(drawsDF)-1)]))
        mu_formular <- graph$nodes_df$rhs[graph$nodes_df$label=='mu']
        X <- str_split(mu_formular, boundary("word"))[[1]][2]
        mu_fit <- unlist(int_fit[,as.numeric(eval(a_label))[i]]) + t(eval(parse_expr(X)) %*% coefs_fit)
        sd_fit <- as.numeric(unlist(drawsDF[,ncol(drawsDF)]))
        Prop_fit[,i] <- dnorm(eval(y_train)[i], mu_fit[,i], sd_fit)

      } else {
        mu_fit <- a_array[,1]
        sd_fit <- array(as.matrix(drawsDF[,2]),c(4000,1))[,1]
        Prop_fit[,i] <- dnorm(eval(y_train)[i], mu_fit, sd_fit)
      }

        # else{
        # int_fit <- drawsDF[,1]
        # coefs_fit <- t(cbind(drawsDF[,2:(ncol(drawsDF)-1)]))
        # mu_formular <- graph$nodes_df$rhs[graph$nodes_df$label=='mu']
        # X <- str_split(mu_formular, boundary("word"))[[1]][2]
        # mu_fit <- unlist(int_fit) + t(eval(parse_expr(X)) %*% coefs_fit)
        # sd_fit <- as.numeric(unlist(drawsDF[,ncol(drawsDF)]))
        # Prop_fit[,i] <- dnorm(eval(y_train)[i], mu_fit[,i], sd_fit) }

    } else if(likelihood_distribution == "poisson"){
      if(plate_flag==1){
        Prop_fit[,i] <- dpois(eval(y_train)[i],a_array[,as.numeric(eval(a_label))[i]])
        Prop_fit[,i] <- ifelse(Prop_fit[,i]==0,10^-200,Prop_fit[,i])
      } else{
        Prop_fit[,i] <- dpois(eval(y_train)[i],a_array[,1])
      }

    } else{Prop_fit[,i] <- -1}
  }

  LLmat <- log(Prop_fit)
  rel_n_eff <- relative_eff(exp(LLmat), chain_id = rep(1:4, each = 1000))
  rel_n_eff <- ifelse(is.na(rel_n_eff),mean(rel_n_eff,na.rm = T),rel_n_eff)
  loo <- loo(LLmat, r_eff = rel_n_eff, cores = 4,save_psis = TRUE)
  return(loo)

}

### Original example from the "loo" package
library("rstanarm")
library("bayesplot")
library("loo")

data(roaches)
str(roaches)

roaches$roach1 <- roaches$roach1 / 100

fit1 <-
  stan_glm(
    formula = y ~ roach1 + treatment + senior,
    #offset = log(exposure2),
    data = roaches,
    family = poisson(link = "log"),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_intercept = normal(0, 5, autoscale = TRUE),
    seed = 12345
  )

loo1 <- loo(fit1, save_psis = TRUE)
plot(loo1)
print(loo1)

### Analyze the data using greta
library(greta)
library(tidyverse)
library(rlang)


X <- roaches %>%
  select(roach1,treatment,senior)
x_greta <- as_data(X)
y_greta <- as_data(roaches$y)

a <- normal(0, 5)
b1 <- normal(0, 2.5)
b2 <- normal(0, 2.5)
b3 <- normal(0, 2.5)
y <- a+ b1*X$roach1 + b2*X$treatment + b3*X$senior
lambda <- exp(y)
distribution(y_greta) <- poisson(lambda)

m_greta <- model(a,b1,b2,b3)
plot(m_greta)
#
draw_m_greta <- mcmc(m_greta, warmup = 3000, n_samples = 1000)
# # draw_trace <- mcmc_trace(draw_m_greta)
# # draw_pairs <- mcmc_pairs(draw_m_greta)
# # draw_trace
# # draw_pairs
#
# summary(draw_m_greta)
para_fit <- summary(draw_m_greta)[[1]][,1]
y_fit <- para_fit[1]+ para_fit[2]*X$roach1 + para_fit[3]*X$treatment + para_fit[4]*X$senior
lambda_fit <- exp(y_fit)
dpois(153,100.247993)
summary(lambda_fit - fit1$fitted.values)

### manually calculate pointwise log-likelihood
ParaEst_post <- as_tibble(rbind(draw_m_greta[[1]],draw_m_greta[[2]],draw_m_greta[[3]],draw_m_greta[[4]]))

a_post <- ParaEst_post[,1]
a_array <- array(as.matrix(a_post),c(4000))

b1_post <- ParaEst_post[,2]
b1_array <- array(as.matrix(b1_post),c(4000))

b2_post <- ParaEst_post[,3]
b2_array <- array(as.matrix(b2_post),c(4000))

b3_post <- ParaEst_post[,3]
b3_array <- array(as.matrix(b3_post),c(4000))

Prop_fit <- matrix(0,nrow = 4000, ncol = nrow(X))
i <- 1
for(i in (1:nrow(X))) {

  Prop_fit[,i] <- dpois(y_greta[i] ,exp(a_array+ b1_array*X$roach1[i] +
                                       b2_array*X$treatment[i] + b3_array*X$senior[i]))
}
dim(Prop_fit)
summary(Prop_fit)
LLmat <- log(Prop_fit)
LLmat[is.na]
rel_n_eff <- relative_eff(exp(LLmat), chain_id = rep(1:4, each = 1000))
rel_n_eff <- ifelse(is.na(rel_n_eff),mean(rel_n_eff,na.rm = T),rel_n_eff)
loo2 <- loo(LLmat, r_eff = rel_n_eff, cores = 4,save_psis = TRUE)
plot(loo2)

### compare loo results from rstanarm and greta
print(loo1)
print(loo2)

############# calculate loo using the carModelDF in causact
# remotes::install_github("flyaflya/causact")
# devtools::install_github('flyaflya/causact')
library(causact)
library(loo)
library(greta)
library(tidyverse)
library(rlang)
#
# ### Example 1 Bernoulli_simple
carModelDF$carModel[1:4]

graph <- dag_create() %>%
  dag_node("Bernoulli","y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node("Probability","theta",
           rhs = beta(2,2),
           child = "y")

graph %>% dag_render()

graph %>% dag_greta(mcmc = T)

loo1 <- dag_loo()
print(loo1)
plot(loo1)
###

# graph %>% causact:::dag_dim()
#
# graph <- graph %>% causact:::dag_dim()
# nodes_df <- graph$nodes_df
# data_input <- nodes_df$data[1]
# eval(parse_expr(paste0("length(",data_input,")")))
#
# data_length <- eval(parse_expr(paste0("length(",data_input,")")))
#
# a_post <- drawsDF[,1]
# a_array <- array(as.matrix(a_post),c(4000))
#
# linear_combination <- a_array
#
# Prop_fit <- matrix(0,nrow = 4000, ncol = data_length)
# for(i in (1:data_length)) {
#   Prop_fit[,i] <- dbinom(carModelDF$getCard[i], 1, linear_combination)
# }
#
# LLmat <- log(Prop_fit)
# rel_n_eff <- relative_eff(exp(LLmat), chain_id = rep(1:4, each = 1000))
# rel_n_eff <- ifelse(is.na(rel_n_eff),mean(rel_n_eff,na.rm = T),rel_n_eff)
# loo <- loo(LLmat, r_eff = rel_n_eff, cores = 4,save_psis = TRUE)
# print(loo)
# plot(loo)

### Exmaple 2 Bernoulli_w_plate
graph <- dag_create() %>%
  dag_node("Bernoulli","y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node("Probability","theta",
           rhs = beta(2,2),
           child = "y") %>%
  dag_plate("Car Model","x",
            nodeLabels = "theta",
            data = carModelDF$carModel,
            addDataNode = TRUE
  )

graph %>% dag_render()

graph %>% dag_greta(mcmc = T)

loo2 <- dag_loo()
print(loo2)
plot(loo2)

print(loo1)
# y_train <- parse_expr(graph$nodes_df$data[1])
# likelihood_distribution <- graph$nodes_df$rhs[1]
# data_length <- eval(parse_expr(paste0("length(",graph$nodes_df$data[1],")")))
#
# plateDimDF <-  graph$plate_index_df %>% dplyr::filter(!is.na(dataNode))
# plate_flag <- ifelse(nrow(plateDimDF)>0,1,0)
# a_label <- parse_expr(plateDimDF$indexLabel)
# a_dim <- parse_expr(paste0(plateDimDF$indexLabel,"_dim"))
#
# if(plate_flag==1){
#   a_post <- drawsDF[,(1:eval(a_dim))]
#   a_array <- array(as.matrix(a_post),c(4000,eval(a_dim)))
# } else{
#   a_post <- drawsDF[,1]
#   a_array <-array(as.matrix(a_post),c(4000))
#   }
#
# linear_combination <- a_array
#
#
# Prop_fit <- matrix(0,nrow = 4000, ncol = data_length)
# for(i in (1:data_length)) {
#   if(likelihood_distribution == "bernoulli"){
#     if(plate_flag==1){
#       Prop_fit[,i] <- dbinom(eval(y_train)[i], 1, a_array[,as.numeric(eval(a_label))[i]])
#     } else{Prop_fit[,i] <- dbinom(eval(y_train)[i], 1, a_array[,1]) }
#
#     } else{Prop_fit[,i] <- -1}
# }
# LLmat <- log(Prop_fit)
# rel_n_eff <- relative_eff(exp(LLmat), chain_id = rep(1:4, each = 1000))
# rel_n_eff <- ifelse(is.na(rel_n_eff),mean(rel_n_eff,na.rm = T),rel_n_eff)
# loo <- loo(LLmat, r_eff = rel_n_eff, cores = 4,save_psis = TRUE)
# print(loo)
# print(loo2)
# plot(loo2)

### Example 3 linear regression
data(attitude)
design <- as.matrix(attitude[, 2:7])

graph <- dag_create() %>%
  dag_node("normal","y",
           rhs = normal(mu,sd),
           data = attitude$rating) %>%
  dag_node("Mean","mu",
           rhs = int+design %*% coefs,
           child = "y") %>%
  dag_node("Intercept","int",
           rhs = normal(0,10),
           child = "mu") %>%
  dag_node("Coefficient","coefs",
           rhs = normal(0,10,dim=ncol(design)),
           child = "mu") %>%
  dag_node("Standard deviation","sd",
           rhs = cauchy(0, 3, truncation = c(0, Inf)),
           child = "y")

graph %>% dag_render()

graph %>% dag_greta(mcmc = T)


graph = graph %>% causact:::dag_dim()
graph = causact:::rhsPriorComposition(graph)
graph = causact:::rhsOperationComposition(graph)

nodeDF = graph$nodes_df
edgeDF = graph$edges_df
argDF = graph$arg_df
plateDF = graph$plate_index_df
plateNodeDF = graph$plate_node_df
dimDF = graph$dim_df

NodeToOutput = nodeDF %>%
  dplyr::filter(obs == TRUE) %>%  ##only observed nodes
  dplyr::inner_join(edgeDF, by = c("id" = "to")) %>%
  distinct(from) %>%
  inner_join(nodeDF,by = c("from"="id")) %>%
  mutate(fitted_flag = if_else(obs == FALSE & distr == TRUE,1,0) ) %>%
  left_join(filter(argDF,argName=="dim"), by = c("from"="rhsID"))

fitted <- NodeToOutput %>%
  dplyr::filter(fitted_flag == 1) %>%
  select(label) %>%
  unlist()

fitted_vector  <-  paste0(fitted,"_fit")

fitted_vector <- drawsDF %>%
  select(fitted)

colnames(fitted_vector) <- paste0(fitted,"_fit")

if(sum(NodeToOutput$fitted_flag)<nrow(NodeToOutput)){
  NodeToOutput_2 = NodeToOutput %>%
    dplyr::filter(fitted_flag == 0) %>%
    rename("id" = "from") %>%
    dplyr::inner_join(edgeDF, by = c("id" = "to")) %>%
    distinct(from) %>%
    inner_join(nodeDF,by = c("from"="id")) %>%
    mutate(fitted_flag = if_else(obs == FALSE & distr == TRUE,1,0) ) %>%
    left_join(filter(argDF,argName=="dim"), by = c("from"="rhsID"))
}

fitted <- NodeToOutput_2 %>%
  dplyr::filter(fitted_flag == 1) %>%
  select(label) %>%
  unlist()


to_calculate <- NodeToOutput %>%
  dplyr::filter(fitted_flag == 0) %>%
  select(label,rhs)

eval(rlang::parse_expr(to_calculate$rhs))

formula_words <- str_split(to_calculate$rhs, boundary("word"))[[1]]
for(i in 1:length(formula_words)){

  fitted_vector <- drawsDF[,grep(formula_words[i], colnames(drawsDF))]
  colnames(fitted_vector) <- paste0(fitted,"_fit")
}
i <- 1



DrawCol <- vector('numeric')

for(i in 1:length(fitted)){
  DrawCol <- grep(fitted[i], colnames(drawsDF))
  fitted_vector <- cbind(fitted_vector,drawsDF[,DrawCol])

}


colnames(fitted_vector) <- paste0(fitted,"_fit")

#
# loo <- dag_loo()
# print(loo)
# plot(loo)

# y_train <- parse_expr(graph$nodes_df$data[1])
# likelihood_distribution <- graph$nodes_df$rhs[1]
# data_length <- eval(parse_expr(paste0("length(",graph$nodes_df$data[1],")")))
#
# plateDimDF <-  graph$plate_index_df %>% dplyr::filter(!is.na(dataNode))
# plate_flag <- ifelse(nrow(plateDimDF)>0,1,0)
# if(plate_flag==1){
#   a_label <- parse_expr(plateDimDF$indexLabel)
#   a_dim <- parse_expr(paste0(plateDimDF$indexLabel,"_dim"))
# }
#
# if(plate_flag==1){
#   a_post <- drawsDF[,(1:eval(a_dim))]
#   a_array <- array(as.matrix(a_post),c(4000,eval(a_dim)))
# } else{
#   a_post <- drawsDF[,1]
#   a_array <-array(as.matrix(a_post),c(4000))
# }
#
#
#
# Prop_fit <- matrix(0,nrow = 4000, ncol = data_length)
# for(i in (1:data_length)) {
#   if(likelihood_distribution == "bernoulli"){
#     if(plate_flag==1){
#       Prop_fit[,i] <- dbinom(eval(y_train)[i], 1, a_array[,as.numeric(eval(a_label))[i]])
#     } else{Prop_fit[,i] <- dbinom(eval(y_train)[i], 1, a_array[,1]) }
#
#   } else if(likelihood_distribution == "normal"){
#
#     if(plate_flag==1){
#       int_fit <- drawsDF[,1:eval(a_dim)]
#       coefs_fit <- t(cbind(drawsDF[,(eval(a_dim)+1):(ncol(drawsDF)-1)]))
#       mu_formular <- graph$nodes_df$rhs[graph$nodes_df$label=='mu']
#       X <- str_split(mu_formular, boundary("word"))[[1]][2]
#       mu_fit <- int_fit[,as.numeric(eval(a_label))[i]] + t(eval(parse_expr(X)) %*% coefs_fit)
#       sd_fit <- drawsDF[,ncol(drawsDF)]
#       Prop_fit[,i] <- dnorm(eval(y_train)[i], mu_fit, sd_fit)
#
#     } else{
#       int_fit <- drawsDF[,1]
#       coefs_fit <- t(cbind(drawsDF[,2:(ncol(drawsDF)-1)]))
#       mu_formular <- graph$nodes_df$rhs[graph$nodes_df$label=='mu']
#       X <- str_split(mu_formular, boundary("word"))[[1]][2]
#       mu_fit <- int_fit + t(eval(parse_expr(X)) %*% coefs_fit)
#       sd_fit <- drawsDF[,ncol(drawsDF)]
#       Prop_fit[,i] <- dnorm(eval(y_train)[i], mu_fit[,i], sd_fit) }
#
#   } else if(likelihood_distribution == "possion"){
#
#   } else{Prop_fit[,i] <- -1}
#  }
# LLmat <- log(Prop_fit)
# rel_n_eff <- relative_eff(exp(LLmat), chain_id = rep(1:4, each = 1000))
# rel_n_eff <- ifelse(is.na(rel_n_eff),mean(rel_n_eff,na.rm = T),rel_n_eff)
# loo <- loo(LLmat, r_eff = rel_n_eff, cores = 4,save_psis = TRUE)
# loo_linear <- loo

### Example 4 linear regression2
graph = dag_create() %>%
  dag_node("Cherry Tree Height","X",
           rhs = normal(mu,sigma),
           data = trees$Height) %>%
  dag_node("Exp Height of Tree","mu",
           rhs = normal(50,24.5),
           child = 'X') %>%
  dag_node("Sd of Tree","sigma",
           rhs = uniform (0,50),
           child = 'X')
graph %>% dag_render()
graph %>% dag_greta(mcmc = TRUE)

loo <- dag_loo()
print(loo)
plot(loo)




### Example 5 Poisson regression
nycTicketsDF = ticketsDF %>%
  group_by(date) %>%
  summarize(numTickets = sum(daily_tickets)) %>%
  mutate(dayOfWeek = lubridate::wday(date, label = TRUE))

graph = dag_create() %>%
  dag_node("# of tickets","K",
           rhs = poisson(lambda),
           data = nycTicketsDF$numTickets) %>%
  dag_node("Exp Number of Tickets","lambda",
           rhs = normal(4500,2000,truncation = c(0,Inf)),
           child = "K") %>%
  dag_plate("Day of The Week","day",
            nodeLabels = "lambda",
            data = nycTicketsDF$dayOfWeek,
            addDataNode = TRUE)
graph %>% dag_render()
graph %>% dag_greta(mcmc = TRUE)

loo <- dag_loo()
print(loo)
plot(loo)

###

# y_train <- parse_expr(graph$nodes_df$data[1])
# likelihood_distribution <- graph$nodes_df$rhs[1]
# data_length <- eval(parse_expr(paste0("length(",graph$nodes_df$data[1],")")))
#
# plateDimDF <-  graph$plate_index_df %>% dplyr::filter(!is.na(dataNode))
# plate_flag <- ifelse(nrow(plateDimDF)>0,1,0)
# if(plate_flag==1){
#   a_label <- parse_expr(plateDimDF$indexLabel)
#   a_dim <- parse_expr(paste0(plateDimDF$indexLabel,"_dim"))
# }
#
# if(plate_flag==1){
#   a_post <- drawsDF[,(1:eval(a_dim))]
#   a_array <- array(as.matrix(a_post),c(4000,eval(a_dim)))
# } else{
#   a_post <- drawsDF[,1]
#   a_array <-array(as.matrix(a_post),c(4000))
# }
#
#
# Prop_fit <- matrix(0,nrow = 4000, ncol = data_length)
# for(i in (1:data_length)) {
#   if(likelihood_distribution == "bernoulli"){
#     if(plate_flag==1){
#       Prop_fit[,i] <- dbinom(eval(y_train)[i], 1, a_array[,as.numeric(eval(a_label))[i]])
#     } else{Prop_fit[,i] <- dbinom(eval(y_train)[i], 1, a_array[,1]) }
#
#   } else if(likelihood_distribution == "normal"){
#
#     if(plate_flag==1){
#       int_fit <- drawsDF[,1:eval(a_dim)]
#       coefs_fit <- t(cbind(drawsDF[,(eval(a_dim)+1):(ncol(drawsDF)-1)]))
#       mu_formular <- graph$nodes_df$rhs[graph$nodes_df$label=='mu']
#       X <- str_split(mu_formular, boundary("word"))[[1]][2]
#       mu_fit <- int_fit[,as.numeric(eval(a_label))[i]] + t(eval(parse_expr(X)) %*% coefs_fit)
#       sd_fit <- drawsDF[,ncol(drawsDF)]
#       Prop_fit[,i] <- dnorm(eval(y_train)[i], mu_fit, sd_fit)
#
#     } else{
#       int_fit <- drawsDF[,1]
#       coefs_fit <- t(cbind(drawsDF[,2:(ncol(drawsDF)-1)]))
#       mu_formular <- graph$nodes_df$rhs[graph$nodes_df$label=='mu']
#       X <- str_split(mu_formular, boundary("word"))[[1]][2]
#       mu_fit <- int_fit + t(eval(parse_expr(X)) %*% coefs_fit)
#       sd_fit <- drawsDF[,ncol(drawsDF)]
#       Prop_fit[,i] <- dnorm(eval(y_train)[i], mu_fit[,i], sd_fit) }
#
#   } else if(likelihood_distribution == "poisson"){
#     if(plate_flag==1){
#       Prop_fit[,i] <- dpois(eval(y_train)[i],exp(a_array[,as.numeric(eval(a_label))[i]]))
#     } else{
#       Prop_fit[,i] <- dpois(eval(y_train)[i],exp(a_array[,1]))
#     }
#
#   } else{Prop_fit[,i] <- -1}
# }
# LLmat <- log(Prop_fit)
# rel_n_eff <- relative_eff(exp(LLmat), chain_id = rep(1:4, each = 1000))
# rel_n_eff <- ifelse(is.na(rel_n_eff),mean(rel_n_eff,na.rm = T),rel_n_eff)
# loo <- loo(LLmat, r_eff = rel_n_eff, cores = 4,save_psis = TRUE)
