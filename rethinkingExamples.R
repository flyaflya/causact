# reThinking Examples
# Multi-levle Chimpanzees Winter 2019 Lectrue 16 minute 9:00
# https://youtu.be/ZG3Oe35R5sY

library(rethinking)
#library(causact)
library(greta)
library(tidyverse)
library(causact)

data("chimpanzees")
chimpanzeesDF = chimpanzees %>%
  mutate(treatment = dplyr::case_when(
    prosoc_left == 0 & condition == 0 ~"Food Right - No Partner",
    prosoc_left == 1 & condition == 0 ~"Food Left - No Partner",
    prosoc_left == 0 & condition == 1 ~"Food Right - With Partner",
    prosoc_left == 1 & condition == 1 ~"Food Left - With Partner",
    TRUE ~ "Unknown"))

graph = dag_create() %>%
  dag_node("Pull Left Handle","L",
           rhs = bernoulli(p),
           data = chimpanzeesDF$pulled_left) %>%
  dag_node("Probability of Pull", "p",
           rhs = ilogit(alpha + gamma + beta),
           child = "L") %>%
  dag_node("Actor Intercept","alpha",
           rhs = normal(alphaBar, sigma_alpha),
           child = "p") %>%
  dag_node("Block Intercept","gamma",
           rhs = normal(0,sigma_gamma),
           child = "p") %>%
  dag_node("Treatment Intercept","beta",
           rhs = normal(0,0.5),
           child = "p") %>%
  dag_node("Actor Population Intercept","alphaBar",
           rhs = normal(0,1.5),
           child = "alpha") %>%
  dag_node("Actor Variation","sigma_alpha",
           rhs = exponential(1),
           child = "alpha") %>%
  dag_node("Block Variation","sigma_gamma",
           rhs = exponential(1),
           child = "gamma") %>%
  dag_plate("Observation","i",
            nodeLabels = c("L","p")) %>%
  dag_plate("Actor","act",
            nodeLabels = c("alpha"),
            data = chimpanzeesDF$actor,
            addDataNode = TRUE) %>%
  dag_plate("Block","blk",
            nodeLabels = c("gamma"),
            data = chimpanzeesDF$block,
            addDataNode = TRUE) %>%
  dag_plate("Treatment","trtmt",
            nodeLabels = c("beta"),
            data = chimpanzeesDF$treatment,
            addDataNode = TRUE)
graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
tidyDrawsDF %>% dagp_plot()
