# Installation

```
# install tidyverse
install.packages("tidyverse")
#install ggplot2
install.packages("ggplot2")
#install greta
install.packages("greta")
#install diagrammeR
install.packages("DiagrammeR")
# install causact
install.packages("remotes")
remotes::install_github("flyaflya/causact")
```

# Examples

```
#greta examples
graph = dag_create() %>%
  dag_node("Sepal Length","y",
           data = iris$Petal.Length,
           rhs = normal) %>%
  dag_node("Exp Sepal Length (Obs.)","mean",
           rhs = int + coef * x,
           child = "y") %>%
  dag_node("Exp Sepal Length (Pop.)","int",
           rhs = normal(0,5),
           child = "mean") %>%
  dag_node("Sepal / Petal Slope","coef",
           rhs = normal(0,3),
           child = "mean") %>%
  dag_node("Std Dev of Obs. Sepal Length","sd",
           rhs = lognormal(0,3),
           child = "y") %>%
  dag_node("Petal Length","x",
           child = "mean",
           data = iris$Petal.Length) %>%
  dag_plate("Observation","i",
           nodeLabels = c("y","mean","x"))
graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
tidyDrawsDF %>% dagp_plot()

```

```
#rethinking example

library(rethinking)
library(greta)
library(tidyverse)

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
```


