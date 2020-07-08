 # Create an empty graph and add 2 nodes by using
 # the `dag_node()` function twice
library(causact)
library(greta)
 # The Eight Schools Example from Gelman et al.:

 schools_dat <- data.frame(y = c(28,  8, -3,  7, -1,  1, 18, 12),
 sigma = c(15, 10, 16, 11,  9, 11, 10, 18), schoolName = paste0("School",1:8))

 graph = dag_create() %>%
   dag_node("Treatment Effect","y",
            rhs = normal(theta, sigma),
            data = schoolsDF$y) %>%
   dag_node("Std Error of Effect Estimates","sigma",
            data = schoolsDF$sigma,
            child = "y") %>%
   dag_node("Exp. Treatment Effect","theta",
            child = "y",
            rhs = avgEffect + schoolEffect) %>%
   dag_node("Pop Treatment Effect","avgEffect",
            child = "theta",
            rhs = normal(0,30)) %>%
   dag_node("School Level Effects","schoolEffect",
            rhs = normal(0,30),
            child = "theta") %>%
   dag_plate("Observation","i",nodeLabels = c("sigma","y","theta")) %>%
   dag_plate("School Name","school",
             nodeLabels = "schoolEffect",
             data = schoolsDF$schoolName,
             addDataNode = TRUE)
 graph %>% dag_render()
 drawsDF = graph %>% dag_greta()
 drawsDF %>% dagp_plot()
