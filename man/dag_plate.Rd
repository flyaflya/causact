% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dag_plate.R
\name{dag_plate}
\alias{dag_plate}
\title{Create a plate representation for repeated nodes.}
\usage{
dag_plate(
  graph,
  descr,
  label,
  nodeLabels,
  data = as.character(NA),
  addDataNode = FALSE,
  rhs = NA
)
}
\arguments{
\item{graph}{a graph object of class \code{dgr_graph} created using \code{dag_create()}.}

\item{descr}{a longer more descriptive label for the cluster/plate.}

\item{label}{a short character string to use as an index.  Any \code{.} in the names is automatically replaced by \verb{_} for interoperability with Python.}

\item{nodeLabels}{a character vector of node labels or descriptions to include in the list of nodes.}

\item{data}{a vector representing the categorical data whose unique values become the plate index.  To use with \code{addDataNode = TRUE}, this vector should represent observations of a variable that can be coerced to a factor.}

\item{addDataNode}{a logical value.  When \code{addDataNode = TRUE}, the code attempts to add a node of observed data that is used as an index for extracting the correct parameter from parent nodes that are on the newly created plate.  Verify the graphical model using \code{dag_render()} to ensure correct behavior.}

\item{rhs}{Optional \code{rhs} expression for when \code{addDataNode = TRUE}.  This can be either a  distribution such as \verb{uniform, normal, lognormal, bernoulli,} etc. or an R expression. Distribution arguments are optional.  Valid values include \code{normal(mu,sigma)}, \code{normal}, and \code{normal(6,2)}.  R computation/expression examples include \code{alpha+beta*x}.  If a distribution is given, this is a random/stochastic node, if a formula is given it is a deterministic node once given the values of its parents.  Quotes should not be used as all function/computations should consist of R objects, functions, and constants.}
}
\value{
an expansion of the input \code{causact_graph} object with an added plate representing the repetition of \code{nodeLabels} for each unique value of \code{data}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Given a graph object of class \code{causact_graph}, create collections of nodes that should be repeated i.e. represent multiple instances of a random variable, random vector, or random matrix.  When nodes are on more than one plate, graph rendering will treat each unique combination of plates as separate plates.
}
\examples{
# single plate example
graph = dag_create() \%>\%
dag_node("Get Card","y",
         rhs = bernoulli(theta),
         data = carModelDF$getCard) \%>\%
  dag_node(descr = "Card Probability by Car",label = "theta",
           rhs = beta(2,2),
           child = "y") \%>\%
  dag_node("Car Model","x",
           data = carModelDF$carModel,
           child = "y") \%>\%
  dag_plate("Car Model","x",
            data = carModelDF$carModel,
            nodeLabels = "theta")
graph \%>\% dag_render()

# multiple plate example
library(dplyr)
poolTimeGymDF = gymDF \%>\%
mutate(stretchType = ifelse(yogaStretch == 1,
                            "Yoga Stretch",
                            "Traditional")) \%>\%
group_by(gymID,stretchType,yogaStretch) \%>\%
  summarize(nTrialCustomers = sum(nTrialCustomers),
            nSigned = sum(nSigned))
graph = dag_create() \%>\%
  dag_node("Cust Signed","k",
           rhs = binomial(n,p),
           data = poolTimeGymDF$nSigned) \%>\%
  dag_node("Probability of Signing","p",
           rhs = beta(2,2),
           child = "k") \%>\%
  dag_node("Trial Size","n",
           data = poolTimeGymDF$nTrialCustomers,
           child = "k") \%>\%
  dag_plate("Yoga Stretch","x",
            nodeLabels = c("p"),
            data = poolTimeGymDF$stretchType,
            addDataNode = TRUE) \%>\%
  dag_plate("Observation","i",
            nodeLabels = c("x","k","n")) \%>\%
  dag_plate("Gym","j",
            nodeLabels = "p",
            data = poolTimeGymDF$gymID,
            addDataNode = TRUE)
graph \%>\% dag_render()
}
