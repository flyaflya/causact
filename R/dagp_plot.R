#' @import ggplot2 tidyr gridExtra ggridges
#' @export

dagp_plot = function(drawsDataFrame) {

  if(!("priorGroup" %in% colnames(drawsDataFrame))) {
    plot = drawsDataFrame %>% gather() %>%
    ggplot(aes(x = value, y = ..scaled..)) +
    geom_density(aes(fill = key)) +
    facet_wrap(~ key, scales = "free_x") +
    theme_minimal() +
    theme(legend.position="none")

    plot
    return()
  }

  plotList = list()
  priorGroups = unique(drawsDataFrame$priorGroup)
  numPriorGroups = length(priorGroups)
  for (i in 1:numPriorGroups) {
    df = drawsDataFrame %>% filter(priorGroup == priorGroups[i])

    plotList[[i]] = df %>% ggplot(aes(x = value, y = key, fill = key)) +
      ggridges::geom_density_ridges(scale = 2, show.legend = FALSE) +
        scale_y_discrete(expand = c(0.01, 0)) +
        scale_x_continuous(expand = c(0.01, 0)) +
      ggridges::theme_ridges() + theme(axis.title = element_blank())

  }

  nCol <- floor(1 + sqrt(numPriorGroups))
  cowplot::plot_grid(plotlist = plotList, ncol=nCol)

}
