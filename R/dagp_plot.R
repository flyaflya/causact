#' @import ggplot2 tidyr
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom cowplot plot_grid
#' @export

dagp_plot = function(drawsDataFrame,alphaSort = FALSE) { # case where untidy posterior draws are provided
  if (!("priorGroup" %in% colnames(drawsDataFrame))) {
    plot = drawsDataFrame %>% gather() %>%
      ggplot(aes(x = value, y = ..scaled..)) +
      geom_density(aes(fill = key)) +
      facet_wrap( ~ key, scales = "free_x") +
      theme_minimal() +
      theme(legend.position = "none")

    plot
  } else { # case where tidy posterior draws are provided
    plotList = list()
    ## filter out NA's like from LKJ prior (we do not know how to plot this)
    drawsDataFrame = drawsDataFrame %>% dplyr::filter(!is.na(priorGroup))
    priorGroups = unique(drawsDataFrame$priorGroup)
    numPriorGroups = length(priorGroups)
    for (i in 1:numPriorGroups) {
      df = drawsDataFrame %>% filter(priorGroup == priorGroups[i])
      # df$key = factor(df$key)
      # ## reorder levels using character sort from stringr
      # if (alphaSort == FALSE) {
      #   df$key = forcats::fct_reorder(df$key,df$value)
      # } else {
      #   df$key = forcats::fct_relevel(df$key,
      #                                 stringr::str_sort(levels(df$key),
      #                                                   numeric = TRUE,
      #                                                   decreasing = TRUE))
      # }

      plotList[[i]] = df %>% group_by(key) %>%
        summarize(q05 = quantile(value,0.05),
                  q25 = quantile(value,0.55),
                  q45 = quantile(value,0.45),
                  q50 = quantile(value,0.50),
                  q55 = quantile(value,0.55),
                  q75 = quantile(value,0.75),
                  q95 = quantile(value,0.95)) %>%
        mutate(credIQR = q75 - q25) %>%
        mutate(reasonableIntervalWidth = 1.5 * quantile(credIQR,0.75)) %>%
        mutate(alphaLevel = ifelse(credIQR > reasonableIntervalWidth, 0.3,1)) %>%
        arrange(alphaLevel,q50) %>%
        mutate(key = factor(key, levels = key)) %>%
        ggplot(aes(y = key, yend = key)) +
        geom_segment(aes(x = q05, xend = q95, alpha = alphaLevel), size = 4, color = "#5f9ea0") +
        geom_segment(aes(x = q45, xend = q55, alpha = alphaLevel), size = 4, color = "#11114e") +
        scale_alpha_continuous(range = c(0.6,1))  +
        guides(alpha = FALSE) +
        theme_minimal(12) +
        labs(y = element_blank(),
             x = "parameter value",
             caption = ifelse(i == numPriorGroups,"90% (light blue) & 10% (dark blue) Credible Intervals - Transparent fill used for high uncertainty intervals.",""))

    }

    nCol <- floor(1 + sqrt(numPriorGroups))
    cowplot::plot_grid(plotlist = plotList, ncol = nCol)
  }
}
