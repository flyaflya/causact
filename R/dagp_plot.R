#' @import ggplot2 tidyr
#' @export

dagp_plot = function(drawsDF) {
  drawsDF %>% tidyr::gather() %>%
    ggplot(aes(x = value, y = ..scaled..)) +
    geom_density(aes(fill = key)) +
    facet_wrap(~key, scales = "free_x") + theme_minimal()
}
