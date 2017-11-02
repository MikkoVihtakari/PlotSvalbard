##' @title A ggplot2 theme for maps
##' @param ... additional arguments passed to \code{\link[ggplot2]{theme}} function
##' @import ggplot2
##' @export

theme_map <- function(...) {
    theme_bw(...) %+replace%
      theme(panel.background = element_blank(),
      panel.border = element_rect(fill = NA, colour = "black", size = 0.2),
      panel.grid = element_blank(), plot.background = element_blank(),
      axis.ticks.x = element_line(colour = "black", size = 0.2),
      axis.ticks.y = element_line(colour = "black", size = 0.2),
      axis.line.x = element_blank(), axis.line.y = element_blank()
      )
  }
