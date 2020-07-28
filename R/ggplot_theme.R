#' @importFrom ggplot2 theme
NULL

#' lh2.fill
#' 
#' default color for documents/decks
#' 
#' @export
lh2.fill <- function(){
  '#008B8B'
}

#' shap.pdp.theme
#'
#' a minimal ggplot2 theme for shap PDP plots
#'
#' @examples
#' shap.pdp.theme()
#' @export
shap.pdp.theme <- function(){
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    #plot.title = element_text(color = 'black', size = 12, face = 'bold'),
    axis.title.x = element_text(color = 'black', size = 11, face = 'bold'),
    axis.title.y = element_text(color = 'black', size = 11, face = 'bold'),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = 'right'
  )
}

#' h2o.shap.theme
#'
#' a minimal ggplot2 theme for shap plots
#'
#' @examples
#' h2o.shap.theme()
#' @export
h2o.shap.theme <- function(){
  theme(
    legend.key = element_rect(size = 3),
    legend.title = element_text(angle = -90),
    legend.text = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.background = element_rect(fill = 'white'),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank())
}

#' lh2.theme
#'
#' LH2 RAC ggplot theme
#'
#' @examples
#' lh2.theme()
#' @export
lh2.theme <- function(){
  theme(
    panel.background = element_rect(fill = "white", colour = "grey",size = 1, linetype = "solid"),
    plot.title = element_text(color = 'black', size = 12, face = 'bold'),
    axis.title.x = element_text(color = 'black', size = 11, face = 'bold'),
    axis.title.y = element_text(color = 'black', size = 11, face = 'bold'),
    axis.text.x = element_text(size = 12,angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    panel.grid.minor = element_line(size = (0.2), colour="grey"),
    panel.grid.major = element_line(size = (0.2), colour="grey"),
    legend.position = 'right')
}
