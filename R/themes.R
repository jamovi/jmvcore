#' The jamovi plot themes
#'
#' This contains the following plot themes:
#' `themes$default` is the default theme
#' `themes$hadley` is a theme based on the default ggplot2 theme
#' `themes$iheartspss` is a theme based on the default SPSS theme
#' `themes$minimal` is a minimal theme
#' `themes$liberace` is a gold theme
#' @export
themes <- R6::R6Class(
    active=list(
        hadley = function() {
            ggplot2::theme(
                text = ggplot2::element_text(size=16, colour='#333333'),
                plot.background = ggplot2::element_rect(fill='transparent', color=NA),
                panel.background = ggplot2::element_rect(fill='#E8E8E8'),
                plot.margin = ggplot2::margin(15, 15, 15, 15),
                axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, 0, 0, 0)),
                axis.text.y = ggplot2::element_text(margin=ggplot2::margin(0, 5, 0, 0)),
                axis.title.x = ggplot2::element_text(margin=ggplot2::margin(10, 0, 0, 0)),
                axis.title.y = ggplot2::element_text(margin=ggplot2::margin(0, 10, 0, 0)),
                plot.title = ggplot2::element_text(margin=ggplot2::margin(0, 0, 15, 0)),
                legend.background = ggplot2::element_rect("transparent"),
                legend.key = ggplot2::element_rect(fill='#E8E8E8'))
        },
        default = function() {
            self$hadley + ggplot2::theme(
                panel.background=ggplot2::element_rect(fill='transparent', color=NA),
                axis.line = ggplot2::element_line(size = .5, colour = "#333333"),
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                legend.key = ggplot2::element_blank())
        },
        iheartspss = function() {
            self$hadley + ggplot2::theme(
                panel.border = ggplot2::element_rect(colour="#333333", fill=NA, size=0.5),
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                legend.key = ggplot2::element_blank())
        },
        minimal = function() {
            self$hadley + ggplot2::theme(
                panel.background= ggplot2::element_rect(fill='transparent', color=NA),
                axis.line = ggplot2::element_blank(),
                panel.grid = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line(colour = '#E8E8E8'),
                panel.grid.minor = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_blank(),
                legend.key = ggplot2::element_blank())
        },
        liberace = function() {
            self$hadley + ggplot2::theme(
                panel.background=ggplot2::element_rect(fill='#FAFAD2'),
                plot.background = ggplot2::element_rect(fill='#EEE8AA'),
                axis.ticks = ggplot2::element_line(size = .5, colour = "#C5B358"),
                panel.grid.major = ggplot2::element_line(colour = '#996515'),
                panel.grid.minor = ggplot2::element_line(colour = '#C5B358'),
                panel.border = ggplot2::element_rect(colour="#C5B358", fill=NA, size=2),
                axis.title=ggplot2::element_text(color='#996515'),
                axis.text=ggplot2::element_text(color='#996515'),
                legend.key = ggplot2::element_blank())
        }
    ))$new()
