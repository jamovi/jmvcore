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
            if (requireNamespace('ggplot2'))
                ggtheme <- ggplot2::theme(
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
                        legend.key = ggplot2::element_rect(fill='#E8E8E8'),
                        legend.title = ggplot2::element_text(colour='#333333'),
                        legend.text = ggplot2::element_text(colour='#333333'),
                        strip.text.x = ggplot2::element_text(colour='#333333'),
                        strip.text.y = ggplot2::element_text(colour='#333333'))
            else
                ggtheme <- NULL

            theme <- private$.getTheme('hadley')

            t <- list('ggtheme'=ggtheme, 'theme'=theme)

            return(t)
        },
        default = function() {
            if (requireNamespace('ggplot2'))
                ggtheme <- self$hadley$ggtheme + ggplot2::theme(
                        panel.background=ggplot2::element_rect(fill='transparent', color=NA),
                        axis.line = ggplot2::element_line(size = .5, colour = "#333333"),
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        legend.key = ggplot2::element_blank(),
                        strip.background = ggplot2::element_rect(fill='transparent', color=NA))
            else
                ggtheme <- NULL

            theme <- private$.getTheme('default')

            t <- list('ggtheme'=ggtheme, 'theme'=theme)

            return(t)
        },
        iheartspss = function() {
            if (requireNamespace('ggplot2'))
                ggtheme <- self$hadley$ggtheme + ggplot2::theme(
                        panel.border = ggplot2::element_rect(colour="#333333", fill=NA, size=0.5),
                        panel.background = ggplot2::element_rect(fill='#F0F0F0'),
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        legend.key = ggplot2::element_blank(),
                        strip.background = ggplot2::element_rect(fill='transparent', color=NA))
            else
                ggtheme <- NULL

            theme <- private$.getTheme('iheartspss')

            t <- list('ggtheme'=ggtheme, 'theme'=theme)

            return(t)
        },
        minimal = function() {
            if (requireNamespace('ggplot2'))
                ggtheme <- self$hadley$ggtheme + ggplot2::theme(
                        panel.background= ggplot2::element_rect(fill='transparent', color=NA),
                        axis.line = ggplot2::element_blank(),
                        panel.grid = ggplot2::element_blank(),
                        panel.grid.major = ggplot2::element_line(colour = '#E8E8E8'),
                        panel.grid.minor = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank(),
                        legend.key = ggplot2::element_blank(),
                        strip.background = ggplot2::element_rect(fill='transparent', color=NA))
            else
                ggtheme <- NULL

            theme <- private$.getTheme('minimal')

            t <- list('ggtheme'=ggtheme, 'theme'=theme)

            return(t)
        },
        liberace = function() {

            if (requireNamespace('ggplot2'))
                ggtheme <- self$hadley$ggtheme + ggplot2::theme(
                        panel.background=ggplot2::element_rect(fill='#FAFAD2'),
                        plot.background = ggplot2::element_rect(fill='#EEE8AA'),
                        axis.ticks = ggplot2::element_line(size = .5, colour = "#C5B358"),
                        panel.grid.major = ggplot2::element_line(colour = '#C5B358'),
                        panel.grid.minor = ggplot2::element_line(colour = '#C5B358'),
                        panel.border = ggplot2::element_rect(colour="#C5B358", fill=NA, size=2),
                        axis.title=ggplot2::element_text(color='#996515'),
                        axis.text=ggplot2::element_text(color='#996515'),
                        legend.key = ggplot2::element_blank(),
                        legend.title = ggplot2::element_text(colour='#996515'),
                        legend.text = ggplot2::element_text(colour='#996515'),
                        strip.text.x = ggplot2::element_text(colour='#996515'),
                        strip.text.y = ggplot2::element_text(colour='#996515'),
                        strip.background = ggplot2::element_rect(fill='transparent', color=NA))
            else
                ggtheme <- NULL

            theme <- private$.getTheme('liberace')

            t <- list('ggtheme'=ggtheme, 'theme'=theme)

            return(t)
        }
    ),
    private = list(
        .getTheme = function(name = 'default') {

            theme <- list()

            if (name == 'default' || name == 'minimal' || name == 'hadley') {

                theme[['color']] <- c('#333333', 'grey35')
                theme[['fill']] <- c('#FFFFFF', 'grey35')

            } else if (name == 'iheartspss') {

                theme[['color']] <- c('#333333', '#333333')
                theme[['fill']] <- c('#F0F0F0', '#d3ce97')

            } else if (name == 'liberace') {

                theme[['color']] <- c('#996515', '#996515')
                theme[['fill']] <- c('#EEE8AA', '#996515')

            }

            return(theme)
        }
    ))$new()
