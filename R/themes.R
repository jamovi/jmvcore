getGlobalTheme <- function(name, palette) {

    ggtheme <- getGGTheme(name, palette)
    theme <- getTheme(name, palette)

    return(list(ggtheme=ggtheme, theme=theme))
}

getGGTheme <- function(name, palette) {

    if (requireNamespace('ggplot2')) {

        base_size <- 16

        if (name == 'hadley')
            ggtheme <- jmvcore::theme_hadley(base_size, palette)
        else if (name == 'minimal')
            ggtheme <- jmvcore::theme_min(base_size, palette)
        else if (name == 'iheartspss')
            ggtheme <- jmvcore::theme_spss(base_size, palette)
        else
            ggtheme <- jmvcore::theme_default(base_size, palette)

    } else {

        ggtheme <- NULL

    }

    return(ggtheme)
}


getTheme = function(name = 'default', palette = 'Dark2') {

    theme <- list()

    if (name == 'iheartspss') {

        theme[['color']] <- c('#333333', '#333333')
        theme[['fill']] <- c('#F0F0F0', '#d3ce97')
        theme[['palette']] <- palette

    } else {

        theme[['color']] <- c('#333333', NULL)
        theme[['fill']] <- c('#FFFFFF', 'grey35')
        theme[['palette']] <- palette

    }

    return(theme)
}

#' Creates the hadley jmv ggplot2 theme
#'
#' @param base_size Font size
#' @param palette Color palette name
#'
#' @return the hadley jmv ggplot2 theme
#' @export
theme_hadley <- function(base_size = 16, palette = 'Dark2') {
    theme <- list(baseTheme(base_size))

    theme <- c(theme, ggPalette(palette))

    return(theme)
}

#' Creates the default jmv ggplot2 theme
#'
#' @param base_size Font size
#' @param palette Color palette name
#'
#' @return the default jmv ggplot2 theme
#' @export
theme_default <- function(base_size = 16, palette = 'Dark2') {
    theme <- list(ggplot2::`%+replace%`(
        baseTheme(base_size),
        ggplot2::theme(
            panel.background=ggplot2::element_rect(fill='transparent', color=NA),
            axis.line = ggplot2::element_line(size = .5, colour = "#333333"),
            legend.key = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill='transparent', color=NA))))

    theme <- c(theme, ggPalette(palette))

    return(theme)
}

#' Creates the spss jmv ggplot2 theme
#'
#' @param base_size Font size
#' @param palette Color palette name
#'
#' @return the spss jmv ggplot2 theme
#' @export
theme_spss <- function(base_size = 16, palette = 'Dark2') {
    theme <- list(ggplot2::`%+replace%`(
        baseTheme(base_size),
        ggplot2::theme(
            panel.border = ggplot2::element_rect(colour="#333333", fill=NA, size=0.5),
            panel.background = ggplot2::element_rect(fill='#F0F0F0'),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill='transparent', color=NA))))

    theme <- c(theme, ggPalette(palette))

    return(theme)
}

#' Creates the minimal jmv ggplot2 theme
#'
#' @param base_size Font size
#' @param palette Color palette name
#'
#' @return the minimal jmv ggplot2 theme
#' @export
theme_min <- function(base_size = 16, palette = 'Dark2') {
    theme <- list(ggplot2::`%+replace%`(
        baseTheme(base_size),
        ggplot2::theme(
            panel.background= ggplot2::element_rect(fill='transparent', color=NA),
            axis.line = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = '#E8E8E8'),
            panel.grid.minor = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill='transparent', color=NA))))

    theme <- c(theme, ggPalette(palette))

    return(theme)
}

seqPalettes <- c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')

otherPalettes <- c('BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu',
                 'RdYlGn', 'Spectral', 'Accent', 'Dark2', 'Paired', 'Pastel1',
                 'Pastel2', 'Set1', 'Set2', 'Set3')

interp <- function(n, pal, begin=0.0, end=1.0) {

    palette <- RColorBrewer::brewer.pal(9, pal)
    rgb <- col2rgb(palette)

    or <- rgb['red',]
    og <- rgb['green',]
    ob <- rgb['blue',]

    ox <- seq(0, 1, length.out=9)
    if (n == 1)
        nx <- (end + begin) / 2
    else
        nx <- seq(begin, end, length.out=n)

    r <- approx(ox, or, nx)$y
    g <- approx(ox, og, nx)$y
    b <- approx(ox, ob, nx)$y

    rgb(r, g, b, maxColorValue=255)
}

#' A function that creates a color palette
#'
#' @param n Number of colors needed
#' @param pal Color palette name
#' @param type 'fill' or 'line'
#'
#' @return a vector of hex color codes
#' @export
colorPalette <- function(n = 5, pal = 'Dark2', type='fill') {

    # extract colors belonging to palette name
    if (pal %in% seqPalettes) {

        if (type == 'fill')
            cols <- interp(n, pal, 0.1, 0.6)
        else
            cols <- interp(n, pal, 0.4, 0.9)

    } else if (pal %in% otherPalettes) {

        cols <- suppressWarnings(RColorBrewer::brewer.pal(n, pal))

    } else if (pal == 'jmv') {
        cols <- c('#E6AC40', '#9F9F9F', '#6B9DE8')
        if (n == 2)
            cols <- cols[c(1,3)]
    } else if (pal == 'grayScale') {

        if (type == 'fill')
            cols <- grDevices::gray.colors(n, start = 0.4, end = 0.9)
        else
            cols <- grDevices::gray.colors(n, start = 0.1, end = 0.7)

    } else {
        cols <- suppressWarnings(RColorBrewer::brewer.pal(n, 'Dark2'))
    }

    # add colors if palette needs more colors
    if (n > length(cols))
        cols <- grDevices::colorRampPalette(cols)(n)

    return(cols[1:n])
}

ggPalette <- function(pal = 'Dark2') {

    fill <- function(n) colorPalette(n, pal=pal, 'fill')
    line <- function(n) colorPalette(n, pal=pal, 'line')

    return(
        list(ggplot2::discrete_scale("fill",   "jmv", fill),
             ggplot2::discrete_scale("colour", "jmv", line))
    )
}

#' The basis for all jmv ggplot2 themes
#'
#' @return the basic jmv ggplot2 theme
baseTheme <- function(base_size = 16) {
    ggplot2::`%+replace%`(
        ggplot2::theme_gray(base_size = base_size),
        ggplot2::theme(
            plot.background = ggplot2::element_rect(fill='transparent', color=NA),
            panel.background = ggplot2::element_rect(fill='#E8E8E8', color=NA),
            plot.margin = ggplot2::margin(15, 15, 15, 15),
            axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, 0, 0, 0), colour='#333333'),
            axis.text.y = ggplot2::element_text(margin=ggplot2::margin(0, 5, 0, 0), colour='#333333'),
            axis.title.x = ggplot2::element_text(margin=ggplot2::margin(10, 0, 0, 0), colour='#333333'),
            axis.title.y = ggplot2::element_text(margin=ggplot2::margin(0, 10, 0, 0), colour='#333333', angle = 90),
            plot.title = ggplot2::element_text(margin=ggplot2::margin(0, 0, 15, 0), colour='#333333'),
            legend.background = ggplot2::element_rect("transparent", color=NA),
            legend.key = ggplot2::element_rect(fill='#E8E8E8', color=NA),
            legend.title = ggplot2::element_text(colour='#333333'),
            legend.text = ggplot2::element_text(colour='#333333'),
            strip.text.x = ggplot2::element_text(colour='#333333'),
            strip.text.y = ggplot2::element_text(colour='#333333')))
}
