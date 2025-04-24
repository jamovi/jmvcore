
#' @method knit_print Table
#' @export
#' @importFrom knitr knit_print
knit_print.Table <- function(table, ...) {
    paste0('Table ', table$title, ' goes here')
}

#' @method knit_print Image
#' @export
#' @importFrom knitr knit_print
knit_print.Image <- function(image, ...) {
    'Image goes here'
}

#' @method knit_print Group
#' @export
#' @importFrom knitr knit_print
knit_print.Group <- function(group, ...) {
    items <- character()
    for (item in group$items) {
        if (item$visible)
            items <- c(items, knit_print(item))
    }
    paste0(items, collapse='\n')
}

#' @method knit_print Array
#' @export
#' @importFrom knitr knit_print
knit_print.Array <- knit_print.Group
