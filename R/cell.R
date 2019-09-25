
#' Constants to specify formatting of Table cells
#'
#' Cell.BEGIN_GROUP adds spacing above a cell
#'
#' Cell.END_GROUP add spacing below a cell
#'
#' Cell.BEGIN_END_GROUP add spacing above and below a cell
#'
#' Cell.NEGATIVE specifies that the cells contents is negative
#'
#' @examples
#' \dontrun{
#'
#' table$addFormat(rowNo=1, col=1, Cell.BEGIN_END_GROUP)
#' }
#'
#' @export
Cell.BEGIN_GROUP <- 1

#' @rdname Cell.BEGIN_GROUP
#' @export
Cell.END_GROUP   <- 2

#' @rdname Cell.BEGIN_GROUP
#' @export
Cell.BEGIN_END_GROUP <- 3

#' @rdname Cell.BEGIN_GROUP
#' @export
Cell.NEGATIVE <- 4

#' @rdname Cell.BEGIN_GROUP
#' @export
Cell.INDENTED <- 8

Cell <- R6::R6Class(
    "Cell",
    cloneable = FALSE,
    private=list(
        .value=NA,
        .footnotes=character(),
        .sups=integer(),
        .symbols=character(),
        .format=0,
        .sortKey=0),
    active=list(
        value=function(x) { if (missing(x)) { return(private$.value) } else { private$.value <- x } },
        sups=function(x) { if (missing(x)) { return(private$.sups) } else { private$.sups <- x } },
        sortKey=function(x) { if (missing(x)) { return(private$.sortKey) } else { private$.sortKey <- x } }
    ),
    public=list(
        initialize=function(v=NA) {
            private$.value <- v
        },
        setValue=function(v) {
            private$.value <- v
            private$.footnotes <- character()
            private$.symbols <- character()
        },
        isNotFilled=function() {
            v <- private$.value
            if (is.null(v))
                return(TRUE)
            if (is.nan(v))
                return(FALSE)
            if (is.na(v))
                return(TRUE)
            return(FALSE)
        },
        isFilled=function() {
            ! self$isNotFilled()
        },
        addFootnote=function(note) {
            private$.footnotes <- c(private$.footnotes, note)
        },
        addFormat=function(format) {
            private$.format <- bitwOr(private$.format, format)
        },
        addSymbol=function(symbol) {
            private$.symbols <- c(private$.symbols, symbol)
        },
        fromProtoBuf=function(cellPB) {

            if (cellPB$has('i')) {
                private$.value <- cellPB$i
            } else if (cellPB$has('d')) {
                private$.value <- cellPB$d
            } else if (cellPB$has('s')) {
                v <- cellPB$s
                Encoding(v) <- 'UTF-8'
                private$.value <- v
            } else if (cellPB$has('o')) {
                if (cellPB$o == jamovi.coms.ResultsCell.Other$MISSING)
                    private$.value <- NA
                else
                    private$.value <- NaN
            }

            private$.footnotes <- cellPB$footnotes
            private$.symbols <- cellPB$symbols
            private$.sortKey <- cellPB$sortKey
        },
        asProtoBuf=function() {

            cell <- RProtoBuf_new(jamovi.coms.ResultsCell,
                        footnotes = private$.footnotes,
                        symbols = private$.symbols,
                        format = private$.format,
                        sortKey = private$.sortKey)

            v <- private$.value

            if (length(v) != 1) {
                cell$o <- jamovi.coms.ResultsCell.Other$MISSING
            }
            else if (inherits(v, "numeric")) {
                cell$d <- v
            }
            else if (is.na(v)) {
                if (is.nan(v))
                    cell$d <- NaN
                else
                    cell$o <- jamovi.coms.ResultsCell.Other$MISSING
            }
            else if (inherits(v, "integer")) {
                cell$i <- v
            }
            else if (inherits(v, "character")) {
                cell$s <- v
            }
            else {
                cell$o <- jamovi.coms.ResultsCell.Other$NOT_A_NUMBER
            }

            cell
        }))
