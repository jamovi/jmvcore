
Cell.BEGIN_GROUP <- 1
Cell.END_GROUP   <- 2
Cell.BEGIN_END_GROUP <- 3
Cell.NEGATIVE <- 4

Cell <- R6::R6Class(
    "Cell",
    active=list(
        isEmpty=function() {
            is.null(self$value) || is.na(self$value)
        }),
    public=list(
        value=NA,
        footnotes=character(),
        sups=integer(),
        symbols=character(),
        format=0,
        initialize=function(v=NA) {
            self$value <- v
        },
        setValue=function(v) {
            self$value <- v
            self$footnotes <- character()
            self$symbols <- character()
        },
        addFootnote=function(note) {
            self$footnotes <- c(self$footnotes, note)
        },
        addFormat=function(format) {
            self$format <- base::bitwOr(self$format, format)
        },
        addSymbol=function(symbol) {
            self$symbols <- c(self$symbols, symbol)
        },
        fromProtoBuf=function(cellPB) {
            if ( ! base::inherits(cellPB, "Message"))
                reject("Cell$fromProtoBuf(): expects a jmvcoms.ResultsCell")
            
            if (cellPB$has('i')) {
                self$value <- cellPB$i
            } else if (cellPB$has('d')) {
                self$value <- cellPB$d
            } else if (cellPB$has('s')) {
                self$value <- cellPB$s
            } else if (cellPB$has('o')) {
                if (cellPB$o == jmvcoms.ResultsCell.Other$MISSING)
                    self$value <- NA
                else
                    self$value <- NaN
            }
            
            self$footnotes <- cellPB$footnotes
            self$symbols <- cellPB$symbols
        },
        asProtoBuf=function() {
            initProtoBuf()
            cell <- RProtoBuf::new(jmvcoms.ResultsCell)
            
            vc <- class(self$value)
            
            if (vc == "integer") {
                if (is.na(self$value))
                    cell$o <- jmvcoms.ResultsCell.Other$MISSING
                else
                    cell$i <- self$value
            } else if (vc == "numeric")
                cell$d <- self$value
            else if (vc == "character")
                cell$s <- self$value
            else if (vc == "logical" && is.nan(vc))
                cell$o <- jmvcoms.ResultsCell.Other$NOT_A_NUMBER
            else
                cell$o <- jmvcoms.ResultsCell.Other$MISSING
            
            cell$footnotes <- self$footnotes
            cell$symbols   <- self$symbols
            cell$format    <- self$format
            
            cell
        }))