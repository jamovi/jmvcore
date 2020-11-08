
#' @rdname Analysis
#' @export
Output <- R6::R6Class('Output',
    inherit=ResultsElement,
    private=list(
        .values=NA),
    active=list(
        values=function(v) {
            private$.values
        }
    ),
    public=list(
        initialize=function(
            options,
            name='',
            title='',
            visible=TRUE,
            clearWith='*',
            refs=character()) {

            super$initialize(
                options=options,
                name=name,
                title=title,
                visible=visible,
                clearWith=clearWith,
                refs=refs)

            private$.values <- NULL
        },
        setValues=function(v) {
            private$.values <- v
            private$.stale <- FALSE
            base::invisible(self)
        },
        isFilled=function() {
            if (private$.stale)
                return(FALSE)
            if (is.null(private$.values))
                return(FALSE)
            return(TRUE)
        },
        asString=function() {
            ''
        },
        fromProtoBuf=function(element, oChanges, vChanges) {
            if ( ! base::inherits(element, "Message"))
                reject("Table$fromProtoBuf() expects a jamovi.coms.ResultsElement")

            private$.stale <- element$stale

            someChanges <- length(oChanges) > 0 || length(vChanges) > 0
            if (someChanges && base::identical('*', private$.clearWith)) {
                private$.stale <- TRUE
            } else if (base::any(oChanges %in% private$.clearWith)) {
                private$.stale <- TRUE
            } else {
                for (clearName in private$.clearWith) {
                    if (base::any(vChanges %in% private$.options$option(clearName)$vars)) {
                        private$.stale <- TRUE
                        break()
                    }
                }
            }

            super$fromProtoBuf(element, oChanges, vChanges)
        },
        asProtoBuf=function(incAsText=FALSE, status=NULL) {
            element <- super$asProtoBuf(incAsText=incAsText, status=status)
            if ( ! is.null(private$.values) && incAsText) {

                values <- private$.values
                outputPB <- element$output

                if (is.character(values))
                    values <- as.factor(values)

                if (is.integer(values)) {
                    outputPB$i <- values
                } else if (is.numeric(private$.values)) {
                    outputPB$d <- values
                } else if (is.factor(values)) {
                    outputPB$i <- as.numeric(values)
                    lvls <- levels(values)
                    for (i in seq_along(lvls)) {
                        levelPB <- RProtoBuf_new(jamovi.coms.VariableLevel)
                        levelPB$label <- lvls[i]
                        levelPB$value <- i
                        outputPB$add('levels', levelPB)
                    }
                } else {
                    # warn the developer?
                }

                element$output <- outputPB
            }
            element
        }
    )
)

#' @rdname Analysis
#' @export
Outputs <- R6::R6Class('Outputs',
    inherit=Output,
    public=list(
        asProtoBuf=function(incAsText=FALSE, status=NULL) {
            element <- super$asProtoBuf(incAsText=incAsText, status=status)
            if ( ! is.null(private$.values) && incAsText) {

                output <- Output$new()
                outputsPB <- element$outputs

                for (var in private$.values) {
                    output$setValue(var)
                    outputPB <- output$asProtoBuf(incAsText, status)
                    outputsPB$add('outputs', outputPB)
                }

                element$outputs <- outputsPB
            }
            element
        }
    )
)
