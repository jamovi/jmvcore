
#' @rdname Analysis
#' @export
Output <- R6::R6Class('Output',
    inherit=ResultsElement,
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
                outputsPB <- try(private$.dataToPB(private$.values))
                if ( ! inherits(outputsPB, 'try-error'))
                    element$outputs <- outputsPB
            }
            element
        }
    ),
    private=list(
        .values=NA,
        .dataToPB=function(data) {
            df <- as.data.frame(data)
            df <- df[,1,drop=FALSE]
            private$.dfToPB(df)
        },
        .dfToPB=function(df) {

            rowNos <- suppressWarnings(as.integer(row.names(df)))
            notNAs <- ! is.na(rowNos)
            rowNos <- rowNos[notNAs]
            df <- df[notNAs,, drop=FALSE]

            outputsPB <- RProtoBuf_new(jamovi.coms.ResultsOutputs)

            for (column in df) {

                outputPB <- RProtoBuf_new(jamovi.coms.ResultsOutput)

                if (is.character(column))
                    column <- as.factor(column)

                if (is.integer(column)) {
                    column <- ifelse(is.na(column), -2147483648, column)
                    outputPB$i <- column
                } else if (is.numeric(column)) {
                    outputPB$d <- column
                } else if (is.factor(column)) {
                    outputPB$i <- as.numeric(column)
                    lvls <- levels(column)
                    for (i in seq_along(lvls)) {
                        levelPB <- RProtoBuf_new(jamovi.coms.VariableLevel)
                        levelPB$label <- lvls[i]
                        levelPB$value <- i
                        outputPB$add('levels', levelPB)
                    }
                } else {
                    # warn the developer?
                }

                outputsPB$add('outputs', outputPB)
            }

            outputsPB$rowNos <- rowNos
            outputsPB
        }
    )
)

#' @rdname Analysis
#' @export
Outputs <- R6::R6Class('Outputs',
    inherit=Output,
    private=list(
        .dataToPB=function(data) {
            df <- as.data.frame(data)
            private$.dfToPB(df)
        }
    )
)
