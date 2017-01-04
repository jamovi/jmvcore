
#' @rdname Analysis
#' @export
Preformatted <- R6::R6Class("Preformatted",
    inherit=ResultsElement,
    private=list(
        .content=''),
    active=list(
        content=function(value) {
            if (base::missing(value))
                return(private$.content)
            private$.content <- value
            private$.stale <- FALSE
            base::invisible(self)
        }
    ),
    public=list(
        initialize=function(
            options,
            name='',
            title='',
            visible=TRUE,
            clearWith='*') {

            super$initialize(
                options=options,
                name=name,
                title=title,
                visible=visible,
                clearWith=clearWith)
        },
        asString=function() {
            private$.content
        },
        fromProtoBuf=function(element, oChanges=NULL, vChanges=NULL) {
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

            private$.content <- element$preformatted
        },
        asProtoBuf=function(incAsText=FALSE, status=NULL) {
            initProtoBuf()
            element <- super$asProtoBuf(incAsText=TRUE, status=status)
            element$preformatted <- private$.content
            element
        }
    )
)

