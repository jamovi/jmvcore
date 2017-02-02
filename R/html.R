
#' @rdname Analysis
#' @importFrom knitr knit_print
#' @export
Html <- R6::R6Class("Html",
    inherit=ResultsElement,
    private=list(
        .scripts=character(),
        .stylesheets=character(),
        .content=''),
    active=list(
        scripts=function(value) {
            if (base::missing(value))
                return(private$.scripts)
            if ( ! is.character(value))
                stop('scripts must be a string')
            private$.scripts <- value
            base::invisible(self)
        },
        stylesheets=function(value) {
            if (base::missing(value))
                return(private$.stylesheets)
            if ( ! is.character(value))
                stop('stylesheets must be a string')
            private$.stylesheets <- value
            base::invisible(self)
        },
        content=function(value) {
            if (base::missing(value))
                return(private$.content)
            knitted <- knitr::knit_print(value)

            knitMeta <- attr(knitted, 'knit_meta')
            if ( ! is.null(knitMeta)) {
                knitMeta <- knitMeta[[1]]

                package  <- self$analysis$package

                srcPath  <- normalizePath(knitMeta$src$file)
                rootPath <- normalizePath(system.file(package=package))
                relPath  <- substring(srcPath, nchar(rootPath))

                scripts <- sapply(knitMeta$script,     function(x) file.path(relPath, x), USE.NAMES=FALSE)
                sss     <- sapply(knitMeta$stylesheet, function(x) file.path(relPath, x), USE.NAMES=FALSE)

                private$.scripts <- scripts
                private$.stylesheets <- sss
            }

            attributes(knitMeta) <- NULL
            private$.content <- knitted
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
        isFilled=function() {
            if (private$.stale)
                return(FALSE)
            if (identical(private$.content, ''))
                return(FALSE)
            return(TRUE)
        },
        asString=function() {
            "\n  [No plain text representation available]\n\n"
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

            private$.content <- element$html$content
            private$.scripts <- element$html$scripts
            private$.stylesheets <- element$html$stylesheets
        },
        asProtoBuf=function(incAsText=FALSE, status=NULL) {
            initProtoBuf()
            element <- super$asProtoBuf(incAsText=TRUE, status=status)
            element$html$content <- private$.content
            element$html$scripts <- private$.scripts
            element$html$stylesheets <- private$.stylesheets
            element
        }
    )
)
