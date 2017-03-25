
#' @rdname Analysis
#' @export
Image <- R6::R6Class("Image",
    inherit=ResultsElement,
    private=list(
        .path=NA,
        .width=400,
        .height=300,
        .renderInitFun=NA,
        .renderFun=NA,
        .requiresData=FALSE),
    active=list(
        width=function() private$.width,
        height=function() private$.height,
        path=function() private$.path,
        requiresData=function() private$.requiresData),
    public=list(
        initialize=function(
            options,
            width=400,
            height=300,
            renderFun=NULL,
            renderInitFun=NULL,
            requiresData=FALSE,
            name=NULL,
            title='',
            visible=TRUE,
            clearWith='*') {

            super$initialize(
                options=options,
                name=name,
                title=title,
                visible=visible,
                clearWith=clearWith)

            private$.width <- width
            private$.height <- height
            private$.renderFun <- renderFun
            private$.renderInitFun <- renderInitFun
            private$.requiresData <- requiresData

            private$.path <- NULL
        },
        isFilled=function() {
            if (private$.stale)
                return(FALSE)
            if (is.null(private$.path))
                return(FALSE)
            return(TRUE)
        },
        print=function() {
            self$.render()
        },
        .render=function(...) {
            if ( ! is.character(private$.renderFun))
                return()

            self$analysis$.render(funName=private$.renderFun, image=self, ...)
        },
        .setPath=function(path) {
            private$.path <- path
        },
        asString=function() {

            if (is.null(private$.path))
                return('')

            pieces <- c(' ', private$.title, '\n')
            pieces <- c(pieces, '\n ', private$.path, '\n')

            return(paste0(pieces, collapse=""))
        },
        asProtoBuf=function(incAsText=FALSE, status=NULL) {
            initProtoBuf()

            path <- private$.path
            if (is.null(path))
                path=''

            image <- RProtoBuf::new(jamovi.coms.ResultsImage,
                width=private$.width,
                height=private$.height,
                path=path)

            result <- super$asProtoBuf(incAsText=incAsText, status=status)
            result$image <- image
            result
        },
        fromProtoBuf=function(element, oChanges=NULL, vChanges=NULL) {
            if ( ! base::inherits(element, "Message"))
                reject("Image$fromProtoBuf() expects a jamovi.coms.ResultsElement")

            someChanges <- length(oChanges) > 0 || length(vChanges) > 0
            if (someChanges && base::identical('*', private$.clearWith))
                return()

            if (base::any(oChanges %in% private$.clearWith))
                return()

            for (clearName in private$.clearWith) {
                if (base::any(vChanges %in% private$.options$option(clearName)$vars))
                    return()
            }

            super$fromProtoBuf(element)

            image <- element$image

            private$.width <- image$width
            private$.height <- image$height
            if (image$path == '')
                private$.path <- NULL
            else
                private$.path <- image$path
        })
)
