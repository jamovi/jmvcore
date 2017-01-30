
#' @rdname Analysis
#' @export
Group <- R6::R6Class("Group",
    inherit=ResultsElement,
    private=list(
        .items=NA,
        deep_clone=function(name, value) {
            if (name == '.items') {
                items <- list()
                for (name in names(value))
                    items[[name]] <- value[[name]]$clone(deep=TRUE)
                return(items)
            }
            value
        }),
    active=list(
        items=function() private$.items,
        itemNames=function() names(private$.items),
        visible=function() {
            for (item in private$.items) {
                if (item$visible)
                    return(TRUE)
            }
            return(FALSE)
        }),
    public=list(
        initialize=function(
            options,
            name=NULL,
            title='no title',
            visible=TRUE,
            clearWith=NULL) {

            super$initialize(
                options=options,
                name=name,
                title=title,
                visible=visible,
                clearWith=clearWith)

            private$.items <- list()
        },
        get=function(name) {
            private$.items[[name]]
        },
        .render=function(...) {
            rendered <- FALSE
            if (self$visible) {
                for (item in private$.items)
                    rendered <- item$.render(...) || rendered
            }
            rendered
        },
        add=function(item) {
            item$.parent = self
            private$.items[[item$name]] <- item
        },
        isFilled=function() {
            for (item in private$.items) {
                if (item$visible && item$isNotFilled())
                    return(FALSE)
            }
            TRUE
        },
        .update=function() {
            if (private$.updated)
                return()

            super$.update()

            for (item in private$.items)
                item$.update()
        },
        asString=function() {

            noneVisible <- TRUE

            pieces <- c('\n ', self$title, '\n')

            for (item in private$.items) {
                if (item$visible) {
                    pieces <- c(pieces, item$asString())
                    noneVisible <- FALSE
                }
            }

            if (noneVisible)
                return('')
            else
                return(paste0(pieces, collapse=""))
        },
        fromProtoBuf=function(pb, oChanges=NULL, vChanges=NULL) {
            if ( ! "Message" %in% class(pb))
                reject("Group::fromProtoBuf(): expected a jamovi.coms.ResultsElement")

            someChanges <- length(oChanges) > 0 || length(vChanges) > 0
            if (someChanges && base::identical('*', private$.clearWith))
                return()

            for (itemPB in pb$group$elements) {
                itemName <- itemPB$name
                target <- private$.items[[itemName]]

                if ( ! is.null(target))
                    target$fromProtoBuf(itemPB, oChanges, vChanges)
            }
        },
        asProtoBuf=function(incAsText=FALSE, status=NULL, prepend=NULL) {
            initProtoBuf()

            group <- RProtoBuf::new(jamovi.coms.ResultsGroup)

            if ( ! is.null(prepend))
                group$add("elements", prepend)

            for (item in private$.items)
                group$add("elements", item$asProtoBuf(incAsText=incAsText, status=status))

            result <- super$asProtoBuf(incAsText=incAsText, status=status)
            result$group <- group

            if (private$.status == 'error') {
                error <- RProtoBuf::new(jamovi.coms.Error,
                    message=private$.error)
                result$error <- error
                result$status <- jamovi.coms.AnalysisStatus$ANALYSIS_ERROR
            }

            result
        })
)
