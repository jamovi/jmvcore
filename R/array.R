
#' @rdname Analysis
#' @export
Array <- R6::R6Class("Array",
    inherit=ResultsElement,
    private=list(
        .items=NA,
        .itemNames=NA,
        .itemKeys=NA,
        .template=NA,
        .itemsExpr="0",
        .itemsValue=0),
    active=list(
        items=function() private$.items,
        itemNames=function() private$.itemNames,
        itemKeys=function() private$.itemKeys),
    public=list(
        initialize=function(
            options,
            template,
            name=NULL,
            title='no title',
            visible=TRUE,
            clearWith=NULL,
            items=0) {

            super$initialize(
                options=options,
                name=name,
                title=title,
                visible=visible,
                clearWith=clearWith)

            private$.template <- template
            private$.itemsExpr <- paste(items)

            private$.items <- list()
            private$.itemKeys <- list()
            private$.itemNames <- character()
        },
        get=function(key=NULL, name=NULL, index=NULL) {

            if ( ! is.null(index))
                index <- index
            else if ( ! is.null(name))
                index <- indexOf(name, private$.itemNames)
            else
                index <- indexOf(key, private$.itemKeys)

            if ( ! is.na(index))
                item <- private$.items[[ index ]]
            else
                reject('No such key or name')

            item
        },
        addItem=function(key) {
            index <- length(private$.items) + 1
            private$.itemKeys[[index]] <- key
            private$.itemNames[[index]] <- rjson::toJSON(key)
            self$.createItem(key, index)
        },
        isFilled=function() {
            for (item in private$.items) {
                if (item$visible && item$isNotFilled())
                    return(FALSE)
            }
            TRUE
        },
        .render=function(...) {
            rendered <- FALSE
            if (self$visible) {
                for (item in private$.items)
                    rendered <- item$.render(...) || rendered
            }
            rendered
        },
        .update=function() {

            if (private$.updated)
                return()

            super$.update()

            if (length(private$.template) == 0)
                return()

            error <- NULL

            newKeys <- try(private$.options$eval(private$.itemsExpr, .key=private$.key, .name=private$.name, .index=private$.index), silent=TRUE)

            if (base::inherits(newKeys, "try-error")) {
                error <- newKeys
                newKeys <- list()
            } else if (is.list(newKeys)) {
                # all good
            } else if (is.character(newKeys)) {
                newKeys <- as.list(newKeys)
            } else if (is.numeric(newKeys) && newKeys[1] > 0) {
                newKeys <- as.list(paste(1:newKeys))
            } else {
                newKeys <- list()
            }

            oldKeys  <- private$.itemKeys
            oldItems <- private$.items

            private$.itemKeys <- newKeys
            private$.itemNames <- sapply(newKeys, rjson::toJSON, USE.NAMES=FALSE)
            private$.items <- list()

            for (i in seq_along(newKeys)) {

                newKey <- newKeys[[i]]
                index <- indexOf(newKey, oldKeys)

                if ( ! is.na(index)) {

                    item <- oldItems[[ index[1] ]]
                    item$.update()
                    private$.items[[i]] <- item

                } else {

                    self$.createItem(newKey, i)
                }
            }

            if ( ! is.null(error))
                rethrow(error)
        },
        .createItem=function(key, index) {

            item <- private$.template$clone(deep=TRUE)
            item$.parent <- self
            item$.setKey(key, index)
            item$.update()

            private$.items[[index]] <- item

            invisible(item)
        },
        clear=function() {
            private$.itemKeys <- list()
            private$.itemNames <- character()
            private$.items <- list()
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
        fromProtoBuf=function(element, oChanges=NULL, vChanges=NULL) {
            if ( ! base::inherits(element, "Message"))
                reject("Array$fromProtoBuf() expects a jamovi.coms.ResultsElement")

            someChanges <- length(oChanges) > 0 || length(vChanges) > 0
            if (someChanges && base::identical('*', private$.clearWith))
                return()

            if (base::any(oChanges %in% private$.clearWith))
                return()

            for (clearName in private$.clearWith) {
                if (base::any(vChanges %in% private$.options$option(clearName)$vars))
                    return()
            }

            bound <- self$getBoundVars(private$.itemsExpr)
            changes <- vChanges[vChanges %in% bound]

            arrayPB <- element$array

            arrayPBIndicesByName <- list()

            for (i in seq_along(arrayPB$elements)) {
                elementPB <- arrayPB$elements[[i]]
                arrayPBIndicesByName[[elementPB$name]] <- i
            }

            for (i in seq_along(private$.itemNames)) {
                itemName <- private$.itemNames[[i]]
                itemKey  <- private$.itemKeys[[i]]

                if ( ! is.na(indexOf(itemKey, changes)))
                    next()

                fromItemIndex <- arrayPBIndicesByName[[itemName]]
                if ( ! is.null(fromItemIndex)) {
                    private$.items[[i]]$fromProtoBuf(arrayPB$elements[[fromItemIndex]], oChanges, vChanges)
                }
            }
        },
        asProtoBuf=function(incAsText=FALSE, status=NULL) {
            initProtoBuf()

            array <- RProtoBuf::new(jamovi.coms.ResultsArray)

            for (item in private$.items)
                array$add("elements", item$asProtoBuf(incAsText=incAsText, status=status))

            result <- super$asProtoBuf(incAsText=incAsText, status=status)
            result$array <- array
            result
        })
)
