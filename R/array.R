
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
        itemKeys=function() private$.itemKeys,
        asDF=function() {
            children <- paste0('\n    ...[[', 1:3, ']]$asDF', collapse='')
            stop("This results array cannot be converted to a data frame.\n",
                 "Perhaps you mean to access some of it's children:",
                 children,
                 call.=FALSE)
        }),
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
        .createImages=function(...) {
            rendered <- FALSE
            if (self$visible) {
                for (item in private$.items)
                    rendered <- item$.createImages(...) || rendered
            }
            rendered
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

            if (is.null(private$.template))
                return()

            error <- NULL

            newKeys <- try(private$.options$eval(private$.itemsExpr, .key=private$.key, .name=private$.name, .index=private$.index), silent=TRUE)

            if (inherits(newKeys, "try-error")) {
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

            if (inherits(item, 'Group')) {
                for (child in item$items) {
                    child$.parent <- item
                }
            }

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

            pieces <- c()

            for (item in private$.items) {
                if (item$visible) {
                    pieces <- c(pieces, item$asString())
                    noneVisible <- FALSE
                }
            }

            if (noneVisible)
                return('')

            v <- paste0(pieces, collapse="")
            if (v == '')
                return('')

            v <- paste0('\n ', base::toupper(self$title), '\n', v)
            Encoding(v) <- 'UTF-8'
            v
        },
        .lookup=function(path) {
            if (length(path) == 0 || identical(path, ""))
                return(self)

            first <- path[1]
            path  <- path[-1]

            element <- self$get(name=first)
            if (length(path) == 0)
                return(element)
            else
                return(element$.lookup(path))
        },
        fromProtoBuf=function(element, oChanges, vChanges) {
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

                keyElems <- unlist(itemKey, use.names=FALSE)
                if (any(keyElems %in% changes))
                    next()

                fromItemIndex <- arrayPBIndicesByName[[itemName]]
                if ( ! is.null(fromItemIndex)) {

                    item <- private$.items[[i]]
                    elementPB <- arrayPB$elements[[fromItemIndex]]
                    item$fromProtoBuf(elementPB, oChanges, vChanges)
                }
            }
        },
        asProtoBuf=function(incAsText=FALSE, status=NULL) {

            array <- RProtoBuf_new(jamovi.coms.ResultsArray)

            for (item in private$.items)
                array$add("elements", item$asProtoBuf(incAsText=incAsText, status=status))

            result <- super$asProtoBuf(incAsText=incAsText, status=status)
            result$array <- array
            result
        })
)

#' @export
length.Array <- function(x) {
    length(x$items)
}

#' @export
names.Array <- function(x) {
    x$itemNames
}

#' @export
`[[.Array` <- function(array, i) {
    if (is.numeric(i))
        return(array$get(index=i))
    else
        return(array$get(name=i))
}

#' @export
as.data.frame.Array <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {

    call <- as.character(sys.call(-1)[2])
    children <- paste0('\n    as.data.frame(', call, '[[', 1:3, ']])', collapse='')

    stop('This results array cannot be converted to a data frame.\n',
         'Perhaps you mean to access some of its children:',
         children,
         call.=FALSE)
}

#' @export
#' @importFrom utils .DollarNames
.DollarNames.Array <- function(x, pattern = "") {
    names <- ls(x, all.names=F, pattern = pattern)
    retain <- c('asDF', 'asString')
    names <- intersect(names, retain)
    names
}
