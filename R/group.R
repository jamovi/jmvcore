
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
        },
        asDF=function() {
            children <- paste0('\n    ...$', self$itemNames, '$asDF', collapse='')
            stop("This results group cannot be converted to a data frame.\n",
                 "Perhaps you mean to access some of it's children:",
                 children,
                 call.=FALSE)
        }),
    public=list(
        initialize=function(
            options,
            name=NULL,
            title='no title',
            visible=TRUE,
            clearWith=NULL,
            refs=list()) {

            super$initialize(
                options=options,
                name=name,
                title=title,
                visible=visible,
                clearWith=clearWith,
                refs=refs)

            private$.items <- list()
        },
        get=function(name) {
            private$.items[[name]]
        },
        .render=function(...) {
            rendered <- FALSE
            if (self$visible) {
                for (item in private$.items) {
                    if (item$visible)
                        rendered <- item$.render(...) || rendered
                }
            }
            rendered
        },
        .createImages=function(...) {
            rendered <- FALSE
            if (self$visible) {
                for (item in private$.items) {
                    if (item$visible)
                        rendered <- item$.createImages(...) || rendered
                }
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
        .lookup=function(path) {
            if (length(path) == 0 || identical(path, ""))
                return(self)

            first <- path[ 1]
            path  <- path[-1]

            element <- self$get(first)
            if (length(path) == 0)
                return(element)
            else
                return(element$.lookup(path))
        },
        asString=function() {

            noneVisible <- TRUE

            pieces <- c('\n ', base::toupper(self$title), '\n')

            for (item in private$.items) {
                if (item$visible) {
                    pieces <- c(pieces, item$asString())
                    noneVisible <- FALSE
                }
            }

            if (noneVisible)
                return('')

            utf8(paste0(pieces, collapse=""))
        },
        fromProtoBuf=function(pb, oChanges, vChanges) {
            if ( ! "Message" %in% class(pb))
                reject("Group::fromProtoBuf(): expected a jamovi.coms.ResultsElement")

            someChanges <- length(oChanges) > 0 || length(vChanges) > 0
            if (someChanges && base::identical('*', private$.clearWith))
                return()

            super$fromProtoBuf(pb, oChanges, vChanges)

            for (itemPB in pb$group$elements) {
                itemName <- itemPB$name
                target <- private$.items[[itemName]]

                if ( ! is.null(target))
                    target$fromProtoBuf(itemPB, oChanges, vChanges)
            }
        },
        asProtoBuf=function(incAsText=FALSE, status=NULL, prepend=NULL, append=NULL) {
            group <- RProtoBuf::new(jamovi.coms.ResultsGroup)

            for (prep in prepend)
                group$add("elements", prep)

            for (item in private$.items)
                group$add("elements", item$asProtoBuf(incAsText=incAsText, status=status))

            result <- super$asProtoBuf(incAsText=incAsText, status=status)
            result$group <- group

            result
        })
)

#' @export
length.Group <- function(x) {
    length(x$items)
}

#' @export
names.Group <- function(x) {
    x$itemNames
}

#' @export
`[[.Group` <- function(group, i) {
    group$get(i)
}

#' @export
as.data.frame.Group <- function(x, ..., stringsAsFactors = default.stringsAsFactors()) {

    call <- as.character(sys.call(-1)[2])
    children <- paste0('\n    as.data.frame(', call, '$', x$itemNames, ')', collapse='')

    stop('This results group cannot be converted to a data frame.\n',
         'Perhaps you mean to access some of its children:',
         children,
         call.=FALSE)
}

#' @export
#' @importFrom utils .DollarNames
.DollarNames.Group <- function(x, pattern = "") {
    names <- ls(x, all.names=F, pattern = pattern)
    retain <- c(x$itemNames, 'asDF', 'asString')
    names <- intersect(names, retain)
    for (name in x$itemNames) {
        item <- x$get(name)
        if ( ! item$visible)
            names <- setdiff(names, name)
    }
    names
}

