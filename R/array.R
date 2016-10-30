
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
        itemNames=function() private$.itemNames),
    public=list(
        initialize=function(key="", index=0, options=Options()) {
            super$initialize(key, index, options)
            private$.items <- list()
            private$.itemKeys <- list()
            private$.itemNames <- list()
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
        .render=function(...) {
            if (self$visible) {
                for (item in private$.items)
                    item$.render(...)
            }
        },
        .setDef=function(name, value) {
            if (name == "items")
                self$.setItemsDef(value)
            else if (name == "template")
                self$.setTemplateDef(value)
            else
                super$.setDef(name, value)
        },
        .setTemplateDef=function(templateDef) {
            private$.template <- templateDef
            private$.updated <- FALSE
        },
        .setItemsDef=function(itemsExpr) {
            private$.itemsExpr <- paste0(itemsExpr)
            private$.updated <- FALSE
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
            if (private$.template$type == "Image")
                item <- Image$new(key, index, private$.options)
            else
                item <- Table$new(key, index, private$.options)
            
            item$.setup(private$.template)
            item$.update()
            item$.parent <- self
            private$.items[[index]] <- item
            
            invisible(item)
        },
        clear=function() {
            private$.itemKeys <- character()
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
            
            for (item in private$.items) {
                if (item$visible)
                    array$add("elements", item$asProtoBuf(incAsText=incAsText, status=status))
            }
            
            result <- super$asProtoBuf(incAsText=incAsText, status=status)
            result$array <- array
            result
        })
)
