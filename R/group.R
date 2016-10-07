
Group <- R6::R6Class("Group",
    inherit=ResultsElement,
    private=list(
        .items=NA),
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
        initialize=function(key="", index=0, options=Options()) {
            super$initialize(key, index, options)
            private$.items <- list()
        },
        get=function(name) {
            private$.items[[name]]
        },
        .render=function(...) {
            for (item in private$.items)
                item$.render(...)
        },
        .setItemsDef=function(itemsDef) {
            
            private$.items <- list()
            
            for (itemDef in itemsDef) {
                
                if (itemDef$type == 'Table')
                    item <- Table$new(itemDef$name, 0, private$.options)
                else if (itemDef$type == 'Array')
                    item <- Array$new(itemDef$name, 0, private$.options)
                else if (itemDef$type == 'Image')
                    item <- Image$new(itemDef$name, 0, private$.options)
                else if (itemDef$type == 'Group')
                    item <- Group$new(itemDef$name, 0, private$.options)
                else
                    stop("Unknown item type")
                
                item$.setup(itemDef)
                item$.parent = self
                private$.items[[itemDef$name]] <- item
            }
        },
        .setDef=function(name, value) {
            if (name == "items")
                self$.setItemsDef(value)
            else
                super$.setDef(name, value)
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
                reject("Group::fromProtoBuf(): expected a jmvcoms.ResultsElement")
            
            for (itemPB in pb$group$elements) {
                itemName <- itemPB$name
                target <- private$.items[[itemName]]
                
                if ( ! is.null(target))
                    target$fromProtoBuf(itemPB, oChanges, vChanges)
            }
        },
        asProtoBuf=function(incAsText=FALSE, prepend=NULL) {
            initProtoBuf()
            
            group <- RProtoBuf::new(jmvcoms.ResultsGroup)
            
            if ( ! is.null(prepend))
                group$add("elements", prepend)
            
            for (item in private$.items) {
                if (item$visible)
                    group$add("elements", item$asProtoBuf(incAsText))
            }
            
            result <- RProtoBuf::new(jmvcoms.ResultsElement,
                name=self$name,
                title=self$title,
                group=group)
            
            if (private$.status == 'error') {
                error <- RProtoBuf::new(jmvcoms.Error,
                    message=private$.error)
                result$error <- error
                result$status <- jmvcoms.AnalysisStatus$ANALYSIS_ERROR
            }
            
            result
        })
)
