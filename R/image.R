
Image <- R6::R6Class("Image",
    inherit=ResultsElement,
    private=list(
        .state=NA,
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
        initialize=function(key="", index=0, options=Options$new()) {
            super$initialize(key, index, options)
            private$.path <- NULL
            private$.state <- NULL
        },
        .render=function(path, ...) {
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
            
            if (base::any(oChanges %in% private$.clearWith))
                return()
            
            for (clearName in private$.clearWith) {
                if (base::any(vChanges %in% private$.options$option(clearName)$vars))
                    return()
            }
            
            image <- element$image
            
            private$.width <- image$width
            private$.height <- image$height
            if (image$path == '')
                private$.path <- NULL
            else
                private$.path <- image$path
        })
)
