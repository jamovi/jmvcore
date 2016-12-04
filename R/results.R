
ResultsElement <- R6::R6Class("ResultsElement",
    private=c(
        .name="",
        .key=NA,
        .titleValue="",
        .titleExpr="",
        .index=NA,
        .visibleExpr="TRUE",
        .visibleValue=TRUE,
        .options=NA,
        .updated=FALSE,
        .status='none',
        .error=NA,
        .clearWith=NA,
        .state=NA,
        .stale=FALSE,
        deep_clone=function(name, value) {
            value
        }),
    active=list(
        name=function() private$.name,
        key=function() private$.key,
        index=function() private$.index,
        options=function() private$.options,
        visible=function() private$.visibleValue,
        title=function() private$.titleValue,
        state=function() private$.state,
        path=function() {
            if ("ResultsElement" %in% class(self$.parent))
                return(paste(self$.parent$path, self$name, sep="/"))
            else
                return(self$name)
        },
        root=function() {
            parent <- self
            while ("ResultsElement" %in% class(parent))
                parent <- parent$.parent
            parent
        },
        analysis=function() {
            parent <- self$.parent
            while ("ResultsElement" %in% class(parent))
                parent <- parent$.parent
            parent
        },
        status=function() {
            private$.status
        }),
    public=list(
        initialize=function(
            options,
            name,
            title,
            visible,
            clearWith) {

            private$.options <- options
            private$.name <- name
            private$.titleExpr <- title
            private$.visibleExpr <- paste0(visible)
            private$.clearWith <- clearWith

            private$.updated <- FALSE
            private$.state <- NULL

            private$.options$addChangeListener(self$.optionsChanged)
        },
        .setKey = function(key, index) {
            private$.key <- key
            private$.name <- rjson::toJSON(key)
            private$.index <- index
        },
        setStatus=function(status) {
            private$.status <- status
        },
        setState=function(state) {
            private$.state <- state
        },
        .update=function() {
            if (private$.updated)
                return()

            private$.updated <- TRUE

            vis <- private$.options$eval(private$.visibleExpr, .key=private$.key, .name=private$.name, .index=private$.index)

            if (is.logical(vis))
                private$.visibleValue = vis
            else
                private$.visibleValue = (length(vis) > 0)

            private$.titleValue <- paste0(private$.options$eval(private$.titleExpr, .key=private$.key, .name=private$.name, .index=private$.index))
        },
        .render=function(...) {

        },
        .optionsChanged=function(...) {
            private$.updated <- FALSE
        },
        .has=function(name) {
            paste0(".", name) %in% names(private)
        },
        setError = function(message) {
            private$.error <- message
            private$.status <- 'error'
        },
        saveAs=function(file, format) {
            if (format != 'text')
                reject(paste0('unrecognised format "', format, '"'))
            base::cat(self$asString(), file=file, sep="")
        },
        asString=function() {
            self$.update()
            ""
        },
        asProtoBuf=function(incAsText=FALSE, status=NULL) {

            initProtoBuf()

            element <- RProtoBuf::new(jamovi.coms.ResultsElement,
                name=private$.name,
                title=self$title,
                stale=private$.stale)

            if ( ! is.null(status))
                element$status <- status
            else if (private$.status == 'running')
                element$status <- jamovi.coms.AnalysisStatus$ANALYSIS_RUNNING
            else if (private$.status == 'inited')
                element$status <- jamovi.coms.AnalysisStatus$ANALYSIS_INITED
            else if (private$.status == 'complete')
                element$status <- jamovi.coms.AnalysisStatus$ANALYSIS_COMPLETE
            else
                element$status <- jamovi.coms.AnalysisStatus$ANALYSIS_NONE

            element
        },
        fromProtoBuf=function(pb, oChanges=NULL, vChanges=NULL) {

        },
        getBoundVars=function(expr) {
            if ( ! startsWith(expr, '('))
                return(NULL)
            if ( ! endsWith(expr, ')'))
                return(NULL)

            optName <- substring(expr, 2, nchar(expr) - 1)

            if ( ! private$.options$has(optName))
                return(NULL)

            return(private$.options$get(optName))
        },
        print=function() {
            cat(self$asString())
        },
        .parent=NA))
