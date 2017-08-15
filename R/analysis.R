
#' the jmvcore Object classes
#' @export
Analysis <- R6::R6Class("Analysis",
    private=list(
        .datasetId="",
        .analysisId="",
        .name="",
        .package="",
        .title="",
        .options=NA,
        .results=NA,
        .status="none",
        .completeWhenFilled=FALSE,
        .init=function() NULL,
        .clear=function(vChanges) NULL,
        .run=function() NULL,
        .readDataset=NA,
        .readDatasetHeader=NA,
        .statePathSource=NA,
        .resourcesPathSource=NA,
        .checkpointCB=NA,
        .data=NA,
        .header=NA,
        .info=NA,
        .version=NA,
        .changed=character(),
        .revision=0,
        .stacktrace='',
        .checkpoint=function(flush=TRUE) {
            if (is.null(private$.checkpointCB))
                return()

            results <- NULL
            if (flush)
                results <- RProtoBuf_serialize(self$asProtoBuf(), NULL)

            cmd <- private$.checkpointCB(results)

            if (is.character(cmd) && cmd == 'restart') {
                self$setStatus('restarting')
                stop(jmvcore::createError('restarting', 'restart'))
            }
        },
        .sourcifyOption=function(option) {
            value <- option$value
            def <- option$default

            if (option$name == 'data')
                return('data = data')

            if ( ! ((is.numeric(value) && isTRUE(all.equal(value, def))) || base::identical(value, def))) {
                return(paste0(option$name, ' = ', sourcify(value, '    ')))
            }
            ''
        },
        .asArgs=function(incData=TRUE) {
            source <- ''
            sep <- '\n    '

            if (incData && self$options$requiresData) {
                as <- private$.sourcifyOption(list(name='data', value='data'))
                source <- paste0(sep, as)
                sep <- paste0(',\n    ')
            }

            for (option in private$.options$options) {
                as <- private$.sourcifyOption(option)
                if ( ! base::identical(as, '')) {
                    source <- paste0(source, sep, as)
                    sep <- paste0(',\n    ')
                }
            }

            source
        }),
    active=list(
        analysisId=function() private$.analysisId,
        name=function() private$.name,
        package=function() private$.package,
        data=function() private$.data,
        options=function() private$.options,
        results=function() private$.results,
        status=function() private$.status,
        complete=function() base::identical(private$.status, 'complete'),
        errored=function() base::identical(private$.status, 'error')),
    public=list(
        initialize=function(
            package,
            name,
            version,
            options,
            results,
            pause=NULL,
            data=NULL,
            datasetId="",
            analysisId="",
            revision=0,
            completeWhenFilled=FALSE,
            ...) {

            private$.package <- package
            private$.name    <- name
            private$.version <- version
            private$.options <- options
            private$.results <- results
            private$.data <- data

            private$.analysisId <- analysisId
            private$.datasetId <- datasetId
            private$.revision <- revision
            private$.completeWhenFilled <- completeWhenFilled

            private$.results$.parent <- self
            private$.options$analysis <- self

            private$.options$addChangeListener(private$.optionsChangedHandler)

            private$.checkpointCB <- NULL
        },
        check=function(checkVars=TRUE) {
            private$.options$check(checkVars=checkVars)
        },
        setStatus=function(status) {
            private$.status <- status
        },
        setError=function(message, stacktrace=NULL) {
            private$.status <- 'error'
            private$.results$setError(message)
            if ( ! is.null(stacktrace))
                private$.stacktrace <- stacktrace
        },
        init=function(noThrow=FALSE) {

            try <- dontTry
            if (noThrow)
                try <- tryStack

            result <- try({
                if (private$.status != "none")
                    return()

                wasNull <- FALSE

                if ( ! self$options$requiresData) {
                    # do nothing
                } else if (is.null(private$.data)) {
                    private$.data <- self$readDataset(TRUE)
                    wasNull <- TRUE
                } else {
                    if ( ! is.data.frame(private$.data))
                        reject("Argument 'data' must be a data frame")
                    private$.data <- select(private$.data, self$options$varsRequired)
                }

                self$options$check(checkVars=FALSE)  # don't check vars till after the .update()
                self$results$.update()
                self$options$check(checkVars=TRUE)

                private$.init()

            }, silent=TRUE)

            if ( ! self$options$requiresData) {
                # do nothing
            } else if (wasNull) {
                private$.data <- NULL
            }

            if (isError(result)) {
                message <- extractErrorMessage(result)
                stack <- attr(result, 'stack')
                self$setError(message, stack)
                private$.status <- 'error'
            } else if (private$.status != 'complete') {
                private$.status <- 'inited'
            }
        },
        run=function(noThrow=FALSE) {

            if (private$.status != "inited")
                self$init()

            wasNull <- FALSE

            if (is.null(private$.data)) {
                wasNull <- TRUE
                private$.data <- self$readDataset()
            }

            private$.status <- "running"

            try <- dontTry
            if (noThrow)
                try <- tryStack

            result <- try({
                result <- private$.run()
            }, silent=TRUE)

            if (wasNull)
                private$.data <- NULL

            if (private$.status == 'restarting') {
                return(FALSE)  # FALSE means don't bother sending results
            } else if (isError(result)) {
                message <- extractErrorMessage(result)
                stack <- attr(result, 'stack')
                self$setError(message, stack)
                private$.status <- 'error'
            } else {
                private$.status <- 'complete'
            }

            return(TRUE)
        },
        print=function() {
            cat(self$results$asString())
        },
        .save=function() {
            path <- private$.statePathSource()
            pb <- self$asProtoBuf(incOptions=TRUE)
            RProtoBuf_serialize(pb, path)
        },
        .load=function(vChanges=character()) {

            initProtoBuf()

            path <- private$.statePathSource()

            if (base::file.exists(path)) {
                pb <- RProtoBuf_read(jamovi.coms.AnalysisResponse, path)
                oChanges <- private$.options$compProtoBuf(pb$options)
                private$.results$fromProtoBuf(pb$results, oChanges, vChanges)
            }

            private$.clear(vChanges)

            if (isTRUE(private$.completeWhenFilled) && self$results$isFilled())
                private$.status <- 'complete'
        },
        render=function() {
            # deprecated
            # can remove this once laken's pushes the new TOSTER to CRAN
        },
        .render=function(funName, image, ...) {

            if (image$requiresData && is.null(private$.data)) {
                private$.data <- self$readDataset()
                on.exit(private$.data <- NULL)
            }

            t <- themes[[self$options$theme]]
            if (is.null(t))
                t <- themes$default

            ev <- parse(text=paste0('private$', funName, '(image, theme = t$theme, ggtheme = t$ggtheme, ...)'))
            result <- eval(ev)

            if (identical(result, FALSE))
                stop('Rendering failed', call.=FALSE)

            result
        },
        .createImages=function(noThrow=FALSE, ...) {
            private$.results$.createImages(ppi=self$options$ppi, noThrow=noThrow, ...)
        },
        .createImage=function(funName, image, ppi=72, noThrow=FALSE, ...) {

            if ( ! is.null(image$path))
                return(FALSE)

            render <- private[[funName]]

            if (image$visible == FALSE)
                return(FALSE)

            if (is.function(render) == FALSE) {
                image$.setPath(NULL)
                return(FALSE)
            }

            if (is.function(private$.resourcesPathSource)) {

                name <- base64enc::base64encode(base::charToRaw(image$name))
                paths <- private$.resourcesPathSource(name, "png")

                base::Encoding(paths$rootPath) <- 'UTF-8'
                base::Encoding(paths$relPath)  <- 'UTF-8'

                fullPath <- paste0(paths$rootPath, '/', paths$relPath)

                multip <- ppi / 72

                grType <- 'cairo'
                if (Sys.info()['sysname'] == 'Windows')
                    grType <- 'windows'

                grDevices::png(type=grType,
                    filename=fullPath,
                    width=image$width * multip,
                    height=image$height * multip,
                    bg='transparent',
                    res=72 * multip)
                on.exit(grDevices::dev.off())
            }

            dataRequired <- FALSE
            if (image$requiresData && is.null(private$.data)) {
                dataRequired <- TRUE
                private$.data <- self$readDataset()
            }

            try <- dontTry
            if (noThrow)
                try <- tryStack

            t <- themes[[self$options$theme]]
            if (is.null(t))
                t <- themes$default

            ev <- parse(text=paste0('private$', funName, '(image, theme = t$theme, ggtheme = t$ggtheme, ...)'))
            result <- try(eval(ev), silent=TRUE)

            if (dataRequired)
                private$.data <- NULL

            if (isError(result)) {
                message <- extractErrorMessage(result)
                stack <- attr(result, 'stack')
                self$setError(message, stack)
                private$.status <- 'error'
                result <- FALSE
            } else if (identical(result, TRUE)) {
                # do nothing
            } else if (identical(result, FALSE)) {
                # do nothing
            } else {
                result <- FALSE
            }

            if (is.function(private$.resourcesPathSource)) {

                if (isTRUE(result))
                    image$.setPath(paths$relPath)
                else
                    image$.setPath(NULL)

            } else {

                image$.setPath(NULL)
            }

            result
        },
        .setReadDatasetSource=function(read) {
            private$.readDataset <- read
        },
        .setReadDatasetHeaderSource=function(read) {
            private$.readDatasetHeader <- read
        },
        .setStatePathSource=function(statePath) {
            private$.statePathSource <- statePath
        },
        .setResourcesPathSource=function(resourcesPathSource) {
            private$.resourcesPathSource <- resourcesPathSource
        },
        .setCheckpoint=function(checkpoint) {
            private$.checkpointCB <- checkpoint
        },
        .readState=function() {
            try({
                if (is.function(private$.statePathSource)) {
                    statePath <- private$.statePathSource()
                    if (file.exists(statePath)) {
                        conn <- file(statePath, open="rb", raw=TRUE)
                        pb <- RProtoBuf_read(jamovi.coms.ResultsElement, conn)
                        close(conn)

                        self$results$fromProtoBuf(pb, NULL, NULL)
                    }
                }
            })
        },
        .saveState=function() {

            if (is.function(private$.statePathSource)) {
                statePath <- private$.statePathSource()
                conn <- file(statePath, open="wb", raw=TRUE)
                RProtoBuf_serialize(self$results$asProtoBuf(), conn)
                close(conn)
            }
        },
        .savePart=function(path, part, ...) {
            partPath <- strsplit(part, '/', fixed=TRUE)[[1]]
            element <- self$results$.lookup(partPath)
            element$saveAs(path)
        },
        readDataset=function(headerOnly=FALSE) {

            if (headerOnly)
                dataset <- private$.readDatasetHeader(self$options$varsRequired)
            else
                dataset <- private$.readDataset(self$options$varsRequired)

            dataset
        },
        optionsChangedHandler=function(optionNames) {
            private$.status <- "none"
        },
        asProtoBuf=function(incOptions=FALSE, incAsText=FALSE) {

            self$init()
            initProtoBuf()

            response <- RProtoBuf_new(jamovi.coms.AnalysisResponse)
            response$datasetId  <- private$.datasetId
            response$analysisId <- self$analysisId
            response$name <- private$.name
            response$ns   <- private$.package
            response$version$major <- private$.version[1]
            response$version$minor <- private$.version[2]
            response$version$revision <- private$.version[3]
            response$revision <- private$.revision

            if (private$.status == "inited") {
                response$status <- jamovi.coms.AnalysisStatus$ANALYSIS_INITED;
            } else if (private$.status == "running") {
                response$status <- jamovi.coms.AnalysisStatus$ANALYSIS_RUNNING;
            } else if (private$.status == "complete") {
                response$status <- jamovi.coms.AnalysisStatus$ANALYSIS_COMPLETE;
            } else {
                response$status <- jamovi.coms.AnalysisStatus$ANALYSIS_ERROR
            }

            prepend <- list()
            if ( ! identical(private$.stacktrace, ''))
                prepend[[length(prepend)+1]] <- RProtoBuf_new(jamovi.coms.ResultsElement, name='debug', title='Debug', preformatted=private$.stacktrace)

            if (incAsText) {
                response$incAsText <- TRUE
                syntax <- RProtoBuf_new(jamovi.coms.ResultsElement, name='syntax', preformatted=self$asSource())
                prepend <- c(list(syntax), prepend)
                response$results <- self$results$asProtoBuf(incAsText=incAsText, status=response$status, prepend=prepend);
            } else {
                response$results <- self$results$asProtoBuf(incAsText=incAsText, status=response$status, prepend=prepend);
            }

            if (incOptions)
                response$options <- private$.options$asProtoBuf()

            response
        },
        serialize=function(incOptions=FALSE, incAsText=FALSE) {
            serial <- tryStack(RProtoBuf_serialize(self$asProtoBuf(incOptions=incOptions, incAsText=incAsText), NULL))
            if (isError(serial))
                serial <- createErrorAnalysis(
                    as.character(serial),
                    attr(serial, 'stack'),
                    private$.package,
                    private$.name,
                    private$.datasetId,
                    private$.analysisId,
                    private$.revision)$serialize()
            serial
        },
        asSource=function() {
            paste0(private$.package, '::', private$.name, '(', private$.asArgs(), ')')
        })
)
