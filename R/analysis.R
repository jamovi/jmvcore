
#' the jmvcore Object classes
#' @importFrom base64enc base64encode
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
        .init=function() NULL,
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
        .checkpoint=function(flush=TRUE) {
            if (is.null(private$.checkpointCB))
                return()

            results <- NULL
            if (flush)
                results <- RProtoBuf::serialize(self$asProtoBuf(), NULL)

            cmd <- private$.checkpointCB(results)

            if (is.character(cmd) && cmd == 'restart') {
                self$setStatus('restarting')
                stop(jmvcore::createError('restarting', 'restart'))
            }
        },
        .sourcifyOption=function(option) {
            value <- option$value
            def <- option$default

            if ( ! ((is.numeric(value) && isTRUE(all.equal(value, def))) || base::identical(value, def))) {
                return(paste0(option$name, '=', sourcify(value, '    ')))
            }
            ''
        },
        .asArgs=function(incData=TRUE) {
            source <- ''
            sep <- '\n    '

            if (incData && self$options$requiresData) {
                source <- paste0(sep, 'data=data')
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
        status=function() private$.status),
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

            private$.results$.parent <- self
            private$.options$analysis <- self

            private$.options$addChangeListener(private$.optionsChangedHandler)

            private$.checkpointCB <- NULL
        },
        check=function() {
            private$.options$check()
        },
        init=function() {
            if (private$.status != "none")
                return()

            wasNull <- FALSE

            if (is.null(private$.data)) {
                private$.data <- self$readDataset(TRUE)
                wasNull <- TRUE
            } else {
                private$.data <- select(private$.data, self$options$varsRequired)
            }

            self$options$check()
            self$results$.update()

            result <- try({
                private$.init()
            })

            if (wasNull)
                private$.data <- NULL

            if (base::inherits(result, 'try-error')) {
                errorMessage <- extractErrorMessage(result)
                private$.results$setError(errorMessage)
                private$.status <- 'error'
            } else {
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

            if (noThrow) {
                result <- try(private$.run(), silent=TRUE)
            } else {
                result <- private$.run()
            }

            if (wasNull)
                private$.data <- NULL

            if (private$.status == 'restarting') {
                return(FALSE)  # FALSE means don't bother sending results
            } else if (base::inherits(result, 'try-error')) {
                errorMessage <- extractErrorMessage(result)
                private$.results$setError(errorMessage)
                private$.status <- 'error'
            } else {
                private$.status <- 'complete'
            }

            return(TRUE)
        },
        print=function() {
            cat(self$results$asString())
        },
        render=function(...) {
            private$.results$.render(ppi=self$options$ppi, ...)
        },
        .save=function() {
            path <- private$.statePathSource()
            pb <- self$asProtoBuf(incOptions=TRUE)
            RProtoBuf::serialize(pb, path)
        },
        .load=function(vChanges=character()) {

            initProtoBuf()

            path <- private$.statePathSource()

            if (base::file.exists(path)) {
                pb <- RProtoBuf::read(jamovi.coms.AnalysisResponse, path)
                oChanges <- private$.options$compProtoBuf(pb$options)
                private$.results$fromProtoBuf(pb$results, oChanges, vChanges)
            }

            if (self$results$isFilled())
                private$.status <- 'complete'
        },
        .render=function(funName, image, ppi=72, ...) {

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
            }

            wasNull <- FALSE

            if (image$requiresData && is.null(private$.data)) {
                wasNull <- TRUE
                private$.data <- self$readDataset()
            }

            rendered <- render(image)

            if (wasNull)
                private$.data <- NULL

            if (is.function(private$.resourcesPathSource)) {

                grDevices::dev.off()

                if (rendered)
                    image$.setPath(paths$relPath)
                else
                    image$.setPath(NULL)

            } else {

                image$.setPath(NULL)
            }

            rendered
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
                    if (base::file.exists(statePath)) {
                        conn <- file(statePath, open="rb", raw=TRUE)
                        pb <- RProtoBuf::read(jamovi.coms.ResultsElement, conn)
                        base::close(conn)

                        self$results$fromProtoBuf(pb)
                    }
                }
            })
        },
        .saveState=function() {

            if (is.function(private$.statePathSource)) {
                statePath <- private$.statePathSource()
                conn <- file(statePath, open="wb", raw=TRUE)
                RProtoBuf::serialize(self$results$asProtoBuf(), conn)
                base::close(conn)
            }
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

            response <- RProtoBuf::new(jamovi.coms.AnalysisResponse)
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
                error <- RProtoBuf::new(jamovi.coms.Error)
                error$message <- private$.error
                response$error <- error
                response$status <- jamovi.coms.AnalysisStatus$ANALYSIS_ERROR;
            }

            if (incAsText) {
                response$incAsText <- TRUE
                syntax <- RProtoBuf::new(jamovi.coms.ResultsElement, name='syntax', syntax=self$asSource())
                response$results <- self$results$asProtoBuf(incAsText=incAsText, status=response$status, prepend=syntax);
            } else {
                response$results <- self$results$asProtoBuf(incAsText=incAsText, status=response$status);
            }

            if (incOptions)
                response$options <- private$.options$asProtoBuf()

            response
        },
        asSource=function() {
            paste0(private$.package, '::', private$.name, '(', private$.asArgs(), ')')
        })
)
