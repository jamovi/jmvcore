

PlotObject <- R6::R6Class('PlotObject',
    public=list(
        initialize=function(fun) {
            self$fun <- fun
        },
        fun=NA,
        print=function() {
            fun <- self$fun
            ret <- fun()
            if ( ! is.null(ret) && ! is.logical(ret))
                print(ret)
        }))

#' the jmvcore Object classes
#' @export
Analysis <- R6::R6Class('Analysis',
    private=list(
        .datasetId='',
        .analysisId='',
        .name='',
        .package='',
        .title='',
        .options=NA,
        .results=NA,
        .status='none',
        .completeWhenFilled=FALSE,
        .requiresMissings=FALSE,
        .init=function() NULL,
        .clear=function(vChanges) NULL,
        .run=function() NULL,
        .postInit=function() NULL,
        .readDataset=NA,
        .readDatasetHeader=NA,
        .statePathSource=NA,
        .resourcesPathSource=NA,
        .checkpointCB=NA,
        .data=NA,
        .dataProvided=TRUE,
        .header=NA,
        .info=NA,
        .version=NA,
        .changed=character(),
        .revision=0,
        .parent=NA,
        .addons=NA,
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
                valueAsSource <- option$valueAsSource
                if ( ! identical(valueAsSource, ''))
                    return(paste0(option$name, ' = ', valueAsSource))
            }
            ''
        },
        .asArgs=function(incData=TRUE) {
            source <- ''
            sep <- '\n    '

            fmla <- self$formula
            if ( ! identical(fmla, '')) {
                source <- paste0(source, sep, 'formula = ', fmla)
                sep <- paste0(',\n    ')
            }

            if (incData && self$options$requiresData) {
                as <- private$.sourcifyOption(list(name='data', value='data'))
                source <- paste0(source, sep, as)
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
        },
        .formula=function() ''),
    active=list(
        analysisId=function() private$.analysisId,
        name=function() private$.name,
        package=function() private$.package,
        data=function() private$.data,
        options=function() private$.options,
        results=function() private$.results,
        status=function() private$.status,
        complete=function() base::identical(private$.status, 'complete'),
        errored=function() base::identical(private$.status, 'error'),
        formula=function() private$.formula(),
        parent=function() private$.parent,
        requiresMissings=function() private$.requiresMissings),
    public=list(
        initialize=function(
            package,
            name,
            version,
            options,
            results,
            pause=NULL,
            data=NULL,
            datasetId='',
            analysisId='',
            revision=0,
            completeWhenFilled=FALSE,
            requiresMissings=FALSE,
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
            private$.requiresMissings <- requiresMissings

            private$.results$.setParent(self)
            private$.options$analysis <- self

            private$.options$addChangeListener(private$.optionsChangedHandler)

            private$.checkpointCB <- NULL

            private$.parent <- NULL
            private$.addons <- list()
        },
        check=function(checkValues=FALSE, checkVars=FALSE, checkData=FALSE) {
            private$.options$check(
                checkValues=checkValues,
                checkVars=checkVars,
                checkData=checkData)
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
                if (private$.status != 'none')
                    return()

                if ( ! self$options$requiresData) {
                    # do nothing
                } else if (is.null(private$.data)) {
                    data <- self$readDataset(TRUE)
                    private$.data <- data
                    private$.dataProvided <- FALSE
                    for (addon in private$.addons) {
                        addon$.__enclos_env__$private$.data <- data
                        addon$.__enclos_env__$private$.dataProvided <- FALSE
                    }
                } else {
                    if ( ! is.data.frame(private$.data))
                        reject("Argument 'data' must be a data frame")
                    private$.data <- select(private$.data, self$options$varsRequired)
                }

                self$options$check(checkValues=TRUE)
                for (addon in private$.addons)
                    addon$options$check(checkValues=TRUE)

                self$results$.update()
                for (addon in private$.addons)
                    addon$results$.update()

                self$options$check(checkVars=TRUE)
                for (addon in private$.addons)
                    addon$options$check(checkVars=TRUE)

                private$.init()
                for (addon in private$.addons)
                    addon$.__enclos_env__$private$.init()

                self$options$check(checkData=TRUE)
                for (addon in private$.addons)
                    addon$options$check(checkData=TRUE)

            }, silent=TRUE)

            if (isError(result)) {
                message <- extractErrorMessage(result)
                stack <- attr(result, 'stack')
                self$setError(message, stack)
                private$.status <- 'error'
            } else if (self$options$gtg == FALSE) {
                private$.status <- 'complete'
            } else if (private$.status != 'complete') {
                private$.status <- 'inited'
            }
        },
        postInit=function(noThrow=FALSE) {

            try <- dontTry
            if (noThrow)
                try <- tryStack

            result <- try({
                private$.postInit()
            }, silent=TRUE)

            if ( ! self$options$requiresData) {
                # do nothing
            } else if ( ! private$.dataProvided) {
                private$.data <- NULL
                for (addon in private$.addons)
                    addon$.__enclos_env__$private$.data <- NULL
            }

            if (isError(result)) {
                message <- extractErrorMessage(result)
                stack <- attr(result, 'stack')
                self$setError(message, stack)
                private$.status <- 'error'
            } else if (self$options$gtg == FALSE) {
                private$.status <- 'complete'
            } else if (private$.status != 'complete') {
                private$.status <- 'inited'
            }

            TRUE
        },
        run=function(noThrow=FALSE) {

            if (private$.status != 'inited') {
                self$init()
                self$postInit()
            }

            if (is.null(private$.data)) {
                private$.dataProvided <- FALSE
                data <- self$readDataset()
                private$.data <- data
                for (addon in private$.addons) {
                    addon$.__enclos_env__$private$.data <- data
                    addon$.__enclos_env__$private$.dataProvided <- FALSE
                }
            }

            private$.status <- 'running'

            try <- dontTry
            if (noThrow)
                try <- tryStack

            result <- try({
                result <- private$.run()
                for (addon in private$.addons) {
                    private$.checkpoint()
                    addon$.__enclos_env__$private$.run()
                }
            }, silent=TRUE)

            if ( ! private$.dataProvided) {
                private$.data <- NULL
                for (addon in private$.addons)
                    addon$.__enclos_env__$private$.data <- NULL
            }

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
        addAddon=function(addon) {
            private$.addons[[length(private$.addons)+1]] <- addon
            addon$.setParent(self)
        },
        print=function() {
            cat(self$results$asString())
        },
        .save=function() {
            try({
                path <- private$.statePathSource()
                Encoding(path) <- 'UTF-8'
                conn <- file(path, open='wb', raw=TRUE)
                on.exit(close(conn), add=TRUE)
                RProtoBuf_serialize(self$asProtoBuf(), conn)
            }, silent=TRUE)
        },
        .load=function(vChanges=character()) {

            try({
                initProtoBuf()

                path <- private$.statePathSource()
                Encoding(path) <- 'UTF-8'

                if (base::file.exists(path)) {
                    conn <- file(path, open='rb', raw=TRUE)
                    on.exit(close(conn), add=TRUE)

                    pb <- RProtoBuf_read(jamovi.coms.AnalysisResponse, conn)
                    oChanges <- private$.options$compProtoBuf(pb$options)
                    private$.results$fromProtoBuf(pb$results, oChanges, vChanges)
                }

                private$.clear(vChanges)

                if (isTRUE(private$.completeWhenFilled) && self$results$isFilled())
                    private$.status <- 'complete'
            }, silent=TRUE)
        },
        .createPlotObject=function(funName, image, ...) {
            if ( ! is.character(funName))
                stop('no render function', call.=FALSE)

            if (image$requiresData && is.null(private$.data)) {
                private$.data <- self$readDataset()
                on.exit(private$.data <- NULL, add=TRUE)
            }

            t <- getGlobalTheme(self$options$theme, self$options$palette)
            fun <- function() do.call(private[[funName]], list(image, theme=t$theme, ggtheme=t$ggtheme, ...))

            return(PlotObject$new(fun))
        },
        .render=function(funName, image, ...) {
            result <- self$.createPlotObject(funName, image, ...)
            image$.setPlot(result)
            if ( ! is.null(result)) {
                suppressWarnings(suppressMessages(print(result)))
                return(TRUE)
            }
            else {
                return(FALSE)
            }
        },
        .createImages=function(noThrow=FALSE, ...) {
            private$.results$.createImages(ppi=self$options$ppi, noThrow=noThrow, ...)
        },
        .createImage=function(funName, image, ppi=72, noThrow=FALSE, ...) {

            if ( ! is.character(funName))
                return(FALSE)

            if ( ! is.null(image$filePath))
                return(FALSE)

            if (image$visible == FALSE)
                return(FALSE)

            render <- private[[funName]]

            if (is.function(render) == FALSE) {
                image$.setPath(NULL)
                return(FALSE)
            }

            if (is.function(private$.resourcesPathSource)) {

                name <- base64enc::base64encode(base::charToRaw(image$name))
                paths <- private$.resourcesPathSource(name, 'png')

                base::Encoding(paths$rootPath) <- 'UTF-8'
                base::Encoding(paths$relPath)  <- 'UTF-8'

                fullPath <- paste0(paths$rootPath, '/', paths$relPath)
                fullPath <- stringi::stri_enc_tonative(fullPath)

                multip <- ppi / 72

                grType <- 'cairo'
                if (Sys.info()['sysname'] == 'Windows')
                    grType <- 'windows'
                else if (Sys.info()['sysname'] == 'Darwin')
                    grType <- 'quartz'

                width <- image$width * multip
                height <- image$height * multip

                if (width < 32)
                    width <- 32
                if (height < 32)
                    height <- 32

                grDevices::png(type=grType,
                    filename=fullPath,
                    width=width,
                    height=height,
                    bg='transparent',
                    res=72 * multip)
                on.exit(grDevices::dev.off(), add=TRUE)
            }

            dataRequired <- FALSE
            if (image$requiresData && is.null(private$.data)) {
                dataRequired <- TRUE
                private$.data <- self$readDataset()
            }

            try <- dontTry
            if (noThrow)
                try <- tryStack

            t <- getGlobalTheme(self$options$theme, self$options$palette)

            ev <- parse(text=paste0('private$', funName, '(image, theme = t$theme, ggtheme = t$ggtheme, ...)'))
            if (noThrow) {
                result <- try(eval(ev), silent=FALSE)
            } else {
                result <- eval(ev)
            }

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
            } else if (is.null(result)) {
                result <- FALSE
            } else {
                suppressWarnings(suppressMessages(print(result)))
                result <- TRUE
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
        .setParent=function(parent) {
            private$.parent <- parent
        },
        .savePart=function(path, part, ...) {
            Encoding(path) <- 'UTF-8'
            Encoding(part) <- 'UTF-8'

            # equivalent to strsplit(part, '/', fixed=TRUE)
            # except ignores / inside quotes
            m <- gregexpr('"[^"]+"|([^/]+)', part)[[1]]
            l <- attr(m, 'match.length')
            partPath <- vapply(seq_along(m), function(i) substr(part, m[i], m[i]+l[i]-1), '')

            element <- self$results$.lookup(partPath)
            path <- stringi::stri_enc_tonative(path)
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
            private$.status <- 'none'
        },
        asProtoBuf=function(incAsText=FALSE) {

            self$init()
            initProtoBuf()

            response <- RProtoBuf_new(jamovi.coms.AnalysisResponse)
            response$instanceId  <- private$.datasetId
            response$analysisId <- self$analysisId
            response$name <- private$.name
            response$ns   <- private$.package
            response$version <- private$.version[1] * 16777216 + private$.version[2] * 65536 + private$.version[3] * 256
            response$revision <- private$.revision

            if (private$.status == 'inited') {
                response$status <- jamovi.coms.AnalysisStatus$ANALYSIS_INITED;
            } else if (private$.status == 'running') {
                response$status <- jamovi.coms.AnalysisStatus$ANALYSIS_RUNNING;
            } else if (private$.status == 'complete') {
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

            ns <- getNamespace(private$.package)
            if ('.jmvrefs' %in% names(ns)) {
                refsLookup <- ns[['.jmvrefs']]
                for (ref in private$.results$getRefs(recurse=TRUE)) {
                    fullRef <- refsLookup[[ref]]
                    if ( ! is.null(fullRef)) {
                        refPB <- RProtoBuf_new(jamovi.coms.Reference)
                        names <- names(fullRef)
                        refPB$name <- ref
                        if ('type' %in% names)
                            refPB$type <- fullRef$type
                        if ('author' %in% names)
                            refPB$authors$complete <- fullRef$author
                        if ('year' %in% names) {
                            year <- fullRef$year
                            if (grepl('^[0-9]+$', year))
                                refPB$year <- as.integer(year)
                            refPB$year2 <- as.character(year)
                        }
                        if ('title' %in% names)
                            refPB$title <- fullRef$title
                        if ('publisher' %in% names)
                            refPB$publisher <- fullRef$publisher
                        if ('url' %in% names)
                            refPB$url <- fullRef$url
                        if ('volume' %in% names)
                            refPB$volume <- paste(fullRef$volume)
                        if ('issue' %in% names)
                            refPB$issue <- paste(fullRef$issue)
                        if ('pages' %in% names)
                            refPB$pages <- fullRef$pages
                        response$add('references', refPB)
                    }
                }
            }

            response$options <- private$.options$asProtoBuf()

            response
        },
        serialize=function(incAsText=FALSE) {
            serial <- tryStack(RProtoBuf_serialize(self$asProtoBuf(incAsText=incAsText), NULL))
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
