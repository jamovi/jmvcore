
Option <- R6::R6Class(
    "Option",
    private=list(
        .name=NA,
        .parent=NA,
        .value=NA,
        .default=NA,
        .check=function(data){},
        deep_clone=function(name, value) {
            value
        }),
    public=list(
        initialize=function(name, value=NULL, ...) {

            private$.parent <- NULL
            private$.name <- name
            self$value <- value

            args <- list(...)
            for (name in names(args)) {
                pname <- paste0('.', name)
                if (any(pname %in% names(private)))
                    private[[pname]] <- args[[name]]
            }
        },
        check=function() {
            data <- private$.parent$.getData()
            private$.check(data)
        },
        .setParent=function(parent) {
            private$.parent <- parent
        },
        .getData=function() {
            private$.parent$.getData()
        }),
    active=list(
        name=function() private$.name,
        default=function() private$.default,
        vars=function() NULL,
        value=function(value) {
            if (base::missing(value))
                return(private$.value)
            private$.value <- value
            base::invisible(self)
        }))

OptionBool <- R6Class(
    "OptionBool",
    inherit=Option,
    private=list(
        .check=function(data) {
            if (length(private$.value) == 1 &&
                private$.value != FALSE &&
                private$.value != TRUE)
                    reject("Argument '{a}' must be either TRUE or FALSE",
                           code="a_must_be_true_or_false",
                           a=self$name)
        }
    ))

OptionList <- R6Class(
    "OptionList",
    inherit=Option,
    private=list(
        .options=NA,
        .check=function(data) {
            # if ( ! (private$.value %in% info$options)) {
            #     options <- paste("'", info$options, "'", collapse=", ", sep="")
            #     reject("Argument '{a}' must be one of {options}", code="a_must_be_one_of", a=info$name, options=options)
            # }
        }
    )
)

OptionVariables <- R6Class(
    "OptionVariables",
    inherit=Option,
    active=list(
        vars=function() private$.value),
    private=list(
        .rejectInf=TRUE,
        .rejectMissing=FALSE,
        .permitted=NA,
        .check=function(data) {

            value <- private$.value

            if (length(value) == 0)
                return()

            if (is.character(value) == FALSE && is.list(value) == FALSE)
                reject("Argument '{a}' must be a character vector", code="a_is_not_a_string", a=self$name)

            notInDataset <- value[ ! (value %in% names(data))]
            if (length(notInDataset) == 1) {

                reject("Argument '{a}' contains '{b}' which is not present in the dataset", code="a_is_not_in_b", a=self$name, b=notInDataset)

            } else if (length(notInDataset) > 1) {

                b <- paste(paste0("'", notInDataset, "'"), collapse=", ")
                reject("Argument '{a}' contains {b} which are not present in the dataset", code="a_are_not_in_b", a=self$name, b=b)
            }

            if (class(private$.permitted) == 'character') {
                permitted <- strsplit(private$.permitted, "|", fixed=TRUE)[[1]]

                for (columnName in value) {

                    column <- data[[columnName]]
                    type <- columnType(column)

                    if ((type %in% permitted) == FALSE)
                        reject("Argument '{a}' specifies column '{b}' which is (and must not be) {c}", code="b_is_wrong_measure_type", a=self$name, b=columnName, c=columnTypeRDescription(column))
                }
            }

            if (private$.rejectInf) {  # Infs rejected by default

                for (columnName in value) {

                    column <- data[[columnName]]
                    if (any(is.infinite(column)))
                        reject("Argument '{a}' specifies column '{b}' which contains (and must not) infinite values", code="b_contains_infinite_values", a=self$name, b=columnName)
                }
            }

            if (private$.rejectMissing) {  # missings not rejected by default

                for (columnName in value) {

                    column <- data[[columnName]]
                    if (any(is.na(column)))
                        reject("Argument '{a}' specifies column '{b}' which contains (and must not) missing values (NAs)", code="b_contains_missing_values", a=self$name, b=columnName)
                }
            }
        })
    )

OptionVariable <- R6Class(
    "OptionVariable",
    inherit=OptionString,
    active=list(
        vars=function() private$.value))

OptionTerms <- R6Class(
    "OptionTerms",
    inherit=OptionArray,
    active=list(
        value=function(value) {
            if (missing(value))
                return(super$value)
            super$value <- value
        }
    ),
    public=list(
        initialize=function(name, value, ...) {
            super$initialize(name, value, OptionVariables$new('term', NULL), ...)
        }
    ))

OptionInt <- R6Class(
    "OptionInt",
    inherit=Option)

OptionNumber <- R6Class(
    "OptionNumber",
    inherit=Option)

OptionString <- R6Class(
    "OptionString",
    inherit=Option)

OptionGroup <- R6Class(
    "OptionGroup",
    inherit=Option,
    public=list(
        initialize=function(name, value, elements, ...) {
            private$.elements <- list()
            for (element in elements) {
                element$.setParent(self)
                private$.elements[[element$name]] <- element
            }
            super$initialize(name, value, ...)
        }
    ),
    active=list(
        value=function(value) {
            if (base::missing(value)) {
                value <- list()
                for (o in private$.elements)
                    value[o$name] <- list(o$value)
                return(value)
            }
            for (name in names(value))
                private$.elements[[name]]$value <- value[[name]]
            base::invisible(self)
        }, vars=function() {
            vars <- list()
            for (element in private$.elements)
                vars <- c(vars, element$vars)
            unique(vars)
        }),
    private=list(
        .elements=NA,
        .check=function(data) {
            for (option in private$.elements)
                option$check()
        },
        deep_clone=function(name, value) {

            if (name == '.elements') {
                elements <- list()
                for (name in names(value)) {
                    element <- value[[name]]$clone(deep=TRUE)
                    element$.setParent(self)
                    elements[[name]] <- element
                }
                return(elements)
            }

            value
        }
    )
)

OptionArray <- R6Class(
    "OptionArray",
    inherit=Option,
    public=list(
        initialize=function(name, value, template, ...) {
            template$.setParent(self)
            private$.template <- template
            private$.elements <- list()
            super$initialize(name, value)
        }),
    active=list(
        value=function(values) {
            if (base::missing(values)) {
                values <- list()
                for (o in private$.elements)
                    values[length(values)+1] <- list(o$value)
                return(values)
            }
            private$.elements <- list()
            for (value in values) {
                clone <- private$.template$clone(deep=TRUE)
                clone$value <- value
                private$.elements[[length(private$.elements)+1]] <- clone
            }
            base::invisible(self)
        },
        vars=function() {
            vars <- list()
            for (element in private$.elements)
                vars <- c(vars, element$vars)
            unique(vars)
        }),
    private=list(
        .template=NA,
        .elements=NA,
        .check=function(data) {

        }
    ))


OptionPairs <- R6Class(
    "OptionPairs",
    inherit=OptionArray,
    public=list(
        initialize=function(name, value, permitted=NULL, suggested=NULL, ...) {
            super$initialize(name, value, template=OptionGroup$new(
                "pairs",
                NULL,
                elements=list(
                    OptionVariable$new(
                        "i1",
                        NULL,
                        suggested=suggested,
                        permitted=permitted),
                    OptionVariable$new(
                        "i2",
                        NULL,
                        suggested=suggested,
                        permitted=permitted))),
                ...)
        }))

parseOptionPB <- function(pb) {
    if (pb$has('i'))
        value <- pb$i
    else if (pb$has('d'))
        value <- pb$d
    else if (pb$has('s'))
        value <- pb$s
    else if (pb$has('o')) {
        if (pb$o == jamovi.coms.AnalysisOption.Other$`TRUE`)
            value <- TRUE
        else if (pb$o == jamovi.coms.AnalysisOption.Other$`FALSE`)
            value <- FALSE
        else
            value <- NULL
    }
    else if (pb$has('c')) {
        value <- list()
        for (i in seq_along(pb$c$options))
            value[i] <- list(parseOptionPB(pb$c$options[[i]]))
        if (pb$c$hasNames)
            names(value) <- pb$c$names
    }
    else
        value <- NULL
    
    value
}

Options <- R6::R6Class(
    "Options",
    private=list(
        .analysis=NA,
        .options=NA,
        .listeners=NA,
        .pb=NA,
        .env=NA),
    active=list(
        analysis=function(analysis) {
            if (base::missing(analysis))
                return(private$.analysis)
            private$.analysis <- analysis
            base::invisible(self)
        },
        varsRequired=function() {
            vars <- list()
            for (option in private$.options)
                vars <- c(vars, option$vars)
            vars <- base::unique(vars)
            vars
        },
        names=function() {
            names(private$.options)
        },
        options=function() private$.options),
    public=list(
        initialize=function(...) {

            private$.analysis <- NULL
            private$.options <- list()
            private$.listeners <- list()
            private$.env <- new.env()
            private$.pb <- NULL

            private$.env[["levels"]] <- self$levels
        },
        .addOption=function(option) {
            option$.setParent(self)
            private$.options[[option$name]] <- option
            private$.env[[option$name]] <- option$value
        },
        .getData=function() {
            if (is.null(private$.analysis))
                return(NULL)
            private$.analysis$data
        },
        check=function() {
            for (option in private$.options)
                option$check()
        },
        values=function() {
            private$.env
        },
        eval=function(value, ...) {

            if (class(value) == "character") {

                if (value == "TRUE")
                    return(TRUE)
                if (value == "FALSE")
                    return(FALSE)

                vars <- list(...)
                for (name in names(vars))
                    private$.env[[name]] <- vars[[name]]
                private$.env[['data']] <- self$.getData()

                nch <- nchar(value)
                if ( ! is.na(suppressWarnings(as.numeric(value))))
                    value <- as.numeric(value)
                else if (nch > 0 && substring(value, 1, 1) == "(" && substring(value, nch) == ")")
                    value <- self$.eval(text=value)
                else
                    value <- jmvcore::format(value, ...)

                if (is.character(value))
                    base::Encoding(value) <- 'UTF-8'

                if (length(names(vars)) > 0)
                    rm(list=names(vars), envir=private$.env)
            }

            value
        },
        .eval=function(text) {

            transformed <- gsub('\\$', '.', text)
            value <- try(base::eval(parse(text=transformed), envir=private$.env), silent=TRUE)

            if (inherits(value, "try-error")) {
                reason <- extractErrorMessage(value)
                stop(format("Could not evaluate '{text}'\n    {reason}", text=text, reason=reason), call.=FALSE)
            }

            value
        },
        set=function(...) {

            values <- list(...)
            for (name in names(values))
                private$.options[[name]]$value <- values[[name]]

            for (listener in private$.listeners)
                listener(names(values))
        },
        setValue=function(name, value) {
            private$.options[[name]]$value <- value

            for (listener in private$.listeners)
                listener(name)
        },
        option=function(name) {
            private$.options[[name]]
        },
        get=function(name) {
            private$.options[[name]]$value
        },
        has=function(name) {
            name %in% names(private$.options)
        },
        levels=function(x) {
            str <- substitute(x)
            expr <- parse(text=paste0("base::levels(data[[", str, "]])"))
            v = eval.parent(expr)
            v
        },
        addChangeListener=function(listener) {
            private$.listeners[[length(private$.listeners)+1]] <- listener
        },
        read=function(raw) {
            initProtoBuf()
            self$fromProtoBuf(jamovi.coms.AnalysisOptions$read(raw))
        },
        asProtoBuf=function() {
            private$.pb
        },
        fromProtoBuf=function(pb) {
            if ( ! "Message" %in% class(pb))
                reject("Group::fromProtoBuf(): expected a jamovi.coms.ResultsElement")
            
            private$.pb <- pb
            
            for (i in seq_along(pb$names)) {
                name <- pb$names[[i]]
                optionPB <- pb$options[[i]]
                value <- parseOptionPB(optionPB)
                private$.options[[name]]$value <- value
                private$.env[[name]] <- private$.options[[name]]$value
            }
        },
        fromJSON=function(json) {
            private$.json <- json
            opts <- rjson::fromJSON(json)
            for (name in names(opts)) {
                value <- opts[[name]]
                private$.options[[name]]$value <- value
                private$.env[[name]] <- value
            }
        })
)
