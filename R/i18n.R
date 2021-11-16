
.i18n <- new.env()

#' @export
. <- function(text, n=1) {
    self <- eval.parent(str2lang('self'))
    self$options$translate(text, n)
}

Translator <- R6Class('Translator',
    private=list(
        .table=NA
    ),
    public=c(
        list(
            translate=function(text, n=1) {
                if (is.null(text) || text == '')
                    return(text)
                result <- self$get(text, n)
                if ( ! is.null(result)) {
                    result <- result[[1]]
                    if (result != '')
                        text <- result
                } else {
                    # if not found, there could be context
                    match <- regexec('(.*) \\[(.*)\\]', text)[[1]]
                    if (match != -1) {
                        # separate the text from the context
                        context <- substring(text, match[3], match[3] + attr(match, 'match.length')[3] - 1)
                        text <- substring(text, match[2], match[2] + attr(match, 'match.length')[2] - 1)
                        key <- paste0(context, '\u0004', text)
                        # try context+text
                        result <- self$get(key, n)
                        if ( ! is.null(result)) {
                            result <- result[[1]]
                            if (result != '')
                                text <- result
                        } else {
                            # try text without the context
                            result <- self$get(text)
                            if ( ! is.null(result)) {
                                result <- result[[1]]
                                if (result != '')
                                    text <- result
                            }
                        }
                    }
                }
                text
            }
        ),
        `if`(requireNamespace('fastmap'),
            list(
                initialize=function(langDef) {
                    private$.table <- fastmap::fastmap()
                    if (length(langDef) > 0) {
                        messages <- langDef$locale_data$messages
                        messages <- messages[names(messages) != ""]
                        private$.table$mset(.list=messages)
                    }
                },
                get=function(text, n=1) {
                    private$.table$get(text)
                }
            ),
            list(
                initialize=function(langDef) {
                    if (length(langDef) == 0) {
                        private$.table <- list()
                    } else {
                        private$.table <- langDef$locale_data$messages
                    }
                },
                get=function(text, n=1) {
                    private$.table[[text]]
                }
            )
        )
    )
)

createTranslator <- function(package, code='en') {

    if (package %in% names(.i18n)) {
        packageEnv <- .i18n[[package]]
    } else {
        .i18n[[package]] <- packageEnv <- new.env()
    }

    if (code == 'C')
        code <- 'en'

    code2 = substring(code, 1, 2)

    if (code %in% names(packageEnv)) {
        langDef <- packageEnv[[code]]
    } else if (code2 %in% names(packageEnv)) {
        langDef <- packageEnv[[code2]]
    } else {
        path <- system.file(sprintf('i18n/%s.json', code), package=package)
        path2 <- system.file(sprintf('i18n/%s.json', code2), package=package)
        if (path != '') {
            langDef <- packageEnv[[code]] <- jsonlite::read_json(path)
        } else if (path2 != '') {
            langDef <- packageEnv[[code2]] <- jsonlite::read_json(path2)
        } else {
            langDef <- packageEnv[[code2]] <- list()  # not available
        }
    }

    Translator$new(langDef)
}
