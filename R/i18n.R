
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
    public=list(
        initialize=function(langDef) {
            if (length(langDef) == 0) {
                private$.table <- list()
            } else {
                private$.table <- langDef$locale_data$messages
            }
        },
        translate=function(text, n=1) {
            if (is.null(text))
                return(text)
            result <- private$.table[[text]]
            if ( ! is.null(result)) {
                result <- result[[1]]
                if (result != '')
                    text <- result
            }
            text
        }
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
