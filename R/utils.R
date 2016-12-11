
.SUPCHARS <- c("\u1D43", "\u1D47", "\u1D48", "\u1D49", "\u1DA0", "\u1D4D", "\u02B0", "\u2071",
               "\u02B2", "\u1D4F", "\u02E1", "\u1D50", "\u207F", "\u1D52", "\u1D56", "\u02B3", "\u02E2",
               "\u1D57", "\u1D58", "\u1D5B", "\u02B7", "\u02E3", "\u02B8", "\u1DBB")

ignore <- function(...) {
    if (length(args) > 0)
        warning(paste(paste0("Ignoring argument '", names(list(...)),"'"), collapse='\n'))
}

createError <- function(formats, code=NULL, ...) {

    message <- format(formats[1], ...)
    error <- simpleError(message)

    for (name in names(formats)) {

        if (name != "") {
            message <- format(formats[[name]], ...)
            error[[name]] <- message
        }
    }

    error$code <- code
    error
}

isError <- function(error) {
    base::inherits(error, 'try-error') || base::inherits(error, 'error')
}

tryNaN <- function(expr) {
    result <- try(expr, silent=TRUE)
    if (base::inherits(result, 'try-error'))
        return(NaN)
    result
}

reject <- function(formats, code=NULL, ...) {
    stop(createError(formats, code, ...))
}

stringifyTerm <- function(components) {
    components <- sapply(components, function(x) {
        if (startsWith(x, '`') && endsWith(x, '`'))
            x <- substring(x, 2, nchar(x)-1)
        x
    })
    sep <- ' \u273B '
    base::Encoding(sep) <- 'UTF-8'
    term <- paste(components, collapse=sep)
    term
}

readTextFile <- function(...) {
    ch <- readChar(paste0(..., collapse=''), nchars=1e6)
    Encoding(ch) <- 'UTF-8'
    ch
}

unquote <- function(string) {
    n <- nchar(string)
    if (n < 2)
        return(string)

    if (substring(string, 1, 1) == '"' && substring(string, n, n) == '"')
        string <- substring(string, 2, n-1)

    string
}

columnType <- function(column) {

    if (inherits(column, "ordered")) {
        return("ordinal")
    } else if (inherits(column, "factor")) {
        return("nominal")
    } else {
        return("continuous")
    }
}

columnTypeRDescription <- function(column) {

    if (is.ordered(column)) {
        return("an ordered factor")
    } else if (is.factor(column)) {
        return("a factor")
    } else {
        return("numeric")
    }
}

cap1st <- function(s) {
    paste0(toupper(substring(s,1,1)), substring(s, 2))
}

format <- function(str, ..., context="normal") {

    args <- list(...)

    for (name in names(args)) {
        value <- args[[name]]
        if (grepl("^\\..+", name)) {
            name <- sub(".", "$", name, fixed=TRUE)
            str  <- gsub(name, value[1], str, fixed=TRUE)
        }
    }

    if (grepl("{}", str, fixed=TRUE)) {

        for (token in args)
            str <- sub("{}", stringify(token, context), str, fixed=TRUE)

    } else {

        if (grepl("\\{[0-9]+\\}", str)) {

            i <- 0
            for (token in args) {
                str <- gsub(paste0("{", i, "}"), stringify(token, context), str, fixed=TRUE)
                i <- i + 1
            }

        }
        if (grepl("\\{[a-zA-Z]+\\}", str)) {

            for (name in names(args)) {
                if (name != "" && is.null(args[[name]]) == FALSE) {
                    str <- gsub(paste0("{", name, "}"), stringify(args[[name]], context), str, fixed=TRUE)
                }
            }
        }
    }

    str
}

spaces <- function(n) {
    s <- ''
    if (n > 0)
        s <- paste(rep(' ', n), collapse='')
    s
}

dotPos <- function(x) {
    floor(log10(x))
}

nDigits <- function(x, negSign=TRUE) {

    # calcs no. digits before the decimal point

    n <- 1

    if (x > 1) {
        n <- base::floor(base::log10(x)) + 1
    } else if (x < -1) {
        n <- base::floor(base::log10(abs(x))) + 1
    }

    if (x < 0 && negSign)
        n <- n + 1

    n
}

measureElements <- function(elems, sf=3, maxdp=Inf, scl=1e-3, sch=1e7, type='number', p=FALSE, zto=FALSE) {

    # non-scientific
    dp <- 0
    maxns <- 0   # max
    minns <- 0

    # scientific
    maxexp <- 0  # max scientific exponent
    minexp <- 0  # min scientific exponent
    negman <- FALSE  # are any of the mantissas negative

    # string
    maxstr <- 0

    maxsupwidth <- 0  # max superscripts width

    if (zto) {
        dp <- sf
        maxdp <- sf
        scl <- 0
        sch <- Inf
    }

    for (elem in elems) {

        sups <- integer()

        if (inherits(elem, "Cell")) {
            sups <- elem$sups
            elem <- elem$value
        }

        if (is.null(elem)) {

            maxstr <- max(maxstr, 1)  # width of '.'

        } else if (is.nan(elem)) {

            maxstr <- max(maxstr, 3)  # width of 'NaN'

        } else if (is.na(elem)) {

            # do nothing

        } else if (is.infinite(elem)) {

            maxstr <- max(maxstr, 4)  # width of '-Inf'

        } else if (inherits(elem, "character")) {

            base::Encoding(elem) <- 'UTF-8'
            maxstr <- max(maxstr, nchar(elem))

        } else if ( ! is.numeric(elem)) {

            maxstr <- 2 + nchar(class(elem)[1])

        } else if (p && elem < .001 && elem >= 0.0) {

            maxstr <- max(maxstr, 6)

        } else if (elem == 0) {

            if (is.integer(elem))
                dp <- max(dp, 0)
            else
                dp <- max(dp, sf-1)

        } else if (abs(elem) > scl && abs(elem) < sch) {

            # non-scientific values

            if (is.integer(elem))
                dp <- max(dp, 0)
            else
                dp <- max(dp, (sf - floor(log10(abs(elem))) - 1))

            maxns <- max(maxns, elem)
            minns <- min(minns, elem)

        } else {

            # scientific values

            exp <- floor(log10(abs(elem)))
            man <- elem / (10 ^ exp)

            maxexp <- max(maxexp, exp)
            minexp <- min(minexp, exp)
            if (man < 0)
                negman <- TRUE
        }

        if (length(sups) > 0)
            maxsupwidth <- max(maxsupwidth, 1 + length(sups))
    }

    maxnsw <- nDigits(maxns)
    minnsw <- nDigits(minns)

    nswidth <- max(maxnsw, minnsw)  # non-scientific width

    dp <- min(maxdp, dp)
    if (dp > 0)
        nswidth <- nswidth + 1 + dp # add a decimal point

    swidth <- 0  # scientific width
    expwidth <- 0

    if (maxexp > 0 || minexp < 0) {

        expwidth <- max(nDigits(maxexp), nDigits(minexp, negSign=FALSE)) + 2  # +2 for the e and the sign
        manwidth <- sf + 1  # sf + room for a decimal point
        if (negman)
            manwidth <- manwidth + 1  # add room for a minus sign

        swidth <- manwidth + expwidth
    }

    width <- max(swidth, nswidth, maxstr)
    width <- width + maxsupwidth

    list(sf=sf, dp=dp, width=width, expwidth=expwidth, supwidth=maxsupwidth)
}

formatElement <- function(elem, w=NULL, expw=NULL, supw=0, dp=2, sf=3, scl=1e-3, sch=1e7, type='number', p=FALSE, zto=FALSE) {

    sups <- integer()
    supspad <- ''

    if (inherits(elem, "Cell")) {
        sups <- elem$sups
        elem <- elem$value

        if (is.null(w) == FALSE)
            w <- w - supw
        thissupw <- length(sups)
        if (thissupw > 0)
            thissupw <- thissupw + 1  # add 1 for the space

        supspad <- repstr(' ', supw - thissupw)
    }

    if (is.null(elem)) {

        padstr <- spaces(max(w - 1, 0))
        str <- paste0(".", padstr)

    } else if (is.nan(elem)) {

        padstr <- spaces(max(w - 3, 0))
        str <- paste0(padstr, "NaN")

    } else if (is.na(elem)) {

        if (is.null(w))
            str <- ''
        else
            str <- repstr(' ', w)

    } else if (is.infinite(elem)) {

        if (elem > 0) {
            padstr <- spaces(max(w - 3, 0))
            str <- paste0(padstr, "Inf")
        }
        else {
            padstr <- spaces(max(w - 4, 0))
            str <- paste0(padstr, "-Inf")
        }

    } else if (inherits(elem, "character")) {

        width <- nchar(elem)
        padstr <- spaces(max(w - width, 0))
        if (type == 'number')
            str <- paste0(padstr, elem)
        else
            str <- paste0(elem, padstr)

    } else if ( ! is.numeric(elem)) {

        str <- paste0("[", class(elem)[1], "]")

    } else if (p && elem < .001 && elem >= 0.0) {

        str <- "< .001"

    } else if (elem == 0 || zto || (abs(elem) > scl && abs(elem) < sch)) {

        # non-scientific values
        str <- sprintf(paste0("%", w, ".", dp, "f"), elem)

    } else {

        # scientific values

        exponent <- floor(log10(abs(elem)))

        sign <- ifelse(exponent >= 0, '+', '-')
        mantissa <- elem / (10^exponent)
        exponent <- abs(exponent)

        expstr <- base::format(exponent, scientific=FALSE)

        exppad <- ''
        if ( ! is.null(expw))
            exppad <- spaces(expw-nchar(expstr)-2)  # 1 for the +/-, 1 for the e
        expstr <- paste0('e', exppad, sign, expstr)

        if ( ! is.null(w))
            manstr <- base::formatC(x=mantissa, width=w-nchar(expstr), digits=sf-1, format="f")
        else
            manstr <- base::formatC(x=mantissa, digits=sf-1, format="f")

        str <- paste0(manstr, expstr)
    }

    if (length(sups) > 0)
        str <- paste0(str, ' ', paste(.SUPCHARS[sups+1], collapse=''))
    str <- paste0(str, supspad)

    base::Encoding(str) <- 'UTF-8'

    str
}

repstr <- function(value, n, join='') {
    if (n > 0)
        return(paste(rep(value, n), collapse=join))
    else
        return('')
}

stringify <- function(value, context="normal") {

    if (context == "R") {

        if (is.null(value))
            return("NULL")
        else
            return(paste0(value))

    } else {

        if (is.null(value))
            return("null")
        else if (identical(value, TRUE))
            return("true")
        else if (identical(value, FALSE))
            return("false")
        else
            return(paste0(value))
    }
}

constructFormula <- function(dep=NULL, terms) {
    rhItems <- list()
    for (term in terms)
        rhItems[[length(rhItems)+1]] <- paste0('`', term, '`', collapse=":")
    rhs <- paste0(rhItems, collapse='+')
    if ( ! is.null(dep)) {
        lhs <- paste0('`', dep, '`')
        formulaStr <- paste0(lhs, '~', rhs)
    } else {
        formulaStr <- rhs
    }
    formulaStr
}

startsWith <- function(x, prefix) {
    return (substring(x, 1, 1) == prefix)
}

endsWith <- function(x, suffix) {
    return (substring(x, nchar(x)) == suffix)
}

isSame <- function(i1, i2) {
    if (is.list(i1) && is.list(i2)) {
        if (base::identical(i1, i2))
            return(TRUE)
        n1 <- sort(names(i1))
        n2 <- sort(names(i2))
        if ( ! base::identical(n1, n2))
            return(FALSE)

        for (n in n1) {
            if ( ! base::identical(i1[[n]], i2[[n]]))
                return(FALSE)
        }

        return(TRUE)

    } else if (base::identical(i1, i2)) {
        return(TRUE)
    }
    FALSE
}

indexOf <- function(item, array) {
    if (is.list(item)) {
        for (i in seq_along(array)) {
            comp <- array[[i]]
            if (isSame(item, comp))
                return(i)
        }
    } else {
        for (i in seq_along(array)) {
            if (base::identical(array[[i]], item))
                return(i)
        }
    }
    NA
}

extractErrorMessage <- function(error) {

    split <- base::strsplit(as.character(error), ":")[[1]]
    last <- split[[length(split)]]
    base::trimws(last)
}

rethrow <- function(error) {

    message <- extractErrorMessage(error)
    stop(message, call.=FALSE)
}

sourcify <- function(object, indent='') {

    if (is.null(object)) {

        return('NULL')
    }
    if (is.logical(object)) {
        if (length(object) == 0)
            return('logical()')
        if (length(object) == 1)
            return(paste0(object))

        source <- 'c('
        sep <- ''

        for (item in object) {
            source <- paste0(source, sep, item)
            sep=', '
        }
        source <- paste0(source, ')')

        return(source)
    }
    if (is.numeric(object)) {

        if (length(object) == 0)
            return('numeric()')
        if (length(object) == 1)
            return(paste(object))

        source <- 'c('
        sep <- ''

        for (item in object) {
            source <- paste0(source, sep, item)
            sep=', '
        }
        source <- paste0(source, ')')

        if (nchar(source) > 40)
            source <- gsub(', ', paste0(',\n    ', indent), source, fixed=TRUE)

        return(source)

    } else if (is.character(object)) {

        if (length(object) == 0)
            return('character()')
        if (length(object) == 1)
            return(paste0('"', object, '"'))

        source <- 'c('
        sep <- ''

        for (item in object) {
            source <- paste0(source, sep, '"', item, '"')
            sep=', '
        }
        source <- paste0(source, ')')

        if (nchar(source) > 40) {
            source <- gsub('c("', paste0('c(\n    ', indent, '"'), source, fixed=TRUE)
            source <- gsub('", ', paste0('",\n    ', indent), source, fixed=TRUE)
        }

        return(source)

    } else if (is.list(object) || is.environment(object)) {

        if (length(object) == 0)
            return('list()')

        indent <- paste0(indent, '    ')
        source <- paste0('list(\n', indent)
        sep <- ''

        nams <- names(object)
        if (is.null(nams)) {

            for (item in object) {
                source <- paste0(source, sep, sourcify(item, indent))
                sep <- paste0(',\n', indent)
            }

        } else {

            for (name in nams) {
                source <- paste0(source, sep, name, '=', sourcify(object[[name]], indent))
                sep <- paste0(',\n', indent)
            }
        }

        source <- paste0(source, ')')
        return(source)
    }

    ''
}
