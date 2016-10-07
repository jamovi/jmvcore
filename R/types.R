
select <- function(df, columnNames) {
    
    out <- list()
    for (i in seq_along(columnNames)) {
        columnName <- unlist(columnNames[i])
        if ( ! is.null(df[[columnName]]))
            out[[columnName]] <- df[[columnName]]
    }
    data <- data.frame(out)
    colnames(data) <- names(out)
    data
}

naOmit <- function(object) {
    
    if (is.data.frame(object)) {
        
        attrList <- list()
        for (name in names(object))
            attrList[[name]] <- base::attributes(object[[name]])
        
        object <- stats::na.omit(object)
        
        for (name in names(attrList))
            base::attributes(object[[name]]) <- attrList[[name]]
    }
    else {
        attrs <- base::attributes(object)
        object <- stats::na.omit(object)
        base::attributes(object) <- attrs
    }
    
    object
}

toNumeric <- function(object) {
    if (is.numeric(object))
        return(object)
    
    if ( ! is.null(base::attr(object, "values", TRUE))) {
        values <- base::attr(object, "values", TRUE)
        class(object) <- "factor"
        return(values[as.integer(object)])
    }
    
    object
}

canBeNumeric <- function(object) {
    is.numeric(object) || ! is.null(attr(object, "values", TRUE))
}

