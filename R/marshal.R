
marshalData <- function(env, ...) {
    data <- list()
    for (value in list(...)) {
        for (var in value)
            data[[var]] <- env[[var]]
    }
    if (length(unique(vapply(data, length, 0))) > 1)
        reject('Not all columns are the same length')

    as.data.frame(data)
}

