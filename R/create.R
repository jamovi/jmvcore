
#' Create an analysis
#'
#' Used internally by jamovi
#'
#' @export
create <- function(ns, name, optionsPB, datasetId, analysisId, revision) {

    analysis <- tryStack({
        options <- eval(parse(text=format('options <- {}::{}Options$new()\n', ns, name)))
        options$read(optionsPB)

        analysis <- eval(parse(text=format(
            '{}::{}Class$new(
            options=options,
            datasetId=datasetId,
            analysisId=analysisId,
            revision=revision)', ns, name)))
    })

    if (isError(analysis))
        analysis <- createErrorAnalysis(
            format("Could not create analysis: {}::{}", ns, name),
            attr(analysis, 'stack'),
            ns,
            name,
            datasetId,
            analysisId,
            revision)

    analysis
}

createErrorAnalysis <- function(error, stack, ns, name, datasetId, analysisId, revision) {
    options <- Options$new()
    analysis <- Analysis$new(
        package=ns,
        name=name,
        version=c(0,0,0),
        options=options,
        results=Group$new(
            options=options,
            name=name,
            title=name),
        datasetId=datasetId,
        analysisId=analysisId,
        revision=revision)
    analysis$setError(error, stack)
    analysis
}
