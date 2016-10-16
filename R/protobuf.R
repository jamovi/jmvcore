
initProtoBuf <- function() {
    if ( ! base::exists('jamovi.coms.Status')) {
        resultsProtoPath <- system.file("jamovi.proto", package="jmvcore")
        if (resultsProtoPath == "")
            resultsProtoPath <- system.file("inst", "jamovi.proto", package="jmvcore")
        if (resultsProtoPath == "")
            stop("jmvcore jamovi.proto not found!", call.=FALSE)

        RProtoBuf::readProtoFiles(resultsProtoPath)
    }
}
