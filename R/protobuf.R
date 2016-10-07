
initProtoBuf <- function() {
    if ( ! base::exists('jmvcoms.Status')) {
        resultsProtoPath <- system.file("enginecoms.proto", package="jmvcore")
        if (resultsProtoPath == "")
            resultsProtoPath <- system.file("inst", "enginecoms.proto", package="jmvcore")
        if (resultsProtoPath == "")
            stop("jmvcore enginecoms.proto not found!", call.=FALSE)

        RProtoBuf::readProtoFiles(resultsProtoPath)
    }
}
