
RProtoBuf_new <- if (requireNamespace('RProtoBuf')) RProtoBuf::new
RProtoBuf_serialize <- if (requireNamespace('RProtoBuf')) RProtoBuf::serialize
RProtoBuf_read <- if (requireNamespace('RProtoBuf')) RProtoBuf::read

initProtoBuf <- function() {
    if ( ! exists('jamovi.coms.Status')) {
        resultsProtoPath <- system.file("jamovi.proto", package="jmvcore")
        if (resultsProtoPath == "")
            resultsProtoPath <- system.file("inst", "jamovi.proto", package="jmvcore")
        if (resultsProtoPath == "")
            stop("jmvcore jamovi.proto not found!", call.=FALSE)

        if (requireNamespace('RProtoBuf'))
            RProtoBuf::readProtoFiles(resultsProtoPath)
    }
}
