
Column <- R6Class("Column",
    private=list(
        .name="",
        .title="",
        .cells=list(),
        .type="",
        .format="",
        .width = 0,
        .measures=list(),
        .measured=FALSE,
        .contentExpr=NA,
        .visibleExpr="TRUE",
        .superTitle=NA,
        .combineBelow=FALSE,
        .options=NULL),
    active=list(
        name=function() private$.name,
        title=function() private$.title,
        cells=function() private$.cells,
        superTitle=function() private$.superTitle,
        hasSuperTitle=function() ( ! is.null(private$.superTitle)),
        width=function() {
            if ( ! private$.measured)
                self$.measure()
            private$.width
        },
        visible=function(value) {
            if (base::missing(value)) {
                v <- private$.options$eval(private$.visibleExpr)
                if (is.logical(v))
                    return(v)
                else
                    return( ! is.null(v))
            }
            private$.visibleExpr <- paste(value)
            base::invisible(self)
        }),
    public=list(
        initialize=function(name, options=Options()) {
            private$.name <- name
            private$.title <- name
            private$.options <- options
            
            private$.measured <- FALSE
            private$.cells <- list()
            private$.superTitle <- NULL
        },
        setTitle=function(title) {
            private$.title <- title
        },
        setSuperTitle=function(title) {
            private$.superTitle <- title
        },
        addCell=function(value, ...) {
            
            if (base::missing(value)) {
                if (is.character(private$.contentExpr))
                    value <- private$.options$eval(private$.contentExpr, ...)
                else
                    value <- NULL
            }
            
            if (inherits(value, "Cell"))
                cell <- value
            else
                cell <- Cell$new(value)
            
            private$.cells[[length(private$.cells)+1]] <- cell
            private$.measured <- FALSE
        },
        setCell=function(row, value) {
            if (row > length(private$.cells))
                stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            cell <- private$.cells[[row]]
            if (is.null(cell))
                stop("no such cell")
            cell$setValue(value)
            private$.measured <- FALSE
        },
        getCell=function(row) {
            if (row > length(private$.cells))
                stop(format("Row '{}' does not exist in the table", row), call.=FALSE)
            
            cell <- private$.cells[[row]]
            if (is.null(cell))
                stop("no such cell")
            cell
        },
        clear=function() {
            private$.cells <- list()
            private$.measured <- FALSE
        },
        .setup=function(def) {
            for (name in names(def)) {
                value <- def[[name]]
                self$.setDef(name, value)
            }
        },
        .setDef=function(name, value) {
            pName <- paste0('.', name)
            if (name == "visible")
                private$.visibleExpr <- value
            else if (name == "content")
                private$.contentExpr <- value
            else if (pName %in% names(private))
                private[[pName]] <- value
            else
                return()
            
            private$.measured <- FALSE
        },
        .measure=function() {
            base::Encoding(private$.title) <- 'UTF-8'
            titleWidth <- nchar(private$.title)
            
            if (private$.type == "integer")
                private$.measures <- silkyMeasureElements(private$.cells, maxdp=0)
            else
                private$.measures <- silkyMeasureElements(private$.cells)
            
            private$.width <- max(private$.measures$width, titleWidth)
            private$.measured <- TRUE
        },
        .titleForPrint=function(width=NULL) {
            base::Encoding(private$.title) <- 'UTF-8'
            if (is.null(width))
                width <- self$width
            w <- nchar(private$.title)
            pad <- spaces(max(0, width - w))
            
            paste0(private$.title, pad)
        },
        .cellForPrint=function(i, measures=NULL, width=NA) {
            if ( ! private$.measured)
                self$.measure()
            
            if (is.null(measures))
                measures <- private$.measures
            
            if ( ! is.na(width))
                measures$width <- width
            
            silkyFormatElement(private$.cells[[i]],
                w=measures$width,
                dp=measures$dp,
                sf=measures$sf,
                expw=measures$expwidth,
                supw=measures$supwidth)
        },
        asProtoBuf=function() {
            initProtoBuf()
            
            column <- RProtoBuf::new(jmvcoms.ResultsColumn,
                name=private$.name,
                title=private$.title,
                type=private$.type,
                format=private$.format)
            
            if (self$hasSuperTitle)
                column$superTitle <- self$superTitle
            
            column$combineBelow <- private$.combineBelow
            
            for (cell in private$.cells)
                column$add("cells", cell$asProtoBuf())
            
            column
        },
        fromProtoBuf=function(columnPB) {
            if ( ! base::inherits(columnPB, "Message"))
                reject("Cell$fromProtoBuf(): expects a jmvcoms.ResultsColumn")
            
            cellsPB <- columnPB$cells
            
            for (i in seq_along(cellsPB)) {
                cellPB <- cellsPB[i]
                cell <- getCell(i)
                cell$fromProtoBuf(cellPB)
            }
        }
    )
)
