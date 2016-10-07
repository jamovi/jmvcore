
State <- R6Class(
    "State",
    private=list(
        .options=NA,
        .optionsLastTime=NA,
        .childrenExpr=character(),
        .childrenNames=NA,
        .children=NA,
        .values=NA,
        .updated=FALSE,
        .version=0),
    public=list(
        initialize=function(version=0, options=NULL, conn=NULL) {
            
            private$.version <- version
            
            if (is.null(options))
                options <- Options$new()
            private$.options <- options
            
            private$.childrenNames <- list()
            private$.children <- list()
            private$.values <- list()
        },
        insertChild=function(i, name, child) {
            private$.childrenNames[[i]] <- name
            private$.children[[i]] <- child
        },
        removeChild=function(name) {
            for (i in seq_along(private$.childrenNames)) {
                if (base::identical(name, private$.childrenNames[[i]])) {
                    private$.childrenNames[[i]] <- NULL
                    private$.children[[i]] <- NULL
                    break()
                }
            }
        },
        hasChild=function(name) {
            for (childName in private$.childrenNames) {
                if (base::identical(name, childName))
                    return(TRUE)
            }
            FALSE
        },
        getChild=function(name) {
            for (i in seq_along(private$.childrenNames)) {
                if (base::identical(name, private$.childrenNames[[i]]))
                    return(private$.children[[i]])
            }
            NULL
        },
        getValue=function(name) {
            private$.values[[name]]
        },
        setValue=function(name, value) {
            private$.values[name] <- list(value)  # NULL friendly
        },
        hasValue=function(name) {
            name %in% names(private$.values)
        },
        removeValue=function(name) {
            private$.values[[name]] <- NULL
        },
        .serialize=function(conn=NULL) {
            base::serialize(self$.toList(), connection=conn)
        },
        .deserialize=function(conn) {
            v <- base::unserialize(connection=conn)
            self$.fromList(v)
            invisible(self)
        },
        .toList=function() {
            
            s <- list(children=list())
            for (i in seq_along(private$.children))
                s$children[[i]] <- private$.children[[i]]$.toList()
            s$childrenNames <- private$.childrenNames
            s$values <- private$.values
            
            s
        },
        .fromList=function(s) {
            
            private$.children <- list()
            private$.childrenNames <- s$childrenNames
            private$.values <- s$values
            
            for (i in seq_along(s$children)) {
                child <- State$new()
                child$.fromList(s$children[[i]])
                private$.children[[i]] <- child
            }
            
            invisible(self)
        },
        .setChildrenExpr=function(expr) {
            private$.childrenExpr <- expr
            private$.updated <- FALSE
        },
        .setup=function(def) {
            for (name in names(def)) {
                value <- def[[name]]
                self$.setDef(name, value)
            }
        },
        .has=function(name) {
            paste0(".", name) %in% names(private)
        },
        .setDef=function(name, value) {
            if (self$.has(name)) {
                private[[paste0(".", name)]] <- value
                private$.updated <- FALSE
            }
        },
        .update=function() {
            if (private$.updated)
                return()
            
            oldChildren <- private$.children
            oldChildrenNames <- private$.childrenNames
            
            newChildren <- list()
            newChildrenNames <- private$.options$eval(private$.childrenExpr)
            
            for (i in seq_along(newChildrenNames)) {
                
                newChildName <- newChildrenNames[[i]]
                newChild <- NA
                
                for (j in seq_along(private$.childrenNames)) {
                    
                    oldChildName <- oldChildrenNames[[j]]
                    
                    if (base::identical(oldChildName, newChildName)) {
                        newChild <- private$.children[[j]]
                        break()
                    }
                }
                
                if (is.na(newChild))
                    newChild <- State$new(private$.options)
                
                newChildren[[i]] <- newChild
            }
            
            private$.children <- newChildren
            private$.childrenNames <- newChildrenNames
            
            .updated <- TRUE
        }
    ))
