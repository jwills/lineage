# Creates a namespace for tracking the transformations that have
# been applied to a set of LineageVectors
Lineage <- function(existing=list()) {
  .lineage <- existing
  function(lv=NULL, id=NULL) {
    if (is.null(lv)) {
      return(.lineage)
    }
    if (is.null(id)) {
      id <- length(.lineage) + 1
      lv@id <- id
    }
    .lineage[[id]] <<- lv
    lv
  }
}

### LineageVector declarations

setClass("LineageVector",
         representation(id="numeric", parents="numeric", lineage="function",
                        fn="character", args="list"),
         contains="vector")

unit.lv <- function(vec, fn="constant", args=list(),
                    parents=numeric(0), lineage=NULL) {
  lv <- new("LineageVector", vec, parents=parents, 
            fn=fn, args=args,
            lineage=lineage)
  return(lineage(lv))
}
map.lv <- function(fn, vecs, args=list()) {
  lins <- unique(
    lapply(
      Filter(function(v) { inherits(v, "LineageVector") }, vecs),
      function(v) { v@lineage }))
  if (length(lins) != 1) {
    stop("Cannot reconcile parent lineages in map.lv")
  }
  lvecs <- lapply(vecs, function(v) {
    if (inherits(v, "LineageVector")) {
      return(v)
    } else {
      return(unit.lv(v, lineage=lins[[1]]))
    }
  })
  parents <- sapply(lvecs, function(v) { v@id })
  vargs <- lapply(lvecs, as.vector)
  value <- do.call(fn, c(vargs, args))
  unit.lv(value, fn=fn, args=args, parents=parents, lineage=lins[[1]])
}

# Override the core method for displaying LineageVectors so that
# they usually look like regular vectors
setMethod("show", signature("LineageVector"),
          function(object) { show(as.vector(object)) })
setMethod("names", signature("LineageVector"),
          function(x) { attr(x, "names") })
setMethod("names<-", signature("LineageVector", "vector"),
          function(x, value) { attr(x, "names") <- as.character(value); x })

# Pattern for single-arg primitive R functions on LineageVectors
xfunc <- function(name) {
  setMethod(name, signature("LineageVector"),
            function(x) { map.lv(name, list(x))})
}
sapply(c("abs", "log", "log10", "sqrt", "floor", "ceiling",
         "is.na", "!",
         "toupper", "tolower"), xfunc)

# Pattern for binary-arg primitive R functions on LineageVectors
e1e2func <- function(name) {
  setMethod(name, signature("LineageVector", "LineageVector"),
            function(e1, e2) { map.lv(name, list(e1, e2))})
  setMethod(name, signature("vector", "LineageVector"),
            function(e1, e2) { map.lv(name, list(e1, e2))})
  setMethod(name, signature("LineageVector", "vector"),
            function(e1, e2) { map.lv(name, list(e1, e2))})  
}
sapply(c("+", "*", "-", "/", "^", "&", "|",
         "==", "!=", "<", "<=", ">", ">="), e1e2func)

#### LineageDataFrame declarations

lineage <- function(df) {
  lin <- Lineage()
  lvs <- lapply(df, function(x) { unit.lv(x, fn="df", lineage=lin) })
  ldf <- as.data.frame(lvs)
  class(ldf) <- c("LineageDataFrame", "data.frame")
  ldf
}
check.lineage <- function(ldf, v) {
  if (is.null(v)) {
    return()
  }
  if (!inherits(v, "LineageVector")) {
    stop("Only LineageVectors may be assigned to LineageDataFrames")
  }
  if (length(ldf) > 0) {
    lin <- ldf[[1]]@lineage
    if (!identical(lin, v@lineage)) {
      stop("Incompatible lineages in LineageDataFrame assignment")
    }
    if (!identical(lin()[[v@id]], v)) {
      vo <- lin()[[v@id]]
      if (all(as.vector(vo) == as.vector(v))) {
        lin(v, v@id) # Perform an update
      } else {
        stop("Input vector was created with unsupported lineage function")
      }
    }
  }
}
assign("[<-.LineageDataFrame", function (x, i, j, value) {
  check.lineage(x, value)
  d <- as.data.frame(x)
  d[j] <- value
  class(d) <- c("LineageDataFrame", "data.frame")
  d
})
assign("[[<-.LineageDataFrame", function (x, i, j, value) {
  check.lineage(x, value)
  d <- as.data.frame(x)
  d[[i]] <- value
  class(d) <- c("LineageDataFrame", "data.frame")
  d
})
assign("$<-.LineageDataFrame", function(x, name, value) {
  check.lineage(x, value)
  do.call("$<-.data.frame", list(x, name, value))
})
       