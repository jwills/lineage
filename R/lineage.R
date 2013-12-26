library(RProtoBuf)
readProtoFiles("proto//lineage.proto")

setClass("Lineage", representation(proto="Message"))
Lineage <- function(existing=NULL) {
  if (is.null(existing)) {
    existing <- new(lineage.LineageProto)
  }
  return(new("Lineage", proto=existing))
}
newvec.Lineage <- function(lin, name=NULL, parents=integer(), cl=NULL) {
  id <- as.integer(length(lin@proto$vec) + 1) 
  ret <- new(lineage.VectorProto, id=id, name=name,
             parent_vec_id=parents, call=cl)
  lin@proto$vec[[id]] <- ret
  return(ret)
}
filterby.Lineage <- function(lin, vec) {
  proto <- lin@proto$clone()
  id <- length(proto$filter_vec_id) + 1
  proto$filter_vec_id[[id]] <- vec@proto$id
  return(Lineage(existing=proto))
}

### LineageVector declarations

setClass("AbstractLineageVector",
         representation(proto="Message", lineage="Lineage"))
setClass("LineageVector", contains=c("vector", "AbstractLineageVector"))
setClass("LineageFactor", contains=c("factor", "AbstractLineageVector"))

to.Args <- function(args) {
  if (length(args) == 0) {
    return(NULL)
  }
  lapply(1:length(args), function(i) {
    ret <- new(lineage.Arg)
    v <- args[[i]]
    if (is.character(v)) {
      ret$str_value <- v
    } else if (is.numeric(v)) {
      ret$num_value <- v
    } else if (is.list(v)) {
      ret$list_value <- to.Args(v)
    }
    if (!is.null(names(args)[i])) {
      ret$name <- names(args)[i]
    }
    return(ret)
  })
}

to.Call <- function(fn, args, var=NULL) {
  return(new(lineage.Call, fn=fn, arg=to.Args(args), assign_to_var=var))
}

unit.lv <- function(vec, lineage, fn=NULL, args=list(),
                    parents=integer(), name=NULL) {
  cl <- NULL
  if (!is.null(fn)) {
    cl <- to.Call(fn, args)
  } else if (length(vec) == 1) {
    cl <- to.Call("constant", list(vec))
  }
  proto <- newvec.Lineage(lineage, name=name, parents=parents, cl=cl)
  cls <- ifelse(inherits(vec, "factor"), "LineageFactor", "LineageVector")
  return(new(cls, vec, proto=proto, lineage=lineage))
}

map.lv <- function(fn, vecs, args=list(), name=NULL) {
  lins <- unique(
    lapply(
      Filter(function(v) { inherits(v, "AbstractLineageVector") }, vecs),
      function(v) { v@lineage }))
  if (length(lins) != 1) {
    stop("Cannot reconcile parent lineages in map.lv")
  }
  lvecs <- lapply(vecs, function(v) {
    if (inherits(v, "AbstractLineageVector")) {
      return(v)
    } else {
      return(unit.lv(v, lins[[1]]))
    }
  })
  parents <- sapply(lvecs, function(v) { v@proto$id })
  vargs <- lapply(lvecs, as.vector)
  value <- do.call(fn, c(vargs, args))
  unit.lv(value, lins[[1]], fn=fn, args=args, parents=parents, name=name)
}

update.lv <- function(v, field, value) {
  v@proto[[field]] <- value
  v@lineage@proto$vec[[v@proto$id]] <- v@proto
  v
}

# Override the core method for displaying LineageVectors so that
# they usually look like regular vectors
setMethod("show", signature("AbstractLineageVector"),
          function(object) {
            if (inherits(object, "LineageFactor")) {
              show(as(object, "factor"))
            } else {
              show(as.vector(object))
            }
          })
setMethod("print", signature("AbstractLineageVector"),
          function(x) {
            if(inherits(x, "LineageFactor")) {
              print(factor(as.character(x))) 
            } else {
              print(as.vector(x))
            }
          })
setMethod("names", signature("AbstractLineageVector"),
          function(x) { attr(x, "names") })
setMethod("names<-", signature("AbstractLineageVector", "vector"),
          function(x, value) { attr(x, "names") <- as.character(value); x })

# Pattern for single-arg primitive R functions on LineageVectors
xfunc <- function(name, cls="LineageVector") {
  setMethod(name, signature(cls), function(x) { map.lv(name, list(x))})
}
sapply(c("abs", "log", "log10", "exp", "sqrt", "floor", "ceiling",
         "is.na", "!", "tolower", "toupper"), xfunc)
sapply(c("tolower", "toupper"), xfunc, cls="LineageFactor")

# Experimenting with if-else methods
setMethod("ifelse", signature("LineageVector", "vector", "vector"),
          function(test, yes, no) {
            map.lv("ifelse", list(test, yes, no))
          })

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
  ldf <- as.data.frame(lapply(df, function(x) {
    unit.lv(x, lin)
  }))
  lapply(1:ncol(df), function(i) {
    update.lv(ldf[[i]], "name", names(df)[i])
  })
  class(ldf) <- c("LineageDataFrame", "data.frame")
  ldf
}
show.lineage <- function(v) {
  writeLines(v@lineage@proto$toString())
}
check.lineage <- function(ldf, v) {
  if (is.null(v)) {
    return()
  }
  if (!inherits(v, "AbstractLineageVector")) {
    stop("Only lineage vectors may be assigned to lineage data.frames")
  }
  if (length(ldf) > 0) {
    lin <- ldf[[1]]@lineage
    if (!identical(lin, v@lineage)) {
      stop("Incompatible lineages in lineage data.frame assignment")
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
  update.lv(value, "name", name)
  do.call("$<-.data.frame", list(x, name, value))
})
assign("names<-.LineageDataFrame", function(x, value) {
  lapply(1:ncol(x), function(i) {
    update.lv(x[[i]], "name", value[i])
  })
  d <- as.data.frame(x)
  names(d) <- value
  class(d) <- c("LineageDataFrame", "data.frame")
  d  
})
       