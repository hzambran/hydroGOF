##################################################
# 'wNSE': weighted Nash-Sutcliffe Efficiency   #
##################################################
# Started: March 2021                            #
##################################################

# Ref:
# @article{hundecha2012,
#   title = {Exploring the Relationship between Changes in Climate and Floods Using a Model-Based Analysis},
#   author = {Hundecha, Yeshewatesfa and Merz, Bruno},
#   year = {2012},
#   month = apr,
#   volume = {48},
#   pages = {1--21},
#   issn = {0043-1397},
#   doi = {10.1029/2011WR010527},
#   url = {http://www.agu.org/pubs/crossref/2012/2011WR010527.shtml},
#   journal = {Water Resources Research},
#   keywords = {“Germany,climate,flood trend,hydrological model},
#   number = {4}
# }

# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'Result': Modified Nash-sutcliffe Efficiency between 'sim' and 'obs'

wNSE <- function(sim, obs, ...) UseMethod("wNSE")

wNSE.default <- function(sim, obs, na.rm = TRUE, ...) {

   if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")

   vi <- valindex(sim, obs)

   obs <- obs[vi]
   sim <- sim[vi]

   # Testing for zero values in obs, which leads to -Inf as result
   zero.index <- which(obs == 0)
   if (length(zero.index > 0)) {
       warning("'wNSE' can not be computed: some elements in 'obs' are zero !",
               call. = FALSE)
   } # IF end

   denominator <- sum(obs * ((obs - mean(obs))^2))

   if (denominator != 0) {

   wNSE <- 1 - (sum(obs * ((obs - sim)^2)) / denominator)

   } else {
      wNSE <- NA
      warning("'sum( obs * ( ( obs - mean(obs) ) )^2 ) = 0', it is not possible to compute 'wNSE'")
     } # ELSE end

   return(wNSE)

} # 'wNSE.default' end


wNSE.matrix <- function(sim, obs, na.rm = TRUE, ...) {

  # Checking that 'sim' and 'obs' have the same dimensions
  if (all.equal(dim(sim), dim(obs)) != TRUE)
    stop( paste("Invalid argument: dim(sim) != dim(obs) ([",
          paste(dim(sim), collapse = " "), "] != [",
          paste(dim(obs), collapse = " "), "])", sep = ""))

  wNSE <- rep(NA, ncol(obs))

  wNSE <- sapply(1:ncol(obs), function(i, x, y) {
                 wNSE[i] <- wNSE.default(x[, i], y[, i], na.rm = na.rm, ...)
               }, x = sim, y = obs)

  names(wNSE) <- colnames(obs)
  return(wNSE)

} # 'wNSE.matrix' end


wNSE.data.frame <- function(sim, obs, na.rm = TRUE, ...) { 

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wNSE.matrix(sim, obs, na.rm = na.rm, ...)

} # 'wNSE.data.frame' end


################################################################################
wNSE.zoo <- function(sim, obs, na.rm=TRUE, ...) {

    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)

    if (is.matrix(sim) | is.data.frame(sim)) {
       wNSE.matrix(sim, obs, na.rm = na.rm, ...)
    } else NextMethod(sim, obs, na.rm = na.rm, ...)

  } # 'wNSE.zoo' end
