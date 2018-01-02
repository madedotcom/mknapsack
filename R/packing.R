library(data.table)

#' Collapse function for the MOQ items
#'
#' Assigns earliest date to purchase order lines until first MOQ is reached within supplier-warehouse
#' @import data.table
#'
#' @export
#' @param units data.table with following fields: sku, utility, volume, moq
#' @return data.table with sku, utility, volume and units fields. first lines for each sku are grouped according to moq
groupFirstMoq <- function(units) {
  sku <- utility <- cnt <- group <- moq <- volume <- NULL # removes NOTEs in check

  pol <- copy(units)
  pol$units <- 1L
  setorder(pol, sku, -utility)

  # Sort by sku
  setkeyv(pol, c("sku"))
  pol[, cnt := 1:.N, by = sku]

  # Up to moq assign the same group.
  pol[, group := as.integer(ifelse(cnt <= moq, 1, cnt)), by = sku]

  # Aggregate to moq group
  res <- pol[, list(utility = sum(utility), volume = sum(volume), units = sum(units)), by = list(sku, group)]
  res[, moq := ifelse(group == 1, 1L, 0L)]
  res$group <- NULL

  return(res)
}


#' Optimal packing into multiple containers
#'
#' Gets containers based on the utility of individual items, their volume and container size
#' @export
#'
#' @param profit vector with profit for item
#' @param volume vector of item sizes in cubic meters
#' @param moq vector of flags where 1 means that row contans MOQ quantity
#' @param cap size of the container in cubic meters
#' @param sold vector with a number of items that were sold on demand
#' @return vector with container numbers keeping the permutation of the original data
#'
#' @examples
#'
#' # Calculate the optimal containers summary for a sample dataset
#' data(unitsbro)
#' library(data.table)
#' units.combined <- data.table(unitsbro)
#' moq <- units.combined$moq
#' utility <- units.combined$utility
#' volume <- units.combined$volume
#' res <- getContainers(utility, volume, moq, 65)
#' units.combined$container <- as.factor(res)
#' #Aggregate solution to container
#' containers <- units.combined[order(container), .(volume = sum(volume),
#' utility = sum(utility)), by = container]
#'
getContainers <- function(profit, volume, moq, cap = 65, sold = rep(0, length(profit))) {

  res <- rep(0L, length(profit))
  container <- 0
  ids <- 1:length(profit)

  profit[sold > 0] <- 10000 # force sold product to be in the first container

  repeat {
    solution <- solveKnapsack(profit, volume, moq, cap)
    if (sum(solution) == 0) break

    container <- container + 1
    pack <- which(solution > 0) # permutations for current container

    res[ids[pack]] <- container # assign container number to result

    # remove items from current container
    profit <- profit[-pack]
    volume <- volume[-pack]
    moq <- moq[-pack]
    ids <- ids[-pack]

    if (length(profit) == 0) break
  }
  return(res)
}

#' Solves knapsack problem with the library defined
#' in KNAPSACK_SOLVE env variable, defaults to lpSolve package.
#' @inherit getContainers
solveKnapsack <- function(profit, volume, moq, cap) {
  solver <- Sys.getenv("KNAPSACK_SOLVE", unset = "cbc_solve")
  if (solver == "lp_solve") {
    res <- solveKnapsack.LP(profit, volume, moq, cap)
  }
  else if (solver == "cbc_solve") {
    res <- solveKnapsack.CBC(profit, volume, moq, cap)
  }
  return(res)
}

#' Solve knapsack problem with lpSolve package
#'
#' @inherit solveKnapsack
solveKnapsack.LP <- function(profit, volume, moq, cap) {
  moq.constraints <- getMoqConstraint(moq)
  moq.lines <- nrow(moq.constraints)
  mod <- lpSolve::lp(direction = "max",
            objective.in = profit,
            const.mat = rbind(volume, moq.constraints),
            const.dir = c("<=", rep(">=", moq.lines)),
            const.rhs = c(cap, rep(0, moq.lines)),
            all.bin = TRUE)
  res <- mod$solution
  return(res)
}

#' Solve knapsack problem with rcbc package
#' @inherit solveKnapsack
#' @import rcbc
#' @seealso https://github.com/dirkschumacher/rcbc
solveKnapsack.CBC <- function(profit, volume, moq, cap) {
  n <- length(profit)

  if (sum(volume) <= cap) {
    return(rep(1, n))
  }

  moq.constraints <- getMoqConstraint(moq)
  moq.lines <- nrow(moq.constraints)

  # CBC solver produces out-of-bound solution if coefs are zero.
  volume[volume == 0] <- 1e-10

  result <- cbc_solve(
    obj = profit,
    mat = rbind(volume, moq.constraints),
    is_integer = rep.int(TRUE, n),
    row_lb = c(0L, rep(0L, moq.lines)),
    row_ub = c(cap, rep(1L, moq.lines)),
    max = TRUE,
    col_lb = rep.int(0L, n),
    col_ub = rep.int(1L, n),
    cbc_args = list(logLevel = 0, Sec = 60));

  res <- rcbc::column_solution(result)
  res[is.na(res)] <- 0;
  res[res >= 2] <- 0; # Values should be between 0 and 1
  res <- as.integer(round(res, 0))
  return(res)
}

#' MOQ contstraint generator
#'
#' Creates matrix of moq constraints for the LP optimisation.
#' It is assumed that there is only one moq position per SKU and
#' data is sorted by sku, therefore SKU index can be calculated
#'
#' @param moq flag that indicates that this position contains MOQ
#' @return matrix that expesses the MOQ constraint:
#'   non-MOQ item cannot be put into container that does not contain MOQ item
getMoqConstraint <- function(moq) {
  sku <- cumsum(moq)
  res <- matrix(nrow = length(sku), ncol = length(sku))
  for (p in unique(sku)) {
    non.moq <- which(sku == p & moq == 0L)
    non.moq.count <- length(non.moq)
    if (non.moq.count > 0 & sum(moq) > 0) {
      # skips cases where we only have one line for a product that contains MOQ
      res[non.moq, non.moq] <- diag(rep(-1, non.moq.count), nrow = non.moq.count)
    }
    res[which(sku == p), which(sku == p & moq == 1L)] <- 1
  }
  res[is.na(res)] <- 0
  res <- subset(res, subset = moq != 1L)
  if(nrow(res) == 0) return(res)
  res <- res[rowSums(res == 0) != ncol(res), ]
  return(res)
}
