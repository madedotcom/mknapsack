#' @import ROI ROI.plugin.cbc
library(data.table)
library(ROI)
library(ROI.plugin.cbc)

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
#' @param moq vector of flags where 1 means that row contans mininum order quantity (MOQ)
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
optimal_containers <- function(profit, volume, moq, cap = 65, sold = rep(0, length(profit))) {
  res <- rep(NA_integer_, length(profit))
  container <- 0
  ids <- 1:length(profit)

  # force sold product to be in the first container
  profit[sold > 0] <- max(profit * volume) * 10

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

#' Deprecated: Optimal packing into multiple containers
#' @noRd
#' @export
getContainers <- function(profit, volume, moq, cap = 65, sold = rep(0, length(profit))) {
  .Deprecated("optimal_containers")
  optimal_containers(profit, volume, moq, cap, sold)
}

#' Solves knapsack problem with the library defined
#' in KNAPSACK_SOLVE env variable, defaults to lpSolve package.
#' @inherit getContainers
solveKnapsack <- function(profit, volume, moq, cap) {
  do.call(solver(), list(profit = profit,
                         volume = volume,
                         moq = moq,
                         cap = cap))
}

solver <- function() {
  name <- Sys.getenv("KNAPSACK_SOLVE", unset = "cbc")
  get(paste0("solveKnapsack.", name))
}

#' Solve knapsack problem with lpSolve package
#' @noRd
#' @inherit solveKnapsack
solveKnapsack.lpsolve <- function(profit, volume, moq, cap) {

  moq.constraints <- moq_constraint(moq)
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
#' @noRd
#' @inherit solveKnapsack
#' @import rcbc
#' @seealso https://github.com/dirkschumacher/rcbc
solveKnapsack.cbc <- function(profit, volume, moq, cap) {

  n <- length(profit)

  if (sum(volume) <= cap) {
    return(rep(1, n))
  }

  moq.constraints <- moq_constraint(moq)
  moq.lines <- nrow(moq.constraints)

  # CBC solver produces out-of-bound solution if coefs are zero.
  volume[volume == 0] <- 1e-10

  lp <- OP(objective = profit,
           constraints = L_constraint(L = rbind(volume, moq.constraints),
                                      dir = c("<=", rep(">=", moq.lines)),
                                      rhs = c(cap, rep(0, moq.lines))),
           maximum = TRUE,
           types = rep("B", length(volume)))

  mod <- ROI_solve(lp, "cbc", control = list(logLevel = 0, sec = 60))
  res <- mod$solution
  res[is.na(res)] <- 0;
  res <- as.integer(round(res, 0))
  res[res >= 2] <- 0; # Values should be between 0 and 1
  res
}

#' Mininum Order Quantity (MOQ) contstraint generator
#'
#' Creates matrix of moq constraints for the LP optimisation.
#' It is assumed that there is only one moq position per SKU and
#' data is sorted by sku, therefore SKU index can be calculated
#'
#' @param moq flag that indicates that this position contains MOQ
#' @return matrix that expesses the MOQ constraint:
#'   non-MOQ item cannot be put into container that does not contain MOQ item
moq_constraint <- function(moq) {
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
  if (nrow(res) == 0) return(res)
  res <- res[rowSums(res == 0) != ncol(res), ]
  return(res)
}
