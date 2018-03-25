suppressPackageStartupMessages({
  library(testthat)
  library(data.table)
  library(Rglpk)
  library(rcbc)
  library(ROI.plugin.glpk)
  library(ROI.plugin.cbc)
  library(ROI)
})

test_check("mknapsack")
