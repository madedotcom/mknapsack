[![Build Status](https://travis-ci.org/madedotcom/mknapsack.svg?branch=master)](https://travis-ci.org/madedotcom/mknapsack)
[![codecov.io](https://codecov.io/github/madedotcom/mknapsack/coverage.svg?branch=master)](https://codecov.io/github/madedotcom/mknapsack?branch=master)


# mknapsack

This package assigns items optimally to containers using Mixed Integer Linear Programming (MILP) solver of your choice.


## Definition of the mknapsack problem
We start with a list of items that we want to order with each assigned a:
  
*  sku - this is an id of the product / item that we want to order. 
*  profit - expected profit from sales of this item
*  volume - this can be m3 of the box for example 
*  moq - mininum order quanity (MOQ)
*  sold - flag that defines if this item must be added as highest priority prior to othe items

Those items should be optimally packed into multiple containers of the a given size (cap). 
Items should be aded to containers in the way that each container is more profitable than the following one.

## Supported solvers

Package implements interface to several solvers which can be set via `mknapsack.solver` option.

Currently you can choose from those options:

* lpsolve - [lp_solve](http://web.mit.edu/lpsolve/doc/)
* cbc - [CBC COIN-OR](https://projects.coin-or.org/Cbc)
* glpk - [GLPK (GNU Linear Programming Kit)](https://www.gnu.org/software/glpk/)

`lpsolve` is default option.

## Example

Solve problem with [CBC COIN-OR](https://projects.coin-or.org/Cbc) solver:

```R
devtools::install_github("dirkschumacher/rcbc")
devtools::install_github("dirkschumacher/ROI.plugin.cbc")
devtools::install_github("byapparov/mknapsack")
library(rcbc)
library(ROI)
library(ROI.plugin.cbc)
library(mknapsack)
options(mknapsack.solver = "cbc")

data(unitsbro)
library(data.table)
units.combined <- data.table(unitsbro)
moq <- units.combined$moq
profit <- units.combined$utility
volume <- units.combined$volume
res <- mknapsack(profit, volume, moq, 65)
units.combined$container <- as.factor(res)

#Aggregate solution to container
containers <- units.combined[order(container), .(volume = sum(volume), profit = sum(profit)), 
                              by = container]
                              
containers

#    container volume  profit
# 1:         1   65.0 18229.3
# 2:         2   65.0 12774.1
# 3:         3   65.0  9598.3
# 4:         4   65.0  8310.2
# 5:         5   65.0  6410.5
# 6:         6   65.0  4132.8
# 7:         7   65.0  4345.5
# 8:         8   43.8   639.9
# 9:         9   42.3  -496.0
# 
```
