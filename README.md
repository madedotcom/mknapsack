[![Build Status](https://travis-ci.org/madedotcom/mknapsack.svg?branch=master)](https://travis-ci.org/madedotcom/mknapsack)
[![codecov.io](https://codecov.io/github/madedotcom/mknapsack/coverage.svg?branch=master)](https://codecov.io/github/madedotcom/mknapsack?branch=master)


# mknapsack

This package can assign optimal containers to the list of items using lp_solve or rcbc libraries.


## Definition of the mknapsack problem
We start with a list of items that we want to order with each assigned a:
  
*  sku - this is an id of the product / item that we want to order. 
*  profit - expected profit from sales of this item
*  volume - this can be m3 of the box for example 
*  moq - mininum order quanity (MOQ)
*  sold - flag that defines if this item must be added as highest priority prior to othe items

Those items should be optimally packed into multiple containers of the a given size (cap). 
Items should be aded to containers in the way that each container is more profitable than the following one.
