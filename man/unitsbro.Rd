% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{unitsbro}
\alias{unitsbro}
\title{Real sample of item utility for BRO created in May 2017}
\format{A data frame with rows and variables
\describe{
 \item{sku}{identifier for the product}
 \item{utility}{proxy of the profit that this item delivers to the company if purchased}
 \item{volume}{volume of the item, usually in cubic meters}
 \item{units}{number of untis that this line contains}
 \item{moq}{If equals one, this line contains the minimum order quantity and shoudl be ordered prior to other lines of the same sku}
}}
\usage{
unitsbro
}
\description{
Dataset contains line items with utility and volume and can be used for
exploration of the package functionality.
}
\keyword{datasets}
