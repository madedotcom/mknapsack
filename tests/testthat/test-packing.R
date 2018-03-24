library(data.table)

context("Container packing")

# remove false warning related to data.table fields
container <- container.moq <- NULL

#' build the test dataset
units <- fread("unit-utility.txt", header = T)
moqs <- c("A" = 10, "B" = 5, "C" = 2)
moqs <- data.table(sku = names(moqs), moq = moqs)
units <- merge(units, moqs, by = "sku", all.x = TRUE)

test_that("Optimal containers assigned", {

  # Combine data provided at unit level to MOQ level
  units.combined <- groupFirstMoq(units)

  expect_identical(sum(units.combined$units), nrow(units),
                   label = "Check that number of units same as was given")
  expect_identical(nrow(units.combined[sku == "A" & units == 10]), 1L,
                   label = "One line has MOQ number of units")
  expect_identical(nrow(units.combined[sku == "A" & units > 10]), 0L,
                   label = "No records have more units than MOQ")
  expect_identical(nrow(units.combined[sku == "A" & !units %in% c(1L, 10L)]), 0L,
                   label = "All other records have one unit")

  # Get vector of optimal containers
  containers <- getContainers(units.combined$utility, units.combined$volume, units.combined$moq, 65)

  units.combined$container <- containers

  # Check first container
  # See optimal solution example for the first container in acceptance doc:
  # https://docs.google.com/spreadsheets/d/1W_dmJ5j0AOnT6Nea12gpksB7Mp5QrLNsh3q_LZJE8Rs
  expected.utility <- 553
  utility <- units.combined[container == 1, sum(utility)]
  expect_gte(utility, expected.utility)

  volume <- units.combined[container == 1, sum(volume)]
  expected.volume <- 65
  expect_lte(volume, expected.volume)

  # Check second container
  utility <- units.combined[container == 2, sum(utility)]
  volume <- units.combined[container == 2, sum(volume)]
  expect_true(volume >= 56)
  expect_lte(volume, expected.volume)
  expect_gte(utility, 95)

})


test_that("Moq constraint is correct", {
  # Combine data provided at unit level to MOQ level
  units.combined <- groupFirstMoq(units)
  l <- length(units.combined$moq)
  moq.constraint.matrix <- getMoqConstraint(units.combined$moq)
  expect <- matrix(c(1, -1, rep(0, l - 2),
                     1, 0, -1, rep(0, l - 3),
                     1, 0, 0, -1, rep(0, l - 4),
                     0, 0, 0, 0, 1, -1, rep(0, l - 6)),
                   nrow = 4, byrow = T)
  expect_equal(moq.constraint.matrix[1:4, ], expect)

  # Non-moq constraint is a zero matrix
  moq <- c(0, 0, 0)
  moq.constraint.matrix <- getMoqConstraint(moq)
  expect_equal(nrow(moq.constraint.matrix), 0, label = "Empty matrix returned if there are no MOQs")

})

test_that("Item with MOQ is added first", {
  units.combined <- fread("moq-order-test.txt", header = T)
  moq <- units.combined$moq
  profit <- units.combined$utility
  volume <- units.combined$volume
  res <- getContainers(profit, volume, moq, 65)
  units.combined$container <- res

  # Aggregate solution to container
  containers <- units.combined[, .(volume = sum(volume), utility = sum(utility)), by = container]

  # Acceptance checks of the solution
  expect_lte(max(containers$volume), 65, label = "Container volume does not exceed the limit")
  expect_true(all(containers[order(container)] == containers[order(-utility)]),
              label = "Container utility decreases for each new container")

  # Calculating the first container position for each moq/non-moq item
  moq.first <- tapply(res[moq == 1], units.combined$sku[moq == 1], min)
  moq.first <- data.table(sku = names(moq.first), container.moq = moq.first)

  non.moq.first <- tapply(res[moq != 1], units.combined$sku[moq != 1], min)
  non.moq.first <- data.table(sku = names(non.moq.first), container = non.moq.first)

  # Making sure that moq container is less or equal to any non-moq container
  dt.check <- merge(moq.first, non.moq.first, by = "sku")
  expect_equal(nrow(dt.check[container.moq > container]), 0L,
              label = "For products that have more than one line check that MOQ line is added prior to other lines")

})

test_that("One item with small volume can be assigned a container", {
  profit <-  673
  volume <- 1.6
  units <- 33
  moq <- 1
  containers <- getContainers(profit, volume, moq, 65)
  expect_equal(containers, 1)
})

test_that("If sold vector is provided, pre-sold items have high priority", {
  # Combine data provided at unit level to MOQ level
  units.combined <- groupFirstMoq(units)

  # Get vector of optimal containers
  containers <- getContainers(units.combined$utility, units.combined$volume, units.combined$moq, 65)
  expect_true(containers[4] %in% c(3, 4)) # here behaviour of CBC and LP_Solve are different

  # Adding the sold
  units.combined[, sold := 0]
  units.combined[4, sold := 1]
  containers <- getContainers(units.combined$utility, units.combined$volume, units.combined$moq, 65, units.combined$sold)
  expect_equal(containers[4], 1, label = "Pre-sold item was moved to container one.")
})



test_that("Packing works where all lines are MOQ lines", {

  profit = c(1207, 4268, 2055, -394)
  volume = c(115.2, 115.2, 31.7625, 31.7625)
  units = c(300L, 300L, 300L, 300L)
  moq = c(1L, 1L, 1L, 1L)

  res <- getMoqConstraint(moq)

  expect_equal(nrow(res), 0, label = "MOQ constraint is empty.")

  # TODO we should make sure that products with MOQ over a full container are processed correctly.
  containers <- getContainers(profit, volume, moq, 65)

})
