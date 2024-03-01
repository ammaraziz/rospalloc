library(testthat)
source("../../aliasor.R")


# Define tests for uncompression
test_that("Test uncompression", {
    aliasor <- Aliasor()
    expect_equal(aliasor$uncompress("BA.1"), "B.1.1.529.1")
    expect_equal(aliasor$uncompress("AY.4"), "B.1.617.2.4")
    expect_equal(aliasor$uncompress("AY.4.3.2"), "B.1.617.2.4.3.2")
    expect_equal(aliasor$uncompress("B.1"), "B.1")
    expect_equal(aliasor$uncompress("B"), "B")
    expect_equal(aliasor$uncompress(""), "")
})

# Define tests for compression
test_that("Test compression", {
    aliasor <- Aliasor()
    expect_equal(aliasor$compress("B.1.1.529.1"), "BA.1")
    expect_equal(aliasor$compress("B.1.617.2.4"), "AY.4")
    expect_equal(aliasor$compress("B.1.617.2.4.3.1"), "AY.4.3.1")
    expect_equal(aliasor$compress("B.1.617.2"), "B.1.617.2")
    expect_equal(aliasor$compress("B.1"), "B.1")
    expect_equal(aliasor$compress("B"), "B")
    expect_equal(aliasor$compress(""), "")
})

# Define tests for except recombinants
test_that("Test except recombinants", {
    aliasor <- Aliasor()
    expect_equal(aliasor$uncompress("XA.1"), "XA.1")
    expect_equal(aliasor$compress("XA.1"), "XA.1")
})

# Define tests for double alias compression
test_that("Test double alias compression", {
    aliasor <- Aliasor()
    expect_equal(aliasor$compress("B.1.1.529.5.3.1.1"), "BE.1")
})

# Define tests for double alias uncompression
test_that("Test double alias uncompression", {
    aliasor <- Aliasor()
    expect_equal(aliasor$uncompress("BE.1"), "B.1.1.529.5.3.1.1")
})

# Define tests for reading from file
# test_that("Test read from file", {
#     aliasor <- Aliasor("path/to/alias_key.json")
#     expect_equal(aliasor$compress("B.1.1.529.1"), "BA.1")
# })

# Define tests for partial alias up to
test_that("Test partial alias up to", {
    aliasor <- Aliasor()
    expect_equal(aliasor$partial_compress("B.1.1.529.1.2", up_to = 0), "B.1.1.529.1.2")
    expect_equal(aliasor$partial_compress("B.1.1.529.2.75.1.2", up_to = 1), "BA.2.75.1.2")
    expect_equal(aliasor$partial_compress("B.1.1.529.2.75.1.2", up_to = 2), "BL.2")
})

# Define tests for partial alias accepted
test_that("Test partial alias accepted", {
    aliasor <- Aliasor()
    expect_equal(aliasor$partial_compress("B.1.1.529.1.2", accepted_aliases = c("BA","AZ")), "BA.1.2")
    expect_equal(aliasor$partial_compress("B.1.617.2.3", accepted_aliases = c("BA","AZ")), "B.1.617.2.3")
    expect_equal(aliasor$partial_compress("B.1.1.529.2.75.1.2", accepted_aliases = c("BA")), "BA.2.75.1.2")
    expect_equal(aliasor$partial_compress("B", accepted_aliases = c("BA")), "B")
})

# Define tests for partial alias combination
test_that("Test partial alias combination", {
    aliasor <- Aliasor()
    expect_equal(aliasor$partial_compress("B.1.1.529.1.2", up_to=1, accepted_aliases=c("BA","AZ")), "BA.1.2")
    expect_equal(aliasor$partial_compress("B.1.617.2.3", up_to=1, accepted_aliases=c("BA","AZ")), "AY.3")
    expect_equal(aliasor$partial_compress("B.1.1.529.2.75.1.2", up_to=3, accepted_aliases=c("BA")), "BL.2")
    expect_equal(aliasor$partial_compress("B.1.1.529.2.75.1.2", up_to=4, accepted_aliases=c("BA")), "BL.2")
    expect_equal(aliasor$partial_compress("B.1.1.529.2.75.1.2", up_to=1, accepted_aliases=c("BA")), "BA.2.75.1.2")
})

# Define tests for parent
test_that("Test parent", {
    aliasor <- Aliasor()
    expect_equal(aliasor$parent("B.1.1.529.1"), "B.1.1.529")
    expect_equal(aliasor$parent("BQ.1"), "BE.1.1.1")
    expect_equal(aliasor$parent("XAA"), "")
    expect_equal(aliasor$parent(""), "")
    expect_equal(aliasor$parent("A"), "")
    expect_equal(aliasor$parent("B"), "")
    expect_equal(aliasor$parent("C.1"), "B.1.1.1")
})

