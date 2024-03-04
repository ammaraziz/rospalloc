library(testthat)

# Define tests for collapse
test_that("Test collapse", {
    expect_equal(collapse("BA.5.1", c("BA.5", "B")), "BA.5")
})

# Define tests for collapse_column
test_that("Test collapse_column", {
    result <- collapse_column(c("BA.5", "BA.2"), c("B"))
    expect_equal(result, c("B", "B"))
})

# Define tests for uncompress_column
test_that("Test uncompress_column", {
    result <- uncompress_column(c("BA.5", "B"))
    expect_equal(result, c("B.1.1.529.5", "B"))
})

# Define tests for collapse_recombinants
test_that("Test collapse_recombinants", {
    result <- collapse_column(c("XBB.1.5.13", "EL.1", "XBF.7.1"), c("Recombinant", "XBF.7"))
    expect_equal(result, c("Recombinant", "Recombinant", "XBF.7"))
})

# Define tests for recombinant_sublineage
test_that("Test recombinant_sublineage", {
    compressed_lineage <- "EL.1"
    result <- collapse(compressed_lineage, c("XBB.1"))
    expect_equal(result, "XBB.1")
})

# Define tests for expand
test_that("Test expand", {
    expect_equal(expand("CH.1.1.17"), "B.1.1.529:BA.2.75.3:BM.4.1.1:CH.1.1.17")
    expect_equal(expand("CH.1.1"), "B.1.1.529:BA.2.75.3:BM.4.1.1:CH.1.1")
    expect_equal(expand("B.1.1.529.5"), "B.1.1.529:BA.5")
    expect_equal(expand("EG.1"), "XBB.1.9.2:EG.1")
})

# Define tests for expand_column
test_that("Test expand_column", {
    result <- expand_column(c("CH.1.1", "CH.1.1.17"))
    expect_equal(result, c("B.1.1.529:BA.2.75.3:BM.4.1.1:CH.1.1", "B.1.1.529:BA.2.75.3:BM.4.1.1:CH.1.1.17"))
})
