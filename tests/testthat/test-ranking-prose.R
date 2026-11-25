test_that("format_ranking_prose ranks highest to lowest by default, with n shown when given", {
  r <- format_ranking_prose(
    group = c("nppes", "dac_pecos_combined", "open_payments", "tmsis"),
    value = c(100, 77.2, 25.4, 63.4), n = c(2000, 2000, 2000, 2000),
    value_label = "coverage", fmt = "%.1f", suffix = "%"
  )
  expect_equal(r$table$group, c("nppes", "dac_pecos_combined", "tmsis", "open_payments"))
  expect_equal(r$highest$group, "nppes")
  expect_equal(r$highest$value, 100)
  expect_equal(r$lowest$group, "open_payments")
  expect_equal(r$lowest$value, 25.4)
  expect_match(r$sentence, "^Ranked by coverage, highest to lowest: ")
  expect_match(r$sentence, "nppes \\(100\\.0%, n = 2,000\\)")
  expect_match(r$sentence, "open_payments \\(25\\.4%, n = 2,000\\)")
  expect_equal(length(r$ties), 0)
})

test_that("format_ranking_prose supports lowest-to-highest order", {
  r <- format_ranking_prose(group = c("A", "B", "C"), value = c(10, 30, 20), decreasing = FALSE)
  expect_equal(r$table$group, c("A", "C", "B"))
  expect_match(r$sentence, "lowest to highest")
  expect_equal(r$highest$group, "A") # "highest" here means "first in the chosen order"
})

test_that("format_ranking_prose flags a display-value tie without hiding the underlying order", {
  r <- format_ranking_prose(group = c("A", "B", "C"), value = c(10.0, 10.04, 20), fmt = "%.1f", suffix = "%")
  expect_setequal(r$ties, c("A", "B")) # both round to 10.0%
  expect_match(r$sentence, "tied: B, A")
  # still ranked by the real underlying value, not collapsed into one row
  expect_equal(nrow(r$table), 3)
})

test_that("format_ranking_prose does not flag groups whose rounded values differ", {
  r <- format_ranking_prose(group = c("A", "B"), value = c(10.0, 10.9), fmt = "%.1f", suffix = "%")
  expect_equal(length(r$ties), 0)
})

test_that("format_ranking_prose rejects fewer than 2 groups", {
  expect_error(format_ranking_prose(group = "A", value = 10), "length >= 2")
})

test_that("format_ranking_prose's register = 'clinical' produces a narrative sentence with no abbreviations or em dashes", {
  r <- format_ranking_prose(
    group = c("the national provider directory", "the Medicare enrollment address source"),
    value = c(1, 16), value_label = "median years to confirmation", fmt = "%.0f",
    decreasing = FALSE, register = "clinical"
  )
  expect_match(r$sentence, "^Among the 2 groups compared, the lowest median years to confirmation was observed for the national provider directory \\(1\\)\\.")
  expect_match(r$sentence, "This was followed, in order, by the Medicare enrollment address source \\(16\\)\\.")
  expect_false(grepl("—", r$sentence)) # no em dash
  expect_false(grepl("^Ranked by", r$sentence)) # not the technical-register template
  # table/highest/lowest/ties are unaffected by register
  expect_equal(r$highest$group, "the national provider directory")
})

test_that("format_ranking_prose's register = 'clinical' states ties in plain language", {
  r <- format_ranking_prose(group = c("A", "B", "C"), value = c(10.0, 10.04, 20), fmt = "%.1f",
                             suffix = "%", register = "clinical")
  expect_match(r$sentence, "had identical values and are considered tied")
  expect_false(grepl("—", r$sentence))
})

test_that("format_ranking_prose defaults to register = 'technical'", {
  r1 <- format_ranking_prose(group = c("A", "B"), value = c(1, 2))
  r2 <- format_ranking_prose(group = c("A", "B"), value = c(1, 2), register = "technical")
  expect_equal(r1$sentence, r2$sentence)
  expect_match(r1$sentence, "^Ranked by")
})

test_that("format_ranking_prose rejects duplicate group names", {
  expect_error(
    format_ranking_prose(group = c("A", "A", "B"), value = c(1, 2, 3)),
    "duplicate group name"
  )
})

test_that("format_ranking_prose rejects an empty or whitespace-only group name", {
  expect_error(format_ranking_prose(group = c("A", ""), value = c(1, 2)), "empty or whitespace")
  expect_error(format_ranking_prose(group = c("A", "   "), value = c(1, 2)), "empty or whitespace")
})

test_that("format_ranking_prose rejects NA values rather than silently ranking around them", {
  expect_error(
    format_ranking_prose(group = c("A", "B"), value = c(1, NA)),
    "any.missing|missing"
  )
})

test_that("format_ranking_prose rejects mismatched n length", {
  expect_error(
    format_ranking_prose(group = c("A", "B"), value = c(1, 2), n = c(10)),
    "Assertion on 'n' failed|len"
  )
})

test_that("format_ranking_prose handles negative values (e.g. percentage-point differences) without error", {
  r <- format_ranking_prose(group = c("A", "B", "C"), value = c(-5.2, 3.1, -1.0),
                             value_label = "change", fmt = "%.1f", suffix = " pts")
  expect_equal(r$highest$group, "B")
  expect_equal(r$lowest$group, "A")
  expect_match(r$sentence, "A \\(-5\\.2 pts\\)")
})

test_that("[red-team] format_ranking_prose does not pad n with leading spaces when group ns differ widely", {
  # Found by a saboteur: format(c(0, 5000), big.mark = ",") right-aligns to
  # equal width by default ("    0", "5,000"), which leaked into the sentence
  # as "n =     0" -- fixed with trim = TRUE.
  r <- format_ranking_prose(c("A", "B"), c(60, 60), n = c(0, 5000), fmt = "%.1f", suffix = "%")
  expect_match(r$sentence, "A \\(60\\.0%, n = 0\\)")
  expect_no_match(r$sentence, "n =\\s{2,}0")
})

test_that("[red-team] format_ranking_prose rejects Inf/-Inf instead of printing them into a sentence", {
  # Found by a saboteur: an upstream division-by-zero or similar bug could
  # hand this function Inf as a "value" -- it used to rank and print it
  # literally ("A (Inf)") rather than flagging the input as broken.
  expect_error(format_ranking_prose(c("A", "B"), c(Inf, 10)), "finite")
  expect_error(format_ranking_prose(c("A", "B"), c(-Inf, 10)), "finite")
})

test_that("[red-team] format_ranking_prose requires a floating-point fmt, not %d or %s", {
  # Found by a saboteur: fmt = "%s" let a value's full floating-point
  # precision leak into a sentence uncontrolled (e.g. "20.123456789%"),
  # defeating the whole point of the fmt parameter. fmt = "%d" passed an
  # earlier version of this same validation (it IS a numeric conversion
  # character) but then crashed with a cryptic base-R error on any
  # non-integer value, since R's sprintf() requires %d to receive a true
  # integer. Both are now rejected with one clear message naming why.
  expect_error(format_ranking_prose(c("A", "B"), c(10.5, 20.3), fmt = "%s"),
               "floating-point sprintf")
  expect_error(format_ranking_prose(c("A", "B"), c(10.5, 20.3), fmt = "%d"),
               "floating-point sprintf")
  # valid floating-point specifiers still work
  expect_silent(format_ranking_prose(c("A", "B"), c(10.5, 20.3), fmt = "%.3g"))
  expect_silent(format_ranking_prose(c("A", "B"), c(10.5, 20.3), fmt = "%e"))
})
