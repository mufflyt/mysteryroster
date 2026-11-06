# --- format_proportion_comparison_prose ---------------------------------

test_that("format_proportion_comparison_prose reports a single proportion correctly", {
  out <- format_proportion_comparison_prose(
    n_a = 204, n_total_a = 1806, label_a = "this cohort",
    unit_label = "NPI-years", condition_label = "had any external corroboration"
  )
  expect_equal(out$a$pct, 11.3)
  expect_equal(out$a$ci_low, 9.9)
  expect_equal(out$a$ci_high, 12.8)
  expect_match(out$sentence_a, "^In this cohort, ")
  expect_match(out$sentence_a, "204 of 1,806 NPI-years had any external corroboration")
  expect_match(out$sentence_a, "11\\.3%")
  expect_match(out$sentence_a, "95% CI 9\\.9-12\\.8%")
  expect_null(out$b)
  expect_null(out$p_value)
  expect_equal(out$combined_text, out$sentence_a)
})

test_that("format_proportion_comparison_prose computes a two-proportion test when n_b is given", {
  out <- format_proportion_comparison_prose(
    n_a = 10, n_total_a = 100, n_b = 40, n_total_b = 100,
    label_a = "before", label_b = "after", unit_label = "cases", condition_label = "were resolved"
  )
  expect_equal(out$a$pct, 10)
  expect_equal(out$b$pct, 40)
  expect_true(is.numeric(out$p_value))
  expect_true(out$p_value < 0.001) # a real, large difference
  expect_true(is.numeric(out$statistic))
  expect_equal(out$df, 1)
  expect_equal(out$diff_pct, 30)
  expect_true(out$diff_ci_low > 0) # after is reliably higher than before
  expect_match(out$comparison_sentence, "10\\.0% of cases were resolved in before \\(10/100\\)")
  expect_match(out$comparison_sentence, "compared with 40\\.0% in after \\(40/100\\)")
  expect_match(out$comparison_sentence, "a difference of 30\\.0 percentage points")
  expect_match(out$comparison_sentence, "chi-squared\\(1, N = 200\\)")
  expect_match(out$comparison_sentence, "p < 0.001")
  expect_match(out$combined_text, "before")
  expect_match(out$combined_text, "after")
})

test_that("format_proportion_comparison_prose reports p as '= x.xxx' when not below 0.001", {
  out <- format_proportion_comparison_prose(
    n_a = 48, n_total_a = 100, n_b = 52, n_total_b = 100
  )
  expect_match(out$comparison_sentence, "p = 0\\.\\d{3}")
})

test_that("format_proportion_comparison_prose's difference CI is signed b minus a", {
  # a < b: difference and its CI should be positive (b higher than a)
  out <- format_proportion_comparison_prose(n_a = 10, n_total_a = 100, n_b = 40, n_total_b = 100)
  expect_equal(out$diff_pct, 30)
  expect_true(out$diff_ci_low < out$diff_ci_high)
  expect_true(out$diff_ci_low > 0)

  # a > b: difference and its CI should be negative (b lower than a)
  out2 <- format_proportion_comparison_prose(n_a = 40, n_total_a = 100, n_b = 10, n_total_b = 100)
  expect_equal(out2$diff_pct, -30)
  expect_true(out2$diff_ci_high < 0)
})

test_that("format_proportion_comparison_prose rejects n_a exceeding n_total_a", {
  expect_error(
    format_proportion_comparison_prose(n_a = 5, n_total_a = 3),
    "n_a cannot exceed n_total_a"
  )
  expect_error(
    format_proportion_comparison_prose(n_a = 1, n_total_a = 10, n_b = 20, n_total_b = 10),
    "n_b cannot exceed n_total_b"
  )
})

test_that("format_proportion_comparison_prose requires both or neither of n_b/n_total_b", {
  expect_error(
    format_proportion_comparison_prose(n_a = 1, n_total_a = 10, n_b = 1),
    "n_total_b"
  )
})

test_that("format_proportion_comparison_prose reproduces the published evidence-ledger numbers exactly", {
  out <- format_proportion_comparison_prose(
    n_a = 691, n_total_a = 3556, n_b = 759, n_total_b = 3556,
    label_a = "the 3-source baseline", label_b = "+ T-MSIS (4 sources)",
    unit_label = "entity-years", condition_label = "were abstained"
  )
  expect_equal(out$a$pct, 19.4)
  expect_equal(out$b$pct, 21.3)
  expect_equal(round(out$p_value, 3), 0.049)
})

# --- attribute_truth_fusion_prose -----------------------------------------

test_that("attribute_truth_fusion_prose reports abstention from a single fit", {
  claims <- data.frame(
    entity_id = c("1", "1", "2", "2"), attribute = "state",
    source_id = c("nppes", "dac", "nppes", "dac"),
    claimed_value = c("CO", "CO", "NY", "NJ"), reference_year = 2024
  )
  fit <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  out <- attribute_truth_fusion_prose(fit, result_label = "this example")
  expect_equal(out$a$n, 1) # entity 2 ties (NY vs NJ), entity 1 doesn't
  expect_equal(out$a$n_total, 2)
  expect_match(out$sentence_a, "In this example, 1 of 2 entity-years were abstained")
  expect_equal(out$method, "majority")
  expect_null(out$reliability_sentence) # majority learns no reliability
})

test_that("attribute_truth_fusion_prose compares two fits via a two-proportion test", {
  claims <- data.frame(
    entity_id = c("1", "1", "2", "2", "3", "3"), attribute = "state",
    source_id = c("nppes", "dac", "nppes", "dac", "nppes", "dac"),
    claimed_value = c("CO", "CO", "NY", "NJ", "TX", "TX"), reference_year = 2024
  )
  fit_a <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  fit_b <- suppressMessages(attribute_truth_fusion(claims[claims$entity_id != "2", ], method = "majority"))
  out <- attribute_truth_fusion_prose(fit_a, compare_to = fit_b,
                                       result_label = "full", compare_to_label = "no tie")
  expect_equal(out$a$n, 1)
  expect_equal(out$b$n, 0)
  expect_true(is.numeric(out$p_value))
  expect_match(out$comparison_sentence, "full")
  expect_match(out$comparison_sentence, "no tie")
})

test_that("attribute_truth_fusion_prose adds a full reliability ranking when source_diagnostics has 2+ sources", {
  fake_fit <- structure(
    list(
      predictions = data.frame(entity_id = c("1", "2"), reference_year = 2024, abstained = c(FALSE, TRUE)),
      candidate_scores = data.frame(),
      source_diagnostics = data.frame(
        source_id = c("nppes", "nppes", "dac", "dac", "op", "op"),
        true_label = c("CO", "NY", "CO", "NY", "CO", "NY"),
        reliability_score = c(0.9, 0.9, 0.4, 0.4, 0.6, 0.6)
      ),
      model_diagnostics = data.frame(method = "dawid_skene"),
      claims = data.frame()
    ),
    class = "mysteryroster_truth"
  )
  out <- attribute_truth_fusion_prose(fake_fit, result_label = "fake")
  expect_match(out$reliability_sentence, "dawid_skene")
  expect_match(out$reliability_sentence, "highest to lowest")
  # all three sources named, in reliability order
  expect_match(out$reliability_sentence, "nppes \\(0\\.900\\), op \\(0\\.600\\), dac \\(0\\.400\\)")
  expect_equal(nrow(out$reliability_table), 3)
  expect_match(out$combined_text, "Ranked by learned source reliability")
})

test_that("attribute_truth_fusion_prose requires a mysteryroster_truth object", {
  expect_error(attribute_truth_fusion_prose(data.frame(x = 1)), "class")
})

test_that("[red-team] attribute_truth_fusion_prose discloses a source excluded from reliability ranking for NA reliability", {
  # Found by a saboteur: a source present in source_diagnostics with NA
  # reliability (e.g. isolated in a disconnected graph component) used to
  # silently vanish from the ranking with no trace -- reading as if only the
  # remaining sources existed, rather than one with indeterminate reliability.
  fake_fit <- structure(
    list(
      predictions = data.frame(entity_id = c("1", "2"), reference_year = 2024, abstained = c(FALSE, TRUE)),
      candidate_scores = data.frame(),
      source_diagnostics = data.frame(
        source_id = c("nppes", "nppes", "dac", "dac", "isolated", "isolated"),
        true_label = c("CO", "NY", "CO", "NY", "CO", "NY"),
        reliability_score = c(0.9, 0.9, 0.4, 0.4, NA, NA)
      ),
      model_diagnostics = data.frame(method = "dawid_skene"),
      claims = data.frame()
    ),
    class = "mysteryroster_truth"
  )
  out <- attribute_truth_fusion_prose(fake_fit, result_label = "fake")
  expect_equal(nrow(out$reliability_table), 2) # only the 2 estimable sources ranked
  expect_match(out$reliability_sentence, "1 source with no estimable reliability \\(isolated\\) excluded")
})

# --- format_ranking_prose/format_proportion_comparison_prose saboteur regressions, shared root cause ---

test_that("[red-team] wilson_ci rejects degenerate conf_level (0 or 1) instead of returning NaN bounds", {
  # Found by a saboteur: conf_level = 1 made qnorm(1) = Inf, propagating NaN
  # all the way into a rendered sentence ("100% CI NaN-NaN%"). conf_level = 0
  # didn't error, but silently returned a zero-width, statistically
  # meaningless interval. Both are now rejected explicitly.
  expect_error(wilson_ci(5, 10, conf_level = 1), "strictly between 0 and 1")
  expect_error(wilson_ci(5, 10, conf_level = 0), "strictly between 0 and 1")
  expect_error(format_proportion_comparison_prose(n_a = 5, n_total_a = 10, conf_level = 1),
               "strictly between 0 and 1")
  expect_error(attribute_lead_lag_duration(
    data.frame(npi = "1", year = 2020:2021, state = c("CO", "CO")),
    data.frame(npi = "1", year = 2020:2021, state = c("NM", "NM")),
    attribute = "state", base_year = 2020L, conf_level = 1
  ), "strictly between 0 and 1")
})

test_that("[red-team] format_proportion_comparison_prose rejects a whitespace-only label", {
  # Found by a saboteur: checkmate::assert_string(x, min.chars = 1) accepts
  # "   " (3 real characters, all whitespace), producing "In   , 10 of..." --
  # grammatically broken with no indication why.
  expect_error(format_proportion_comparison_prose(n_a = 1, n_total_a = 2, label_a = "   "),
               "empty or whitespace-only")
  expect_error(format_proportion_comparison_prose(n_a = 1, n_total_a = 2, unit_label = "  "),
               "empty or whitespace-only")
})

test_that("[red-team] format_proportion_comparison_prose marks a degenerate two-proportion test as not computable throughout, with no negative-zero CI", {
  # Found by a saboteur: 0/1 vs 0/1 makes stats::prop.test() return without
  # erroring, but with p.value/statistic = NaN (not NA) and a conf.int that
  # rendered as a nonsensical "95% CI -0.0 to -0.0%" (IEEE negative zero from
  # -100 * 0) even though the test itself was already flagged not computable.
  r <- format_proportion_comparison_prose(n_a = 0, n_total_a = 1, n_b = 0, n_total_b = 1)
  expect_true(identical(r$p_value, NA_real_)) # not raw NaN
  expect_true(is.na(r$diff_ci_low))
  expect_true(is.na(r$diff_ci_high))
  expect_match(r$comparison_sentence, "CI not computable")
  expect_no_match(r$comparison_sentence, "-0\\.0")
  expect_match(r$comparison_sentence, "p not computable")
})

test_that("[red-team] attribute_truth_fusion_prose discloses when result and compare_to used different methods", {
  # Found by a saboteur: comparing a "majority" fit's abstention rate against
  # a "dawid_skene" fit's gave a clean-looking sentence with zero indication
  # the two methods' abstention means different things (an exact vote tie vs.
  # a posterior below some threshold) -- an apples-to-oranges read a reader
  # has no way to catch from the sentence text alone.
  claims <- data.frame(
    entity_id = c("1", "1", "2", "2"), attribute = "state",
    source_id = c("nppes", "dac", "nppes", "dac"),
    claimed_value = c("CO", "CO", "NY", "NJ"), reference_year = 2024
  )
  fit_a <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  fit_b <- fit_a
  fit_b$model_diagnostics$method <- "dawid_skene"
  out <- attribute_truth_fusion_prose(fit_a, compare_to = fit_b, result_label = "A", compare_to_label = "B")
  expect_match(out$comparison_sentence, "A used `majority` and B used `dawid_skene`")
  expect_match(out$comparison_sentence, "not necessarily comparable across different methods")
  expect_match(out$combined_text, "not necessarily comparable")

  # same method on both sides: no spurious note
  fit_c <- suppressMessages(attribute_truth_fusion(claims[claims$entity_id != "2", ], method = "majority"))
  out2 <- attribute_truth_fusion_prose(fit_a, compare_to = fit_c, result_label = "A", compare_to_label = "C")
  expect_no_match(out2$comparison_sentence, "Note:")
})
