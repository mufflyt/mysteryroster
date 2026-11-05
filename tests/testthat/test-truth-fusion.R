# --- attribute_claims_validate ----------------------------------------------

test_that("attribute_claims_validate reports duplicates and multi-value source-years separately", {
  claims <- data.frame(
    entity_id = c("1", "1", "1", "2"), attribute = "state",
    source_id = c("nppes", "dac", "dac", "nppes"),
    claimed_value = c("CO", "CO", "NM", "NY"), reference_year = 2024
  )
  validated <- suppressMessages(attribute_claims_validate(claims))
  report <- attr(validated, "report")
  expect_equal(report$n[report$check == "rows"], 4)
  expect_equal(report$n[report$check == "duplicate_source_years"], 1)
  expect_equal(report$n[report$check == "multi_value_source_years"], 1)
  expect_equal(nrow(validated), 4) # nothing dropped
})

test_that("attribute_claims_validate distinguishes an exact duplicate from a multi-value claim", {
  exact_dup <- data.frame(
    entity_id = "1", attribute = "state", source_id = "dac",
    claimed_value = c("CO", "CO"), reference_year = 2024
  )
  r1 <- attr(suppressMessages(attribute_claims_validate(exact_dup)), "report")
  expect_equal(r1$n[r1$check == "duplicate_source_years"], 1)
  expect_equal(r1$n[r1$check == "multi_value_source_years"], 0)

  multi_value <- data.frame(
    entity_id = "1", attribute = "state", source_id = "dac",
    claimed_value = c("CO", "NM"), reference_year = 2024
  )
  r2 <- attr(suppressMessages(attribute_claims_validate(multi_value)), "report")
  expect_equal(r2$n[r2$check == "duplicate_source_years"], 1)
  expect_equal(r2$n[r2$check == "multi_value_source_years"], 1)
})

test_that("attribute_claims_validate keeps rows with a missing claimed_value, doesn't treat as negative", {
  claims <- data.frame(
    entity_id = c("1", "2"), attribute = "state", source_id = c("nppes", "dac"),
    claimed_value = c("CO", NA), reference_year = 2024
  )
  validated <- suppressMessages(attribute_claims_validate(claims))
  expect_equal(nrow(validated), 2) # the NA row is kept, not dropped
  expect_equal(attr(validated, "report")$n[attr(validated, "report")$check == "missing_claimed_value"], 1)
})

test_that("attribute_claims_validate flags invalid intervals and respects on_invalid_interval", {
  claims <- data.frame(
    entity_id = "1", attribute = "state", source_id = "dac", claimed_value = "CO",
    reference_year = 2024, valid_from = as.Date("2024-06-01"), valid_to = as.Date("2024-01-01")
  )
  flagged <- suppressMessages(attribute_claims_validate(claims))
  expect_equal(attr(flagged, "report")$n[attr(flagged, "report")$check == "invalid_interval"], 1)
  expect_error(attribute_claims_validate(claims, on_invalid_interval = "error"), "valid_to before valid_from")
})

test_that("attribute_claims_validate's on_duplicate = 'error' stops the call", {
  claims <- data.frame(
    entity_id = "1", attribute = "state", source_id = "dac",
    claimed_value = c("CO", "CO"), reference_year = 2024
  )
  expect_error(attribute_claims_validate(claims, on_duplicate = "error"), "duplicate rows")
})

test_that("attribute_claims_validate fails closed on missing columns and missing identifiers", {
  expect_error(attribute_claims_validate(data.frame(entity_id = "1")), "missing required column")
  claims <- data.frame(entity_id = c("1", ""), attribute = "state", source_id = "dac",
                        claimed_value = "CO", reference_year = 2024)
  expect_error(attribute_claims_validate(claims), "missing or empty entity_id")
})

# --- attribute_truth_fusion --------------------------------------------------

test_that("attribute_truth_fusion's majority vote counts distinct sources, not raw rows", {
  claims <- data.frame(
    entity_id = c("1", "1", "1"), attribute = "state",
    source_id = c("nppes", "dac", "op"),
    claimed_value = c("CO", "CO", "NM"), reference_year = 2024
  )
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  expect_s3_class(result, "mysteryroster_truth")
  pred <- result$predictions
  expect_equal(pred$predicted_value, "CO")
  expect_equal(pred$model_score, round(2 / 3, 4))
  expect_false(pred$abstained)
  expect_equal(pred$n_sources, 3)
})

test_that("attribute_truth_fusion abstains on an exact tie, with predicted_value NA", {
  claims <- data.frame(
    entity_id = "1", attribute = "state", source_id = c("nppes", "dac"),
    claimed_value = c("NY", "NJ"), reference_year = 2024
  )
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  expect_true(result$predictions$abstained)
  expect_equal(result$predictions$abstention_reason, "tied_vote")
  expect_true(is.na(result$predictions$predicted_value))
})

test_that("attribute_truth_fusion's abstain_threshold abstains a non-tied but weak majority", {
  claims <- data.frame(
    entity_id = "1", attribute = "state",
    source_id = c("a", "b", "c"), claimed_value = c("CO", "CO", "NM"),
    reference_year = 2024
  )
  no_threshold <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  expect_false(no_threshold$predictions$abstained)

  with_threshold <- suppressMessages(
    attribute_truth_fusion(claims, method = "majority", abstain_threshold = 0.8)
  )
  expect_true(with_threshold$predictions$abstained)
  expect_equal(with_threshold$predictions$abstention_reason, "below_abstain_threshold")
})

test_that("attribute_truth_fusion gives a source with 2 legitimate values a vote in each tally", {
  claims <- data.frame(
    entity_id = c("1", "1", "1"), attribute = "state",
    source_id = c("multi", "multi", "other"),
    claimed_value = c("CO", "NM", "CO"), reference_year = 2024
  )
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  cand <- result$candidate_scores
  expect_equal(cand$n_votes[cand$candidate_value == "CO"], 2)
  expect_equal(cand$n_votes[cand$candidate_value == "NM"], 1)
  # "multi" counted once as a distinct source, "other" once -> n_sources = 2
  expect_equal(result$predictions$n_sources, 2)
})

test_that("attribute_truth_fusion requires attribute_name when claims has >1 attribute", {
  claims <- data.frame(
    entity_id = "1", attribute = c("state", "specialty"),
    source_id = "a", claimed_value = c("CO", "OBGYN"), reference_year = 2024
  )
  expect_error(attribute_truth_fusion(claims, method = "majority"), "distinct attributes")
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority", attribute_name = "state"))
  expect_equal(result$predictions$attribute, "state")
})

test_that("attribute_truth_fusion rejects an unimplemented method or mismatched backend", {
  claims <- data.frame(entity_id = "1", attribute = "state", source_id = "a",
                        claimed_value = "CO", reference_year = 2024)
  expect_error(attribute_truth_fusion(claims, method = "dawid_skene"), "not yet implemented")
  expect_error(attribute_truth_fusion(claims, method = "majority", backend = "crowd_kit"),
               "requires backend 'r_native'")
})

test_that("attribute_truth_fusion messages (does not error) when an unused argument is set", {
  claims <- data.frame(entity_id = "1", attribute = "state", source_id = "a",
                        claimed_value = "CO", reference_year = 2024)
  expect_message(attribute_truth_fusion(claims, method = "majority", max_iterations = 5L),
                  "'max_iterations' is not used")
})

test_that("attribute_truth_fusion's source_diagnostics is an empty, typed tibble for majority", {
  claims <- data.frame(entity_id = "1", attribute = "state", source_id = "a",
                        claimed_value = "CO", reference_year = 2024)
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  expect_equal(nrow(result$source_diagnostics), 0)
  expect_true(all(c("source_id", "reliability_score") %in% names(result$source_diagnostics)))
})

test_that("print.mysteryroster_truth summarizes without erroring", {
  claims <- data.frame(entity_id = "1", attribute = "state", source_id = "a",
                        claimed_value = "CO", reference_year = 2024)
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  expect_output(print(result), "mysteryroster_truth")
})

# --- attribute_truth_backends ------------------------------------------------

test_that("attribute_truth_backends reports only majority as implemented", {
  backends <- attribute_truth_backends()
  expect_true(backends$available[backends$backend == "r_native"])
  expect_false(any(backends$available[backends$backend != "r_native"]))
})
