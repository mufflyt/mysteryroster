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

test_that("on_source_conflict = 'single_value' (default) excludes a source with 2 conflicting values", {
  claims <- data.frame(
    entity_id = c("1", "1", "1"), attribute = "state",
    source_id = c("multi", "multi", "other"),
    claimed_value = c("CO", "NM", "CO"), reference_year = 2024
  )
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  pred <- result$predictions
  # "multi" is internally conflicted (CO and NM for the same entity-year) and
  # is excluded entirely -- it never casts a vote for either value.
  expect_equal(pred$n_sources_excluded, 1)
  expect_equal(pred$n_sources, 1) # only "other" voted
  expect_equal(pred$predicted_value, "CO")
  expect_equal(pred$model_score, 1) # "other" alone, unanimous among counted votes
  expect_equal(nrow(result$candidate_scores), 1) # NM never appears -- "multi" was excluded
})

test_that("on_source_conflict = 'multi_value' lets a conflicted source vote for every value", {
  claims <- data.frame(
    entity_id = c("1", "1", "1"), attribute = "state",
    source_id = c("multi", "multi", "other"),
    claimed_value = c("CO", "NM", "CO"), reference_year = 2024
  )
  result <- suppressMessages(
    attribute_truth_fusion(claims, method = "majority", on_source_conflict = "multi_value")
  )
  cand <- result$candidate_scores
  expect_equal(cand$n_votes[cand$candidate_value == "CO"], 2)
  expect_equal(cand$n_votes[cand$candidate_value == "NM"], 1)
  # "multi" counted once as a distinct source, "other" once -> n_sources = 2
  expect_equal(result$predictions$n_sources, 2)
  expect_equal(result$predictions$n_sources_excluded, 0)
})

test_that("on_source_conflict = 'single_value' abstains when EVERY source is conflicted", {
  claims <- data.frame(
    entity_id = "1", attribute = "state",
    source_id = c("a", "a"), claimed_value = c("CO", "NM"), reference_year = 2024
  )
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  pred <- result$predictions
  expect_true(pred$abstained)
  expect_equal(pred$abstention_reason, "no_single_value_sources")
  expect_true(is.na(pred$predicted_value))
  expect_equal(pred$n_sources, 0)
  expect_equal(pred$n_sources_excluded, 1)
  expect_equal(nrow(result$candidate_scores), 0)
})

test_that("a duplicate row (same source, same value, repeated) never inflates the vote count", {
  claims <- data.frame(
    entity_id = "1", attribute = "state",
    source_id = c("a", "a", "b"), claimed_value = c("CO", "CO", "NM"), reference_year = 2024
  )
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  # a's repeated identical CO row counts once; this is a genuine tie (1 vs 1),
  # not CO winning 2-1 -- if it were not deduped, this would NOT abstain.
  expect_true(result$predictions$abstained)
  expect_equal(result$predictions$abstention_reason, "tied_vote")
  expect_equal(result$predictions$n_sources, 2)
})

test_that("entity-years that would collide under a pasted string key are kept separate", {
  # "1"+23 and "12"+3 both paste to "123" with no separator -- regression
  # test for the string-key collision this package now avoids entirely.
  claims <- data.frame(
    entity_id = c("1", "1", "12", "12"), attribute = "state",
    source_id = c("a", "b", "a", "b"),
    claimed_value = c("CO", "NM", "TX", "TX"), reference_year = c(23, 23, 3, 3)
  )
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  pred <- result$predictions
  expect_equal(nrow(pred), 2) # two distinct entity-years, never merged into one
  row_1_23 <- pred[pred$entity_id == "1" & pred$reference_year == 23, ]
  row_12_3 <- pred[pred$entity_id == "12" & pred$reference_year == 3, ]
  expect_equal(nrow(row_1_23), 1)
  expect_true(row_1_23$abstained) # "1"/23 is a genuine tie (CO vs NM)
  expect_equal(nrow(row_12_3), 1)
  expect_false(row_12_3$abstained) # "12"/3 is unanimous (TX, TX)
  expect_equal(row_12_3$predicted_value, "TX")
})

test_that("two different attributes for the same entity and year never mix votes", {
  claims <- data.frame(
    entity_id = c("1", "1", "1", "1"), attribute = c("state", "state", "specialty", "specialty"),
    source_id = c("a", "b", "a", "b"),
    claimed_value = c("CO", "CO", "OBGYN", "FM"), reference_year = 2024
  )
  state_result <- suppressMessages(attribute_truth_fusion(claims, method = "majority", attribute_name = "state"))
  specialty_result <- suppressMessages(
    attribute_truth_fusion(claims, method = "majority", attribute_name = "specialty")
  )
  expect_equal(state_result$predictions$predicted_value, "CO")
  expect_false(state_result$predictions$abstained)
  expect_true(specialty_result$predictions$abstained) # OBGYN vs FM is a tie
  # Neither result's candidate_scores ever contains the other attribute's values.
  expect_false(any(c("OBGYN", "FM") %in% state_result$candidate_scores$candidate_value))
  expect_false(any(c("CO") %in% specialty_result$candidate_scores$candidate_value))
})

test_that("candidate vote shares, n_sources, and denominators stay mathematically consistent", {
  claims <- data.frame(
    entity_id = c("1", "1", "1", "1"), attribute = "state",
    source_id = c("a", "b", "c", "d"),
    claimed_value = c("CO", "CO", "CO", "NM"), reference_year = 2024
  )
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  pred <- result$predictions
  cand <- result$candidate_scores
  expect_equal(sum(cand$n_votes), pred$n_sources + pred$n_sources_excluded * 0) # no exclusions here
  expect_equal(sum(cand$n_votes), 4)
  # Every candidate's own vote_share recomputes to n_votes / n_sources.
  expect_equal(cand$vote_share, round(cand$n_votes / pred$n_sources, 4))
  # The predicted value's vote_share equals model_score.
  winner_row <- cand[cand$candidate_value == pred$predicted_value, ]
  expect_equal(winner_row$vote_share, pred$model_score)
})

test_that("the result labels its score as an uncalibrated vote share and preserves original claims", {
  claims <- data.frame(
    entity_id = "1", attribute = "state", source_id = c("a", "b"),
    claimed_value = c("CO", "CO"), reference_year = 2024, raw_value = c("Colorado", "CO")
  )
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  expect_equal(result$predictions$score_type, "vote_share")
  expect_true("raw_value" %in% names(result$claims)) # not dropped
  expect_equal(result$claims$raw_value, c("Colorado", "CO")) # unmodified
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
  expect_error(attribute_truth_fusion(claims, method = "truthfinder"), "not yet implemented")
  expect_error(attribute_truth_fusion(claims, method = "majority", backend = "crowd_kit"),
               "requires backend 'r_native'")
  expect_error(attribute_truth_fusion(claims, method = "dawid_skene", backend = "r_native"),
               "requires backend 'crowd_kit'")
})

test_that("attribute_truth_fusion never silently falls back to majority when crowd_kit is unavailable", {
  skip_if(isTRUE(.mysteryroster_crowdkit_probe()$available), "crowd-kit is installed in this session")
  claims <- data.frame(
    entity_id = c("1", "1"), attribute = "state", source_id = c("a", "b"),
    claimed_value = c("CO", "NM"), reference_year = 2024
  )
  expect_error(attribute_truth_fusion(claims, method = "dawid_skene"), "backend 'crowd_kit' is not available")
  expect_error(attribute_truth_fusion(claims, method = "one_coin_dawid_skene"),
               "backend 'crowd_kit' is not available")
})

test_that("attribute_truth_fusion rejects on_source_conflict = 'multi_value' for crowd_kit", {
  claims <- data.frame(
    entity_id = c("1", "1", "1"), attribute = "state", source_id = c("multi", "multi", "other"),
    claimed_value = c("CO", "NM", "CO"), reference_year = 2024
  )
  expect_error(
    attribute_truth_fusion(claims, method = "dawid_skene", on_source_conflict = "multi_value"),
    "not supported by backend 'crowd_kit'"
  )
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
  expect_true(all(
    c("source_id", "attribute", "true_label", "observed_label", "error_probability", "reliability_score") %in%
      names(result$source_diagnostics)
  ))
})

test_that("attribute_truth_fusion's model_diagnostics reports NA backend_version/n_disconnected_components for majority", {
  claims <- data.frame(entity_id = "1", attribute = "state", source_id = "a",
                        claimed_value = "CO", reference_year = 2024)
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  expect_true(is.na(result$model_diagnostics$backend_version))
  expect_true(is.na(result$model_diagnostics$n_disconnected_components))
})

test_that("print.mysteryroster_truth summarizes without erroring", {
  claims <- data.frame(entity_id = "1", attribute = "state", source_id = "a",
                        claimed_value = "CO", reference_year = 2024)
  result <- suppressMessages(attribute_truth_fusion(claims, method = "majority"))
  expect_output(print(result), "mysteryroster_truth")
})

# --- attribute_truth_backends ------------------------------------------------

test_that("attribute_truth_backends reports r_native as always available and probes crowd_kit live", {
  backends <- attribute_truth_backends()
  expect_true(backends$available[backends$backend == "r_native"])
  expect_false(backends$available[backends$backend == "truthdiscovery"])
  expect_true(grepl("dawid_skene", backends$methods_supported[backends$backend == "crowd_kit"]))
  expect_true(grepl("one_coin_dawid_skene", backends$methods_supported[backends$backend == "crowd_kit"]))
  # crowd_kit's reported availability must match a direct probe, whatever this
  # machine's Python situation happens to be -- never hardcoded.
  expect_equal(backends$available[backends$backend == "crowd_kit"],
               isTRUE(.mysteryroster_crowdkit_probe()$available))
})

# --- attribute_truth_fusion: crowd_kit backend (Dawid-Skene family) ----------
# All of these require a real Python + crowd-kit, which is never required to
# use this package -- they skip entirely on a machine where
# .mysteryroster_crowdkit_probe() reports unavailable (including plain CI).

skip_unless_crowdkit <- function() {
  testthat::skip_if_not(isTRUE(.mysteryroster_crowdkit_probe()$available),
                         "crowd-kit is not available in this R session")
}

# 16 entities, true status alternates active/inactive. source_1 is highly
# reliable (one error, at entity 6); source_2 is always correct; source_3
# always claims "active" regardless of truth -- a pure false-positive-biased
# source with zero true discriminative value (sensitivity = 1, specificity = 0).
.truth_fusion_crowdkit_fixture <- function() {
  entities <- 1:16
  true_label <- ifelse(entities %% 2 == 1, "active", "inactive")
  rows <- do.call(rbind, lapply(seq_along(entities), function(i) {
    e <- entities[i]
    t <- true_label[i]
    source_1 <- if (e == 6) "active" else t
    data.frame(
      entity_id = as.character(e), attribute = "status",
      source_id = c("source_1", "source_2", "source_3"),
      claimed_value = c(source_1, t, "active"), reference_year = 2024
    )
  }))
  list(claims = rows, true_label = stats::setNames(true_label, as.character(entities)))
}

test_that("attribute_truth_fusion(dawid_skene) ranks a false-positive-biased source at chance reliability", {
  skip_unless_crowdkit()
  fixture <- .truth_fusion_crowdkit_fixture()
  result <- suppressMessages(suppressWarnings(
    attribute_truth_fusion(fixture$claims, method = "dawid_skene")
  ))
  reliability <- unique(result$source_diagnostics[c("source_id", "reliability_score")])
  reliability_by_source <- stats::setNames(reliability$reliability_score, reliability$source_id)

  # source_3 never discriminates true status at all (always says "active"):
  # a complete, correctly-estimated confusion matrix puts it at chance (0.5),
  # not at a misleadingly high score from only ever being scored on one cell.
  expect_equal(reliability_by_source[["source_3"]], 0.5)
  expect_gt(reliability_by_source[["source_1"]], reliability_by_source[["source_3"]])
  expect_gt(reliability_by_source[["source_2"]], reliability_by_source[["source_3"]])
})

test_that("attribute_truth_fusion(dawid_skene) exposes the biased source's false-positive rate explicitly", {
  skip_unless_crowdkit()
  fixture <- .truth_fusion_crowdkit_fixture()
  result <- suppressMessages(suppressWarnings(
    attribute_truth_fusion(fixture$claims, method = "dawid_skene")
  ))
  source_3_rows <- result$source_diagnostics[result$source_diagnostics$source_id == "source_3", ]
  false_positive_rate <- source_3_rows$error_probability[
    source_3_rows$true_label == "inactive" & source_3_rows$observed_label == "active"
  ]
  expect_equal(false_positive_rate, 1)
})

test_that("attribute_truth_fusion(dawid_skene) is deterministic across repeated calls", {
  skip_unless_crowdkit()
  fixture <- .truth_fusion_crowdkit_fixture()
  result_1 <- suppressMessages(suppressWarnings(attribute_truth_fusion(fixture$claims, method = "dawid_skene")))
  result_2 <- suppressMessages(suppressWarnings(attribute_truth_fusion(fixture$claims, method = "dawid_skene")))
  expect_equal(result_1$predictions, result_2$predictions)
  expect_equal(result_1$source_diagnostics, result_2$source_diagnostics)
})

test_that("attribute_truth_fusion(dawid_skene) can converge on a confidently wrong answer", {
  skip_unless_crowdkit()
  # Documents a real limitation (see vignette("truth-fusion")): a converged,
  # high-confidence fit is not evidence of accuracy. On this fixture, DS
  # converges but still mis-infers entity 6 (true status "inactive") as
  # "active", because the one coincidental agreement between source_1's lone
  # error and source_3's permanent bias outweighs source_2's lone correct
  # vote once source_3 is (correctly) judged uninformative but not literally
  # zero-weight.
  fixture <- .truth_fusion_crowdkit_fixture()
  result <- suppressMessages(suppressWarnings(attribute_truth_fusion(fixture$claims, method = "dawid_skene")))
  entity_6 <- result$predictions[result$predictions$entity_id == "6", ]
  expect_false(entity_6$abstained)
  expect_equal(entity_6$predicted_value, "active")
  expect_equal(fixture$true_label[["6"]], "inactive")
  expect_gt(entity_6$model_score, 0.9)
  expect_true(result$model_diagnostics$converged)
})

test_that("attribute_truth_fusion(one_coin_dawid_skene) also ranks the biased source lowest", {
  skip_unless_crowdkit()
  fixture <- .truth_fusion_crowdkit_fixture()
  result <- suppressMessages(suppressWarnings(
    attribute_truth_fusion(fixture$claims, method = "one_coin_dawid_skene")
  ))
  skill <- stats::setNames(result$source_diagnostics$reliability_score, result$source_diagnostics$source_id)
  expect_lt(skill[["source_3"]], skill[["source_1"]])
  expect_lt(skill[["source_3"]], skill[["source_2"]])
  expect_true(all(is.na(result$source_diagnostics$true_label)))
  expect_true(all(is.na(result$source_diagnostics$observed_label)))
})

test_that("attribute_truth_fusion(dawid_skene) flags a disconnected worker-task graph", {
  skip_unless_crowdkit()
  claims <- data.frame(
    entity_id = c("1", "1", "2", "2", "3", "3", "4", "4"), attribute = "status",
    source_id = c("a", "b", "a", "b", "c", "d", "c", "d"),
    claimed_value = c("active", "active", "inactive", "inactive", "active", "inactive", "inactive", "inactive"),
    reference_year = 2024
  )
  expect_warning(
    result <- attribute_truth_fusion(claims, method = "dawid_skene"),
    "2 disconnected components"
  )
  result <- suppressMessages(suppressWarnings(attribute_truth_fusion(claims, method = "dawid_skene")))
  expect_equal(result$model_diagnostics$n_disconnected_components, 2L)
})

test_that("attribute_truth_fusion(dawid_skene) flags nonconvergence within max_iterations", {
  skip_unless_crowdkit()
  fixture <- .truth_fusion_crowdkit_fixture()
  expect_warning(
    result <- attribute_truth_fusion(fixture$claims, method = "dawid_skene", max_iterations = 1L),
    "did not converge"
  )
  result <- suppressMessages(suppressWarnings(
    attribute_truth_fusion(fixture$claims, method = "dawid_skene", max_iterations = 1L)
  ))
  expect_false(result$model_diagnostics$converged)
  expect_equal(result$model_diagnostics$n_iterations, 1L)
  expect_equal(result$model_diagnostics$n_warnings, 1L)
})

test_that("attribute_truth_fusion(dawid_skene) abstains on every source excluded for a task, like majority", {
  skip_unless_crowdkit()
  claims <- data.frame(
    entity_id = c("1", "1", "2", "2"), attribute = "status",
    source_id = c("conflicted", "conflicted", "clean", "other"),
    claimed_value = c("active", "inactive", "active", "inactive"), reference_year = 2024
  )
  result <- suppressMessages(suppressWarnings(attribute_truth_fusion(claims, method = "dawid_skene")))
  entity_1 <- result$predictions[result$predictions$entity_id == "1", ]
  expect_true(entity_1$abstained)
  expect_equal(entity_1$abstention_reason, "no_single_value_sources")
  expect_equal(entity_1$n_sources, 0L)
  expect_equal(entity_1$n_sources_excluded, 1L)
})

test_that("attribute_truth_fusion(dawid_skene) candidate_scores n_votes is a raw tally, vote_share is a posterior", {
  skip_unless_crowdkit()
  fixture <- .truth_fusion_crowdkit_fixture()
  result <- suppressMessages(suppressWarnings(attribute_truth_fusion(fixture$claims, method = "dawid_skene")))
  entity_1 <- result$candidate_scores[result$candidate_scores$entity_id == "1", ]
  expect_equal(sum(entity_1$n_votes), 3L) # all 3 sources voted "active" on entity 1 (true status "active")
  expect_true(all(entity_1$score_type == "posterior_probability"))
  expect_true(all(entity_1$vote_share >= 0 & entity_1$vote_share <= 1))
})

test_that("attribute_truth_fusion(dawid_skene) respects abstain_threshold on the posterior probability", {
  skip_unless_crowdkit()
  fixture <- .truth_fusion_crowdkit_fixture()
  result <- suppressMessages(suppressWarnings(
    attribute_truth_fusion(fixture$claims, method = "dawid_skene", abstain_threshold = 0.999)
  ))
  expect_true(any(result$predictions$abstention_reason == "below_abstain_threshold"))
})
