test_that("Feature 1: attribute_enrollment_cardinality_audit and attribute_location_specificity work accurately", {
  enrollments <- data.frame(
    npi = c("1", "2", "3", "4", "5"),
    year = 2019,
    enrollment_id = c("I1", "I2", "I3", "I4", "I5")
  )
  reassignments <- data.frame(
    npi = c("2", "3", "3", "4"),
    year = 2019,
    enrollment_id = c("G1", "G2", "G3", "G4")
  )
  # G1 has 1 site, G2 has 2 suites in same ZIP, G3 has 2 ZIPs in CO, G4 spans CO and WY
  locations <- data.frame(
    year = 2019,
    enrollment_id = c("I1", "G1", "G2", "G2", "G3", "G3", "G4", "G4"),
    location_id = c("L1", "L2_A", "L3_S1", "L3_S2", "L4_Z1", "L4_Z2", "L5_CO", "L5_WY"),
    practice_city = c("DENVER", "DENVER", "BOULDER", "BOULDER", "DENVER", "DENVER", "DENVER", "CHEYENNE"),
    practice_zip5 = c("80204", "80204", "80301", "80301", "80206", "80210", "80204", "82001"),
    practice_state = c("CO", "CO", "CO", "CO", "CO", "CO", "CO", "WY")
  )
  linked <- attribute_enrollment_locations(enrollments, reassignments, locations)$links
  ref_st <- data.frame(id = c("1", "2", "3", "4", "5"), year = 2019, practice_state = "CO")

  card <- attribute_enrollment_cardinality_audit(linked, reference_state = ref_st)
  expect_true(is.data.frame(card))
  expect_equal(nrow(card), 6L)
  # Group-reassigned linked rows = 7 (G1..G4), distinct (id, zip5) = 6 (since G2's L3_S1 and L3_S2 share 80301)
  expect_equal(card$n_rows_or_pairs[1], 7L)
  expect_equal(card$n_rows_or_pairs[card$aggregation_level == "distinct_id_zip5"], 6L)

  corrob <- list(
    nppes = data.frame(id = c("1", "2", "3", "4"), year = 2019, practice_zip5 = c("80204", "80204", "80301", "99999")),
    tmsis = data.frame(id = c("1", "2"), year = 2019, practice_zip5 = c("80204", "80204"))
  )
  spec <- attribute_location_specificity(
    linked,
    corroboration_sources = corrob,
    provider_plural = "nurse-midwives"
  )
  expect_equal(nrow(spec$clinicians), 5L)
  non_tot_tiers <- spec$tiers[spec$tiers$specificity_tier != "Total", ]
  expect_equal(sum(non_tot_tiers$n_clinicians), 5L)
  expect_equal(round(sum(non_tot_tiers$pct_of_cohort), 1), 100.0)
  expect_equal(nrow(spec$funnel), 4L)
  expect_match(spec$plain_english_summary, "directly linked to")
  expect_match(spec$plain_english_summary, "billing organizations")
})

test_that("Feature 2: attribute_geographic_concordance enforces 100% mutually exclusive sum", {
  dir_df <- data.frame(
    npi = c("1", "2", "3", "4", "5"),
    year = 2026,
    practice_zip5 = c("80204", "80204", "80204", "80204", "80204"),
    mailing_zip5  = c("80204", "80206", "80301", "80204", "80204")
  )
  ref_df <- data.frame(
    npi = c("1", "2", "3", "4", "5"),
    year = 2026,
    reference_zip5 = c("80204", "80206", "80301", "80210", "98101")
  )
  conc <- attribute_geographic_concordance(dir_df, ref_df)
  me_no_tot <- conc$mutually_exclusive[conc$mutually_exclusive$concordance_category != "Total", ]
  expect_equal(nrow(me_no_tot), 5L)
  expect_equal(sum(me_no_tot$n), 5L)
  expect_equal(sum(me_no_tot$pct), 100.0)
  expect_true(all(me_no_tot$n == 1L))
  expect_equal(nrow(conc$marginal_indicators), 4L)
})

test_that("Feature 3: attribute_disagreement_trajectories partitions all 6 longitudinal trajectories", {
  a <- data.frame(
    npi = c("1", "1", "2", "2", "3", "3", "4", "4", "5", "5", "6"),
    year = c(2019, 2026, 2019, 2026, 2019, 2026, 2019, 2026, 2019, 2026, 2019),
    zip5 = c("80204", "80204", "80204", "80301", "80204", "80301",
             "80204", "98101", "80204", "80204", "80204")
  )
  b <- data.frame(
    npi = c("1", "1", "2", "2", "3", "3", "4", "4", "5", "5", "6"),
    year = c(2019, 2026, 2019, 2026, 2019, 2026, 2019, 2026, 2019, 2026, 2019),
    zip5 = c("80301", "80204", "80301", "80301", "80301", "80204",
             "80301", "98101", "80301", "80301", "80301")
  )
  dec <- attribute_disagreement_trajectories(
    a, b, attribute = "zip5", base_year = 2019, max_panel_year = 2026,
    source_a_label = "PECOS", source_b_label = "NPPES"
  )
  expect_equal(dec$disc_base_n, 6L)
  six_rows <- dec$summary[!dec$summary$trajectory_code %in% c("resolved_total", "total"), ]
  expect_equal(nrow(six_rows), 6L)
  expect_true(all(six_rows$n == 1L))
  expect_equal(sum(six_rows$n), 6L)
  expect_equal(dec$summary$n[dec$summary$trajectory_code == "resolved_total"], 4L)
  expect_match(dec$prose, "mutually exclusive longitudinal trajectory decomposition")
})

test_that("Feature 4: attribute_enumeration_lifecycle separates certification lapse from NPI deactivation and deceased records", {
  roster <- data.frame(
    npi = c("1", "2", "3", "4", "5"),
    cert_status = c("ACTIVE", "EXPIRED", "RETIRED", "DECEASED", "DECEASED"),
    match_tier = c("Tier 1", "Tier 1", "Tier 1", "Tier 1", "Tier 2"),
    event_year = c(NA, 2020, 2021, 2018, 2019)
  )
  active_nppes <- data.frame(npi = c("1", "2", "3", "5"))
  deact_nppes <- data.frame(npi = "4", deactivation_date = "2018-09-01")
  panels <- list(
    pecos = data.frame(npi = c("1", "2"), year = c(2024, 2024)),
    tmsis = data.frame(npi = c("1", "3", "5"), year = c(2024, 2020, 2022))
  )
  life <- attribute_enumeration_lifecycle(
    roster, active_nppes,
    deactivated_enumeration = deact_nppes,
    activity_panels = panels,
    event_year_col = "event_year"
  )
  expect_equal(nrow(life$clinicians), 5L)
  expect_equal(sum(life$matrix_summary$n), 5L)
  overall_dec <- life$deceased_audit[life$deceased_audit$match_tier == "Overall", ]
  expect_equal(overall_dec$n_deceased, 2L)
  expect_equal(overall_dec$n_confirmed_deactivated, 1L)
  expect_equal(overall_dec$n_still_active_nppes, 1L)
  expect_equal(overall_dec$n_post_event_downstream_activity, 1L)
})

test_that("Feature 5: attribute_source_vintage_audit detects missing sub-feeds and coverage jumps", {
  pop <- expand.grid(npi = as.character(1:10), year = 2018:2021, stringsAsFactors = FALSE)
  pecos <- data.frame(
    npi = c(as.character(1:6), as.character(1:6), as.character(1:6)),
    year = c(rep(2018, 6), rep(2019, 6), rep(2021, 6)) # missing in 2020
  )
  dac <- data.frame(
    npi = rep(as.character(1:2), 4),
    year = rep(2018:2021, each = 2)
  )
  audit <- attribute_source_vintage_audit(
    pop,
    sources = list(pecos = pecos, dac = dac),
    source_families = list(medicare = c("pecos", "dac")),
    jump_threshold_pct = 15.0
  )
  med_2020 <- audit$family_decomposition[audit$family_decomposition$year == 2020, ]
  expect_equal(med_2020$vintage_status, "partial_subfeed_omission")
  expect_equal(med_2020$missing_subfeeds, "pecos")
  expect_true(nrow(audit$flagged_discontinuities) >= 2L)
  # Regression test for a real bug: dplyr::bind_rows() treats a *named* list's
  # names as a nested-column key rather than flattening its rows, so binding
  # fam_list (named by family) without unname() silently corrupted every
  # composite-family row in annual_coverage/flagged_discontinuities into NAs
  # -- invisible to a row-count-only assertion, only caught by inspecting
  # real multi-year data.
  expect_false(anyNA(audit$annual_coverage$source))
  expect_false(anyNA(audit$annual_coverage$source_type))
  expect_true("medicare" %in% audit$annual_coverage$source)
  flagged_family <- audit$flagged_discontinuities[
    audit$flagged_discontinuities$source == "medicare", , drop = FALSE
  ]
  expect_true(nrow(flagged_family) >= 1L)
  expect_false(anyNA(flagged_family$pct_present))
})

test_that("Feature 6: attribute_inconsistency_table computes per-attribute and composite OIG Table 4 rows", {
  a <- data.frame(
    npi = c("1", "2", "3", "4"), year = 2024,
    zip5 = c("80204", "80204", "80204", "80204"),
    state = c("CO", "CO", "CO", "CO")
  )
  b <- data.frame(
    npi = c("1", "2", "3", "4"), year = 2024,
    zip5 = c("80204", "80301", "80204", "98101"),
    state = c("CO", "CO", "NM", "WA")
  )
  t4 <- attribute_inconsistency_table(a, b, attributes = c("zip5", "state"))
  expect_equal(nrow(t4$table4_summary), 4L)
  # NPI 1 matches both; NPI 2 mismatches zip5; NPI 3 mismatches state; NPI 4 mismatches both -> 3 of 4 (75%) have >= 1 inconsistent variable
  union_row <- t4$table4_summary[t4$table4_summary$attribute == "any_variable_inconsistent_union", ]
  expect_equal(union_row$n_compared, 4L)
  expect_equal(union_row$n_disjoint_inconsistent, 3L)
  expect_equal(union_row$pct_disjoint_inconsistent, 75.0)
})

test_that("Feature 7: normalize_directory_attribute normalizes and validates format & Luhn check", {
  z <- normalize_directory_attribute(
    c("80204-1234", "7030", "901", "00000", "ZIP UNKNOWN", NA),
    normalizer = "zip5"
  )
  expect_equal(z$normalized_value, c("80204", "07030", "00901", NA, NA, NA))
  expect_equal(z$format_valid, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))

  ph <- normalize_directory_attribute(
    c("(303) 555-0199", "1-303-555-0199", "000-000-0000", ""),
    normalizer = "phone10"
  )
  expect_equal(ph$normalized_value, c("3035550199", "3035550199", NA, NA))
  expect_equal(ph$format_valid, c(TRUE, TRUE, FALSE, FALSE))

  # 1234567893 is a valid Luhn NPI (prefix 80840 + 123456789 -> check digit 3)
  npi_res <- normalize_directory_attribute(c("1234567893", "1234567890", "ABC"), normalizer = "npi10")
  expect_equal(npi_res$format_valid, c(TRUE, FALSE, FALSE))
})

test_that("Feature 8: attribute_oig_audit_prose and attribute_oig_audit_html integrate location_specificity", {
  record_presence <- data.frame(
    id = c("1", "2", "1", "2"), year = 2019,
    source = c("nppes", "nppes", "pecos", "pecos"),
    attribute = "practice_zip5", record_present = c(TRUE, TRUE, TRUE, TRUE),
    field_status = c("populated", "populated", "populated", "populated")
  )
  sources <- list(
    nppes = data.frame(npi = c("1", "2"), year = 2019, state = c("CO", "CO")),
    pecos = data.frame(npi = c("1", "2"), year = 2019, state = c("CO", "CO"))
  )
  set_summary <- attribute_set_agreement_summary(attribute_set_agreement(sources, attribute = "state"))
  enrollments <- data.frame(npi = c("1", "2"), year = 2019, enrollment_id = c("I1", "I2"))
  reassignments <- data.frame(
    npi = "2", year = 2019,
    enrollment_id = "G1"
  )
  locations <- data.frame(
    year = 2019, enrollment_id = c("I1", "G1"),
    location_id = c("L1", "L2"), practice_zip5 = c("80204", "80204"), practice_state = c("CO", "CO")
  )
  linked <- attribute_enrollment_locations(enrollments, reassignments, locations)$links
  spec <- attribute_location_specificity(linked, provider_plural = "nurse-midwives")

  prose <- attribute_oig_audit_prose(
    record_presence, set_summary, year = 2019,
    source_a = "nppes", source_b = "pecos",
    attribute = "practice_zip5", provider_noun = "nurse-midwife",
    location_specificity = spec
  )
  expect_match(prose, "directly linked to")
  expect_match(prose, "billing organizations")

  html <- attribute_oig_audit_html(
    record_presence, set_summary, year = 2019,
    source_a = "nppes", source_b = "pecos",
    attribute = "practice_zip5", provider_noun = "nurse-midwife",
    location_specificity = spec
  )
  expect_match(html, "Appendix A2: Clinician-Level Location Specificity")
})

test_that("Feature 9: design_verification_sample computes Neyman, cost-optimal, and Manski bounds", {
  frame <- data.frame(
    npi = as.character(1:1000),
    stratum = c(rep("tier1_pinpoint", 600), rep("tier2_multisite", 300), rep("tier3_discordant", 100))
  )
  des <- design_verification_sample(
    frame,
    allocation_method = "cost_optimal",
    target_moe = 0.04,
    prior_p = c(tier1_pinpoint = 0.90, tier2_multisite = 0.60, tier3_discordant = 0.30),
    unit_cost = c(tier1_pinpoint = 1, tier2_multisite = 2, tier3_discordant = 4),
    response_rate = 0.85
  )
  expect_equal(nrow(des$strata), 3L)
  expect_equal(des$design_summary$N_total, 1000L)
  expect_true(des$design_summary$expected_moe <= 0.045)
  expect_equal(round(des$design_summary$expected_manski_width, 2), 0.15)

  samp <- stratified_listing_sample(frame, allocation = des$allocation_vector, seed = 42)
  expect_equal(nrow(samp), sum(des$allocation_vector))
})

test_that("Feature 10: attribute_spatial_distance computes Haversine km/miles and 5 commute-shed bands", {
  dir_df <- data.frame(
    npi = c("1", "2", "3", "4", "5"),
    year = 2026,
    practice_zip5 = c("80204", "80204", "80204", "80204", "80204"),
    lat = c(39.7392, 39.7392, 39.7392, 39.7392, 39.7392),
    lon = c(-105.0186, -105.0186, -105.0186, -105.0186, -105.0186)
  )
  ref_df <- data.frame(
    npi = c("1", "2", "3", "4", "5"),
    year = 2026,
    reference_zip5 = c("80204", "80203", "80012", "80525", "98101"),
    lat = c(39.7392, 39.7310, 39.7000, 40.5500, 47.6062),
    lon = c(-105.0186, -104.9800, -104.8300, -105.0700, -122.3321)
  )
  sp <- attribute_spatial_distance(
    dir_df, ref_df,
    dir_lat_col = "lat", dir_lon_col = "lon",
    ref_lat_col = "lat", ref_lon_col = "lon"
  )
  expect_equal(nrow(sp$records), 5L)
  expect_equal(sp$records$distance_km[1], 0)
  non_tot <- sp$band_summary[sp$band_summary$spatial_band != "Total", ]
  expect_equal(sum(non_tot$n), 5L)
  expect_equal(sum(non_tot$pct), 100.0)
  expect_true(all(non_tot$n == 1L))

  # Also test SCF3 fallback when no lat/lon is supplied
  sp_fallback <- attribute_spatial_distance(
    dir_df[, c("npi", "year", "practice_zip5")],
    ref_df[, c("npi", "year", "reference_zip5")]
  )
  expect_equal(nrow(sp_fallback$records), 5L)
  expect_equal(sp_fallback$records$distance_km[1], 0)
})

test_that("Feature 11: attribute_billing_group_topology computes Shannon entropy and archetypes", {
  individual <- data.frame(npi = c("1", "2", "3"), year = 2019, enrollment_id = c("I1", "I2", "I3"))
  reassignments <- data.frame(
    npi = c("1", "2", "3"), year = 2019, enrollment_id = c("G_SOLO", "G_LOCAL", "G_MULTI")
  )
  locations <- data.frame(
    year = 2019,
    enrollment_id = c("G_SOLO", "G_LOCAL", "G_LOCAL", "G_MULTI", "G_MULTI"),
    location_id = c("L1", "L2", "L3", "L4", "L5"),
    practice_state = c("CO", "CO", "CO", "CO", "WY"),
    practice_zip5 = c("80204", "80204", "80206", "80204", "82001")
  )
  linked <- attribute_enrollment_locations(individual, reassignments, locations)$links
  topo <- attribute_billing_group_topology(linked)
  expect_equal(nrow(topo$groups), 3L)
  expect_equal(topo$groups$effective_n_zip5[topo$groups$enrollment_id == "G_SOLO"], 1.0)
  expect_equal(topo$groups$effective_n_zip5[topo$groups$enrollment_id == "G_LOCAL"], 2.0)
  expect_equal(topo$groups$n_unique_states[topo$groups$enrollment_id == "G_MULTI"], 2L)
  expect_equal(sum(topo$archetype_summary$n_groups), 3L)
  expect_equal(nrow(topo$clinician_weights), 3L)
})

test_that("Feature 13: attribute_hierarchical_truth_fusion resolves coarse-to-fine geographic consensus", {
  s1 <- data.frame(
    npi = c("1", "2", "3", "4"), year = 2026,
    state = c("CO", "CO", "CO", "CO"),
    zip5 = c("80204", "80204", "80204", "80204")
  )
  s2 <- data.frame(
    npi = c("1", "2", "3", "4"), year = 2026,
    state = c("CO", "CO", "CO", "NY"),
    zip5 = c("80204", "80206", "80301", "10001")
  )
  s3 <- data.frame(
    npi = c("1", "2", "3", "4"), year = 2026,
    state = c("CO", "WA", "WY", "CA"),
    zip5 = c("80204", "98101", "82001", "90210")
  )
  hf <- attribute_hierarchical_truth_fusion(list(nppes = s1, pecos = s2, tmsis = s3))
  expect_equal(nrow(hf$entities), 4L)
  # NPI 1 resolves at zip5 (80204); NPI 2 resolves at scf3 (802); NPI 3 resolves at state (CO); NPI 4 is unresolved
  expect_equal(hf$entities$finest_resolved_grain, c("zip5", "scf3", "state", "unresolved"))
  expect_equal(hf$entities$finest_resolved_value, c("80204", "802", "CO", NA))
})

test_that("Feature 15: attribute_mover_stayer_transition_matrix classifies 5 mobility regimes and Markov matrix", {
  panel <- data.frame(
    npi = c(rep("1", 4), rep("2", 4), rep("3", 4), rep("4", 4), rep("5", 4)),
    year = rep(2021:2024, 5),
    state = c(
      "CO", "CO", "CO", "CO", # 1: Strict Stayer
      "KS", "KS", "KS", "KS", # 2: Concurrent Multi-State Stayer
      "CO", "CO", "WY", "WY", # 3: One-Time Permanent Mover
      "NY", "NJ", "NY", "NJ", # 4: Alternating / Return Commuter
      "CO", "WY", "UT", "AZ"  # 5: Multi-State Serial Mover
    )
  )
  conc <- data.frame(npi = c("2", "2"), year = c(2022, 2022), state = c("KS", "MO"))
  ms <- attribute_mover_stayer_transition_matrix(panel, concurrent_source = conc)
  expect_equal(nrow(ms$clinicians), 5L)
  non_tot <- ms$regime_summary[ms$regime_summary$mobility_regime != "Total", ]
  expect_equal(sum(non_tot$n), 5L)
  expect_true(all(non_tot$n == 1L))
  expect_true(all(ms$clinicians$any_contiguous_border_move[ms$clinicians$id %in% c("3", "4", "5")]))
  expect_true(nrow(ms$state_transition_matrix) > 0L)
})

test_that("Feature 16: attribute_audit_diff validates partition sums and reconciles metric diffs", {
  base_m <- c(
    pecos_linkage_pct = 95.7,
    board_zip5_concordance_sum_pct = 103.6,
    medicare_2018_coverage_pct = 22.7
  )
  rev_m <- c(
    pecos_linkage_pct = 95.7,
    board_zip5_concordance_sum_pct = 100.0,
    medicare_2018_coverage_pct = 46.84
  )
  reasons <- c(
    board_zip5_concordance_sum_pct = "Replaced overlapping marginal ZIP3/mailing counts with 5-way mutually exclusive hierarchy.",
    medicare_2018_coverage_pct = "Merged 2018 DAC historical release with 2018 PECOS snapshot."
  )
  part_tbl <- data.frame(
    category = c("Cat 1", "Cat 2", "Total"),
    n = c(60, 40, 100),
    pct = c(60.0, 40.0, 100.0)
  )
  rec <- attribute_audit_diff(
    base_m, rev_m,
    partitions = list(board_concordance = part_tbl),
    reasons = reasons
  )
  expect_equal(nrow(rec$diff_table), 3L)
  expect_equal(rec$diff_table$status, c("unchanged", "reconciled_change", "reconciled_change"))
  expect_true(rec$partition_checks$identity_valid[1])
  expect_match(rec$markdown_table, "board_zip5_concordance_sum_pct")
  expect_match(rec$html_table, "<table class=\"oig-table\">")
})
