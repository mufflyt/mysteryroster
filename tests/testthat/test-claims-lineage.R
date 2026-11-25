test_that("attribute_claims_collapse_lineage collapses an agreeing family to one vote", {
  claims <- data.frame(
    entity_id = c("1", "1", "1"), attribute = "state",
    source_id = c("dac", "pecos", "nppes"),
    claimed_value = c("CO", "CO", "CO"), reference_year = 2024
  )
  collapsed <- suppressMessages(attribute_claims_collapse_lineage(
    claims, lineage_groups = list(dac_pecos = c("dac", "pecos"))
  ))
  expect_equal(nrow(collapsed), 2) # dac+pecos -> one row, nppes untouched
  expect_equal(sort(collapsed$source_id), sort(c("dac_pecos", "nppes")))
  report <- attr(collapsed, "report")
  expect_equal(report$n[report$check == "entity_attribute_years_collapsed_agree"], 1)
  expect_equal(report$n[report$check == "entity_attribute_years_dropped_conflict"], 0)
})

test_that("attribute_claims_collapse_lineage drops a disagreeing family by default, doesn't pick one", {
  claims <- data.frame(
    entity_id = "1", attribute = "state",
    source_id = c("dac", "pecos", "nppes"),
    claimed_value = c("CO", "NM", "CO"), reference_year = 2024
  )
  collapsed <- suppressMessages(attribute_claims_collapse_lineage(
    claims, lineage_groups = list(dac_pecos = c("dac", "pecos"))
  ))
  expect_equal(nrow(collapsed), 1) # only nppes survives; dac/pecos dropped, not averaged or chosen
  expect_equal(collapsed$source_id, "nppes")
  report <- attr(collapsed, "report")
  expect_equal(report$n[report$check == "entity_attribute_years_dropped_conflict"], 1)
  expect_equal(report$n[report$check == "entity_attribute_years_collapsed_agree"], 0)

  conflicts <- attr(collapsed, "conflicting_groups")
  expect_equal(nrow(conflicts), 1)
  expect_equal(conflicts$family, "dac_pecos")
})

test_that("attribute_claims_collapse_lineage on_conflict = 'flag' keeps the disagreeing rows", {
  claims <- data.frame(
    entity_id = "1", attribute = "state",
    source_id = c("dac", "pecos"),
    claimed_value = c("CO", "NM"), reference_year = 2024
  )
  flagged <- suppressMessages(attribute_claims_collapse_lineage(
    claims, lineage_groups = list(dac_pecos = c("dac", "pecos")), on_conflict = "flag"
  ))
  expect_equal(nrow(flagged), 2)
  expect_setequal(flagged$source_id, c("dac", "pecos")) # original ids preserved, not renamed to family
})

test_that("attribute_claims_collapse_lineage passes through a single-source-present family member", {
  claims <- data.frame(
    entity_id = c("1", "2"), attribute = "state",
    source_id = c("dac", "nppes"), # pecos has no claim at all for entity 1
    claimed_value = c("CO", "NY"), reference_year = 2024
  )
  collapsed <- suppressMessages(attribute_claims_collapse_lineage(
    claims, lineage_groups = list(dac_pecos = c("dac", "pecos"))
  ))
  expect_equal(nrow(collapsed), 2)
  expect_true("dac_pecos" %in% collapsed$source_id) # renamed to the family, not left as "dac"
  expect_false("dac" %in% collapsed$source_id)
  report <- attr(collapsed, "report")
  expect_equal(report$n[report$check == "entity_attribute_years_single_source"], 1)
})

test_that("attribute_claims_collapse_lineage leaves sources outside any group untouched", {
  claims <- data.frame(
    entity_id = c("1", "1"), attribute = "state",
    source_id = c("open_payments", "trilliant"),
    claimed_value = c("CO", "NM"), reference_year = 2024
  )
  collapsed <- suppressMessages(attribute_claims_collapse_lineage(
    claims, lineage_groups = list(dac_pecos = c("dac", "pecos"))
  ))
  expect_equal(nrow(collapsed), 2)
  expect_setequal(collapsed$source_id, c("open_payments", "trilliant"))
})

test_that("attribute_claims_collapse_lineage keeps families independent of each other", {
  claims <- data.frame(
    entity_id = c("1", "1", "1", "1"), attribute = "state",
    source_id = c("dac", "pecos", "doximity", "healthgrades"),
    claimed_value = c("CO", "CO", "NM", "NY"), reference_year = 2024
  )
  collapsed <- suppressMessages(attribute_claims_collapse_lineage(
    claims,
    lineage_groups = list(
      dac_pecos = c("dac", "pecos"),
      scrape_family = c("doximity", "healthgrades")
    )
  ))
  # dac_pecos agrees -> 1 vote; scrape_family disagrees -> dropped entirely
  expect_equal(nrow(collapsed), 1)
  expect_equal(collapsed$source_id, "dac_pecos")
})

test_that("attribute_claims_collapse_lineage output feeds attribute_truth_fusion unchanged in shape", {
  claims <- data.frame(
    entity_id = c("1", "1", "2", "2"), attribute = "state",
    source_id = c("dac", "pecos", "dac", "pecos"),
    claimed_value = c("CO", "CO", "NM", "NJ"), reference_year = 2024
  )
  collapsed <- suppressMessages(attribute_claims_collapse_lineage(
    claims, lineage_groups = list(dac_pecos = c("dac", "pecos"))
  ))
  validated <- suppressMessages(attribute_claims_validate(collapsed))
  expect_equal(nrow(validated), 1) # entity 1 collapses to one vote; entity 2's conflict drops entirely
})

test_that("attribute_claims_collapse_lineage rejects malformed lineage_groups", {
  claims <- data.frame(
    entity_id = "1", attribute = "state", source_id = "dac",
    claimed_value = "CO", reference_year = 2024
  )
  expect_error(
    attribute_claims_collapse_lineage(claims, lineage_groups = list(solo = "dac")),
    "fewer than 2 members"
  )
  expect_error(
    attribute_claims_collapse_lineage(
      claims, lineage_groups = list(a = c("dac", "pecos"), b = c("pecos", "nppes"))
    ),
    "more than one lineage group"
  )
  expect_error(
    attribute_claims_collapse_lineage(claims, lineage_groups = list(c("dac", "pecos"))),
    "names"
  )
})

test_that("attribute_claims_collapse_lineage requires the standard claims columns", {
  bad_claims <- data.frame(entity_id = "1", source_id = "dac", claimed_value = "CO")
  expect_error(
    attribute_claims_collapse_lineage(bad_claims, lineage_groups = list(g = c("dac", "pecos"))),
    "missing required column"
  )
})
