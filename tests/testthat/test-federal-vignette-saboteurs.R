# Run the same chunks readers execute, with one deliberate change per case.
federal_saboteur <- function(mutation = base::quote(NULL)) {
  path <- testthat::test_path(
    "..", "..", "vignettes", "recent-audit-workflows.Rmd"
  )
  if (!base::file.exists(path)) {
    path <- base::system.file(
      "doc", "recent-audit-workflows.Rmd", package = "mysteryroster",
      mustWork = TRUE
    )
  }
  vignette_lines <- base::readLines(path, warn = FALSE)
  sandbox <- base::new.env(parent = base::globalenv())
  run_chunk <- function(label) {
    header <- base::match(
      base::paste0("```{r ", label, "}"), vignette_lines
    )
    following <- vignette_lines[base::seq.int(
      header + 1L, base::length(vignette_lines)
    )]
    closing <- base::match("```", following)
    chunk <- following[base::seq_len(closing - 1L)]
    base::suppressMessages(
      base::eval(base::parse(text = chunk), envir = sandbox)
    )
  }
  for (label in base::c("audit-frame", "presence-and-agreement",
                        "federal-extracts")) {
    run_chunk(label)
  }
  base::eval(mutation, envir = sandbox)
  for (label in base::c("federal-roles", "federal-coverage",
                        "federal-field-presence")) {
    run_chunk(label)
  }
  sandbox
}

federal_presence_row <- function(sandbox, physician, registry, field) {
  dplyr::filter(
    sandbox$expanded_presence, id == physician, source == registry,
    attribute == field
  )
}

federal_present_ids <- function(sandbox, registry) {
  dplyr::filter(
    sandbox$expanded_populations$coverage,
    source == registry, record_present
  )$id
}

testthat::test_that(
  "F1: repeated payments do not inflate coverage or ZIP counts", {
  study <- federal_saboteur(base::quote({
    payments <- dplyr::bind_rows(payments, payments)
  }))
  testthat::expect_setequal(federal_present_ids(study, "open_payments"),
                            base::c("1", "5"))
  row <- federal_presence_row(study, "1", "open_payments",
                              "recipient_business_zip5")
  testthat::expect_equal(row$n_field_rows, 1L)
})

testthat::test_that(
  "F2: repeated Medicaid rows do not inflate the role ledger", {
  study <- federal_saboteur(base::quote({
    medicaid <- dplyr::bind_rows(medicaid, medicaid)
  }))
  testthat::expect_equal(base::nrow(study$medicaid_roles), 2L)
  testthat::expect_setequal(
    federal_present_ids(study, "medicaid_spending"), base::c("2", "3")
  )
})

testthat::test_that(
  "F3: additional Medicare service codes do not inflate ZIP counts", {
  study <- federal_saboteur(base::quote({
    medicare <- dplyr::bind_rows(
      medicare, dplyr::mutate(medicare[1, ], hcpcs = "99214")
    )
  }))
  testthat::expect_setequal(
    federal_present_ids(study, "medicare_provider_service"), base::c("1", "3")
  )
  row <- federal_presence_row(study, "1", "medicare_provider_service",
                              "nppes_provider_zip5")
  testthat::expect_equal(row$n_populated, 1L)
})

testthat::test_that(
  "F4: billing organizations outside the frame stay outside", {
  study <- federal_saboteur(base::quote({
    medicaid$billing_npi[1] <- "901"
  }))
  testthat::expect_false(
    base::any(study$medicaid_roles$npi %in% base::c("900", "901"))
  )
  testthat::expect_setequal(
    federal_present_ids(study, "medicaid_spending"), base::c("2", "3")
  )
})

testthat::test_that(
  "F5: missing servicing NPI never inherits the billing role", {
  study <- federal_saboteur(base::quote({
    medicaid <- medicaid[3, ]
  }))
  testthat::expect_identical(study$medicaid_roles$npi, "2")
  testthat::expect_identical(study$medicaid_roles$npi_role, "billing")
})

testthat::test_that(
  "F6: one physician can have both Medicaid roles without fanout", {
  study <- federal_saboteur(base::quote({
    medicaid$billing_npi[1] <- "3"
  }))
  testthat::expect_setequal(
    study$medicaid_roles$npi_role[study$medicaid_roles$npi == "3"],
    base::c("billing", "servicing")
  )
  testthat::expect_equal(
    base::sum(federal_present_ids(study, "medicaid_spending") == "3"), 1L
  )
})

testthat::test_that(
  "F7: prior-year Medicaid claims do not enter the audit year", {
  study <- federal_saboteur(base::quote({
    medicaid$claim_month[1:2] <- base::c("2023-01", "2023-02")
  }))
  testthat::expect_identical(
    federal_present_ids(study, "medicaid_spending"), "2"
  )
})

testthat::test_that(
  "F8: a row with neither NPI does not invent a Medicaid observation", {
  study <- federal_saboteur(base::quote({
    medicaid <- dplyr::bind_rows(
      medicaid, dplyr::mutate(medicaid[1, ], billing_npi = NA_character_,
                             servicing_npi = NA_character_)
    )
  }))
  testthat::expect_false(base::anyNA(study$medicaid_roles$npi))
  testthat::expect_equal(base::nrow(study$medicaid_roles), 2L)
})

testthat::test_that(
  "F9: distinct recipient ZIPs survive payment deduplication", {
  study <- federal_saboteur(base::quote({
    payments$recipient_business_zip5[2] <- "80045"
  }))
  row <- federal_presence_row(study, "1", "open_payments",
                              "recipient_business_zip5")
  testthat::expect_equal(row$n_populated, 2L)
})

testthat::test_that(
  "F10: a blank recipient ZIP remains a present missing field", {
  study <- federal_saboteur(base::quote({
    payments$recipient_business_zip5[1:2] <- "  "
  }))
  row <- federal_presence_row(study, "1", "open_payments",
                              "recipient_business_zip5")
  testthat::expect_identical(row$field_status, "field_value_missing")
  testthat::expect_true(row$completeness_eligible)
})

testthat::test_that(
  "F11: a missing Medicare ZIP retains the utilization record", {
  study <- federal_saboteur(base::quote({
    medicare$nppes_provider_zip5[1:2] <- NA_character_
  }))
  row <- federal_presence_row(study, "1", "medicare_provider_service",
                              "nppes_provider_zip5")
  testthat::expect_identical(row$field_status, "field_value_missing")
  testthat::expect_true(row$record_present)
})

testthat::test_that(
  "F12: a stray Medicaid ZIP column is excluded from address completeness", {
  study <- federal_saboteur(base::quote({
    medicaid$practice_zip5 <- "80204"
  }))
  row <- federal_presence_row(study, "3", "medicaid_spending",
                              "practice_zip5")
  testthat::expect_identical(row$field_status, "field_not_in_extract")
  testthat::expect_false(row$completeness_eligible)
})

testthat::test_that(
  "F13: a matching Medicare ZIP shares NPPES address lineage", {
  study <- federal_saboteur(base::quote({
    medicare$nppes_provider_zip5[medicare$npi == "3"] <- "80218"
  }))
  catalog <- dplyr::filter(
    study$expanded_dictionary,
    attribute == "nppes_provider_zip5",
    source %in% base::c("nppes", "medicare_provider_service")
  )
  testthat::expect_identical(catalog$lineage,
                             base::rep("nppes_address", 2L))
})

testthat::test_that(
  "F14: a different Medicare ZIP does not become a directory practice ZIP", {
  study <- federal_saboteur(base::quote({
    medicare$nppes_provider_zip5[medicare$npi == "3"] <- "99999"
  }))
  row <- federal_presence_row(study, "3", "medicare_provider_service",
                              "practice_zip5")
  testthat::expect_identical(row$field_status, "field_not_in_extract")
  testthat::expect_false(row$completeness_eligible)
})

testthat::test_that(
  "F15: partial extract absence stays unknown", {
  study <- federal_saboteur(base::quote({
    medicare <- dplyr::filter(medicare, npi != "3")
  }))
  row <- federal_presence_row(study, "3", "medicare_provider_service",
                              "nppes_provider_zip5")
  testthat::expect_identical(
    row$field_status, "not_observed_in_incomplete_extract"
  )
  testthat::expect_false(row$completeness_eligible)
})

testthat::test_that(
  "F16: whitespace aliases cannot inflate recipient coverage", {
  testthat::expect_error(federal_saboteur(base::quote({
    payments$npi[2] <- " 1 "
  })), "duplicate keys")
})

testthat::test_that(
  "F17: a fabricated Medicaid practice ZIP contradicts field availability", {
  testthat::expect_error(federal_saboteur(base::quote({
    claims <- dplyr::bind_rows(
      claims, tibble::tibble(
        npi = "3", year = 2024L, source = "medicaid_spending",
        attribute = "practice_zip5", value = "80204"
      )
    )
  })), "claims contradict")
})

testthat::test_that(
  "F18: recipient business ZIP cannot pass as a practice ZIP claim", {
  testthat::expect_error(federal_saboteur(base::quote({
    claims <- dplyr::bind_rows(
      claims, tibble::tibble(
        npi = "1", year = 2024L, source = "open_payments",
        attribute = "practice_zip5", value = "80204"
      )
    )
  })), "claims contradict")
})

testthat::test_that(
  "F19: zero payments retain the declared Open Payments source", {
  study <- federal_saboteur(base::quote({
    payments <- payments[0, ]
  }))
  row <- federal_presence_row(study, "1", "open_payments",
                              "recipient_business_zip5")
  testthat::expect_identical(
    row$field_status, "not_observed_in_incomplete_extract"
  )
})

testthat::test_that(
  "F20: zero Medicaid physicians retain the declared Medicaid source", {
  study <- federal_saboteur(base::quote({
    medicaid$billing_npi <- "900"
    medicaid$servicing_npi <- NA_character_
  }))
  row <- federal_presence_row(study, "3", "medicaid_spending",
                              "practice_zip5")
  testthat::expect_identical(
    row$field_status, "not_observed_in_incomplete_extract"
  )
  testthat::expect_false(row$completeness_eligible)
})
