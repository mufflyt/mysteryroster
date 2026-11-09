testthat::test_that("verified fields retain weights in all-sampled bounds", {
  sample <- base::data.frame(
    npi = base::as.character(1:5), year = 2020,
    weight = base::c(1, 3, 2, 6, 8), stratum = "a",
    eligibility_status = "eligible"
  )
  claims <- base::data.frame(
    npi = sample$npi, year = 2020, source = "registry",
    attribute = "zip", value = "10001"
  )
  questions <- attribute_verification_questionnaire(sample, claims)
  answers <- base::data.frame(
    question_id = questions$question_id[base::match(
      base::as.character(1:3), questions$id
    )],
    answer = base::c("correct", "never_correct", "unable_to_verify"),
    verified_at = base::as.Date("2020-11-08"),
    respondent_role = "physician", verification_method = "telephone"
  )
  contacts <- base::data.frame(
    npi = "4", year = 2020,
    contacted_at = base::as.Date("2020-11-08"),
    channel = "telephone", outcome = "no_answer"
  )
  audit <- attribute_nonresponse_audit(
    sample, questions, answers, contacts
  )
  classified <- attribute_verification_status(
    audit, base::data.frame(source = "registry", attribute = "zip"),
    independent_methods = "telephone"
  )
  design <- listing_validity_design(classified$validity)
  attempted <- validity_sensitivity_bounds(design)
  sampled <- validity_sensitivity_bounds(
    design, denominator = "all_sampled"
  )
  testthat::expect_equal(attempted$denominator, base::c(4, 12, 12))
  testthat::expect_equal(sampled$denominator, base::c(4, 20, 20))
  testthat::expect_equal(sampled$pct_valid, base::c(25, 85, 5))
  testthat::expect_equal(validity_estimates(design)$estimate, 0.05)
  testthat::expect_equal(
    validity_estimates(design, definitive_only = TRUE)$estimate, 0.25
  )
  testthat::expect_equal(base::nrow(classified$exclusions), 0)
  testthat::expect_equal(base::sum(classified$validity$weight), 20)
})
