audit_fixture <- function() {
  sample <- data.frame(npi = c("1", "2", "3"), year = 2026, weight = c(2, 3, 5),
                        eligibility_status = "eligible")
  claims <- data.frame(npi = sample$npi, year = 2026, source = "nppes",
                        attribute = "practice_zip5", value = "80204")
  questions <- suppressMessages(attribute_verification_questionnaire(sample, claims))
  list(sample = sample, claims = claims, questions = questions,
       no_contacts = data.frame(npi = character(0), year = numeric(0),
                                 contacted_at = as.Date(character(0)), channel = character(0),
                                 outcome = character(0)))
}

one_answer <- function(f, answer = "correct", id = "1") {
  data.frame(
    question_id = f$questions$question_id[f$questions$id == id], answer = answer,
    verified_at = as.Date("2026-11-08"), respondent_role = "physician"
  )
}

test_that("attribute_nonresponse_audit reproduces the hand-traced reference fixture", {
  f <- audit_fixture()
  contacts <- data.frame(npi = c("1", "2"), year = 2026, contacted_at = as.Date("2026-11-08"),
                          channel = "telephone", outcome = c("reached", "refused"))
  audit <- suppressMessages(attribute_nonresponse_audit(
    f$sample, f$questions, one_answer(f, "formerly_correct"), contacts
  ))
  expect_equal(nrow(audit$providers), 3)
  expect_equal(audit$response_rates$pct_complete, 20)
  expect_equal(audit$providers$response_status[audit$providers$id == "1"], "complete_response")
  expect_equal(audit$providers$response_status[audit$providers$id == "2"], "refused")
  expect_equal(audit$providers$response_status[audit$providers$id == "3"], "not_attempted")
  expect_true(audit$items$incorrect[audit$items$id == "1"])
  expect_equal(sum(audit$flow$weighted_total), 10)
})

test_that("attribute_nonresponse_audit distinguishes refusal from unreachable nonresponse", {
  f <- audit_fixture()
  refused <- data.frame(npi = "2", year = 2026, contacted_at = as.Date("2026-11-08"),
                         channel = "telephone", outcome = "refused")
  audit <- suppressMessages(attribute_nonresponse_audit(f$sample, f$questions, one_answer(f), refused))
  expect_equal(sum(audit$items$incorrect), 0) # refusal is not incorrectness
  expect_true("refused" %in% audit$providers$response_status)

  no_answer <- refused; no_answer$outcome <- "no_answer"
  audit2 <- suppressMessages(attribute_nonresponse_audit(f$sample, f$questions, one_answer(f), no_answer))
  expect_true("contacted_no_response" %in% audit2$providers$response_status)
  expect_false("refused" %in% audit2$providers$response_status)
})

test_that("attribute_nonresponse_audit rejects an answer with a future verified_at", {
  f <- audit_fixture()
  ans <- one_answer(f)
  ans$verified_at <- Sys.Date() + 365
  expect_error(
    attribute_nonresponse_audit(f$sample, f$questions, ans, f$no_contacts),
    "future"
  )
})

test_that("attribute_nonresponse_audit rejects a duplicate assertion under two question_ids", {
  f <- audit_fixture()
  dup <- f$questions[1, ]
  dup$question_id <- "different_id_same_assertion"
  q_dup <- rbind(f$questions, dup)
  expect_error(
    attribute_nonresponse_audit(f$sample, q_dup, one_answer(f), f$no_contacts),
    "duplicate assertion"
  )
})

test_that("attribute_nonresponse_audit rejects an inconsistent value_missing flag", {
  f <- audit_fixture()
  q_bad <- f$questions
  q_bad$value_missing[1] <- !q_bad$value_missing[1]
  expect_error(
    attribute_nonresponse_audit(f$sample, q_bad, one_answer(f), f$no_contacts),
    "value_missing"
  )
})

test_that("attribute_nonresponse_audit rejects zero or infinite sampling weights", {
  f <- audit_fixture()
  s_zero <- f$sample; s_zero$weight[1] <- 0
  expect_error(attribute_nonresponse_audit(s_zero, f$questions, one_answer(f), f$no_contacts), "weight")
  s_inf <- f$sample; s_inf$weight[1] <- Inf
  expect_error(attribute_nonresponse_audit(s_inf, f$questions, one_answer(f), f$no_contacts), "weight")
})

test_that("attribute_nonresponse_audit rejects an answer category not in the codebook", {
  f <- audit_fixture()
  expect_error(
    attribute_nonresponse_audit(f$sample, f$questions, one_answer(f, "probably_correct"), f$no_contacts),
    "codebook"
  )
})

test_that("attribute_nonresponse_audit rejects an answer referencing an invented question_id", {
  f <- audit_fixture()
  ans <- one_answer(f)
  ans$question_id <- "not_a_real_question"
  expect_error(
    attribute_nonresponse_audit(f$sample, f$questions, ans, f$no_contacts),
    "not in questions"
  )
})

test_that("attribute_nonresponse_audit rejects a duplicate answer to the same question", {
  f <- audit_fixture()
  dup_ans <- rbind(one_answer(f), one_answer(f))
  expect_error(
    attribute_nonresponse_audit(f$sample, f$questions, dup_ans, f$no_contacts),
    "duplicate keys"
  )
})

test_that("attribute_nonresponse_audit rejects a contact for a provider outside the sample", {
  f <- audit_fixture()
  bad_contact <- data.frame(npi = "not_in_sample", year = 2026, contacted_at = as.Date("2026-11-08"),
                             channel = "telephone", outcome = "reached")
  expect_error(
    attribute_nonresponse_audit(f$sample, f$questions, one_answer(f), bad_contact),
    "do not reconcile"
  )
})

test_that("attribute_nonresponse_audit gives an undefined rate, not zero, when everyone is confirmed ineligible", {
  f <- audit_fixture()
  s_inelig <- f$sample
  s_inelig$eligibility_status <- "confirmed_ineligible"
  audit <- suppressMessages(attribute_nonresponse_audit(s_inelig, f$questions, one_answer(f), f$no_contacts))
  expect_equal(audit$response_rates$w_denominator, 0)
  expect_true(is.na(audit$response_rates$pct_complete))
  expect_true(is.na(audit$response_rates$pct_any_response))
  expect_equal(sum(audit$flow$n_providers), 3) # confirmed-ineligible providers still appear in flow
})

test_that("attribute_nonresponse_audit keeps unknown eligibility in the response denominator", {
  f <- audit_fixture()
  f$sample$eligibility_status[2] <- "unknown"
  audit <- suppressMessages(attribute_nonresponse_audit(f$sample, f$questions, one_answer(f), f$no_contacts))
  expect_equal(audit$response_rates$w_denominator, 10)
})

test_that("attribute_nonresponse_audit does not inflate the denominator from repeated contact attempts", {
  f <- audit_fixture()
  one <- data.frame(npi = "2", year = 2026, contacted_at = as.Date("2026-11-08"),
                     channel = "telephone", outcome = "no_answer")
  repeated <- rbind(one, one, one)
  audit <- suppressMessages(attribute_nonresponse_audit(f$sample, f$questions, one_answer(f), repeated))
  p2 <- audit$providers[audit$providers$id == "2", ]
  expect_equal(p2$n_attempts, 3)
  expect_equal(audit$response_rates$w_denominator, 10)
  expect_equal(sum(audit$flow$n_providers), 3)
})

test_that("attribute_nonresponse_audit treats unable_to_verify as answered but not definitive", {
  f <- audit_fixture()
  audit <- suppressMessages(attribute_nonresponse_audit(
    f$sample, f$questions, one_answer(f, "unable_to_verify"), f$no_contacts
  ))
  expect_equal(sum(audit$items$answered), 1)
  expect_equal(sum(audit$items$definitive), 0)
})

test_that("attribute_nonresponse_audit treats not_applicable as answered but never incorrect", {
  f <- audit_fixture()
  audit <- suppressMessages(attribute_nonresponse_audit(
    f$sample, f$questions, one_answer(f, "not_applicable"), f$no_contacts
  ))
  expect_equal(sum(audit$items$answered), 1)
  expect_equal(sum(audit$items$definitive), 0)
  expect_equal(sum(audit$items$incorrect), 0)
})

test_that("attribute_nonresponse_audit is stable under shuffled answer row order", {
  f <- audit_fixture()
  answers <- data.frame(
    question_id = f$questions$question_id,
    answer = c("correct", "never_correct", "unable_to_verify"),
    verified_at = as.Date("2026-11-08"), respondent_role = "physician"
  )
  before <- suppressMessages(attribute_nonresponse_audit(f$sample, f$questions, answers, f$no_contacts))
  after <- suppressMessages(attribute_nonresponse_audit(
    f$sample, f$questions, answers[rev(seq_len(nrow(answers))), ], f$no_contacts
  ))
  expect_identical(before$providers, after$providers)
  expect_identical(before$response_rates, after$response_rates)
})

test_that("attribute_nonresponse_audit requires the standard columns on every input", {
  f <- audit_fixture()
  expect_error(
    attribute_nonresponse_audit(f$sample[, "npi", drop = FALSE], f$questions, one_answer(f), f$no_contacts),
    "missing column"
  )
})
