fixture_populations <- function() {
  list(
    population = data.frame(npi = c("1", "2"), year = 2026L),
    record_index = data.frame(
      npi = c("1", "2", "1"), year = 2026L, source = c("nppes", "nppes", "pecos")
    ),
    manifest = data.frame(year = 2026L, source = c("nppes", "pecos"), extract_complete = TRUE)
  )
}

test_that("attribute_analysis_populations separates coverage, completeness, and consistency by size", {
  f <- fixture_populations()
  p <- suppressMessages(attribute_analysis_populations(
    f$population, f$record_index, f$manifest, pair = c("nppes", "pecos")
  ))
  expect_equal(nrow(p$coverage), 4) # 2 npis x 2 sources
  expect_equal(nrow(p$completeness), 3) # npi 2 has no pecos record
  expect_equal(nrow(p$consistency), 1) # only npi 1 has a present record in both sources
  expect_equal(p$consistency$id, "1")
  expect_equal(p$accuracy_eligible, p$consistency)
})

test_that("attribute_analysis_populations marks a missing record absent only when the extract is complete", {
  f <- fixture_populations()
  p <- suppressMessages(attribute_analysis_populations(
    f$population, f$record_index, f$manifest, pair = c("nppes", "pecos")
  ))
  missing_pecos <- p$coverage[p$coverage$source == "pecos" & !p$coverage$record_present, ]
  expect_true(all(missing_pecos$record_status == "absent_from_declared_extract"))
})

test_that("attribute_analysis_populations does not treat an incomplete extract's absence as evidence", {
  f <- fixture_populations()
  f$manifest$extract_complete <- c(TRUE, FALSE) # pecos's extract is declared incomplete
  p <- suppressMessages(attribute_analysis_populations(
    f$population, f$record_index, f$manifest, pair = c("nppes", "pecos")
  ))
  missing_pecos <- p$coverage[p$coverage$source == "pecos" & !p$coverage$record_present, ]
  expect_true(all(missing_pecos$record_status == "not_observed_in_incomplete_extract"))
})

test_that("attribute_analysis_populations defaults accuracy_sample to empty and accepts a valid one", {
  f <- fixture_populations()
  p <- suppressMessages(attribute_analysis_populations(
    f$population, f$record_index, f$manifest, pair = c("nppes", "pecos")
  ))
  expect_equal(nrow(p$accuracy_sample), 0)

  sample <- data.frame(npi = "1", year = 2026L)
  p2 <- suppressMessages(attribute_analysis_populations(
    f$population, f$record_index, f$manifest, pair = c("nppes", "pecos"), accuracy_sample = sample
  ))
  expect_equal(nrow(p2$accuracy_sample), 1)
})

test_that("attribute_analysis_populations rejects an accuracy_sample outside the pair's overlap", {
  f <- fixture_populations()
  sample <- data.frame(npi = "2", year = 2026L) # npi 2 has no pecos record -- not in consistency
  expect_error(
    attribute_analysis_populations(
      f$population, f$record_index, f$manifest, pair = c("nppes", "pecos"), accuracy_sample = sample
    ),
    "outside the pair's overlap"
  )
})

test_that("attribute_analysis_populations rejects duplicate record_index keys", {
  f <- fixture_populations()
  f$record_index <- rbind(f$record_index, f$record_index[1, ])
  expect_error(
    attribute_analysis_populations(f$population, f$record_index, f$manifest, pair = c("nppes", "pecos")),
    "duplicate keys"
  )
})

test_that("attribute_analysis_populations rejects duplicate manifest keys", {
  f <- fixture_populations()
  f$manifest <- rbind(f$manifest, f$manifest[1, ])
  expect_error(
    attribute_analysis_populations(f$population, f$record_index, f$manifest, pair = c("nppes", "pecos")),
    "duplicate keys"
  )
})

test_that("attribute_analysis_populations rejects a record_index snapshot absent from manifest", {
  f <- fixture_populations()
  f$record_index$source[1] <- "unregistered_registry"
  expect_error(
    attribute_analysis_populations(f$population, f$record_index, f$manifest, pair = c("nppes", "pecos")),
    "absent from manifest"
  )
})

test_that("attribute_analysis_populations requires every population year to have snapshot metadata", {
  f <- fixture_populations()
  f$population$year[2] <- 2025L
  expect_error(
    attribute_analysis_populations(f$population, f$record_index, f$manifest, pair = c("nppes", "pecos")),
    "snapshot metadata"
  )
})

test_that("attribute_analysis_populations rejects a pair source not present in manifest", {
  f <- fixture_populations()
  expect_error(
    attribute_analysis_populations(f$population, f$record_index, f$manifest, pair = c("nppes", "ghost")),
    "not in manifest"
  )
})

test_that("attribute_analysis_populations requires extract_complete to be logical with no NA", {
  f <- fixture_populations()
  f$manifest$extract_complete <- c("yes", "no")
  expect_error(attribute_analysis_populations(f$population, f$record_index, f$manifest, pair = c("nppes", "pecos")))
})

test_that("attribute_analysis_populations requires the standard columns on each input", {
  f <- fixture_populations()
  expect_error(
    attribute_analysis_populations(f$population[, "npi", drop = FALSE], f$record_index, f$manifest,
                                     pair = c("nppes", "pecos")),
    "missing column"
  )
})
