# Adversarial tests for normalize_directory_attribute().
# Each block pins a way the previous hand-rolled normalizers silently
# produced a DIFFERENT valid-looking value instead of the right one or an
# explicit rejection.

zip_norm <- function(x) normalize_directory_attribute(x, "zip5")

test_that("zip5: ZIP+4 that lost its leading zero is not shifted into another ZIP", {
  # Boston 02101-1234 after a numeric round-trip; the old code returned 21011
  expect_identical(zip_norm("2101-1234")$normalized_value, "02101")
  expect_identical(zip_norm("21011234")$normalized_value, "02101")
  expect_identical(zip_norm("2101 1234")$normalized_value, "02101")
  # two leading zeros lost: 00501-1234
  expect_identical(zip_norm("5011234")$normalized_value, "00501")
  # intact forms are unchanged
  expect_identical(zip_norm(c("02101-1234", "021011234", "80204-1234"))$normalized_value,
                   c("02101", "02101", "80204"))
})

test_that("zip5: float artifacts do not gain a trailing digit", {
  # old: "2101.0" -> digits "21010" -> accepted as ZIP 21010
  expect_identical(zip_norm(c("2101.0", "80204.0", "901.00"))$normalized_value,
                   c("02101", "80204", "00901"))
})

test_that("zip5: scientific notation is rejected, never reassembled from its digits", {
  res <- zip_norm(c("8.0204e4", "1.2e5", "8E+04", "8.0204e+04"))
  expect_true(all(is.na(res$normalized_value)))
  expect_true(all(res$validation_status == "invalid_zip5"))
  expect_false(any(res$format_valid))
})

test_that("zip5: digit runs that are not ZIP-shaped are rejected, not truncated", {
  # a phone number in the ZIP column used to become ZIP 30355
  res <- zip_norm(c("3035550199", "123456", "13035550199", "12", "7"))
  expect_true(all(is.na(res$normalized_value)))
  expect_true(all(res$validation_status == "invalid_zip5"))
})

test_that("zip5: documented contract is preserved", {
  z <- zip_norm(c("80204-1234", "7030", "901", "00000", "ZIP UNKNOWN", NA, "", "99951", "00500"))
  expect_identical(z$normalized_value,
                   c("80204", "07030", "00901", NA, NA, NA, NA, NA, NA))
  expect_identical(z$validation_status[6:7], c("missing_or_blank", "missing_or_blank"))
  expect_identical(zip_norm(character(0))$normalized_value, character(0))
  expect_identical(zip_norm(c(80204, 2101))$normalized_value, c("80204", "02101"))
})

test_that("scf3 inherits every zip5 fix", {
  s <- normalize_directory_attribute(c("2101-1234", "2101.0", "1.2e5", "3035550199", "80204"), "scf3")
  expect_identical(s$normalized_value, c("021", "021", NA, NA, "802"))
})

test_that("zip5 output is idempotent and always 5 digits when valid", {
  raw <- c("2101-1234", "21011234", "501", "80204.0", "CO 80204", "7030")
  once <- zip_norm(raw)$normalized_value
  expect_true(all(grepl("^[0-9]{5}$", once)))
  expect_identical(zip_norm(once)$normalized_value, once)
})

name_norm <- function(x) normalize_directory_attribute(x, "person_name")

test_that("person_name: a surname that collides with a credential survives", {
  # old: \\bDO\\b was deleted unconditionally -> "ANH", colliding with any other Anh
  expect_identical(name_norm("Anh Do")$normalized_value, "ANH DO")
  expect_identical(name_norm("Yo-Yo Ma")$normalized_value, "YO-YO MA")
  # ...while the credential itself is still stripped
  expect_identical(name_norm("Jane Smith, DO")$normalized_value, "JANE SMITH")
  expect_false(identical(name_norm("Anh Do")$normalized_value,
                         name_norm("Anh Le")$normalized_value))
})

test_that("person_name: diacritics fold instead of splitting the name", {
  # old: "JOS GARC A"
  accented <- name_norm("Jos\u00e9 Garc\u00eda")$normalized_value
  expect_identical(accented, "JOSE GARCIA")
  expect_identical(accented, name_norm("Jose Garcia")$normalized_value)
  expect_identical(name_norm("Zo\u00eb N\u00fa\u00f1ez")$normalized_value, "ZOE NUNEZ")
})

test_that("person_name: credentials outside the old short list are removed", {
  expect_identical(name_norm("Jane Doe FACOG")$normalized_value, "JANE DOE")
  expect_identical(name_norm("John Smith PhD")$normalized_value, "JOHN SMITH")
  expect_identical(name_norm("Dr. Jane Smith, M.D.")$normalized_value, "JANE SMITH")
  expect_identical(name_norm("John Smith Jr.")$normalized_value, "JOHN SMITH")
})

test_that("person_name: parenthetical nickname or maiden name is not part of the name", {
  expect_identical(name_norm("Jane Doe (Smith)")$normalized_value, "JANE DOE")
  expect_identical(name_norm("Cynthia (Cindi) Smith")$normalized_value, "CYNTHIA SMITH")
})

test_that("person_name: credential-only and too-short input is invalid, never a fake name", {
  res <- name_norm(c("MD", "Dr.", "J", "123", "RN, MSN"))
  expect_true(all(is.na(res$normalized_value)))
  expect_true(all(res$validation_status == "invalid_person_name"))
  miss <- name_norm(c(NA, "", "   "))
  expect_true(all(miss$validation_status == "missing_or_blank"))
  expect_identical(nrow(name_norm(character(0))), 0L)
})

test_that("person_name: spelling variants of one person normalize identically", {
  variants <- c("Mary O'Brien", "MARY O'BRIEN", "  mary   o'brien ", "Dr. Mary O'Brien, MD")
  expect_identical(unique(name_norm(variants)$normalized_value), "MARY O'BRIEN")
})

test_that("npi10 agrees with the canonical mysterynpi validator on every input", {
  good <- c("1234567893", "1962780445", "1396113270", "2000000002")
  bad <- c("1234567890", "0000000006", "3000000000", "ABC", "123456789", "12345678931")
  res <- normalize_directory_attribute(c(good, bad, NA, ""), "npi10")
  expect_identical(res$format_valid, c(rep(TRUE, 4), rep(FALSE, 6), FALSE, FALSE))
  expect_identical(res$validation_status[11:12], rep("missing_or_blank", 2))
  expect_true(all(res$validation_status[5:10] == "invalid_npi_luhn"))
  # every single-digit mutation of a valid NPI is rejected
  base <- "1962780445"
  mutants <- unlist(lapply(1:10, function(pos) {
    vapply(setdiff(as.character(0:9), substr(base, pos, pos)), function(d) {
      m <- base
      substr(m, pos, pos) <- d
      m
    }, character(1))
  }))
  expect_false(any(normalize_directory_attribute(mutants, "npi10")$format_valid))
  # separators are still tolerated, as before
  expect_identical(
    normalize_directory_attribute("123-456-7893", "npi10")$normalized_value, "1234567893"
  )
})

test_that("zip5 and scf3 reject malformed multi-part values", {
  raw <- c("999-999-9999", "9999.9", "80204-123", "80204 12345")
  expect_false(any(zip_norm(raw)$format_valid))
  expect_false(any(normalize_directory_attribute(raw, "scf3")$format_valid))
})

test_that("person_name strips slash and semicolon separated credentials", {
  expect_false(any(name_norm(c("MD/RN", "MD; RN"))$format_valid))
  expect_identical(name_norm("Jane Smith, MD/RN")$normalized_value, "JANE SMITH")
})

test_that("person_name keeps uppercase surnames in surname-first names", {
  expect_identical(name_norm(c("MA, YO-YO", "DO, ANH"))$normalized_value,
                   c("MA YO-YO", "DO ANH"))
  expect_identical(name_norm("Jane Smith, MA")$normalized_value, "JANE SMITH")
  expect_false(name_norm("MA, RN")$format_valid)
})
