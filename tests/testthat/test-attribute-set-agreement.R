test_that("attribute_set_agreement flags any_shared vs same_set correctly for a multi-value source", {
  s <- list(
    nppes = data.frame(npi = c("1", "1", "2"), year = 2024,
                        state = c("CO", "NM", "CO")), # npi 1 has two states
    pecos = data.frame(npi = c("1", "2"), year = 2024, state = c("CO", "NM"))
  )
  sa <- attribute_set_agreement(s, attribute = "state")
  expect_equal(nrow(sa), 2)

  r1 <- sa[sa$id == "1", ]
  expect_true(r1$any_shared) # {CO, NM} vs {CO} -- CO is shared
  expect_false(r1$same_set) # but the sets aren't equal
  expect_equal(r1$n_values_a, 2)
  expect_equal(r1$n_values_b, 1)

  r2 <- sa[sa$id == "2", ]
  expect_false(r2$any_shared) # CO vs NM -- no overlap at all
  expect_false(r2$same_set)
})

test_that("attribute_set_agreement treats same_set as order-independent set equality", {
  s <- list(
    a = data.frame(npi = "9", year = 2024, state = c("CO", "NM")),
    b = data.frame(npi = c("9", "9"), year = 2024, state = c("NM", "CO")) # same set, listed in reverse
  )
  sa <- attribute_set_agreement(s, attribute = "state")
  expect_true(sa$any_shared)
  expect_true(sa$same_set)
})

test_that("attribute_set_agreement would silently cartesian-product under plain attribute_agreement", {
  # This is the exact gap attribute_set_agreement exists to close: attribute_agreement()
  # merges on (id, year) alone, so a source with 2 rows for one identifier-year produces an
  # uncontrolled cross product rather than a comparison. Document the contrast directly.
  s <- list(
    nppes = data.frame(npi = c("1", "1"), year = 2024, state = c("CO", "NM")),
    pecos = data.frame(npi = c("1"), year = 2024, state = c("CO"))
  )
  plain <- attribute_agreement(s, attribute = "state")
  expect_equal(nrow(plain), 2) # cross product: both of nppes's rows paired against pecos's one row

  sets <- attribute_set_agreement(s, attribute = "state")
  expect_equal(nrow(sets), 1) # one row per identifier-year, a genuine set comparison
  expect_true(sets$any_shared)
  expect_false(sets$same_set)
})

test_that("attribute_set_agreement supports a pure cross-section with year = NULL", {
  s <- list(
    a = data.frame(npi = c("1", "1"), state = c("CO", "NM")),
    b = data.frame(npi = "1", state = "CO")
  )
  sa <- attribute_set_agreement(s, attribute = "state", year = NULL)
  expect_equal(nrow(sa), 1)
  expect_true(is.na(sa$year))
})

test_that("attribute_set_agreement normalizes values by trimming and upper-casing", {
  s <- list(
    a = data.frame(npi = "1", year = 2024, state = c(" co ", "nm")),
    b = data.frame(npi = "1", year = 2024, state = "CO")
  )
  sa <- attribute_set_agreement(s, attribute = "state")
  expect_equal(sa$values_a, "CO|NM")
  expect_true(sa$any_shared)
})

test_that("attribute_set_agreement drops missing/empty values before comparing", {
  s <- list(
    a = data.frame(npi = c("1", "1"), year = 2024, state = c("CO", NA)),
    b = data.frame(npi = "1", year = 2024, state = "")
  )
  expect_error(
    attribute_set_agreement(s, attribute = "state"),
    "no identifier-year is covered by more than one source"
  )
})

test_that("attribute_set_agreement errors on a missing column", {
  s <- list(a = data.frame(npi = "1"), b = data.frame(npi = "1", state = "CO"))
  expect_error(attribute_set_agreement(s, attribute = "state"), "has no column 'state'")
})

test_that("attribute_set_agreement errors when no source has any non-missing value", {
  s <- list(
    a = data.frame(npi = "1", year = 2024, state = NA),
    b = data.frame(npi = "1", year = 2024, state = "")
  )
  expect_error(
    attribute_set_agreement(s, attribute = "state"),
    "no non-missing attribute values in any source"
  )
})

test_that("attribute_set_agreement requires at least two uniquely-named sources", {
  s <- list(a = data.frame(npi = "1", state = "CO"))
  expect_error(attribute_set_agreement(s, attribute = "state"))
})

test_that("attribute_set_agreement carries source names as an attribute, in order", {
  s <- list(
    nppes = data.frame(npi = "1", year = 2024, state = "CO"),
    pecos = data.frame(npi = "1", year = 2024, state = "CO")
  )
  sa <- attribute_set_agreement(s, attribute = "state")
  expect_equal(attr(sa, "sources"), c("nppes", "pecos"))
})
