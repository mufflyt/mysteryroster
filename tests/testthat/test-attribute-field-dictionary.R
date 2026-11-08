test_that("attribute_field_dictionary cross-joins sources over the field catalog", {
  fields <- data.frame(
    attribute = c("practice_zip5", "mailing_zip5", "credential"),
    definition = c("Practice location ZIP", "Mailing address ZIP; not practice",
                    "Credential text; not a specialty code"),
    location_role = c("practice", "mailing", "identity"),
    normalizer = c("zip5", "zip5", "credential_review")
  )
  dictionary <- attribute_field_dictionary(fields, sources = c("nppes", "pecos", "dac"))
  expect_equal(nrow(dictionary), 9) # 3 sources x 3 attributes
  expect_setequal(dictionary$source, c("nppes", "pecos", "dac"))
})

test_that("attribute_field_dictionary keeps mailing and practice location roles separate", {
  fields <- data.frame(
    attribute = c("practice_zip5", "mailing_zip5"),
    definition = c("Practice location ZIP", "Mailing address ZIP; not practice"),
    location_role = c("practice", "mailing"), normalizer = c("zip5", "zip5")
  )
  dictionary <- attribute_field_dictionary(fields, sources = "nppes")
  roles <- dictionary$location_role[dictionary$attribute %in% c("mailing_zip5", "practice_zip5")]
  expect_setequal(roles, c("mailing", "practice"))
})

test_that("attribute_field_dictionary never invents a requirement status", {
  fields <- data.frame(attribute = "practice_zip5", definition = "d",
                        location_role = "practice", normalizer = "zip5")
  dictionary <- attribute_field_dictionary(fields, sources = c("nppes", "pecos"))
  expect_true(all(dictionary$requirement == "unreviewed"))
  expect_true(all(is.na(dictionary$requirement_reference)))
  expect_true(all(is.na(dictionary$specification_date)))
})

test_that("attribute_field_dictionary assigns declared lineage groups and leaves others as singletons", {
  fields <- data.frame(attribute = "practice_zip5", definition = "d",
                        location_role = "practice", normalizer = "zip5")
  dictionary <- attribute_field_dictionary(
    fields, sources = c("nppes", "pecos", "dac"),
    lineage_groups = list(dac_pecos = c("dac", "pecos"))
  )
  shared <- dictionary[dictionary$source %in% c("dac", "pecos"), ]
  expect_true(all(shared$lineage == "dac_pecos"))
  solo <- dictionary[dictionary$source == "nppes", ]
  expect_true(all(solo$lineage == "nppes")) # no declared group -> its own name
})

test_that("attribute_field_dictionary rejects a lineage group with fewer than 2 members", {
  fields <- data.frame(attribute = "a", definition = "d", location_role = "x", normalizer = "y")
  expect_error(
    attribute_field_dictionary(fields, sources = "s", lineage_groups = list(solo = "s")),
    "fewer than 2 members"
  )
})

test_that("attribute_field_dictionary rejects a source in more than one lineage group", {
  fields <- data.frame(attribute = "a", definition = "d", location_role = "x", normalizer = "y")
  expect_error(
    attribute_field_dictionary(
      fields, sources = c("a", "b", "c"),
      lineage_groups = list(x = c("a", "b"), y = c("b", "c"))
    ),
    "more than one lineage group"
  )
})

test_that("attribute_field_dictionary rejects a lineage member not in sources", {
  fields <- data.frame(attribute = "a", definition = "d", location_role = "x", normalizer = "y")
  expect_error(
    attribute_field_dictionary(fields, sources = "s", lineage_groups = list(fam = c("s", "ghost"))),
    "not in sources"
  )
})

test_that("attribute_field_dictionary rejects duplicate attributes in the field catalog", {
  fields <- data.frame(attribute = c("a", "a"), definition = "d", location_role = "x", normalizer = "y")
  expect_error(attribute_field_dictionary(fields, sources = "s"), "must be unique")
})

test_that("attribute_field_dictionary requires the standard field-catalog columns", {
  expect_error(
    attribute_field_dictionary(data.frame(attribute = "a"), sources = "s"),
    "missing column"
  )
})

test_that("attribute_field_dictionary requires non-empty, unique, non-blank sources", {
  fields <- data.frame(attribute = "a", definition = "d", location_role = "x", normalizer = "y")
  expect_error(attribute_field_dictionary(fields, sources = character(0)))
  expect_error(attribute_field_dictionary(fields, sources = c("a", "a")))
  expect_error(attribute_field_dictionary(fields, sources = "  "), "non-blank")
})
