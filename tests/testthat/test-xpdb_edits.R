context("test xpdb_edits")

test_that('Allow assignment within object while maintaining the class', {
  xpdb_dollar <- xpdb_ex_pk
  xpdb_dollar$options$quiet <- TRUE
  expect_equal(class(xpdb_dollar), class(xpdb_ex_pk))

  xpdb_bracket <- xpdb_ex_pk
  xpdb_bracket[["options"]]$quiet <- TRUE
  expect_equal(class(xpdb_bracket), class(xpdb_ex_pk))
})
