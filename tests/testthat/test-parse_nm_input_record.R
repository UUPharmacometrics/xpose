context("Check parse_nm_input_record")

test_that("parse_nm_input_record", {
  expect_equal(parse_nm_input_record(character(0)), character(0),
               info="Empty gives empty")
  expect_equal(parse_nm_input_record("FOO"), c(FOO="FOO"),
               info="Self-naming applies")
  expect_equal(parse_nm_input_record("ID"), c(ID="ID"),
               info="Self-naming applies for reserved words")
  expect_equal(parse_nm_input_record("ID=FOO"), c(ID="FOO"),
               info="Different-naming applies for reserved words")
  expect_equal(parse_nm_input_record("FOO=ID"), c(ID="FOO"),
               info="Different-naming makes reserved words into the name regardless of order")
  expect_equal(parse_nm_input_record("FOO=ID BAR"), c(ID="FOO", BAR="BAR"),
               info="Multiple inputs work with mixed assignment or not")
  expect_equal(parse_nm_input_record("FOO=ID BAR=BAZ"), c(ID="FOO", BAR="BAZ"),
               info="Multiple inputs work with both assigned")
  expect_equal(parse_nm_input_record(c("FOO=ID", "BAR")), c(ID="FOO", BAR="BAR"),
               info="Vector input works")
  expect_equal(parse_nm_input_record(data.frame(baz=c("ID=FOO", "BAR"))),
               c(ID="FOO", BAR="BAR"),
               info="Data.frames work (even when the input is a factor)")
  expect_error(parse_nm_input_record(data.frame(baz=c("ID=FOO", "BAR"), a=1)),
               info="Data.frames are appropriately not recognized")
  expect_equal(parse_nm_input_record(data.frame(subroutine=c("inp", "inp", "foo"),
                                                code=c("ID=FOO", "BAR", "===="),
                                                a=1)),
               c(ID="FOO", BAR="BAR"),
               info="Data.frames with the right columns are recognized")
  expect_equal(parse_nm_input_record(
    structure(list(code=data.frame(subroutine=c("inp", "inp", "foo"),
                                   code=c("ID=FOO", "BAR", "===="),
                                   a=1)),
              class="xpose_data")),
    c(ID="FOO", BAR="BAR"),
    info="xpose_data has the right part extracted")
})