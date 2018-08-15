context("Check parse_nm_input_record")

test_that("parse_nm_input_record", {
  expect_equal(parse_nm_input_record(character(0)), character(0),
               info="Empty gives empty")
  expect_equal(parse_nm_input_record("FOO"), c(FOO="FOO")[-1],
               info="Same name returns nothing (no remapping is required)")
  expect_equal(parse_nm_input_record("ID"), c(ID="ID")[-1],
               info="Same name returns nothing (no remapping is required for reserved words, either)")
  expect_equal(parse_nm_input_record("ID=FOO"), c(ID="FOO"),
               info="Different-naming applies for reserved words")
  expect_equal(parse_nm_input_record("FOO=ID"), c(ID="FOO"),
               info="Different-naming makes reserved words into the name regardless of order")
  expect_equal(parse_nm_input_record("FOO=ID BAR"), c(ID="FOO"),
               info="Multiple inputs work with mixed assignment or not, and only renamed values are returned")
  expect_equal(parse_nm_input_record("FOO=ID BAR=BAZ"), c(ID="FOO", BAR="BAZ"),
               info="Multiple inputs work with both assigned")
  expect_equal(parse_nm_input_record(c("FOO=ID", "BAR")), c(ID="FOO"),
               info="Vector input works")
  expect_equal(parse_nm_input_record(data.frame(baz=c("ID=FOO", "BAR"))),
               c(ID="FOO"),
               info="Data.frames work (even when the input is a factor)")
  expect_error(parse_nm_input_record(data.frame(baz=c("ID=FOO", "BAR"), a=1)),
               info="Data.frames are appropriately not recognized")
  expect_equal(parse_nm_input_record(data.frame(subroutine=c("inp", "inp", "foo"),
                                                code=c("ID=FOO", "BAR", "===="),
                                                a=1)),
               c(ID="FOO"),
               info="Data.frames with the right columns are recognized")
  expect_equal(parse_nm_input_record(
    structure(list(code=data.frame(subroutine=c("inp", "inp", "foo"),
                                   code=c("ID=FOO", "BAR", "===="),
                                   a=1)),
              class="xpose_data")),
    c(ID="FOO"),
    info="xpose_data has the right part extracted")
})