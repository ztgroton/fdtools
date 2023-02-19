test_that("`err_key_from_strip_part` and `err_key_from_records` produce equal results", {
  expect_equal(
    err_key_from_strip_part(strip_part(IMF_POS_LIST, colnames(IMF_POS_LIST$data$pl$records)[c(2,7)]))$err,
    err_key_from_records(IMF_POS_LIST, colnames(IMF_POS_LIST$data$pl$records)[c(2,7)])$err
  )
})
