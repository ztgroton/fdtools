test_that("`strip_part_from_record` and `strip_product_r` produce identical results", {
  expect_true(
    strip_part_equal(
      strip_part_from_record(IMF_POS_LIST, colnames(IMF_POS_LIST$data$pl$records)[2:3]),
      strip_product_r(
        strip_part_from_record(IMF_POS_LIST, colnames(IMF_POS_LIST$data$pl$records)[2]),
        strip_part_from_record(IMF_POS_LIST, colnames(IMF_POS_LIST$data$pl$records)[3])
      )
    )
  )
})
