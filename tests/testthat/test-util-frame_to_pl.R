test_that("`frame_to_pl` and `pl_to_frame` are inverses", {
  expect_identical(IMF_DATA, pl_to_frame(frame_to_pl(IMF_DATA), TRUE))
  expect_identical(IMF_DATA, pl_to_frame(frame_to_pl(IMF_DATA), FALSE))
})

test_that("`pl_to_frame` produces identical outputs", {
  expect_identical(
    pl_to_frame(frame_to_pl(IMF_DATA), TRUE),
    pl_to_frame(frame_to_pl(IMF_DATA), FALSE)
  )
})
