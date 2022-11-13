sample <- data.frame("value"=1:10)
test1 <- group_into(sample, "value", "group", c("Small", "Medium", "Large"))
test_that("group_into function test", {
  expect_equal(as.character(test1$group[1]), "Small")
  expect_error(group_into(sample, "value_not_existing", "area", c("Small", "Medium", "Large")))
  expect_error(group_into(sampe, "value", "group", c())) # empty vector
})

rm(sample)
rm(test1)