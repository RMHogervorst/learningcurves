test_that("user errors are captured", {
    test <- data.frame(
        type=c(rep(c("a","b","c"),2),rep("d",8)),
        thingy =rep(c(1,2),7)
    )

  # n steps not possible with min, max
  # strata not in dataset

  # total lenght, steps and min cannot be empty, steps must be integer
  # test prop between 0 and 1
})

test_that("sane defaults are chosen",{
    test <- data.frame(
        type=c(rep(c("a","b","c"),2),rep("d",8)),
        thingy =rep(c(1,2),7)
    )
    # if(no min, choose at least n_strata)
    expect_error(
        incremental_set(
            dataset=test,
            steps = 3,
            min_data_size = NULL,
            prop = 3 / 4,
            strata = type), "Cannot take 3 out of 6 with min 4"
    )
    result1 <- incremental_set(
        dataset=test,
        steps = 2,
        min_data_size = NULL,
        prop = 3 / 4,
        strata = type)
    expect_equal(nrow(result1), 2)
    expect_equal(length(result1$splits[[1]]$in_id), 4)
})
