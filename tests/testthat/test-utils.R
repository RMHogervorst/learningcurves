test_that("sequences of indices work correctly", {
    expect_equal(seq_builder(step=1,stepsize=1,min=1), 1)
    expect_true(all(seq_builder(2,1,1)== c(1,2)))
    expect_equal(seq_builder(1,1,3),  c(1,2,3))
    expect_true(all(seq_builder(2,1,3)== c(1,2,3,4)))
    expect_equal(seq_builder(step = 6,stepsize = 2,min = 1), c(1:11))
})

test_that("subset_by_idx works with purrr and alone",{
    expect_equal(subset_idx(c(99, 80,70,60), c(1,2,3)) , c(99,80,70))
    expect_equal(
        purrr::map(list(c(1,3,2)), subset_idx, idx=c(99, 70, 60)),
                 list(c(99, 60, 70))
    )
})

test_that("incremental indices work correctly", {
    ex1 <- incremental_indices(min = 4,max = 10, steps = 3)
    expect_equal(length(ex1), 3)
    expect_equal(ex1[[1]], 1:4)
    expect_true(max(ex1[[3]]) <= 10)
    ex2 <- incremental_indices(5, 10, 5)
    expect_equal(length(ex2), 5)
    expect_true(max(ex2[[5]]) <= 10)
})


test_that("copied functions from rsample perform",{
    # strata check
    expect_error(strata_check("petal.width", c("petal.length","horse")))
    expect_error(strata_check(iris, "petal.width"))
    expect_invisible(strata_check("a", c("a","b","c")))
    # names0
    expect_equal(names0(3),c("x1","x2","x3"))
    expect_equal(names0(0), character(0))
})


test_that("preselect_each_class puts at least one of every stratum", {
    test <- data.frame(
        type=c(rep(c("a","b","c"),2),rep("d",8)),
        thingy =rep(c(1,2),7)
    )
    preselected <- preselect_each_class(test,"type")
    expect_equal(length(preselected$train),4)
    # all classes are in training and test indices
    expect_equal(
        unique(test$type[preselected$train]),
        unique(test$type))
    expect_equal(
        unique(test$type[preselected$test]),
        unique(test$type))
})

test_that("Proportional split retrieves prop examples.",{
    prop_test <- proportional_split(c(1:10), 4/5)
    expect_true(all(c(prop_test$train, prop_test$test) %in% c(1:10)))
    expect_equal(length(prop_test$train), 8)
    expect_equal(length(prop_test$train) + length(prop_test$test), 10)
})


test_that("Warn on step value works",{
    expect_error(
        notify_on_step_problem(
            n_testsamples = 5,
            n_trainsamples = 10,
            min =2,
            max_steps =6
            ),
        "Cannot create that many steps of this data, max steps possible: 3")
    expect_error(
        notify_on_step_problem(
            n_testsamples = 5,
            n_trainsamples = 10,
            min =2,
            max_steps =5
        ),
        "Cannot take 5 out of 5 test cases with min 2")
})
