#' Learning curve
#'
#' A method described by Andrew Ng in Coursera Machine Learning videos.
#' A learning curve is a plot of model learning performance over experience
#' or time.
#' Diagnostic tool. Can diagnose model problems such as underfit or overfit of model
#' Commonly used in deep learning, after x epochs or something. but can be
#' used in other machine learning too.
#'
#' Returns 2 curves.
#'
#' Train Learning Curve: Learning curve calculated from the training dataset that gives an idea of how well the model is learning.
#' Validation Learning Curve: Learning curve calculated from a hold-out validation dataset that gives an idea of how well the model is generalizing.

#' ## Procedure
#' Train model on a few points, predict add a few points, predict again
#' all the way to the total size of training set.
#' Plot train error and test error over sample size. These plots can
#' indicate bias or variance.
#' High bias: errors are high, not even good performance on the training set.
#' Almost the same errors on training and cv set. More data does not really help.
#' High variance: low train error. High CV error.
#'
#' ## Details
#' We're reusing the infrastructure created by rsample package here, because
#' frankly it is very elegant and works incredibly well.
#' rsample makes use of the fact that a dataset will only be copied if you
#' modify it. Rsample objects contain the same dataset and indices of the
#' rows we will use. That way there will be no unneccessary copies of data.

#' Create incremental dataset
#'
#' Create 'learning curve' dataset. with incrementally
#' larger training and validation sets.
#'
#' When using the strata argument every split is garanteed to have at least one
#' of each class in both test and training. However the proportions can mismatch
#' your total dataset. Generally you use this function to figure out if your
#' algorithm and preprocessing is worthwile to continue. You do not usually care
#' that much if the data exactly matches proportions.This is iintened as an easy
#' check to see if your algorightm is converting, and if more inormatoin leads
#' to better results.
#'
#'
#' @export
incremental_set <- function(dataset, steps = 10, min_data_size = NULL, prop = 3 / 4, strata = NULL) {
    if (!missing(strata)) {
        strata <- tidyselect::vars_select(names(dataset), !!enquo(strata))
        if (length(strata) == 0) {
            strata <- NULL
        }
    }
    strata_check(strata, names(dataset))
    if(!is.null(strata)){
        preselected_indices <- preselect_each_class(dataset, strata)
        n_unique_in_strata <- length(preselected_indices$train)
        all_indices <- 1:nrow(dataset)
        others <- all_indices[!all_indices %in% purrr::flatten_int(preselected_indices)]
        prop_splits <- proportional_split(others, prop=prop)
        full_train_idx <- c(preselected_indices$train, prop_splits$train)
        full_test_idx <- c(preselected_indices$test, prop_splits$test)
        min_data_size <- ifelse(is.null(min_data_size), n_unique_in_strata, max(min_data_size, n_unique_in_strata))
    }else{
        all_indices <- 1:nrow(dataset)
        prop_splits <- proportional_split(all_indices, prop=prop)
        full_train_idx <-  prop_splits$train
        full_test_idx <- prop_splits$test
        min_data_size <- ifelse(is.null(min_data_size), 1, min_data_size)
    }
    notify_on_step_problem(
        n_testsamples=length(full_test_idx),
        n_trainsamples = length(full_train_idx),
        min=min_data_size,
        steps=steps
        )
    train_indices <- incremental_indices(min_data_size, length(full_train_idx),steps = steps)
    test_indices <- incremental_indices(min_data_size, length(full_test_idx),steps = steps)
    split_objs <- purrr::map2(train_indices, test_indices, create_rsample_obj, data=dataset)
    result <- tibble::tibble(
        splits = split_objs,
        id = names0(length(split_objs), "Increment")
    )
    class(result) <- c("incremental_set",'rset',class(result))
    result
}

