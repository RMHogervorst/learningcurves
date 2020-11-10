
seq_builder <- function(step, stepsize, min) {
  1:(stepsize * (step - 1) + min)
}

subset_idx <- function(idx, seq) {
  idx[seq]
}

incremental_indices <- function(min, max, steps) {
  stepsize <- max(1, floor((max - min) / steps))
  incremental_indices <- list()
  for (step in seq_len(steps)) {
    incremental_indices[[step]] <- seq_builder(step, stepsize, min)
  }
  incremental_indices
}

notify_on_step_problem <- function(n_testsamples,n_trainsamples, min, steps){
    if(n_testsamples < steps | n_trainsamples < steps){
        min_val <- min(n_testsamples, n_trainsamples)
        steps <- min_val-min
        stop(paste0("Cannot create that many steps of this data, max steps possible: ",steps),call. = FALSE)
    }
    if(steps + min > n_testsamples){
        stop(paste0("Cannot take ", steps, " out of ",n_testsamples, " test cases with min ",min),call. = FALSE)
    }
    if(steps + min > n_trainsamples){
        stop(paste0("Cannot take ", steps, " out of ",n_trainsamples, "train cases with min ",min),call. = FALSE)
    }
}

# not exported from rsample, lets not break everything and copy their code
# So this code is GPL2 too.
names0 <- function(num, prefix = "x") {
  if (num == 0L) {
    return(character())
  }
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}
# not exported from rsample, lets not break everything and copy their code
# So this code is GPL2 too.
strata_check <- function(strata, vars) {
  if (!is.null(strata)) {
    if (!is.character(strata) | length(strata) != 1) {
      rlang::abort("`strata` should be a single character value.")
    }
    if (!(strata %in% vars)) {
      rlang::abort(strata, " is not in `data`.")
    }
  }
  invisible(NULL)
}


preselect_each_class <- function(data, strata) {
  strata_tps <- unique(data[[strata]])
  per_class <- purrr::map(strata_tps, ~ which(data[[strata]] == .x))
  if (min(lengths(per_class)) < 2) {
    stop("need at least 2 of every class", call. = FALSE)
  }
  examples <- purrr::map(per_class, sample, size = 2)
  trainindices <- purrr::map_int(examples, 1)
  testindices <- purrr::map_int(examples, 2)
  list(
    train = trainindices,
    test = testindices
  )
}


proportional_split <- function(indices, prop) {
  n_indices <- length(indices)
  train_n <- floor(n_indices * prop)
  if (n_indices - train_n == 0) {
    stop("There are not enough samples to make this proportion split", call. = FALSE)
  }
  train_examples <- sample(indices, size = train_n, replace = FALSE)
  test_examples <- indices[!indices %in% train_examples]
  list(
    train = train_examples,
    test = test_examples
  )
}


create_rsample_obj <- function(analysis_, assessment_, data, class = NULL) {
    res <- structure(
        list(
            data = data,
            in_id = analysis_,
            out_id = assessment_
        ),
        class = "rsplit"
    )
    if (!is.null(class)) {
        class(res) <- c(class(res),class)
    }
    res
}
