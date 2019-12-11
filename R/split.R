
##' @export
mlsample <- function(x = identity, sample_n = NULL, sample_prop = NULL, sample_replace = FALSE, ...) {
    if (is.function(x))
        return(mlfunction("mlsplit"))
    n <- 
        if (!is.null(sample_n)) {
            sample_n
        } else {
            if (is.null(sample_prop))
                stop("At least one of `sample_prop` and `sample_n` must be provided")
            if (sample_prop < 0 || sample_prop > 1)
                stop("`sample_prop` must be between 0 and 1")
            floor(sample_prop * data_nrow(x))
        }
    sub <- sample.int(data_nrow(x), n, replace = sample_replace)
    mlcontinue(data_subset(x, sub))
}

##' @export
mlbranch <- function(x = identity, nbranches = NULL, branch_props = NULL,
                     branch_by = NULL, ids = NULL, ...) {
    if (is.function(x))
        return(mlfunction("mlbranch"))
    ix <- split_ix(x, nr = nbranches, props = branch_props,
                           by = branch_by, ids = ids, type = "branch")
    xout <- mllist()
    for (bnm in names(ix)) {
        ## dispatch for non-empty indexes only
        if (length(ix[[bnm]]) > 0) {
            val <- mldispatch(data_subset(x, ix[["bnm"]]),
                              branch = bnm,
                              start_pos = pos(x) + 1L)
            xout <- modifyList(xout, val)
        }
    }
    xout
}

##' @export
mlbatch <- function(x = identity, batch_size = NULL, batch_by = NULL, ids = NULL, ...) {
    if (is.function(x))
        return(mlfunction("mlbatch"))
    ix <- fixed_split_ix(x, size = batch_size, by = batch_by, ids = ids, type = "batch")
    old_batch <- x[["batch"]]
    old_batch.ix <- x[["batch.ix"]]
    data <- x[["data"]]
    for (bnm in names(ix)) {
        ## dispatch for non-empty indexes only
        if (length(ix[[bnm]]) > 0) {
            x <- mldispatch(data_subset(assoc(x,
                                              "data", data, 
                                              "batch", bnm, 
                                              "batch.ix", ix[["bnm"]]),
                                        ix[[bnm]]),
                            branch = bnm,
                            start_pos = pos(x) + 1L)
        }
    }
    assoc(x,
          "batch", old_batch,
          "batch.ix", old_batch.ix)    
}

##' @export
mlsplit <- function(x = identity, nsplits = NULL, split_props = NULL,
                    split_by = NULL, ids = NULL, ...) {
    if (is.function(x))
        return(mlfunction("mlsplit"))
    ix <- split_ix(x, n = nsplits, props = split_props,
                   by = split_by, ids = ids, type = "split")
    assoc(mlcontinue(assoc(x,
                           "split.ix", ix,
                           "split.data", x[["data"]])),
          "split.data", x[["split.data"]], 
          "split.ix", x[["split.ix"]])
}

##' @export
mlwith <- function(x = identity, with, ...) {
    if (is.function(x))
        return(mlfunction("mlwith"))
    split <- x[["split.ix"]][[with]]
    if (is.null(split))
        stop(sprintf("Data split '%s' is unavailable", with))
    split_name <- names(x[["split.ix"]])[[with]]
    assoc(mldispatch(data_subset(assoc(x,
                                       "data", x[["split.data"]],
                                       "split", split_name),
                                 split),
                     branch = names(split),
                     start_pos = pos(x) + 1L),
          "split", x[["split"]])          
}



## INTERNALS
plural_types <- list2env(list("branch" = "branches",
                              "split" = "splits"))

.idsix <- function(x, ids) {
    if (length(ids) > 1) do.call("interaction", x[["data"]][ids])
    else as.factor(x[["data"]][[ids]])
}

split_ix <- function(x, size = NULL, n = NULL, props = NULL, by = NULL, ids = NULL, type) {
    if (is.null(by) && is.null(size)) {
        random_split_ix(x, n = n, props = props, ids = ids, type = type)
    } else {
        if (!is.null(props))
            warning(sprintf("`%s_size` or `%s_by` is supplied; ignoring `%s_props`", type, type, type))
        if (!is.null(n))
            warning(sprintf("`%s_size` or `%s_by` is supplied; ignoring `n%s`", type, type, plural_types[[type]]))
        fixed_split_ix(x, size = size, by = by, ids = ids, type = type)
    }
}

fixed_split_ix <- function(x, size = NULL, by = NULL, ids = NULL, type) {
    if (!is.null(by)) {
        if (!is.null(ids))
            warning(sprintf("Both `%s_by` and `ids` are supplied to mlsplit; ignoring `ids`", type))
        if (!is.null(size))
            warning(sprintf("Both `%s_by` and `%s_size` are supplied to mlsplit; ignoring `%s_size`", type, type, type))
        ix <-
            if (length(by) > 1)
                do.call("interaction", x[["data"]][by])
            else
                as.factor(x[["data"]][[by]])
        ix <- map(set_names(levels(ix)), ~ which(ix == .x))
    } else if (!is.null(size)) {
        nrows <- data_nrow(x)
        nbkts <- nrows %/% size + 1L
        if (is.null(ids)) {
            ix <- rep(seq_len(nbkts), each = size, length.out = nrows)
            ix <- split(seq_len(nrows), ix)
            names(ix) <- paste0(type, names(ix))
        } else {
            ix <- .idsix(x, ids)
            bkts <- as.integer(FeatureHashing::hashed.value(levels(ix)) %% nbkts)
            ix <- split(seq_len(nrows), bkts[as.integer(ix)])
            names(ix) <- paste0(type, seq_along(unique(bkts)))
        }
    } else {
        stop(sprintf("One of '%s_by' or `%s_size` must be supplied", type))
    }
    ix
}

random_split_ix <- function(x, n = NULL, props = NULL, by = NULL, ids = NULL, type) {
    
    ## make sure that both `props` and `n` are defined
    if (is.null(props)) {
        if (is.null(n))
            stop(sprintf("At least one of 'n%s', '%s_props', '%s_by' must be specified",
                         plural_types[[type]], type, type))
        props <- setNames(rep.int(1/n, n), paste0(type, 1:n))
    } else {
        if (!is.null(n)) 
            warning(sprintf("Both `%s_props` and `n%s` supplied; ignoring `n%s`",
                            type, plural_types[[type]], plural_types[[type]]))
        n <- length(props)
        props <- props/sum(props)
    }
    names <- names(props)

    ## resampling:
    
    if (is.null(ids)) {
        nrows <- data_nrow(x)
        ## simple sampling
        ix <- sample.int(n, nrows, replace = n < nrows, prob = props)
        ix <- map(set_names(1:n, names), ~ which(ix == .x))
    } else {
        ## hash ids into buckets
        nbkts <- 1e5L
        thresholds <- cumsum(props) * nbkts
        ix <- .byix(x, by)
        bkts <- FeatureHashing::hashed.value(levels(ix)) %% nbkts
        levels(ix) <- names[findInterval(bkts, thresholds, all.inside = TRUE)]
        ix <- map(set_names(levels(ix)), ~ which(ix == .x))
    }

    ix
}
