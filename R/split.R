
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
    
    nrows <- data_nrow(x)
    
    if (is.null(branch_by)) {

        ## 1) SUBSAMPLING
        
        ## make sure that both branch_props and nbranches are defined
        if (is.null(branch_props)) {
            if (is.null(nbranches))
                stop("At least one of nbranches, branch_props, branch_by must be specified")
            branch_props <- setNames(rep.int(1/nbranches, nbranches), paste0("branch", 1:nbranches))
        } else {
            if (!is.null(nbranches)) 
                warning("Both `branch_props` and `nbranches` supplied; ignoring `nbranches`")
            nbranches <- length(branch_props)
            branch_props <- branch_props/sum(branch_props)
        }
        branch_names <- names(branch_props)

        ## resampling:
        
        if (is.null(ids)) {
            ## simple sampling
            ix <- sample.int(nbranches, nrows, replace = nbranches < nrows, prob = branch_props)
            ix <- map(set_names(1:nbranches, branch_names), ~ which(ix == .x))
        } else {
            ## hash ids into buckets
            nbkts <- 1e5L
            thresholds <- cumsum(branch_props) * nbkts
            ix <-
                if (length(branch_by) > 1)
                    do.call("interaction", x[["data"]][branch_by])
                else
                    as.factor(x[["data"]][[branch_by]])
            bkts <- FeatureHashing::hashed.value(levels(ix)) %% nbkts
            levels(ix) <- branch_names[findInterval(bkts, thresholds, all.inside = TRUE)]
            ix <- map(set_names(levels(ix)), ~ which(ix == .x))
        }
        
    } else {

        ## 2) PREDEFINED SPLIT_BY
        
        if (!is.null(ids))
            warning("Both `branch_by` and `ids` are supplied to mlbranch; ignoring `ids`")
        ix <-
            if (length(branch_by) > 1)
                do.call("interaction", x[["data"]][branch_by])
            else
                as.factor(x[["data"]][[branch_by]])
        ix <- map(set_names(levels(ix)), ~ which(ix == .x))
        
    }

    ## dispatch for non-empty indexes
    old_branch <- x[["branch"]]
    xout <- mllist()
    for (l in names(ix)) {
        if (length(ix[[l]]) > 0) {
            x[["branch"]] <- ix
            val <- mldispatch(data_subset(x, ix[[l]]),
                              branch = l,
                              start_pos = x[["cxtenv"]][["pos"]] + 1L)
            xout <- modifyList(xout, val)
        }
    } 
    xout
}

