
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
mlsplit <- function(x = identity, nsplits = NULL, split_props = NULL,
                    split_by = NULL, ids = NULL, ...) {
    if (is.function(x))
        return(mlfunction("mlsplit"))
    
    nrows <- data_nrow(x)
    
    if (is.null(split_by)) {

        ## 1) SUBSAMPLING
        
        ## make sure that both split_props and nsplits are defined
        if (is.null(split_props)) {
            if (is.null(nsplits))
                stop("At least one of nsplits, split_props, split_by must be specified")
            split_props <- setNames(rep.int(1/nsplits, nsplits), paste0("split", 1:nsplits))
        } else {
            if (!is.null(nsplits)) 
                warning("Both `split_props` and `nsplits` supplied; ignoring `nsplits`")
            nsplits <- length(split_props)
            split_props <- split_props/sum(split_props)
        }
        split_names <- names(split_props)

        ## resampling:
        
        if (is.null(ids)) {
            ## simple sampling
            ix <- sample.int(nsplits, nrows, replace = nsplits < nrows, prob = split_props)
            ix <- map(set_names(1:nsplits, split_names), ~ which(ix == .x))
        } else {
            ## hash ids into buckets
            nbkts <- 1e5L
            thresholds <- cumsum(split_props) * nbkts
            ix <-
                if (length(split_by) > 1)
                    do.call("interaction", x[["data"]][split_by])
                else
                    as.factor(x[["data"]][[split_by]])
            bkts <- FeatureHashing::hashed.value(levels(ix)) %% nbkts
            levels(ix) <- split_names[findInterval(bkts, thresholds, all.inside = TRUE)]
            ix <- map(set_names(levels(ix)), ~ which(ix == .x))
        }
        
    } else {

        ## 2) PREDEFINED SPLIT_BY
        
        if (!is.null(ids))
            warning("Both `split_by` and `ids` are supplied to mlsplit; ignoring `ids`")
        ix <-
            if (length(split_by) > 1)
                do.call("interaction", x[["data"]][split_by])
            else
                as.factor(x[["data"]][[split_by]])
        ix <- map(set_names(levels(ix)), ~ which(ix == .x))
        
    }

    ## dispatch for non-empty indexes
    old_split <- x[["split"]]
    xout <- mllist()
    for (l in names(ix)) {
        if (length(ix[[l]]) > 0) {
            x[["split"]] <- ix
            val <- mldispatch(data_subset(x, ix[[l]]),
                              branch = l,
                              start_pos = x[["cxtenv"]][["pos"]] + 1L)
            xout <- modifyList(xout, val)
        }
    } 
    xout
}
