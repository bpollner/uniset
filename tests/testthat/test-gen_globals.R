# library(testthat)

ptp <- path.package("uniset")
fp <- paste0(ptp, "/R/gen_globals.R")
test_that("all character", {
    expect_true({
        tenv <- new.env()
        sys.source(fp, envir = tenv)
        a <- ls(tenv)
        vec <- NULL
        for (i in 1: length(ls(tenv))) {
            vec <- c(vec, unlist(get(a[i]), tenv))
        } # end i
        vec
        all(is.character(vec))
    })
}) # EOT

