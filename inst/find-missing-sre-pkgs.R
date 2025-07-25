##' Find missing dependencies in R packages


##' This was run on premium9 on May 1 2024, the list of packages at the bottom
##' reflect the missing packages as of that run.

#' Get list of installed packages

lib <- .libPaths()[2]

installed <- installed.packages()

#' loop over packages and get the dependencies from the DESCRIPTION file
deps <- lapply(rownames(installed),
           function(x) {
             lib <- installed[x, "LibPath"]
             desc::desc_get_deps(file.path(lib, x))
           })

all_deps <- do.call(rbind, deps)

all_deps <- unique(all_deps[, c("type", "package")])

missing_depends <- sort(setdiff(all_deps$package[all_deps$type == "Depends"], c(installed[,"Package"], "R")))
missing_imports <- sort(setdiff(all_deps$package[all_deps$type == "Imports"], installed[,"Package"]))
missing_suggests <- sort(setdiff(all_deps$package[all_deps$type == "Suggests"], installed[,"Package"]))
missing_enhances <- sort(setdiff(all_deps$package[all_deps$type == "Enhances"], installed[,"Package"]))

#' Missing Depends and missing Imports are most important - critical to package functionality.

(need <- sort(unique(c(missing_depends, missing_imports))))

#' Since we don't have access to install from the internet, it would make sense to
#' include packages in Suggests and Enhances as well.

sort(setdiff(unique(c(missing_suggests, missing_enhances)), need))
