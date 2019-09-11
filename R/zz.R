.onLoad <- function(libname, pkgname) {
  op <- options()
  op.easydb <- list(
    easydb.overwrite = FALSE,
    easydb.name = "Antoine Fabri",
    easydb.desc.author = "Antoine Fabri <antoine.fabri@gmail.com> [aut, cre]",
    easydb.desc.license = "GPL-3",
    easydb.desc.suggests = NULL,
    easydb.desc = list()
  )
  toset <- !(names(op.easydb) %in% names(op))
  if(any(toset)) options(op.easydb[toset])
  invisible()
}
