if (!exists("JDKGE", mode = "function")) {
  root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
  source(file.path(root, "R", "valindex.R"))
  source(file.path(root, "R", "preproc.R"))
  source(file.path(root, "R", "JDKGE.R"))
}
