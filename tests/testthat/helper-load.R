root <- normalizePath(file.path("..", ".."), mustWork = TRUE)

if (!exists("valindex", mode = "function")) {
  source(file.path(root, "R", "valindex.R"))
}

if (!exists("preproc", mode = "function")) {
  source(file.path(root, "R", "preproc.R"))
}

if (!exists("JDKGE", mode = "function")) {
  source(file.path(root, "R", "JDKGE.R"))
}

if (!exists("PMR", mode = "function")) {
  source(file.path(root, "R", "PMR.R"))
}

if (!exists("plot2", mode = "function")) {
  source(file.path(root, "R", "plot2.R"))
}

if (!exists("ggof", mode = "function")) {
  source(file.path(root, "R", "ggof.R"))
}
