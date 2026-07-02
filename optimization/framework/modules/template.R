## =====================================================================
## modules/template.R — prepare the working APSIM file
## =====================================================================
## Copies the production grid-sim template into the calibration sandbox
## and applies three calibration-only edits (never touching the shared
## template):
##   1. patch the daily report to record phenology stage names + rename
##      it (avoids a results-table name collision with the EndOfYear
##      report), so predicted stage-transition dates can be read out.
##   2. clone one calibration cultivar per GROUP from its production
##      parent, so each group's parameters can be optimized independently.
## These are pure JSON edits via jsonlite (apsimx's helpers only edit
## existing single values, not node structure).
## =====================================================================

## Generic recursive helpers over the parsed APSIM JSON tree.
.find_node <- function(node, name) {
  if (identical(node$Name, name)) return(node)
  for (k in node$Children %||% list()) {
    hit <- .find_node(k, name); if (!is.null(hit)) return(hit)
  }
  NULL
}
`%||%` <- function(a, b) if (is.null(a)) b else a

prepare_template <- function() {
  src <- abspath(CONFIG$template)
  dst <- file.path(abspath(CONFIG$sim_dir), CONFIG$base_file)
  if (file.exists(src)) file.copy(src, dst, overwrite = TRUE)

  tpl <- fromJSON(dst, simplifyVector = FALSE)

  ## --- 1. patch the daily Field report -----------------------------
  patch_report <- function(node, path = "") {
    nm <- node$Name
    newpath <- if (nzchar(path)) paste(path, nm, sep = ".") else nm
    if (identical(newpath, "Simulations.Simulation.Field.Report")) {
      node$Name <- "PhenologyReport"                 # avoid table-name collision
      extra <- c("[Soybean].Phenology.Stage",
                 "[Soybean].Phenology.CurrentPhaseName",
                 "[Soybean].Phenology.CurrentStageName")
      have <- vapply(node$VariableNames, identity, character(1))
      add  <- setdiff(extra, have)
      if (length(add)) node$VariableNames <- c(node$VariableNames[1:2], as.list(add),
                                               node$VariableNames[-(1:2)])
      return(node)
    }
    if (!is.null(node$Children)) node$Children <- lapply(node$Children, patch_report, path = newpath)
    node
  }
  tpl <- patch_report(tpl)

  ## --- 2. clone one cultivar per GROUP -----------------------------
  clone_into <- function(node, path = "") {
    nm <- node$Name
    newpath <- if (nzchar(path)) paste(path, nm, sep = ".") else nm
    target <- sub("^\\.", "", CONFIG$cultivar_root)   # ".Simulations...Elli" -> "Simulations...Elli"
    if (identical(newpath, target)) {
      have <- vapply(node$Children, function(c) c$Name, character(1))
      for (g in names(GROUPS)) {
        if (g %in% have) next
        parent <- .find_node(tpl, GROUPS[[g]]$clone_from)
        if (is.null(parent)) stop("Clone source cultivar not found: ", GROUPS[[g]]$clone_from)
        clone <- parent; clone$Name <- g              # copy-on-modify => independent deep copy
        node$Children <- c(node$Children, list(clone))
      }
      return(node)
    }
    if (!is.null(node$Children)) node$Children <- lapply(node$Children, clone_into, path = newpath)
    node
  }
  tpl <- clone_into(tpl)

  write(toJSON(tpl, auto_unbox = TRUE, null = "null", pretty = TRUE), dst)
  message("[template] prepared ", dst, " (report patched, ", length(GROUPS), " cultivars cloned)")
}

prepare_template()
