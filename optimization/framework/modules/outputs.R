## =====================================================================
## modules/outputs.R — save results and write ready-to-use cultivars
## =====================================================================
## For every group this saves: fitted coefficients (CSV), and the
## predicted-vs-observed phenology and yield comparison tables (RDS) that
## the figures are built from. After all groups finish it also writes a
## fresh copy of the production template with the calibrated parameters
## baked into NEW named cultivars, so they can be used directly in the
## grid simulation without re-editing anything.
## =====================================================================

save_group_outputs <- function(group, res) {
  od <- abspath(CONFIG$out_dir)
  write.csv(res$coef, file.path(od, paste0(group, "-coefficients.csv")), row.names = FALSE)
  saveRDS(res$uncal$phen,  file.path(od, paste0(group, "-uncalibrated-phen.rds")))
  saveRDS(res$cal$phen,    file.path(od, paste0(group, "-calibrated-phen.rds")))
  saveRDS(res$uncal$yield, file.path(od, paste0(group, "-uncalibrated-yield.rds")))
  saveRDS(res$cal$yield,   file.path(od, paste0(group, "-calibrated-yield.rds")))
}

## Replace the RHS of each APSIM Command line with the calibrated value.
## Matches each line to a parameter by its unique `apsim` substring and
## rewrites only the part after "=", preserving the exact left-hand path.
update_commands <- function(commands, values) {
  vapply(commands, function(line) {
    for (pn in names(PARAMETERS)) {
      spec <- PARAMETERS[[pn]]
      if (grepl(spec$apsim, line, fixed = TRUE)) {
        lhs <- sub("=.*$", "", line)
        return(paste0(lhs, "= ", spec$render(values[[pn]])))
      }
    }
    line
  }, character(1), USE.NAMES = FALSE)
}

## Write a fresh template copy with one calibrated cultivar per group,
## named "<group><suffix>" (e.g. EarlyMG4_cal), inserted in the cultivar
## folder alongside the originals.
write_calibrated_cultivars <- function(all_best, suffix = "_cal") {
  src <- abspath(CONFIG$template)
  dst <- file.path(abspath(CONFIG$out_dir), "calibrated-cultivars.apsimx")
  tpl <- fromJSON(src, simplifyVector = FALSE)
  target <- sub("^\\.", "", CONFIG$cultivar_root)

  insert <- function(node, path = "") {
    nm <- node$Name
    newpath <- if (nzchar(path)) paste(path, nm, sep = ".") else nm
    if (identical(newpath, target)) {
      have <- vapply(node$Children, function(c) c$Name, character(1))
      for (g in names(all_best)) {
        parent <- .find_node(tpl, GROUPS[[g]]$clone_from)
        clone  <- parent
        clone$Name    <- paste0(g, suffix)
        clone$Command <- as.list(update_commands(unlist(parent$Command), all_best[[g]]))
        if (!(clone$Name %in% have)) node$Children <- c(node$Children, list(clone))
      }
      return(node)
    }
    if (!is.null(node$Children)) node$Children <- lapply(node$Children, insert, path = newpath)
    node
  }
  tpl <- insert(tpl)
  write(toJSON(tpl, auto_unbox = TRUE, null = "null", pretty = TRUE), dst)
  message("[outputs] wrote ready-to-use cultivars (", paste0(names(all_best), suffix, collapse = ", "),
          ") -> ", dst)
}

save_progress <- function() {
  od <- abspath(CONFIG$out_dir)
  prog <- do.call(function(...) rbind_fill(list(...)), PROGRESS$rows)
  write.csv(prog, file.path(od, "optimization-progress.csv"), row.names = FALSE)
  prog
}

## Minimal rbind that tolerates differing columns across stages (each
## stage logs only its own parameters), filling absent ones with NA.
rbind_fill <- function(dfs) {
  cols <- unique(unlist(lapply(dfs, names)))
  do.call(rbind, lapply(dfs, function(d) {
    miss <- setdiff(cols, names(d)); for (m in miss) d[[m]] <- NA
    d[cols]
  }))
}
