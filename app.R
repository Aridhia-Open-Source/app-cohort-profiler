# ══════════════════════════════════════════════════════════════════════════════
#  Aridhia DRE - Cohort Profiler  (v4.2)
#  Single-file app.R for deployment to a DRE Project Workspace
#
#  v1   - Core profiling: missingness, outliers, correlations, distributions
#  v2   - Group Analysis tab, Timeline tab, data quality (duplicates, identifiers)
#  v3   - Plot export (download PNG per chart), Variable labelling (upload label CSV)
#  v4   - Cohort Comparison tab (overlay two datasets), Range Validation tab (user-defined rules)
#  v4.1 - Exports now SAVE directly into the workspace via a folder picker
#         (default /home/workspace/files/) instead of browser downloads
#  v4.2 - Relational (multi-table) profiling: load a folder of linked CSVs,
#         detect the common identifier, map relationships (ER diagram), and run
#         linkage-integrity diagnostics (key uniqueness, orphans, subject
#         coverage, cardinality, key format drift). Any loaded table can be made
#         active to drive the existing single-file tabs.
# ══════════════════════════════════════════════════════════════════════════════


# ── 1. DEPENDENCIES ───────────────────────────────────────────────────────────

required_packages <- c("shiny", "shinydashboard", "DT", "ggplot2",
                       "dplyr", "tidyr", "scales", "readr", "tools")

# Install any missing package from the workspace-allowlisted CRAN mirror, then load.
# requireNamespace() avoids reinstalling packages that are already present.
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("[ cohort profiler ] installing missing package: %s", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(paste0("Required package '", pkg,
                "' is not installed and could not be installed automatically."),
         call. = FALSE)
  library(pkg, character.only = TRUE)
}

message("[ cohort profiler ] libraries loaded.")


# ── 2. WORKSPACE CONFIGURATION ────────────────────────────────────────────────

WORKSPACE_FILES <- if (dir.exists("/home/workspace/files")) {
  "/home/workspace/files"
} else {
  file.path(getwd(), "data")
}

message("[ cohort profiler ] workspace root: ", WORKSPACE_FILES)


# ── 3. FILE BROWSER MODAL ─────────────────────────────────────────────────────

FILE_PATTERN <- "\\.csv$"

open_file_modal <- function(dir, sfx = "") {
  nav_id  <- paste0("nav_to",    sfx)
  open_id <- paste0("open_local", sfx)

  if (!dir.exists(dir)) {
    showNotification(paste("Directory not found:", dir), type = "error"); return()
  }
  items      <- list.files(dir, full.names = FALSE, all.files = FALSE)
  full_paths <- file.path(dir, items)
  is_dir     <- dir.exists(full_paths)
  dirs       <- sort(items[is_dir])
  files      <- sort(items[!is_dir & grepl(FILE_PATTERN, items, ignore.case = TRUE)])

  root     <- WORKSPACE_FILES
  rel_path <- sub(paste0("^", gsub("([.+*?|(){}\\[\\]^$\\\\])", "\\\\\\1", root)), "", dir)
  parts    <- Filter(nzchar, strsplit(rel_path, "/")[[1]])

  crumbs <- tagList(
    tags$a(href = "#", class = "fb-crumb-link",
           onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'}); return false;", nav_id, root),
           "files"),
    tags$span(class = "fb-crumb-sep", " / ")
  )
  for (i in seq_along(parts)) {
    p <- file.path(root, paste(parts[seq_len(i)], collapse = "/"))
    crumbs <- tagList(crumbs,
      tags$a(href = "#", class = "fb-crumb-link",
             onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'}); return false;", nav_id, p),
             parts[i]),
      tags$span(class = "fb-crumb-sep", " / "))
  }

  body_content <- tagList(
    div(class = "fb-modal-breadcrumb", crumbs),
    if (dir != root)
      div(class = "fb-modal-row fb-modal-dir",
          onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", nav_id, dirname(dir)),
          icon("arrow-up"), tags$span(" ..")),
    lapply(dirs, function(d)
      div(class = "fb-modal-row fb-modal-dir",
          onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", nav_id, file.path(dir, d)),
          icon("folder"), tags$span(class = "fb-modal-name", d))),
    lapply(files, function(f) {
      full <- file.path(dir, f); info <- file.info(full)
      size <- if (!is.na(info$size))
        if (info$size < 1024^2) paste0(round(info$size / 1024, 1), " KB")
        else paste0(round(info$size / 1024^2, 1), " MB") else ""
      div(class = "fb-modal-row fb-modal-file",
          onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", open_id, full),
          icon("file-csv"), tags$span(class = "fb-modal-name", f),
          tags$span(class = "fb-modal-size", size))
    }),
    if (length(dirs) == 0 && length(files) == 0)
      p(class = "fb-modal-empty", "No CSV files found in this directory.")
  )
  title_sfx <- if (sfx == "_b") " - Dataset B" else ""
  showModal(modalDialog(title = tagList(icon("folder-open"), paste0(" Open CSV File", title_sfx)),
    body_content, size = "l", easyClose = TRUE, footer = modalButton("Cancel")))
}


# ── SAVE-LOCATION MODAL ───────────────────────────────────────────────────────
# Directory picker for writing exports into the workspace. Navigates folders
# (rooted at WORKSPACE_FILES - cannot go above it), lets the user edit the file
# name and optionally create a new folder, then confirm with the footer button.
# Distinct input ids (save_*) so it never collides with the open-file browser.

open_save_modal <- function(dir, default_filename = "") {
  if (!dir.exists(dir)) {
    showNotification(paste("Directory not found:", dir), type = "error"); return()
  }
  items      <- list.files(dir, full.names = FALSE, all.files = FALSE)
  full_paths <- file.path(dir, items)
  is_dir     <- dir.exists(full_paths)
  dirs       <- sort(items[is_dir])

  root     <- WORKSPACE_FILES
  rel_path <- sub(paste0("^", gsub("([.+*?|(){}\\[\\]^$\\\\])", "\\\\\\1", root)), "", dir)
  parts    <- Filter(nzchar, strsplit(rel_path, "/")[[1]])

  crumbs <- tagList(
    tags$a(href = "#", class = "fb-crumb-link",
           onclick = sprintf("Shiny.setInputValue('save_nav_to','%s',{priority:'event'}); return false;", root),
           "files"),
    tags$span(class = "fb-crumb-sep", " / ")
  )
  for (i in seq_along(parts)) {
    p <- file.path(root, paste(parts[seq_len(i)], collapse = "/"))
    crumbs <- tagList(crumbs,
      tags$a(href = "#", class = "fb-crumb-link",
             onclick = sprintf("Shiny.setInputValue('save_nav_to','%s',{priority:'event'}); return false;", p),
             parts[i]),
      tags$span(class = "fb-crumb-sep", " / "))
  }

  body_content <- tagList(
    div(class = "fb-modal-breadcrumb", crumbs),
    div(class = "fb-save-current", icon("folder-open"),
        tags$span(class = "fb-save-current-path", dir)),
    div(class = "fb-save-dirlist",
      if (dir != root)
        div(class = "fb-modal-row fb-modal-dir",
            onclick = sprintf("Shiny.setInputValue('save_nav_to','%s',{priority:'event'})", dirname(dir)),
            icon("arrow-up"), tags$span(" ..")),
      lapply(dirs, function(d)
        div(class = "fb-modal-row fb-modal-dir",
            onclick = sprintf("Shiny.setInputValue('save_nav_to','%s',{priority:'event'})", file.path(dir, d)),
            icon("folder"), tags$span(class = "fb-modal-name", d))),
      if (length(dirs) == 0)
        p(class = "fb-modal-empty",
          "No subfolders here \u2014 save in this folder, or create a new one below.")
    ),
    tags$hr(style = "margin:14px 0; border-color:#CDD5DF;"),
    div(class = "control-label", "File name"),
    textInput("save_filename", label = NULL, value = default_filename, width = "100%"),
    div(class = "control-label", style = "margin-top:6px;", "Create new folder (optional)"),
    div(style = "display:flex; gap:8px; align-items:flex-start;",
        div(style = "flex:1;",
            textInput("save_new_folder", label = NULL,
                      placeholder = "New folder name", width = "100%")),
        actionButton("create_save_folder", label = tagList(icon("folder-plus"), " Create"),
                     class = "btn-newfolder"))
  )

  showModal(modalDialog(
    title = tagList(icon("floppy-disk"), " Save to Workspace"),
    body_content, size = "l", easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_save", label = tagList(icon("floppy-disk"), " Save File"),
                   class = "btn-save-confirm")
    )
  ))
}


# ── 4. HELPERS ────────────────────────────────────────────────────────────────

# Null/empty-coalescing operator: returns b when a is NULL or zero-length.
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# Variable label lookup - returns human-readable label if available, else name
col_label <- function(name, lmap = list()) {
  lab <- lmap[[name]]
  if (!is.null(lab) && nzchar(trimws(lab))) trimws(lab) else name
}

# Apply labels to a vector of names, with original name appended in parentheses
col_label_with_name <- function(names, lmap = list()) {
  vapply(names, function(n) {
    lab <- lmap[[n]]
    if (!is.null(lab) && nzchar(trimws(lab))) paste0(trimws(lab), " (", n, ")") else n
  }, character(1))
}

col_type_label <- function(x) {
  if (is.numeric(x))                                   return("numeric")
  if (is.logical(x))                                   return("logical")
  if (inherits(x, "Date") || inherits(x, "POSIXct"))  return("date/time")
  return("character")
}

count_outliers <- function(x) {
  x <- x[!is.na(x)]
  if (!is.numeric(x) || length(x) < 4) return(NA_integer_)
  q1 <- quantile(x, 0.25); q3 <- quantile(x, 0.75); iqr <- q3 - q1
  if (iqr == 0) return(0L)
  as.integer(sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)))
}

summarise_col <- function(x, name) {
  n_na <- sum(is.na(x))
  row  <- data.frame(Column = name, Type = col_type_label(x),
                     N_Missing = n_na,
                     Pct_Missing = round(100 * n_na / length(x), 1),
                     N_Unique    = length(unique(na.omit(x))),
                     N_Outliers  = count_outliers(x),
                     stringsAsFactors = FALSE)
  if (is.numeric(x)) {
    row$Min    <- round(min(x,    na.rm = TRUE), 3)
    row$Median <- round(median(x, na.rm = TRUE), 3)
    row$Mean   <- round(mean(x,   na.rm = TRUE), 3)
    row$Max    <- round(max(x,    na.rm = TRUE), 3)
    row$SD     <- round(sd(x,     na.rm = TRUE), 3)
  } else { row$Min <- row$Median <- row$Mean <- row$Max <- row$SD <- NA_real_ }
  row
}

profile_dataset <- function(df)
  dplyr::bind_rows(lapply(names(df), function(cn) summarise_col(df[[cn]], cn)))

top_values <- function(x, n = 15) {
  tbl <- sort(table(x, useNA = "no"), decreasing = TRUE)
  data.frame(Value = names(tbl), Count = as.integer(tbl),
             Pct = round(100 * as.integer(tbl) / sum(!is.na(x)), 1),
             stringsAsFactors = FALSE)[seq_len(min(n, length(tbl))), ]
}

detect_data_quality <- function(df) {
  n_dup <- sum(duplicated(df)); n <- nrow(df)
  near_id <- names(df)[sapply(names(df), function(cn) {
    x  <- df[[cn]]; nu <- length(unique(na.omit(x)))
    (is.character(x) || is.integer(x)) && n >= 10 && (nu / n) >= 0.95
  })]
  list(n_dup_rows = n_dup, near_id_cols = near_id)
}

DATE_FMTS <- c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d", "%d-%m-%Y", "%Y%m%d")

parse_dates_safe <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(as.Date(x))
  for (f in DATE_FMTS) {
    d <- suppressWarnings(as.Date(as.character(x), format = f))
    if (mean(!is.na(d)) >= 0.8) return(d)
  }
  as.Date(NA_character_)
}

detect_date_cols <- function(df) {
  native <- names(df)[sapply(df, function(x) inherits(x, c("Date","POSIXct","POSIXlt")))]
  parsed <- character(0)
  for (col in names(df)[sapply(df, is.character)]) {
    samp <- na.omit(df[[col]])[seq_len(min(60, sum(!is.na(df[[col]]))))]
    if (length(samp) == 0) next
    if (any(sapply(DATE_FMTS, function(f)
      mean(!is.na(suppressWarnings(as.Date(samp, format = f)))) >= 0.8)))
      parsed <- c(parsed, col)
  }
  unique(c(native, parsed))
}

floor_to_period <- function(d, period) {
  switch(period,
    "Week"    = as.Date(format(d - as.integer(format(d, "%u")) + 1)),
    "Month"   = as.Date(format(d, "%Y-%m-01")),
    "Quarter" = {
      m <- as.integer(format(d, "%m")); qm <- ((m - 1) %/% 3) * 3 + 1
      as.Date(paste0(format(d, "%Y"), "-", sprintf("%02d", qm), "-01"))
    },
    "Year" = as.Date(paste0(format(d, "%Y"), "-01-01")),
    as.Date(format(d, "%Y-%m-01"))
  )
}

# ── PLOT THEME ────────────────────────────────────────────────────────────────

.dre_theme <- function(base = 14)
  ggplot2::theme_minimal(base_size = base) +
  ggplot2::theme(
    plot.background    = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
    panel.background   = ggplot2::element_rect(fill = "#F7F9FB", colour = NA),
    panel.grid.major   = ggplot2::element_line(colour = "#DDE4EC"),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor   = ggplot2::element_blank(),
    axis.text          = ggplot2::element_text(colour = "#3A5060", size = base - 2),
    axis.title         = ggplot2::element_text(colour = "#1A2A3A", size = base - 1),
    plot.title         = ggplot2::element_text(colour = "#1A2A3A", face = "bold", size = base + 1),
    plot.subtitle      = ggplot2::element_text(colour = "#4A6070", size = base - 3),
    legend.text        = ggplot2::element_text(colour = "#4A6070"),
    legend.position    = "bottom"
  )

.dre_theme_grid <- function(base = 14)
  .dre_theme(base) +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "#DDE4EC"))

# ── PLOT FUNCTIONS ────────────────────────────────────────────────────────────
# All accept lmap = list() for variable label substitution.

plot_missingness <- function(df, lmap = list(), max_rows = 300) {
  df_s <- dplyr::slice_sample(df, n = min(max_rows, nrow(df)))
  df_s <- dplyr::mutate(df_s, dplyr::across(dplyr::everything(), as.character))
  mdf  <- df_s %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(-row_id, names_to = "column", values_to = "value") %>%
    dplyr::mutate(missing = is.na(value))
  pct   <- sapply(df, function(x) round(100 * mean(is.na(x)), 1))
  orig  <- rev(names(df))
  ylabs <- paste0(vapply(orig, col_label, character(1), lmap = lmap), " (", pct[orig], "%)")
  mdf$column <- factor(mdf$column, levels = orig)
  ggplot2::ggplot(mdf, ggplot2::aes(x = row_id, y = column, fill = missing)) +
    ggplot2::geom_tile(linewidth = 0) +
    ggplot2::scale_fill_manual(values = c("FALSE" = "#007A6E", "TRUE" = "#D4E4EE"),
                               labels = c("FALSE" = "Present", "TRUE" = "Missing"), name = NULL) +
    ggplot2::scale_y_discrete(labels = ylabs) +
    ggplot2::labs(x = "Row (sample)", y = NULL, title = "Missingness heatmap",
                  subtitle = paste0("Sample of up to 300 rows  \u00b7  ",
                                    sum(is.na(df)), " total missing values")) +
    .dre_theme()
}

plot_distribution <- function(df, col_name, lmap = list()) {
  x <- na.omit(df[[col_name]]); if (length(x) == 0) return(NULL)
  q1 <- quantile(x, 0.25); q3 <- quantile(x, 0.75); iqr <- q3 - q1
  lo <- q1 - 1.5 * iqr; hi <- q3 + 1.5 * iqr
  n_out <- if (iqr > 0) sum(x < lo | x > hi) else 0L
  norm_label <- tryCatch({
    xs <- if (length(x) <= 5000) x else sample(x, 5000)
    if (length(xs) >= 3) {
      sw <- shapiro.test(xs)
      if (sw$p.value < 0.001) "Non-normal (Shapiro-Wilk p < 0.001)"
      else paste0("Approx. normal (Shapiro-Wilk p = ", round(sw$p.value, 3), ")")
    } else ""
  }, error = function(e) "")
  xlab <- col_label(col_name, lmap)
  ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(bins = min(max(10, nclass.Sturges(x)), 50),
                            fill = "#007A6E", colour = "#FFFFFF", alpha = 0.85) +
    ggplot2::geom_vline(xintercept = median(x), colour = "#1A2A3A",
                        linetype = "dashed", linewidth = 0.8) +
    { if (iqr > 0) list(
        ggplot2::geom_vline(xintercept = lo, colour = "#6B3FA0", linetype = "dotted", linewidth = 0.7),
        ggplot2::geom_vline(xintercept = hi, colour = "#6B3FA0", linetype = "dotted", linewidth = 0.7)
    )} +
    ggplot2::labs(x = xlab, y = "Count",
                  title    = paste0("Distribution: ", xlab),
                  subtitle = paste0("n=", length(x), "  median=", round(median(x), 3),
                                    "  mean=", round(mean(x), 3), "  SD=", round(sd(x), 3),
                                    "  outliers=", n_out,
                                    if (nchar(norm_label) > 0) paste0("\n", norm_label) else "")) +
    .dre_theme()
}

plot_categories <- function(df, col_name, lmap = list()) {
  tv <- top_values(df[[col_name]]); if (nrow(tv) == 0) return(NULL)
  tv$Value <- factor(tv$Value, levels = rev(tv$Value))
  xlab <- col_label(col_name, lmap)
  ggplot2::ggplot(tv, ggplot2::aes(x = Count, y = Value)) +
    ggplot2::geom_col(fill = "#007A6E", alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(Pct, "%")),
                       hjust = -0.1, colour = "#4A6070", size = 3.8) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(x = "Count", y = NULL, title = paste0("Top values: ", xlab),
                  subtitle = paste0("Top ", nrow(tv), " of ",
                                    length(unique(na.omit(df[[col_name]]))), " unique values")) +
    .dre_theme()
}

plot_outlier_detail <- function(x, col_name, lmap = list()) {
  x <- na.omit(x)
  if (!is.numeric(x) || length(x) < 4) return(NULL)
  q1 <- quantile(x, 0.25); q3 <- quantile(x, 0.75); iqr <- q3 - q1
  lo <- q1 - 1.5 * iqr; hi <- q3 + 1.5 * iqr
  mn <- mean(x); med <- median(x)
  n_out <- if (iqr > 0) sum(x < lo | x > hi) else 0L
  dens    <- density(x, n = 512)
  dens_df <- data.frame(x = dens$x, y = dens$y,
                        region = ifelse(dens$x < lo | dens$x > hi, "outlier", "normal"))
  pts <- data.frame(x = x, outlier = x < lo | x > hi,
                    y_jit = runif(length(x), -0.012, 0.012))
  xr <- range(c(dens$x, x)); xp <- diff(xr) * 0.05
  xlab <- col_label(col_name, lmap)
  ggplot2::ggplot() +
    { if (iqr > 0) list(
        ggplot2::annotate("rect", xmin = -Inf, xmax = lo, ymin = -Inf, ymax = Inf, fill = "#6B3FA0", alpha = 0.07),
        ggplot2::annotate("rect", xmin = hi, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#6B3FA0", alpha = 0.07)
    )} +
    ggplot2::geom_area(data = dplyr::filter(dens_df, region == "normal"),
                       ggplot2::aes(x = x, y = y), fill = "#007A6E", alpha = 0.22) +
    ggplot2::geom_area(data = dplyr::filter(dens_df, region == "outlier"),
                       ggplot2::aes(x = x, y = y), fill = "#6B3FA0", alpha = 0.42) +
    ggplot2::geom_line(data = dens_df, ggplot2::aes(x = x, y = y), colour = "#007A6E", linewidth = 1.2) +
    { if (iqr > 0) list(
        ggplot2::geom_vline(xintercept = lo, colour = "#6B3FA0", linetype = "dashed", linewidth = 0.8),
        ggplot2::geom_vline(xintercept = hi, colour = "#6B3FA0", linetype = "dashed", linewidth = 0.8)
    )} +
    ggplot2::geom_vline(xintercept = med, colour = "#1A2A3A", linetype = "solid", linewidth = 0.7, alpha = 0.7) +
    ggplot2::geom_vline(xintercept = mn,  colour = "#C07000", linetype = "dashed", linewidth = 0.7, alpha = 0.8) +
    ggplot2::geom_point(data = pts, ggplot2::aes(x = x, y = y_jit, colour = outlier),
                        size = 2, alpha = 0.55, shape = 16) +
    ggplot2::scale_colour_manual(values = c("FALSE" = "#007A6E", "TRUE" = "#6B3FA0"),
                                 labels = c("FALSE" = "Within fences", "TRUE" = "Outlier"), name = NULL) +
    { if (iqr > 0) list(
        ggplot2::annotate("text", x = lo, y = max(dens$y) * 0.97,
                          label = paste0("Q1-1.5xIQR\n", round(lo, 3)),
                          colour = "#6B3FA0", size = 3.2, hjust = 1.05, vjust = 1, lineheight = 1.1),
        ggplot2::annotate("text", x = hi, y = max(dens$y) * 0.97,
                          label = paste0("Q3+1.5xIQR\n", round(hi, 3)),
                          colour = "#6B3FA0", size = 3.2, hjust = -0.05, vjust = 1, lineheight = 1.1)
    )} +
    ggplot2::scale_x_continuous(limits = c(xr[1] - xp, xr[2] + xp), expand = c(0, 0)) +
    ggplot2::labs(x = xlab, y = "Density", title = paste0("Outlier detail: ", xlab),
                  subtitle = paste0("n=", length(x), "  median=", round(med, 3),
                                    "  mean=", round(mn, 3), "  ",
                                    n_out, " outlier", if (n_out != 1) "s" else "",
                                    " (", round(100 * n_out / length(x), 1), "%)")) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#F7F9FB", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#DDE4EC"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(colour = "#4A6070"),
      axis.title       = ggplot2::element_text(colour = "#4A6070"),
      plot.title       = ggplot2::element_text(colour = "#1A2A3A", face = "bold"),
      plot.subtitle    = ggplot2::element_text(colour = "#4A6070", size = 11),
      legend.text      = ggplot2::element_text(colour = "#4A6070"),
      legend.position  = "bottom",
      legend.key       = ggplot2::element_rect(fill = NA, colour = NA)
    )
}

plot_correlation <- function(df, lmap = list()) {
  num_df <- dplyr::select(df, dplyr::where(is.numeric))
  if (ncol(num_df) < 2) return(NULL)
  cor_mat <- cor(num_df, use = "pairwise.complete.obs")
  cols    <- seq_len(ncol(num_df))
  n_mat   <- outer(cols, cols, FUN = Vectorize(function(i, j)
    sum(complete.cases(num_df[, c(i, j)]))))
  dimnames(n_mat) <- dimnames(cor_mat)
  cor_long <- merge(
    { cl <- as.data.frame(as.table(cor_mat)); names(cl) <- c("Var1","Var2","r"); cl },
    { nl <- as.data.frame(as.table(n_mat));   names(nl) <- c("Var1","Var2","n"); nl }
  )
  cor_long$label <- ifelse(cor_long$Var1 == cor_long$Var2, "",
                           paste0(round(cor_long$r, 2), "\nn=", cor_long$n))
  lvls <- colnames(cor_mat)
  # Apply labels to axis
  llvls <- vapply(lvls, col_label, character(1), lmap = lmap)
  cor_long$Var1 <- factor(vapply(as.character(cor_long$Var1), col_label, character(1), lmap = lmap),
                          levels = llvls)
  cor_long$Var2 <- factor(vapply(as.character(cor_long$Var2), col_label, character(1), lmap = lmap),
                          levels = rev(llvls))
  ggplot2::ggplot(cor_long, ggplot2::aes(x = Var1, y = Var2, fill = r)) +
    ggplot2::geom_tile(colour = "#FFFFFF", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = label), colour = "#1A2A3A", size = 3.2, lineheight = 1.2) +
    ggplot2::scale_fill_gradientn(
      colours = c("#C0392B","#F0F0F0","#007A6E"),
      values  = scales::rescale(c(-1, 0, 1)), limits = c(-1, 1),
      name    = "Pearson r", na.value = "#DDE4EC") +
    ggplot2::labs(x = NULL, y = NULL, title = "Correlation matrix",
                  subtitle = paste0(ncol(num_df), " numeric columns  \u00b7  Pearson r  \u00b7  pairwise complete obs")) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#F7F9FB", colour = NA),
      panel.grid       = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(colour = "#4A6070", angle = 35, hjust = 1, size = 10),
      axis.text.y      = ggplot2::element_text(colour = "#4A6070", size = 10),
      plot.title       = ggplot2::element_text(colour = "#1A2A3A", face = "bold"),
      plot.subtitle    = ggplot2::element_text(colour = "#4A6070"),
      legend.text      = ggplot2::element_text(colour = "#4A6070"),
      legend.title     = ggplot2::element_text(colour = "#4A6070"),
      legend.position  = "right"
    )
}

plot_group_box <- function(df, num_col, group_col, lmap = list()) {
  x_raw <- df[[num_col]]; g_raw <- as.character(df[[group_col]])
  keep  <- !is.na(x_raw) & !is.na(g_raw)
  x <- x_raw[keep]; g <- g_raw[keep]
  if (length(x) < 4) return(NULL)
  top_grps <- names(sort(table(g), decreasing = TRUE))[seq_len(min(20, length(unique(g))))]
  x <- x[g %in% top_grps]; g <- g[g %in% top_grps]
  kw_label <- tryCatch({
    if (length(unique(g)) >= 2) {
      kw <- kruskal.test(x ~ factor(g))
      paste0("Kruskal-Wallis p", if (kw$p.value < 0.001) " < 0.001"
             else paste0(" = ", round(kw$p.value, 3)))
    } else ""
  }, error = function(e) "")
  med_order <- names(sort(tapply(x, g, median, na.rm = TRUE)))
  plot_df   <- data.frame(x = x, g = factor(g, levels = med_order))
  n_grps    <- length(unique(g))
  pal <- colorRampPalette(c("#007A6E","#4DD9CA","#5A2A90","#C07000"))(n_grps)
  angle_x <- if (max(nchar(unique(as.character(g)))) > 6) 30 else 0
  xlab <- col_label(group_col, lmap); ylab <- col_label(num_col, lmap)
  ggplot2::ggplot(plot_df, ggplot2::aes(x = g, y = x, fill = g, colour = g)) +
    ggplot2::geom_boxplot(alpha = 0.22, outlier.shape = NA, linewidth = 0.7) +
    ggplot2::geom_jitter(width = 0.18, alpha = 0.35, size = 1.5, shape = 16) +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    ggplot2::scale_colour_manual(values = pal, guide = "none") +
    ggplot2::labs(x = xlab, y = ylab,
                  title    = paste0(ylab, "  by  ", xlab),
                  subtitle = paste0("n=", length(x), "  \u00b7  ", n_grps, " group",
                                    if (n_grps != 1) "s" else "", "  (ordered by median)",
                                    if (nchar(kw_label) > 0) paste0("  \u00b7  ", kw_label) else "")) +
    .dre_theme_grid() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle_x,
                                                        hjust = if (angle_x > 0) 1 else 0.5))
}

plot_timeline_density <- function(df, date_col, period = "Month", lmap = list()) {
  dates <- parse_dates_safe(df[[date_col]]); valid <- !is.na(dates)
  if (sum(valid) < 2) return(NULL)
  d   <- floor_to_period(dates[valid], period)
  cnt <- as.data.frame(table(Period = as.character(d)), stringsAsFactors = FALSE)
  cnt$Period <- as.Date(cnt$Period); cnt$Freq <- as.integer(cnt$Freq)
  bar_w <- if (nrow(cnt) > 1) as.numeric(diff(range(cnt$Period)) / nrow(cnt)) * 0.82 else 20
  xlab <- col_label(date_col, lmap)
  ggplot2::ggplot(cnt, ggplot2::aes(x = Period, y = Freq)) +
    ggplot2::geom_col(fill = "#007A6E", alpha = 0.82, width = bar_w) +
    ggplot2::scale_x_date(date_labels = if (period == "Year") "%Y" else "%b %Y") +
    ggplot2::labs(x = period, y = "Row count",
                  title    = paste0("Record density by ", period, ":  ", xlab),
                  subtitle = paste0(sum(valid), " dated records  \u00b7  ",
                                    format(min(dates, na.rm = TRUE)), " to ",
                                    format(max(dates, na.rm = TRUE)))) +
    .dre_theme_grid() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

plot_timeline_completeness <- function(df, date_col, track_col, period = "Month", lmap = list()) {
  dates <- parse_dates_safe(df[[date_col]]); vals <- df[[track_col]]; valid <- !is.na(dates)
  if (sum(valid) < 2) return(NULL)
  tmp <- data.frame(Period = floor_to_period(dates[valid], period), missing = is.na(vals[valid]))
  agg <- dplyr::summarise(dplyr::group_by(tmp, Period),
    completeness = round(100 * mean(!missing), 1), n = dplyr::n(), .groups = "drop")
  agg$Period <- as.Date(as.character(agg$Period))
  tlab <- col_label(track_col, lmap)
  ggplot2::ggplot(agg, ggplot2::aes(x = Period, y = completeness)) +
    ggplot2::geom_line(colour = "#007A6E", linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(size = n), colour = "#007A6E", alpha = 0.8) +
    ggplot2::scale_size_continuous(name = "n records", range = c(2, 7)) +
    ggplot2::scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    ggplot2::scale_x_date(date_labels = if (period == "Year") "%Y" else "%b %Y") +
    ggplot2::geom_hline(yintercept = 80, colour = "#D4721A", linetype = "dashed", linewidth = 0.7) +
    ggplot2::annotate("text", x = min(agg$Period), y = 82, label = "80% threshold",
                      colour = "#D4721A", size = 3.5, hjust = 0) +
    ggplot2::labs(x = period, y = "Completeness (%)",
                  title    = paste0(tlab, "  completeness over time"),
                  subtitle = paste0("By ", period, "  \u00b7  point size = record count")) +
    .dre_theme_grid() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                   legend.text = ggplot2::element_text(colour = "#4A6070", size = 11))
}

# SUMMARY CSV EXPORT
build_summary_csv <- function(df, file_path, source_path, lmap = list()) {
  cs <- profile_dataset(df)
  if (length(lmap) > 0) {
    cs$Label <- vapply(cs$Column, col_label, character(1), lmap = lmap)
    cs       <- cs[, c("Column","Label", setdiff(names(cs), c("Column","Label"))), drop = FALSE]
  }
  cs$Outlier_Note <- ifelse(is.na(cs$N_Outliers), "",
                            ifelse(cs$N_Outliers == 0, "none",
                                   paste0(cs$N_Outliers, " (IQR fences)")))
  dq <- detect_data_quality(df)
  header <- data.frame(
    Field = c("Source file","Report generated","Rows","Columns","Total missing",
              "Completeness (%)","Duplicate rows","Near-identifier columns"),
    Value = c(basename(source_path), format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              nrow(df), ncol(df), sum(is.na(df)),
              round(100 * (1 - sum(is.na(df)) / (nrow(df) * ncol(df))), 1),
              dq$n_dup_rows,
              if (length(dq$near_id_cols) > 0) paste(dq$near_id_cols, collapse = "; ") else "none"),
    stringsAsFactors = FALSE)
  con <- file(file_path, open = "w")
  writeLines("## Dataset summary", con); write.csv(header, con, row.names = FALSE)
  writeLines("", con)
  writeLines("## Column statistics", con); write.csv(cs, con, row.names = FALSE)
  close(con)
}

# PLOT DOWNLOAD HELPER - saves any ggplot to a PNG file at print quality
save_plot_png <- function(p, file, width = 12, height = 7) {
  ggplot2::ggsave(file, plot = p, width = width, height = height,
                  dpi = 300, bg = "white", device = "png")
}

# ── v4 HELPERS ────────────────────────────────────────────────────────────────

# Overlapping density plot for a single numeric column across two datasets
plot_density_overlay <- function(df_a, df_b, col_name,
                                 label_a = "Dataset A", label_b = "Dataset B",
                                 lmap = list()) {
  xa <- na.omit(df_a[[col_name]]); xb <- na.omit(df_b[[col_name]])
  if (!is.numeric(xa) || !is.numeric(xb)) return(NULL)
  if (length(xa) < 3 || length(xb) < 3) return(NULL)
  da <- density(xa, n = 512); db <- density(xb, n = 512)
  dens_df <- rbind(
    data.frame(x = da$x, y = da$y, Dataset = label_a, stringsAsFactors = FALSE),
    data.frame(x = db$x, y = db$y, Dataset = label_b, stringsAsFactors = FALSE)
  )
  xlab <- col_label(col_name, lmap)
  ggplot2::ggplot(dens_df, ggplot2::aes(x = x, y = y, fill = Dataset, colour = Dataset)) +
    ggplot2::geom_area(alpha = 0.22, position = "identity") +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = median(xa), colour = "#007A6E",
                        linetype = "dashed", linewidth = 0.8, alpha = 0.8) +
    ggplot2::geom_vline(xintercept = median(xb), colour = "#6B3FA0",
                        linetype = "dashed", linewidth = 0.8, alpha = 0.8) +
    ggplot2::scale_fill_manual(values   = c("#007A6E", "#6B3FA0"), name = NULL) +
    ggplot2::scale_colour_manual(values = c("#007A6E", "#6B3FA0"), name = NULL) +
    ggplot2::labs(x = xlab, y = "Density",
                  title    = paste0("Distribution comparison: ", xlab),
                  subtitle = paste0(label_a, "  n=", length(xa),
                                    "  median=", round(median(xa), 3),
                                    "     ", label_b, "  n=", length(xb),
                                    "  median=", round(median(xb), 3))) +
    .dre_theme() +
    ggplot2::theme(legend.position = "top")
}

# Side-by-side proportional bar chart for a single categorical column
plot_category_compare <- function(df_a, df_b, col_name,
                                   label_a = "Dataset A", label_b = "Dataset B",
                                   lmap = list()) {
  make_prop <- function(df, lbl) {
    tbl <- sort(table(na.omit(df[[col_name]])), decreasing = TRUE)
    n   <- nrow(df)
    data.frame(Value   = names(tbl),
               Pct     = as.numeric(tbl) / n * 100,
               Dataset = lbl, stringsAsFactors = FALSE)
  }
  pa <- make_prop(df_a, label_a); pb <- make_prop(df_b, label_b)
  top_vals <- pa$Value[seq_len(min(15, nrow(pa)))]
  combined <- rbind(pa[pa$Value %in% top_vals, ], pb[pb$Value %in% top_vals, ])
  combined$Value <- factor(combined$Value, levels = rev(top_vals))
  xlab <- col_label(col_name, lmap)
  ggplot2::ggplot(combined, ggplot2::aes(x = Pct, y = Value, fill = Dataset)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.85) +
    ggplot2::scale_fill_manual(values = c("#007A6E", "#6B3FA0"), name = NULL) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(x, 1), "%"),
                                 expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = "% of dataset", y = NULL,
                  title = paste0("Category comparison: ", xlab)) +
    .dre_theme() +
    ggplot2::theme(legend.position = "top")
}

# Summary statistics table comparing matched columns from two datasets
compare_stats_tbl <- function(df_a, df_b, label_a = "Dataset A", label_b = "Dataset B") {
  common <- intersect(names(df_a), names(df_b))
  if (length(common) == 0) return(NULL)
  dplyr::bind_rows(lapply(common, function(cn) {
    xa <- df_a[[cn]]; xb <- df_b[[cn]]
    row <- data.frame(
      Column     = cn,
      Type       = col_type_label(xa),
      N_A        = sum(!is.na(xa)),
      Miss_A     = paste0(round(100 * mean(is.na(xa)), 1), "%"),
      N_B        = sum(!is.na(xb)),
      Miss_B     = paste0(round(100 * mean(is.na(xb)), 1), "%"),
      stringsAsFactors = FALSE
    )
    names(row)[3:6] <- c(paste0("N_", label_a), paste0("Miss_", label_a),
                          paste0("N_", label_b), paste0("Miss_", label_b))
    if (is.numeric(xa) && is.numeric(xb)) {
      row$Mean_A   <- round(mean(xa,   na.rm = TRUE), 3)
      row$Mean_B   <- round(mean(xb,   na.rm = TRUE), 3)
      row$Median_A <- round(median(xa, na.rm = TRUE), 3)
      row$Median_B <- round(median(xb, na.rm = TRUE), 3)
      row$SD_A     <- round(sd(xa,     na.rm = TRUE), 3)
      row$SD_B     <- round(sd(xb,     na.rm = TRUE), 3)
      names(row)[7:12] <- c(paste0("Mean_", label_a),   paste0("Mean_", label_b),
                             paste0("Median_", label_a), paste0("Median_", label_b),
                             paste0("SD_", label_a),     paste0("SD_", label_b))
    } else {
      row$Mean_A <- row$Mean_B <- row$Median_A <- row$Median_B <- row$SD_A <- row$SD_B <- NA_real_
    }
    row
  }))
}

# Parse range rules from a text block ("column,min,max" per line)
# Returns a data.frame with columns: col, lo, hi
parse_range_rules <- function(txt) {
  lines <- trimws(strsplit(txt, "\n")[[1]])
  lines <- lines[nzchar(lines) & !grepl("^#", lines) & !grepl("^column", lines, ignore.case = TRUE)]
  if (length(lines) == 0) return(NULL)
  rows <- lapply(lines, function(ln) {
    parts <- trimws(strsplit(ln, ",")[[1]])
    if (length(parts) < 3) return(NULL)
    lo <- suppressWarnings(as.numeric(parts[2]))
    hi <- suppressWarnings(as.numeric(parts[3]))
    if (is.na(lo) && is.na(hi)) return(NULL)
    data.frame(col = parts[1], lo = lo, hi = hi, stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(Filter(Negate(is.null), rows))
}

# Apply range rules to a dataset; return violation summary
apply_range_rules <- function(df, rules) {
  if (is.null(rules) || nrow(rules) == 0) return(NULL)
  dplyr::bind_rows(lapply(seq_len(nrow(rules)), function(i) {
    cn <- rules$col[i]; lo <- rules$lo[i]; hi <- rules$hi[i]
    if (!cn %in% names(df) || !is.numeric(df[[cn]])) {
      return(data.frame(Column = cn, Rule = format_rule(lo, hi),
                        N_Violations = NA_integer_, Pct_Violations = NA_real_,
                        Sample_Values = "column not found or not numeric",
                        stringsAsFactors = FALSE))
    }
    x    <- df[[cn]]
    mask <- (!is.na(x)) &
            ((!is.na(lo) & x < lo) | (!is.na(hi) & x > hi))
    n_v  <- sum(mask)
    samp <- if (n_v > 0)
      paste(head(sort(unique(round(x[mask], 3))), 5), collapse = ", ")
    else ""
    data.frame(Column = cn, Rule = format_rule(lo, hi),
               N_Violations = n_v,
               Pct_Violations = round(100 * n_v / sum(!is.na(x)), 1),
               Sample_Values = samp, stringsAsFactors = FALSE)
  }))
}

format_rule <- function(lo, hi) {
  if (!is.na(lo) && !is.na(hi)) paste0("[", lo, ", ", hi, "]")
  else if (!is.na(lo))           paste0("\u2265 ", lo)
  else                           paste0("\u2264 ", hi)
}

# UI HELPER - save-to-workspace button shown above a plot, right-aligned
plot_dl_btn <- function(id, label = "Save PNG") {
  div(style = "display:flex; justify-content:flex-end; margin-bottom:8px;",
      actionButton(id, label = tagList(icon("floppy-disk"), " ", label),
                   class = "btn-dl-plot"))
}


# ══════════════════════════════════════════════════════════════════════════════
#  RELATIONAL (MULTI-TABLE) PROFILING - PHASE 1
#  Whole-folder CSV load, candidate-key detection, relationship mapping, and
#  linkage-integrity diagnostics for datasets split across several linked CSVs.
#  These operate on a NAMED LIST of data frames; the single-file profiling engine
#  above is reused unchanged on whichever table the user makes "active".
# ══════════════════════════════════════════════════════════════════════════════

# ── FOLDER PICKER MODAL ───────────────────────────────────────────────────────
# Like open_file_modal, but selects a *directory* of CSVs rather than one file.
# Shows the CSV count beside each folder and a "Load all CSVs here" confirm button.
open_folder_modal <- function(dir) {
  if (!dir.exists(dir)) {
    showNotification(paste("Directory not found:", dir), type = "error"); return()
  }
  items      <- list.files(dir, full.names = FALSE, all.files = FALSE)
  full_paths <- file.path(dir, items)
  is_dir     <- dir.exists(full_paths)
  dirs       <- sort(items[is_dir])
  n_csv_here <- length(list.files(dir, pattern = FILE_PATTERN, ignore.case = TRUE))

  root     <- WORKSPACE_FILES
  rel_path <- sub(paste0("^", gsub("([.+*?|(){}\\[\\]^$\\\\])", "\\\\\\1", root)), "", dir)
  parts    <- Filter(nzchar, strsplit(rel_path, "/")[[1]])

  crumbs <- tagList(
    tags$a(href = "#", class = "fb-crumb-link",
           onclick = sprintf("Shiny.setInputValue('folder_nav_to','%s',{priority:'event'}); return false;", root),
           "files"),
    tags$span(class = "fb-crumb-sep", " / ")
  )
  for (i in seq_along(parts)) {
    p <- file.path(root, paste(parts[seq_len(i)], collapse = "/"))
    crumbs <- tagList(crumbs,
      tags$a(href = "#", class = "fb-crumb-link",
             onclick = sprintf("Shiny.setInputValue('folder_nav_to','%s',{priority:'event'}); return false;", p),
             parts[i]),
      tags$span(class = "fb-crumb-sep", " / "))
  }

  body_content <- tagList(
    div(class = "fb-modal-breadcrumb", crumbs),
    div(class = "fb-save-current",
        div(class = "fb-save-current-path", icon("folder-open"), " ", dir)),
    div(style = "margin:2px 0 10px; font-size:0.86rem; color:#4A6070;",
        if (n_csv_here > 0)
          tagList(icon("file-csv"),
                  sprintf(" %d CSV file%s directly in this folder",
                          n_csv_here, if (n_csv_here != 1) "s" else ""))
        else "No CSV files directly in this folder \u2014 open a subfolder below."),
    if (dir != root)
      div(class = "fb-modal-row fb-modal-dir",
          onclick = sprintf("Shiny.setInputValue('folder_nav_to','%s',{priority:'event'})", dirname(dir)),
          icon("arrow-up"), tags$span(" ..")),
    lapply(dirs, function(d) {
      sub_n <- length(list.files(file.path(dir, d), pattern = FILE_PATTERN, ignore.case = TRUE))
      div(class = "fb-modal-row fb-modal-dir",
          onclick = sprintf("Shiny.setInputValue('folder_nav_to','%s',{priority:'event'})", file.path(dir, d)),
          icon("folder"), tags$span(class = "fb-modal-name", d),
          tags$span(class = "fb-modal-size", if (sub_n > 0) sprintf("%d CSV", sub_n) else ""))
    }),
    if (length(dirs) == 0)
      p(class = "fb-modal-empty",
        "No subfolders here. Use the button below to load the CSVs in this folder.")
  )

  showModal(modalDialog(
    title = tagList(icon("folder-tree"), " Load a folder of CSV files"),
    body_content, size = "l", easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("load_folder_confirm",
                   label = tagList(icon("layer-group"),
                                   sprintf(" Load %d CSV file%s here", n_csv_here,
                                           if (n_csv_here != 1) "s" else "")),
                   class = "btn-save-confirm")
    )
  ))
}

# ── KEY DETECTION ─────────────────────────────────────────────────────────────
# Per-column key diagnostics for one table. A primary-key candidate is a column
# whose non-missing values are all unique and which has no missing values.
detect_candidate_keys <- function(df) {
  n <- nrow(df)
  do.call(rbind, lapply(names(df), function(cn) {
    x     <- df[[cn]]
    n_na  <- sum(is.na(x))
    nu    <- length(unique(x[!is.na(x)]))
    ratio <- if (n > 0) nu / n else 0
    data.frame(column = cn, type = col_type_label(x), n_unique = nu, n_missing = n_na,
               uniq_ratio = round(ratio, 4), is_key = (n_na == 0 && nu == n && n > 0),
               stringsAsFactors = FALSE)
  }))
}

# Best single-column primary key: a unique+complete column, preferring id-like names.
best_primary_key <- function(df) {
  ck   <- detect_candidate_keys(df)
  keys <- ck$column[ck$is_key]
  if (length(keys) == 0) return(NA_character_)
  id_like <- keys[grepl("id", tolower(keys))]
  if (length(id_like) > 0) return(id_like[1])
  keys[1]
}

# Normalise a column name for cross-table matching (case/punctuation-insensitive).
.norm_name <- function(s) gsub("[^a-z0-9]", "", tolower(trimws(s)))

# Find the actual column name in a table matching a normalised key, else NA.
resolve_col <- function(df, norm_name) {
  hit <- names(df)[vapply(names(df), function(cn) .norm_name(cn) == norm_name, logical(1))]
  if (length(hit) > 0) hit[1] else NA_character_
}

# Columns shared (by normalised name) across >= 2 tables - the candidate links.
detect_shared_columns <- function(tables) {
  reg <- list(); disp <- list()
  for (tn in names(tables)) {
    seen <- character(0)
    for (cn in names(tables[[tn]])) {
      k <- .norm_name(cn)
      if (!nzchar(k) || k %in% seen) next
      seen <- c(seen, k)
      reg[[k]] <- c(reg[[k]], tn)
      if (is.null(disp[[k]])) disp[[k]] <- cn
    }
  }
  shared <- Filter(function(v) length(v) >= 2, reg)
  if (length(shared) == 0) return(NULL)
  out <- do.call(rbind, lapply(names(shared), function(k)
    data.frame(norm_name = k, column = disp[[k]], n_tables = length(shared[[k]]),
               tables = paste(shared[[k]], collapse = ", "), stringsAsFactors = FALSE)))
  out[order(-out$n_tables, out$column), , drop = FALSE]
}

# Given a chosen link column (normalised), split holders into spine + children.
# Spine = the table where the link is a unique, complete key (most distinct IDs).
classify_link <- function(tables, link_norm) {
  holders <- character(0); info <- list()
  for (tn in names(tables)) {
    col <- resolve_col(tables[[tn]], link_norm)
    if (is.na(col)) next
    x    <- tables[[tn]][[col]]
    n    <- length(x); n_na <- sum(is.na(x)); nu <- length(unique(x[!is.na(x)]))
    info[[tn]] <- list(col = col, n = n, n_na = n_na, n_unique = nu, type = col_type_label(x),
                       is_unique = (n_na == 0 && nu == n && n > 0))
    holders <- c(holders, tn)
  }
  if (length(holders) == 0) return(NULL)
  uniq_holders <- holders[vapply(holders, function(h) info[[h]]$is_unique, logical(1))]
  spine <- if (length(uniq_holders) > 0)
             uniq_holders[which.max(vapply(uniq_holders, function(h) info[[h]]$n_unique, numeric(1)))]
           else
             holders[which.max(vapply(holders, function(h) info[[h]]$n_unique, numeric(1)))]
  list(holders = holders, info = info, spine = spine,
       children = setdiff(holders, spine), spine_is_clean = info[[spine]]$is_unique)
}

# ── LINKAGE DIAGNOSTICS ───────────────────────────────────────────────────────
spine_ids <- function(tables, cls) {
  x <- tables[[cls$spine]][[cls$info[[cls$spine]]$col]]
  unique(x[!is.na(x)])
}

# Orphans, subject coverage, and cardinality for each child against the spine.
linkage_child_report <- function(tables, cls) {
  ref <- spine_ids(tables, cls)
  do.call(rbind, lapply(cls$children, function(ch) {
    col      <- cls$info[[ch]]$col
    x        <- tables[[ch]][[col]]
    present  <- x[!is.na(x)]
    n_orphan <- sum(!(present %in% ref))
    matched  <- present[present %in% ref]
    n_cov    <- length(unique(matched))
    card     <- as.integer(table(matched))
    data.frame(
      child            = ch,
      link_col         = col,
      n_rows           = length(x),
      n_missing_key    = sum(is.na(x)),
      n_orphan         = n_orphan,
      pct_orphan       = if (length(present) > 0) round(100 * n_orphan / length(present), 1) else 0,
      subjects_covered = n_cov,
      pct_coverage     = if (length(ref) > 0) round(100 * n_cov / length(ref), 1) else 0,
      n_childless      = length(ref) - n_cov,
      card_min         = if (length(card) > 0) min(card) else 0L,
      card_median      = if (length(card) > 0) as.numeric(stats::median(card)) else 0,
      card_max         = if (length(card) > 0) max(card) else 0L,
      relationship     = if (length(card) > 0 && max(card) == 1) "1:1" else "1:many",
      stringsAsFactors = FALSE)
  }))
}

# A small sample of orphan IDs for one child (disclosure is admin-governed here).
orphan_sample <- function(tables, cls, child, k = 8) {
  ref     <- spine_ids(tables, cls)
  x       <- tables[[child]][[cls$info[[child]]$col]]
  present <- x[!is.na(x)]
  orph    <- unique(present[!(present %in% ref)])
  if (length(orph) == 0) return("")
  paste(utils::head(orph, k), collapse = ", ")
}

# Cardinality vector (rows per spine subject) for one child, for plotting.
cardinality_vector <- function(tables, cls, child) {
  ref     <- spine_ids(tables, cls)
  x       <- tables[[child]][[cls$info[[child]]$col]]
  present <- x[!is.na(x)]
  as.integer(table(present[present %in% ref]))
}

# Type + format of the link column across all tables that hold it (drift check).
linkage_format_drift <- function(tables, cls) {
  do.call(rbind, lapply(cls$holders, function(tn) {
    col    <- cls$info[[tn]]$col
    x      <- tables[[tn]][[col]]
    chr    <- as.character(x[!is.na(x)])
    ws     <- any(chr != trimws(chr))
    lead0  <- any(grepl("^0[0-9]", chr))
    widths <- if (length(chr) > 0) range(nchar(chr)) else c(NA, NA)
    samp   <- if (length(chr) > 0) paste(utils::head(unique(chr), 3), collapse = ", ") else ""
    data.frame(
      table         = tn,
      column        = col,
      r_type        = class(x)[1],
      width         = if (any(is.na(widths))) "" else if (widths[1] == widths[2]) as.character(widths[1])
                      else paste0(widths[1], "-", widths[2]),
      whitespace    = if (ws) "yes" else "no",
      leading_zeros = if (lead0) "yes" else "no",
      sample        = samp,
      stringsAsFactors = FALSE)
  }))
}

# ── RELATIONAL PLOTS ──────────────────────────────────────────────────────────
# Entity-relationship diagram drawn in ggplot2 (no extra dependencies, and it
# exports through the same save-PNG path as every other chart).
plot_er_diagram <- function(tables, cls, child_rep = NULL) {
  spine    <- cls$spine
  children <- cls$children
  isolated <- setdiff(names(tables), cls$holders)
  node_w <- 2.8; node_h <- 1.05

  mk_node <- function(name, x, y, role) {
    df  <- tables[[name]]
    key <- if (!is.na(cls$info[[name]]$col)) cls$info[[name]]$col else best_primary_key(df)
    data.frame(name = name, x = x, y = y, role = role, rows = nrow(df),
               key = key %||% "\u2014", stringsAsFactors = FALSE)
  }

  nodes <- mk_node(spine, 0, 0, "spine")
  nc <- length(children)
  if (nc > 0) {
    ys <- if (nc == 1) 0 else seq((nc - 1), -(nc - 1), length.out = nc) * 1.5
    for (i in seq_along(children)) nodes <- rbind(nodes, mk_node(children[i], 7, ys[i], "child"))
  }
  ni <- length(isolated)
  if (ni > 0) {
    base_y <- min(nodes$y) - 2.6
    xs <- if (ni == 1) 3.5 else seq(0, 7, length.out = ni)
    for (i in seq_along(isolated)) nodes <- rbind(nodes, mk_node(isolated[i], xs[i], base_y, "isolated"))
  }

  edges <- NULL
  if (nc > 0) {
    edges <- do.call(rbind, lapply(children, function(ch) {
      rel <- if (!is.null(child_rep)) child_rep$relationship[child_rep$child == ch][1] else "1:many"
      data.frame(x = node_w / 2, xend = 7 - node_w / 2, y = 0,
                 yend = nodes$y[nodes$name == ch], label = rel %||% "1:many",
                 stringsAsFactors = FALSE)
    }))
  }

  fill_for <- c(spine = "#007A6E", child = "#5A8FA8", isolated = "#B0413E")
  g <- ggplot2::ggplot()
  if (!is.null(edges))
    g <- g +
      ggplot2::geom_segment(data = edges, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            colour = "#4A6070", linewidth = 0.7,
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"), type = "closed")) +
      ggplot2::geom_label(data = edges, ggplot2::aes(x = (x + xend) / 2, y = (y + yend) / 2, label = label),
                          fill = "#FFFFFF", colour = "#1A2A3A", size = 3.1, label.size = 0)
  g +
    ggplot2::geom_tile(data = nodes, ggplot2::aes(x = x, y = y, fill = role),
                       width = node_w, height = node_h, colour = "#FFFFFF", linewidth = 1) +
    ggplot2::geom_text(data = nodes, ggplot2::aes(x = x, y = y + 0.20, label = name),
                       colour = "#FFFFFF", fontface = "bold", size = 3.7) +
    ggplot2::geom_text(data = nodes,
                       ggplot2::aes(x = x, y = y - 0.22,
                                    label = paste0(format(rows, big.mark = ","), " rows  \u00b7  key: ", key)),
                       colour = "#FFFFFF", size = 2.7) +
    ggplot2::scale_fill_manual(values = fill_for, guide = "none") +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::labs(title = "Dataset relationships",
                  subtitle = paste0("Spine: ", spine, "  \u00b7  ", nc, " child table",
                                    if (nc != 1) "s" else "",
                                    if (ni > 0) paste0("  \u00b7  ", ni, " unlinked") else "")) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      plot.title      = ggplot2::element_text(colour = "#1A2A3A", face = "bold", size = 15),
      plot.subtitle   = ggplot2::element_text(colour = "#4A6070", size = 11),
      plot.margin     = ggplot2::margin(20, 45, 20, 45))
}

# Histogram of rows-per-subject for one child table.
plot_cardinality <- function(v, child_name, link_col) {
  if (length(v) == 0) return(NULL)
  med <- stats::median(v); mx <- max(v)
  ggplot2::ggplot(data.frame(rows_per_subject = v), ggplot2::aes(x = rows_per_subject)) +
    ggplot2::geom_histogram(binwidth = 1, fill = "#007A6E", colour = "#FFFFFF", alpha = 0.85) +
    ggplot2::geom_vline(xintercept = med, colour = "#1A2A3A", linetype = "dashed", linewidth = 0.8) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
    ggplot2::labs(x = paste0("Rows per subject in ", child_name), y = "Number of subjects",
                  title = paste0("Cardinality: ", child_name),
                  subtitle = paste0("Linked on ", link_col, "  \u00b7  ", length(v),
                                    " subjects with \u22651 row  \u00b7  median ", round(med, 1),
                                    ", max ", mx)) +
    .dre_theme_grid()
}

# ── LINKAGE REPORT EXPORT ─────────────────────────────────────────────────────
build_linkage_csv <- function(tables, cls, child_rep, drift, file_path) {
  con <- file(file_path, open = "w"); on.exit(close(con))
  writeLines("## Linkage integrity report", con)
  writeLines(paste0("# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), con)
  writeLines(paste0("# Spine table: ", cls$spine, "  (link column: ", cls$info[[cls$spine]]$col, ")"), con)
  writeLines(paste0("# Spine key unique & complete: ", if (cls$spine_is_clean) "yes" else "NO"), con)
  writeLines("", con)
  writeLines("## Per-table link column", con)
  ktab <- do.call(rbind, lapply(cls$holders, function(h) data.frame(
    table = h, link_column = cls$info[[h]]$col, type = cls$info[[h]]$type,
    n_rows = cls$info[[h]]$n, n_distinct_ids = cls$info[[h]]$n_unique,
    n_missing = cls$info[[h]]$n_na, unique_complete = cls$info[[h]]$is_unique,
    role = if (h == cls$spine) "spine" else "child", stringsAsFactors = FALSE)))
  utils::write.csv(ktab, con, row.names = FALSE)
  writeLines("", con)
  if (!is.null(child_rep)) {
    writeLines("## Child tables: referential integrity, coverage & cardinality", con)
    utils::write.csv(child_rep, con, row.names = FALSE)
    writeLines("", con)
  }
  if (!is.null(drift)) {
    writeLines("## Key format across tables", con)
    utils::write.csv(drift, con, row.names = FALSE)
  }
}
# ── 5. INLINE CSS ─────────────────────────────────────────────────────────────

APP_CSS <- "
:root {
  --bg:         #F4F6F9;
  --surface:    #FFFFFF;
  --surface-alt:#EDF0F4;
  --border:     #CDD5DF;
  --teal:       #007A6E;
  --text:       #1A2A3A;
  --muted:      #4A6070;
  --purple:     #5A2A90;
  --warn-bg:    #FFF8E6;
  --warn-border:#E0B040;
  --warn-text:  #7A5000;
  --out-bg:     #F5F0FB;
  --out-border: #C0A8E0;
  --out-text:   #4A1A80;
  --info-bg:    #EBF5FB;
  --info-border:#A8D0E6;
  --info-text:  #1A4A6A;
  --sidebar-bg: #1E3248;
  --sidebar-txt:#D0DDE8;
  --sidebar-mut:#7A9AB0;
  --sidebar-acc:#4DD9CA;
}
body, html { background: var(--bg) !important; color: var(--text) !important;
  font-family: 'Segoe UI', system-ui, sans-serif; font-size: 15px; }
.skin-black .main-header .logo,
.skin-black .main-header .navbar { background: var(--sidebar-bg) !important;
  border-bottom: 2px solid var(--teal) !important; }
.skin-black .main-header .logo { color: var(--sidebar-acc) !important;
  font-weight: 700; font-size: 1.05rem; }
.skin-black .main-sidebar, .skin-black .left-side { background: var(--sidebar-bg) !important; }
.content-wrapper, .right-side { background: var(--bg) !important; color: var(--text) !important; }
.skin-black .wrapper { background: var(--bg) !important; }
.box { background: var(--surface) !important; color: var(--text) !important; }
.sidebar-section-label { font-size: 0.75rem; color: var(--sidebar-mut);
  text-transform: uppercase; letter-spacing: 1.2px; margin-bottom: 8px; margin-top: 14px; }
.sidebar-section-label:first-of-type { margin-top: 0; }
.sidebar-divider { border: none; border-top: 1px solid rgba(255,255,255,0.1); margin: 14px 0 10px; }
.sidebar-root-note { font-size: 0.78rem; color: var(--sidebar-mut); word-break: break-all;
  margin-top: 12px; padding-top: 12px; border-top: 1px solid rgba(255,255,255,0.1); }
.sidebar-root-note span { color: var(--sidebar-acc); }
.btn-open-file { background: rgba(255,255,255,0.1) !important;
  border: 1px solid rgba(77,217,202,0.45) !important; color: var(--sidebar-txt) !important;
  font-weight: 600 !important; border-radius: 8px !important; font-size: 0.92rem !important;
  width: 100%; text-align: left !important; padding: 9px 12px !important; }
.btn-open-file:hover { background: rgba(77,217,202,0.18) !important;
  border-color: var(--sidebar-acc) !important; color: var(--sidebar-acc) !important; }
.btn-export { background: rgba(77,217,202,0.12) !important;
  border: 1px solid rgba(77,217,202,0.45) !important; color: var(--sidebar-acc) !important;
  font-weight: 600 !important; border-radius: 8px !important; font-size: 0.92rem !important;
  width: 100%; text-align: left !important; margin-top: 6px !important; padding: 9px 12px !important; }
.btn-export:hover { background: rgba(77,217,202,0.28) !important; }
.btn-label-upload { background: rgba(255,255,255,0.07) !important;
  border: 1px solid rgba(77,217,202,0.3) !important; color: var(--sidebar-txt) !important;
  font-weight: 500 !important; border-radius: 8px !important; font-size: 0.88rem !important;
  width: 100%; text-align: left !important; padding: 8px 12px !important; }
/* ── Plot download button ── */
.btn-dl-plot { background: var(--surface-alt) !important;
  border: 1px solid var(--border) !important; color: var(--teal) !important;
  font-weight: 600 !important; border-radius: 6px !important; font-size: 0.88rem !important;
  padding: 5px 14px !important; }
.btn-dl-plot:hover { background: #E8F5F3 !important; border-color: var(--teal) !important; }
/* ── Label status badge ── */
.label-badge { background: rgba(77,217,202,0.15); border: 1px solid rgba(77,217,202,0.4);
  border-radius: 6px; padding: 8px 12px; margin-top: 8px; font-size: 0.82rem;
  color: var(--sidebar-acc); }
.label-badge-none { background: rgba(255,255,255,0.06); border: 1px solid rgba(255,255,255,0.12);
  border-radius: 6px; padding: 8px 12px; margin-top: 8px; font-size: 0.82rem;
  color: var(--sidebar-mut); }
.fb-hint { color: var(--sidebar-mut); font-size: 0.88rem; padding: 8px 0; }
.fb-selected-badge { background: rgba(255,255,255,0.08); border: 1px solid rgba(77,217,202,0.4);
  border-radius: 8px; padding: 10px 14px; margin-top: 10px; }
.fb-selected-label { font-size: 0.73rem; color: var(--sidebar-acc); text-transform: uppercase;
  letter-spacing: 1px; margin-bottom: 4px; }
.fb-selected-name { font-weight: 600; font-size: 0.93rem; word-break: break-all; color: var(--sidebar-txt); }
.fb-selected-meta { font-size: 0.8rem; color: var(--sidebar-mut); margin-top: 3px; }
.stat-row { display: flex; gap: 14px; flex-wrap: wrap; margin-bottom: 28px; }
.stat-card { flex: 1 1 110px; background: var(--surface); border: 1px solid var(--border);
  border-radius: 10px; padding: 16px 14px; text-align: center; box-shadow: 0 1px 4px rgba(0,0,0,0.06); }
.stat-number      { font-size: 2rem; font-weight: 700; color: var(--teal); line-height: 1.1; }
.stat-number-warn { font-size: 2rem; font-weight: 700; color: #B04000; line-height: 1.1; }
.stat-label { font-size: 0.88rem; color: var(--muted); margin-top: 6px; }
.empty-state { text-align: center; padding: 80px 40px; color: var(--muted); }
.empty-icon  { font-size: 3.5rem; margin-bottom: 16px; }
.section-tag { font-size: 0.75rem; color: var(--teal); text-transform: uppercase;
  letter-spacing: 1.5px; margin-bottom: 8px; font-weight: 600; }
.corr-empty { text-align: center; padding: 60px 40px; color: var(--muted); font-size: 1.05rem; }
.alert-missing { background: var(--warn-bg); border: 1px solid var(--warn-border);
  border-radius: 8px; padding: 14px 18px; margin-bottom: 16px; }
.alert-missing-title { color: var(--warn-text); font-weight: 600; margin-bottom: 6px; }
.alert-outlier { background: var(--out-bg); border: 1px solid var(--out-border);
  border-radius: 8px; padding: 14px 18px; margin-bottom: 16px; }
.alert-outlier-title { color: var(--out-text); font-weight: 600; margin-bottom: 6px; }
.alert-info { background: var(--info-bg); border: 1px solid var(--info-border);
  border-radius: 8px; padding: 14px 18px; margin-bottom: 16px; }
.alert-info-title { color: var(--info-text); font-weight: 600; margin-bottom: 6px; }
.alert-list { margin: 0; padding-left: 20px; color: var(--muted); font-size: 0.95rem; }
.control-panel { background: var(--surface-alt); border: 1px solid var(--border);
  border-radius: 10px; padding: 16px 14px; margin-bottom: 12px; }
.control-label { font-size: 0.75rem; color: var(--teal); text-transform: uppercase;
  letter-spacing: 1.2px; font-weight: 600; margin-bottom: 6px; }
.nav-tabs { border-color: var(--border); background: var(--surface);
  border-radius: 8px 8px 0 0; padding: 4px 8px 0; }
.nav-tabs > li > a { color: var(--muted); background: transparent; border: none;
  font-size: 0.95rem; font-weight: 500; padding: 10px 14px; }
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:focus,
.nav-tabs > li.active > a:hover { color: var(--teal) !important;
  background: var(--bg) !important; border-bottom: 3px solid var(--teal) !important;
  border-top: 1px solid var(--border); border-left: 1px solid var(--border);
  border-right: 1px solid var(--border); font-weight: 700 !important; }
.nav-tabs > li > a:hover { color: var(--text) !important; background: var(--surface-alt) !important; }
.tab-content { padding: 24px; background: var(--surface);
  border: 1px solid var(--border); border-top: none; border-radius: 0 0 8px 8px; }
.dataTables_wrapper { color: var(--text); font-size: 0.95rem; }
table.dataTable thead { background: var(--surface-alt); }
table.dataTable thead th { color: var(--text) !important; font-weight: 600 !important;
  border-bottom: 2px solid var(--border) !important; }
table.dataTable tbody tr { background: var(--surface) !important; }
table.dataTable tbody tr:nth-child(even) { background: #F8FAFB !important; }
table.dataTable tbody tr:hover { background: #E8F5F3 !important; }
table.dataTable tbody td { color: var(--text) !important; border-color: var(--border) !important; }
.dataTables_filter input { background: var(--surface) !important; color: var(--text) !important;
  border: 1px solid var(--border) !important; border-radius: 6px; }
.dataTables_length select { background: var(--surface) !important; color: var(--text) !important;
  border: 1px solid var(--border) !important; border-radius: 6px; }
.dataTables_info, .dataTables_paginate { color: var(--muted) !important; }
.paginate_button { color: var(--muted) !important; border-radius: 4px !important; }
.paginate_button.current, .paginate_button.current:hover { background: var(--teal) !important;
  color: #fff !important; border-color: var(--teal) !important; }
.paginate_button:hover { background: var(--surface-alt) !important; color: var(--text) !important; }
.shiny-plot-output { border-radius: 8px; overflow: hidden;
  box-shadow: 0 1px 6px rgba(0,0,0,0.08); border: 1px solid var(--border); }
.type-pill { display: inline-block; padding: 3px 10px; border-radius: 20px;
  font-size: 0.78rem; font-weight: 600; text-transform: uppercase; }
.type-numeric   { background: rgba(0,122,110,0.12); color: #005548; }
.type-character { background: rgba(74,96,112,0.12); color: #2A4050; }
.type-logical   { background: rgba(140,90,0,0.12);  color: #6B4400; }
.type-datetime  { background: rgba(90,42,144,0.12); color: var(--purple); }
.modal-content { background: var(--surface) !important; border: 1px solid var(--border) !important;
  color: var(--text) !important; }
.modal-header { background: var(--surface-alt) !important; border-bottom: 1px solid var(--border) !important; }
.modal-title  { color: var(--text) !important; font-weight: 600; }
.modal-footer { background: var(--surface-alt) !important; border-top: 1px solid var(--border) !important; }
.modal-body   { background: var(--surface) !important; }
.fb-modal-breadcrumb { display: flex; align-items: center; flex-wrap: wrap;
  padding: 8px 12px; background: var(--surface-alt); border-radius: 6px;
  margin-bottom: 8px; font-size: 0.88rem; gap: 2px; border: 1px solid var(--border); }
.fb-crumb-link { color: var(--teal) !important; text-decoration: none !important; font-weight: 600; }
.fb-crumb-link:hover { text-decoration: underline !important; }
.fb-crumb-sep  { color: var(--muted); }
.fb-modal-row  { display: flex; align-items: center; gap: 10px; padding: 11px 16px;
  border-bottom: 1px solid var(--border); cursor: pointer; transition: background 0.12s;
  font-size: 0.95rem; color: var(--text); }
.fb-modal-row:last-child { border-bottom: none; }
.fb-modal-dir  { color: var(--muted); }
.fb-modal-dir:hover  { background: var(--surface-alt); }
.fb-modal-file:hover { background: #E8F5F3; border-left: 3px solid var(--teal); }
.fb-modal-file .fa, .fb-modal-file .fas { color: var(--teal); }
.fb-modal-dir  .fa, .fb-modal-dir  .fas { color: var(--muted); }
.fb-modal-name { flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.fb-modal-size { font-size: 0.82rem; color: var(--muted); white-space: nowrap; }
.fb-modal-empty { padding: 32px 20px; color: var(--muted); text-align: center; }
.form-control, select { background: var(--surface) !important; color: var(--text) !important;
  border: 1px solid var(--border) !important; border-radius: 6px !important; }
.selectize-input { background: var(--surface) !important; color: var(--text) !important;
  border: 1px solid var(--border) !important; border-radius: 6px !important; }
.selectize-dropdown { background: var(--surface) !important; color: var(--text) !important;
  border: 1px solid var(--border) !important; }
.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active { background: var(--surface-alt) !important; }
/* File input inside dark sidebar */
.shiny-input-container label { color: var(--sidebar-mut) !important; font-size: 0.82rem !important; }
input[type=file] { color: var(--sidebar-txt) !important; font-size: 0.82rem; }
/* ── Save-to-workspace modal ── */
.fb-save-current { display:flex; align-items:center; gap:6px; padding:9px 12px;
  background: var(--info-bg); border:1px solid var(--info-border); border-radius:6px;
  color: var(--info-text); font-size:0.9rem; margin-bottom:10px; }
.fb-save-current .fa, .fb-save-current .fas { color: var(--info-text); }
.fb-save-current-path { word-break:break-all; font-weight:600; }
.fb-save-dirlist { max-height:230px; overflow-y:auto; border:1px solid var(--border);
  border-radius:6px; }
.fb-save-dirlist .fb-modal-row:last-child { border-bottom:none; }
.btn-save-confirm { background: var(--teal) !important; color:#FFFFFF !important;
  border:1px solid var(--teal) !important; font-weight:600 !important;
  border-radius:6px !important; }
.btn-save-confirm:hover { background:#005F56 !important; border-color:#005F56 !important;
  color:#FFFFFF !important; }
.btn-newfolder { background: var(--surface-alt) !important; border:1px solid var(--border) !important;
  color: var(--teal) !important; font-weight:600 !important; border-radius:6px !important;
  white-space:nowrap; }
.btn-newfolder:hover { background:#E8F5F3 !important; border-color: var(--teal) !important; }
"


# ── 6. UI ─────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = span(
      style = "color:#4DD9CA; font-weight:700; letter-spacing:-0.5px;",
      "ARIDHIA  ",
      span(style = "color:#A8C8D8; font-weight:400; font-size:0.92rem;", "Cohort Profiler")
    ),
    titleWidth = 280
  ),

  dashboardSidebar(
    width = 280,
    tags$style(APP_CSS),
    tags$div(style = "padding: 16px;",

      # ── Dataset section ──────────────────────────────────────────────────────
      div(class = "sidebar-section-label", "Dataset"),
      actionButton("open_file", label = tagList(icon("folder-open"), " Browse Workspace Files"),
                   class = "btn-open-file"),
      uiOutput("selected_file_ui"),
      uiOutput("reopen_btn_ui"),
      uiOutput("export_btn_ui"),

      # ── Column Labels section ─────────────────────────────────────────────
      tags$hr(class = "sidebar-divider"),
      div(class = "sidebar-section-label", "Column Labels"),
      p(style = "font-size:0.82rem; color:#7A9AB0; margin-bottom:8px; line-height:1.45;",
        "Upload a 2-column CSV (", tags$code(style="color:#4DD9CA;", "column,label"),
        ") to rename columns across all plots and displays."),
      fileInput("label_file", label = NULL, accept = ".csv",
                placeholder = "No label file",
                buttonLabel = tagList(icon("tag"), " Label CSV")),
      uiOutput("label_status_ui"),
      uiOutput("label_clear_ui"),

      # ── Dataset B section ─────────────────────────────────────────────────
      tags$hr(class = "sidebar-divider"),
      div(class = "sidebar-section-label", "Compare Dataset (B)"),
      p(style = "font-size:0.82rem; color:#7A9AB0; margin-bottom:8px; line-height:1.45;",
        "Load a second CSV to overlay distributions and compare statistics in the",
        tags$strong(style="color:#4DD9CA;", " Compare"), " tab."),
      actionButton("open_file_b", label = tagList(icon("folder-open"), " Browse for Dataset B"),
                   class = "btn-open-file"),
      uiOutput("selected_file_b_ui"),
      uiOutput("clear_b_btn_ui"),

      # ── Relational dataset (multi-table) ──────────────────────────────────
      tags$hr(class = "sidebar-divider"),
      div(class = "sidebar-section-label", "Relational dataset (multi-table)"),
      p(style = "font-size:0.82rem; color:#7A9AB0; margin-bottom:8px; line-height:1.45;",
        "Load a whole folder of linked CSVs to map how they join and check linkage",
        tags$strong(style = "color:#4DD9CA;", " integrity"), ". See the ",
        tags$strong(style = "color:#4DD9CA;", "Relationships"), " and ",
        tags$strong(style = "color:#4DD9CA;", "Linkage"), " tabs."),
      actionButton("open_folder", label = tagList(icon("folder-tree"), " Load Folder of CSVs"),
                   class = "btn-open-file"),
      uiOutput("tables_status_ui"),
      uiOutput("active_table_ui"),
      uiOutput("clear_tables_ui"),

      div(class = "sidebar-root-note", "Root: ", tags$span(WORKSPACE_FILES))
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-black .main-header, .skin-black .main-header .logo,
      .skin-black .main-header .navbar, .main-header { background-color: #1E3248 !important; }
      .skin-black .main-header .logo,
      .skin-black .main-header .navbar { border-bottom: 2px solid #007A6E !important; }
      .skin-black .main-header .logo { color: #4DD9CA !important; font-size: 1.05rem !important; }
      .skin-black .main-header .navbar .nav > li > a,
      .skin-black .main-header .navbar .sidebar-toggle { color: #A8C8D8 !important;
        background-color: transparent !important; }
      .skin-black .main-sidebar, .skin-black .left-side { background-color: #1E3248 !important; }
      .skin-black .sidebar-menu > li > a { color: #D0DDE8 !important; }
      html, body { background-color: #F4F6F9 !important; color: #1A2A3A !important; }
      .wrapper, .skin-black .wrapper { background-color: #F4F6F9 !important; }
      .skin-black .content-wrapper, .skin-black .right-side,
      .content-wrapper, .right-side { background-color: #F4F6F9 !important; }
    "))),
    uiOutput("main_ui")
  )
)


# ── 7. SERVER ─────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  selected_file <- reactiveVal(NULL)
  loaded        <- reactiveVal(NULL)
  load_error    <- reactiveVal(NULL)
  loaded_path   <- reactiveVal(NULL)
  label_cleared <- reactiveVal(0)   # increment to force re-read after clear
  pending_save  <- reactiveVal(NULL)             # list(default_name, writer)
  save_dir      <- reactiveVal(WORKSPACE_FILES)  # current folder in the save dialog

  observeEvent(input$open_file,   { open_file_modal(WORKSPACE_FILES) })
  observeEvent(input$reopen_file, { open_file_modal(WORKSPACE_FILES) })
  observeEvent(input$nav_to,      { open_file_modal(input$nav_to)    })

  observeEvent(input$open_local, {
    removeModal(); path <- input$open_local
    if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
      showNotification("Only CSV files can be opened.", type = "warning", duration = 4); return()
    }
    if (!file.exists(path)) {
      showNotification(paste("File not found:", path), type = "error", duration = 5); return()
    }
    selected_file(path)
    withProgress(message = "Loading...", value = 0.3, {
      result <- tryCatch({
        df <- readr::read_csv(path, show_col_types = FALSE)
        load_error(NULL); incProgress(0.7); df
      }, error = function(e) { load_error(conditionMessage(e)); NULL })
      loaded(result); loaded_path(path)
    })
  })

  # ── SAVE TO WORKSPACE (replaces browser downloads) ─────────────────────────
  # start_save() records what to write and opens the folder picker rooted at
  # WORKSPACE_FILES. The user navigates, edits the filename, optionally creates a
  # folder, then confirms; the file is written straight into the workspace so the
  # file manager and airlock can see it. No browser download is involved.

  start_save <- function(default_name, writer) {
    if (!dir.exists(WORKSPACE_FILES))
      dir.create(WORKSPACE_FILES, recursive = TRUE, showWarnings = FALSE)
    pending_save(list(default_name = default_name, writer = writer))
    save_dir(WORKSPACE_FILES)
    open_save_modal(WORKSPACE_FILES, default_name)
  }

  # Navigate folders inside the save dialog, preserving the typed filename
  observeEvent(input$save_nav_to, {
    save_dir(input$save_nav_to)
    ps   <- pending_save()
    name <- if (nzchar(input$save_filename %||% "")) input$save_filename
            else if (!is.null(ps)) ps$default_name else ""
    open_save_modal(input$save_nav_to, name)
  })

  # Create a new folder in the current location, then move into it
  observeEvent(input$create_save_folder, {
    nf <- trimws(input$save_new_folder %||% "")
    if (!nzchar(nf)) {
      showNotification("Enter a folder name.", type = "warning", duration = 4); return()
    }
    if (!grepl("^[A-Za-z0-9 ._-]+$", nf)) {
      showNotification("Folder name may use letters, digits, spaces, _ . - only.",
                       type = "warning", duration = 5); return()
    }
    newdir <- file.path(save_dir(), nf)
    if (!dir.exists(newdir)) {
      ok <- tryCatch(dir.create(newdir, showWarnings = FALSE), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        showNotification("Could not create folder.", type = "error", duration = 5); return()
      }
    }
    save_dir(newdir)
    ps   <- pending_save()
    name <- if (nzchar(input$save_filename %||% "")) input$save_filename
            else if (!is.null(ps)) ps$default_name else ""
    open_save_modal(newdir, name)
  })

  # Confirm: write the pending file into the chosen folder
  observeEvent(input$confirm_save, {
    ps <- pending_save()
    if (is.null(ps)) { removeModal(); return() }
    dir   <- save_dir()
    fname <- trimws(input$save_filename %||% "")
    if (!nzchar(fname)) {
      showNotification("Please enter a file name.", type = "warning", duration = 4); return()
    }
    # If the user removed the extension, restore the one from the default name
    want_ext <- tools::file_ext(ps$default_name)
    if (nzchar(want_ext) && !nzchar(tools::file_ext(fname)))
      fname <- paste0(fname, ".", want_ext)
    if (!dir.exists(dir)) {
      showNotification("Destination folder no longer exists.", type = "error", duration = 5); return()
    }
    full <- file.path(dir, fname)
    ok <- tryCatch({ ps$writer(full); TRUE },
                   error = function(e) {
                     showNotification(paste("Save failed:", conditionMessage(e)),
                                      type = "error", duration = 8); FALSE
                   })
    if (isTRUE(ok)) {
      removeModal()
      showNotification(
        tagList(icon("circle-check"),
                span(style = "margin-left:6px;", paste0("Saved to ", full))),
        type = "message", duration = 7)
    }
  })

  # ── Label map ─────────────────────────────────────────────────────────────
  label_map <- reactive({
    label_cleared()  # take dependency so clearing re-evaluates
    f <- input$label_file
    if (is.null(f)) return(list())
    tryCatch({
      ldf <- readr::read_csv(f$datapath, col_names = TRUE, show_col_types = FALSE)
      # Accept either (column, label) or (name, label) as headers; fall back to positions
      if (ncol(ldf) < 2) return(list())
      cn <- names(ldf)
      name_col  <- if ("column" %in% tolower(cn)) cn[tolower(cn) == "column"][1]
                   else if ("name" %in% tolower(cn)) cn[tolower(cn) == "name"][1]
                   else cn[1]
      label_col <- if ("label" %in% tolower(cn)) cn[tolower(cn) == "label"][1]
                   else cn[2]
      lmap <- as.list(stats::setNames(ldf[[label_col]], ldf[[name_col]]))
      # Remove NA or empty labels
      lmap[vapply(lmap, function(v) is.na(v) || !nzchar(trimws(v)), logical(1))] <- NULL
      lmap
    }, error = function(e) {
      showNotification(paste("Could not read label file:", conditionMessage(e)),
                       type = "warning", duration = 6)
      list()
    })
  })

  observeEvent(input$clear_labels, {
    label_cleared(label_cleared() + 1)
    # Reset the fileInput by updating the session (standard Shiny workaround)
    shinyjs_reset <- function(id) {
      session$sendCustomMessage("shinyjs-reset", id)
    }
    showNotification("Labels cleared.", type = "message", duration = 3)
  })

  output$label_status_ui <- renderUI({
    lm <- label_map()
    if (length(lm) == 0)
      return(div(class = "label-badge-none", icon("tag"), " No labels loaded"))
    div(class = "label-badge",
        icon("check"), " ", length(lm), " label", if (length(lm) != 1) "s" else "", " loaded")
  })

  output$label_clear_ui <- renderUI({
    req(length(label_map()) > 0)
    tags$div(style = "margin-top:6px;",
             actionButton("clear_labels",
                          label = tagList(icon("xmark"), " Clear Labels"),
                          class = "btn-open-file",
                          style = "font-size:0.82rem !important; padding:6px 10px !important;"))
  })

  # ── Sidebar file widgets ───────────────────────────────────────────────────
  output$selected_file_ui <- renderUI({
    sel <- selected_file()
    if (is.null(sel)) return(div(class = "fb-hint", style = "margin-top:10px;", "No file selected"))
    info <- file.info(sel)
    size <- if (!is.na(info$size))
      if (info$size < 1024^2) paste0(round(info$size / 1024, 1), " KB")
      else paste0(round(info$size / 1024^2, 1), " MB") else ""
    div(class = "fb-selected-badge",
        div(class = "fb-selected-label", "LOADED"),
        div(class = "fb-selected-name",  basename(sel)),
        div(class = "fb-selected-meta",  size, " \u00b7 CSV"))
  })

  output$reopen_btn_ui <- renderUI({
    req(selected_file())
    tags$div(style = "margin-top:8px;",
             actionButton("reopen_file", label = tagList(icon("rotate"), " Open Different File"),
                          class = "btn-open-file"))
  })

  output$export_btn_ui <- renderUI({
    req(loaded())
    tags$div(actionButton("save_summary",
                          label = tagList(icon("floppy-disk"), " Save Summary CSV"),
                          class = "btn-export"))
  })

  observeEvent(input$save_summary, {
    df <- loaded(); req(df)
    lp <- loaded_path(); lm <- label_map()
    default_name <- paste0(tools::file_path_sans_ext(basename(lp)),
                           "_profile_", format(Sys.Date(), "%Y%m%d"), ".csv")
    start_save(default_name, function(path) build_summary_csv(df, path, lp, lm))
  })

  # ── Dataset B (comparison) state & observers ───────────────────────────────
  selected_file_b <- reactiveVal(NULL)
  loaded_b        <- reactiveVal(NULL)
  loaded_path_b   <- reactiveVal(NULL)

  observeEvent(input$open_file_b,  { open_file_modal(WORKSPACE_FILES, sfx = "_b") })
  observeEvent(input$nav_to_b,     { open_file_modal(input$nav_to_b,  sfx = "_b") })

  observeEvent(input$open_local_b, {
    removeModal(); path <- input$open_local_b
    if (!grepl("\\.csv$", path, ignore.case = TRUE)) {
      showNotification("Only CSV files can be opened.", type = "warning", duration = 4); return()
    }
    if (!file.exists(path)) {
      showNotification(paste("File not found:", path), type = "error", duration = 5); return()
    }
    selected_file_b(path)
    withProgress(message = "Loading Dataset B...", value = 0.3, {
      result <- tryCatch({
        df <- readr::read_csv(path, show_col_types = FALSE)
        incProgress(0.7); df
      }, error = function(e) {
        showNotification(paste("Could not load Dataset B:", conditionMessage(e)),
                         type = "error", duration = 6)
        NULL
      })
      loaded_b(result); loaded_path_b(path)
    })
  })

  observeEvent(input$clear_b, { loaded_b(NULL); selected_file_b(NULL); loaded_path_b(NULL) })

  output$selected_file_b_ui <- renderUI({
    sel <- selected_file_b()
    if (is.null(sel))
      return(div(class = "label-badge-none", style = "margin-top:8px;", "No Dataset B loaded"))
    info <- file.info(sel)
    size <- if (!is.na(info$size))
      if (info$size < 1024^2) paste0(round(info$size / 1024, 1), " KB")
      else paste0(round(info$size / 1024^2, 1), " MB") else ""
    div(class = "fb-selected-badge",
        div(class = "fb-selected-label", "DATASET B"),
        div(class = "fb-selected-name",  basename(sel)),
        div(class = "fb-selected-meta",  size, " \u00b7 CSV"))
  })

  output$clear_b_btn_ui <- renderUI({
    req(selected_file_b())
    tags$div(style = "margin-top:6px;",
             actionButton("clear_b", label = tagList(icon("xmark"), " Clear Dataset B"),
                          class = "btn-open-file",
                          style = "font-size:0.82rem !important; padding:6px 10px !important;"))
  })

  # ── Core reactives ─────────────────────────────────────────────────────────
  col_summary  <- reactive({ df <- loaded(); req(df); profile_dataset(df) })
  numeric_cols <- reactive({ df <- loaded(); req(df); names(df)[sapply(df, is.numeric)] })
  cat_cols     <- reactive({
    df <- loaded(); req(df)
    names(df)[sapply(df, function(x) is.character(x) || is.factor(x) || is.logical(x))]
  })
  date_cols    <- reactive({ df <- loaded(); req(df); detect_date_cols(df) })
  outlier_cols <- reactive({
    cs <- col_summary(); cs$Column[!is.na(cs$N_Outliers) & cs$N_Outliers > 0]
  })

  # Labelled choice lists for selectInputs
  labelled_choices <- function(raw_names) {
    lm <- label_map()
    stats::setNames(as.list(raw_names),
                    vapply(raw_names, function(n) col_label_with_name(n, lm), character(1)))
  }

  observeEvent(loaded(), {
    nc <- numeric_cols(); cc <- cat_cols(); dc <- date_cols()
    first_any <- if (length(nc) > 0) nc[1] else if (length(cc) > 0) cc[1] else NULL
    if (!is.null(first_any)) updateSelectInput(session, "dist_col",    selected = first_any)
    oc <- outlier_cols()
    if (length(oc) > 0)      updateSelectInput(session, "outlier_col", selected = oc[1])
    if (length(nc) > 0)      updateSelectInput(session, "grp_num_col", selected = nc[1])
    if (length(cc) > 0)      updateSelectInput(session, "grp_cat_col", selected = cc[1])
    if (length(dc) > 0)      updateSelectInput(session, "tl_date_col", selected = dc[1])
    non_date <- setdiff(names(loaded()), dc)
    if (length(non_date) > 0) updateSelectInput(session, "tl_track_col", selected = non_date[1])
  }, ignoreInit = TRUE)

  effective_dist_col <- reactive({
    df <- loaded(); req(df); nc <- numeric_cols(); cc <- cat_cols(); col <- input$dist_col
    if (!is.null(col) && col %in% names(df)) return(col)
    if (length(nc) > 0) nc[1] else if (length(cc) > 0) cc[1] else names(df)[1]
  })
  effective_outlier_col <- reactive({
    df <- loaded(); req(df); oc <- outlier_cols(); col <- input$outlier_col
    if (!is.null(col) && col %in% oc) return(col)
    if (length(oc) > 0) oc[1] else NULL
  })

  # ── Main UI ────────────────────────────────────────────────────────────────
  output$main_ui <- renderUI({
    err <- load_error(); df <- loaded()
    if (!is.null(err))
      return(fluidRow(column(12, div(class = "empty-state",
        div(class = "empty-icon", "\u26a0\ufe0f"),
        h4("Could not load file"), p(style = "color:#4A6070;", err)))))
    if (is.null(df))
      return(fluidRow(column(12, div(class = "empty-state",
        div(class = "empty-icon", "\U0001f52c"), h4("No dataset loaded"),
        p(style = "color:#4A6070; max-width:420px; margin:0 auto;",
          "Click ", strong("Browse Workspace Files"), " in the sidebar to begin.")))))

    fluidRow(column(12, tabsetPanel(id = "profile_tabs",
      tabPanel("Overview",       value = "overview",  br(), uiOutput("overview_ui")),
      tabPanel("Column Summary", value = "summary",   br(),
        div(class = "section-tag", "Per-column statistics \u00b7 outliers via Tukey IQR fences"),
        DTOutput("summary_table")),
      tabPanel("Missingness",    value = "missing",   br(),
        plot_dl_btn("dl_miss"),
        plotOutput("miss_plot", height = "500px")),
      tabPanel("Correlations",   value = "corr",      br(), uiOutput("corr_ui")),
      tabPanel("Outliers",       value = "outliers",  br(), uiOutput("outlier_ui")),
      tabPanel("Distributions",  value = "dist",      br(),
        fluidRow(column(3, div(class = "section-tag", "Column"), uiOutput("col_picker")),
                 column(9, plot_dl_btn("dl_dist"), plotOutput("dist_plot", height = "420px")))),
      tabPanel("Group Analysis", value = "group",     br(), uiOutput("group_ui")),
      tabPanel("Timeline",       value = "timeline",  br(), uiOutput("timeline_ui")),
      tabPanel("Compare",        value = "compare",   br(), uiOutput("compare_ui")),
      tabPanel("Relationships", value = "relationships", br(), uiOutput("relationships_ui")),
      tabPanel("Linkage",       value = "linkage",       br(), uiOutput("linkage_ui")),
      tabPanel("Validation",     value = "validate",  br(), uiOutput("validate_ui")),
      tabPanel("Data Preview",   value = "preview",   br(), DTOutput("data_preview")),
      tabPanel("Help", value = "help",
        fluidRow(column(8, offset = 2, br(),
          h3("Cohort Profiler \u2014 User Guide"), hr(),
          h4("Overview"),
          p("Automated statistical profile of CSV datasets from your DRE workspace.
             No data leaves the secure boundary."),
          h4("Column Labels"),
          p("Upload a two-column CSV with headers ", tags$code("column"), " and ",
            tags$code("label"), " to replace cryptic column names with human-readable descriptions
            across all plots, axis labels, and select menus. The original column names are retained
            in parentheses so outputs remain traceable. Labels are also written to the exported
            summary CSV. Useful when sharing plots with clinical collaborators unfamiliar with
            data coding conventions."),
          p("Example label file:"),
          tags$pre(style = "background:#F4F6F9; padding:10px; border-radius:6px; font-size:0.9rem;",
                   "column,label\nage_at_dx,Age at Diagnosis\nrbans_total,RBANS Total Score\nbmi_baseline,BMI (Baseline)"),
          h4("Saving Plots & Tables"),
          p("Every chart has a ", tags$strong("Save PNG"), " button, and the sidebar has a ",
            tags$strong("Save Summary CSV"), " button. Clicking either opens a folder picker:
            choose where in the workspace to write the file (it opens at ",
            tags$code("/home/workspace/files/"), " by default), navigate into subfolders or
            create a new one, edit the file name, then click ", tags$strong("Save File"), ".
            Files are written directly into the workspace \u2014 nothing goes to your browser's
            download folder, so the file manager and airlock can see them immediately. Saved
            images are 300 DPI and use whatever labels are currently loaded. To move files
            outside the workspace, use the standard airlock review workflow."),
          h4("Tabs"),
          tags$dl(
            tags$dt("Overview"),
            tags$dd("Row/column counts, completeness, outlier total, duplicate rows, and near-identifier alerts."),
            tags$dt("Column Summary"),
            tags$dd("Per-column stats table with label column when labels are loaded."),
            tags$dt("Missingness"),
            tags$dd("Heatmap with labelled y-axis. Save PNG button above chart."),
            tags$dt("Correlations"),
            tags$dd("Pearson r matrix with labelled axes. Save PNG available."),
            tags$dt("Outliers"),
            tags$dd("Density curve with shaded tails. Save PNG available."),
            tags$dt("Distributions"),
            tags$dd("Histogram or top-value bar chart with Shapiro-Wilk normality result. Save PNG."),
            tags$dt("Group Analysis"),
            tags$dd("Boxplot + jitter split by categorical group, Kruskal-Wallis p-value. Save PNG."),
            tags$dt("Timeline"),
            tags$dd("Record density over time + column completeness trend. Save PNG per chart."),
            tags$dt("Compare"),
            tags$dd("Load a second CSV (Dataset B) via the sidebar to overlay distributions, compare
                     category proportions, and view a side-by-side statistics table for all matched columns.
                     Use this to check harmonisation between sites, or to compare pipeline versions."),
            tags$dt("Relationships"),
            tags$dd("Load a whole folder of linked CSVs (",
                     tags$strong("Load Folder of CSVs"), " in the sidebar). The app detects the
                     common identifier shared across files, identifies the subject ", tags$em("spine"),
                     " (the table where that ID is a unique, complete key) and the child tables that
                     reference it, and draws an entity-relationship diagram. Use the ",
                     tags$strong("Active table"), " selector to profile any one of the loaded tables
                     in all the single-file tabs above."),
            tags$dt("Linkage"),
            tags$dd("Integrity checks across the loaded tables, framed as what a relational load would
                     reject: key uniqueness and completeness on the spine, orphaned child rows
                     (referential integrity), subject coverage (structural missingness \u2014 subjects
                     absent from a child table, not merely null), cardinality (rows per subject), and
                     key format drift (type or formatting differences that would break a join). Export
                     the findings with ", tags$strong("Save Linkage Report (CSV)"), "."),
            tags$dt("Validation"),
            tags$dd("Define expected value ranges per column as comma-separated rules (column,min,max).
                     The app flags violations with count, percentage, and sample values.
                     Leave min or max blank to apply a one-sided bound."),
            tags$dt("Data Preview"),
            tags$dd("Paginated raw data with per-column search filters.")),
          h4("Notes"),
          tags$ul(
            tags$li("Large files (>100 MB) may take several seconds to load."),
            tags$li("Source datasets are read-only \u2014 loading and profiling never alters them."),
            tags$li("Saved plots and summaries are written into the workspace folder you choose
                     (default ", tags$code("/home/workspace/files/"), "), where the file manager
                     and airlock can see them."),
            tags$li("Loading a folder reads every CSV in it; files that fail to parse are skipped
                     with a notification, and the largest table is made active automatically.")),
          hr(), p(tags$em("Aridhia Cohort Profiler v4.2  \u00b7  aridhia.com"))
        ))
      )
    )))
  })

  # ── Overview ───────────────────────────────────────────────────────────────
  output$overview_ui <- renderUI({
    df <- loaded(); req(df)
    cs <- col_summary(); path <- loaded_path()
    n_rows <- nrow(df); n_cols <- ncol(df); n_miss <- sum(is.na(df))
    pct_comp       <- round(100 * (1 - n_miss / (n_rows * n_cols)), 1)
    type_counts    <- table(cs$Type)
    total_outliers <- sum(cs$N_Outliers, na.rm = TRUE)
    dq             <- detect_data_quality(df)

    tagList(
      div(class = "section-tag", "Dataset"),
      h4(style = "margin:0 0 2px;", basename(path)),
      p(style = "color:#4A6070; font-size:0.82rem; margin-bottom:20px;", path),

      div(class = "stat-row",
          div(class = "stat-card",
              div(class = "stat-number", format(n_rows, big.mark = ",")),
              div(class = "stat-label", "Rows")),
          div(class = "stat-card",
              div(class = "stat-number", n_cols),
              div(class = "stat-label", "Columns")),
          div(class = "stat-card",
              div(class = "stat-number", format(n_miss, big.mark = ",")),
              div(class = "stat-label", "Missing values")),
          div(class = "stat-card",
              div(class = "stat-number", paste0(pct_comp, "%")),
              div(class = "stat-label", "Completeness")),
          div(class = "stat-card",
              div(class = "stat-number",
                  style = if (total_outliers > 0) "color:#6B3FA0;" else "",
                  format(total_outliers, big.mark = ",")),
              div(class = "stat-label", "Outliers (IQR)")),
          div(class = "stat-card",
              div(class = if (dq$n_dup_rows > 0) "stat-number-warn" else "stat-number",
                  format(dq$n_dup_rows, big.mark = ",")),
              div(class = "stat-label", "Duplicate rows"))),

      div(class = "section-tag", "Column types"),
      div(style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:24px;",
          lapply(names(type_counts), function(t) {
            cls <- switch(t, "numeric" = "type-pill type-numeric",
                          "character" = "type-pill type-character",
                          "logical"   = "type-pill type-logical",
                          "date/time" = "type-pill type-datetime", "type-pill type-character")
            div(class = cls, paste0(t, " \u00d7 ", type_counts[[t]]))
          })),

      { nid <- dq$near_id_cols
        if (length(nid) > 0)
          div(class = "alert-info",
              div(class = "alert-info-title", "\U0001f511  Possible identifier columns (>95% unique values)"),
              tags$ul(class = "alert-list", lapply(nid, function(col) {
                nu <- length(unique(na.omit(df[[col]])))
                tags$li(paste0(col, " \u2014 ", format(nu, big.mark = ","), " unique values (",
                               round(100 * nu / nrow(df), 1), "% of rows)"))
              })))
      },
      { hm <- cs[cs$Pct_Missing > 20, , drop = FALSE]
        if (nrow(hm) > 0)
          div(class = "alert-missing",
              div(class = "alert-missing-title", "\u26a0  Columns with >20% missing values"),
              tags$ul(class = "alert-list",
                      lapply(seq_len(nrow(hm)), function(i)
                        tags$li(paste0(hm$Column[i], " \u2014 ", hm$Pct_Missing[i], "% missing")))))
      },
      { oc <- cs[!is.na(cs$N_Outliers) & cs$N_Outliers > 0, , drop = FALSE]
        if (nrow(oc) > 0)
          div(class = "alert-outlier",
              div(class = "alert-outlier-title", "\u25ca  Columns with outliers (Tukey IQR fences)"),
              tags$ul(class = "alert-list",
                      lapply(seq_len(nrow(oc)), function(i)
                        tags$li(paste0(oc$Column[i], " \u2014 ", oc$N_Outliers[i],
                                       " outlier", if (oc$N_Outliers[i] != 1) "s" else "")))))
      }
    )
  })

  # ── Column Summary ─────────────────────────────────────────────────────────
  output$summary_table <- renderDT({
    cs  <- col_summary(); req(cs); lm <- label_map()
    # Add label column if labels are loaded
    if (length(lm) > 0) {
      cs$Label <- vapply(cs$Column, col_label, character(1), lmap = lm)
      cs <- cs[, c("Column","Label", setdiff(names(cs), c("Column","Label"))), drop = FALSE]
    }
    cs$Type <- paste0('<span class="type-pill type-', gsub("/","",cs$Type), '">', cs$Type, '</span>')
    miss_col <- if ("Label" %in% names(cs)) "Pct_Missing" else "Pct_Missing"
    cs[[miss_col]] <- paste0(
      '<div style="background:#EEF2F6;border-radius:4px;height:14px;width:100%;">',
      '<div style="background:', ifelse(cs$N_Missing > 0, '#D4721A', '#007A6E'), ';',
      'width:', pmin(cs$Pct_Missing, 100), '%;height:100%;border-radius:4px;"></div></div>',
      '<span style="font-size:0.92rem;color:#4A6070;">', cs$Pct_Missing, '%</span>')
    cs$N_Outliers <- ifelse(is.na(cs$N_Outliers), '<span style="color:#4A6070">\u2014</span>',
      ifelse(cs$N_Outliers > 0,
             paste0('<span style="color:#6B3FA0;font-weight:700;">', cs$N_Outliers, '</span>'),
             '<span style="color:#007A6E">0</span>'))
    datatable(cs, escape = FALSE, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE, dom = 'ftip',
                             columnDefs = list(list(className = 'dt-left', targets = '_all')))) %>%
      formatStyle(columns = seq_along(cs), backgroundColor = '#FFFFFF', color = '#1A2A3A')
  })

  # ── Missingness ────────────────────────────────────────────────────────────
  output$miss_plot <- renderPlot({
    df <- loaded(); req(df)
    withProgress(message = "Rendering heatmap...",
                 { plot_missingness(df, lmap = label_map()) })
  }, bg = "#FFFFFF")

  observeEvent(input$dl_miss, {
    df <- loaded(); req(df); lm <- label_map()
    h  <- max(6, ncol(df) * 0.32 + 2)
    p  <- plot_missingness(df, lmap = lm)
    if (is.null(p)) { showNotification("Nothing to save.", type = "warning"); return() }
    start_save(paste0("missingness_", format(Sys.Date(), "%Y%m%d"), ".png"),
               function(path) save_plot_png(p, path, width = 13, height = h))
  })

  # ── Correlations ───────────────────────────────────────────────────────────
  output$corr_ui <- renderUI({
    df <- loaded(); req(df); nc <- numeric_cols()
    if (length(nc) < 2)
      return(div(class = "corr-empty",
                 "\U0001f4ca Correlation matrix requires at least 2 numeric columns.", br(), br(),
                 span(style = "color:#4A6070;font-size:0.9rem;",
                      paste0("This dataset has ", length(nc), " numeric column",
                             if (length(nc) != 1) "s" else "", "."))))
    tagList(
      plot_dl_btn("dl_corr"),
      plotOutput("corr_plot", height = paste0(max(420, length(nc) * 38 + 120), "px"))
    )
  })
  output$corr_plot <- renderPlot({
    df <- loaded(); req(df)
    withProgress(message = "Computing correlations...",
                 { plot_correlation(df, lmap = label_map()) })
  }, bg = "#FFFFFF")
  observeEvent(input$dl_corr, {
    df <- loaded(); req(df); nc <- numeric_cols(); lm <- label_map()
    sz <- max(7, length(nc) * 0.55 + 2)
    p  <- plot_correlation(df, lmap = lm)
    if (is.null(p)) { showNotification("Nothing to save.", type = "warning"); return() }
    start_save(paste0("correlations_", format(Sys.Date(), "%Y%m%d"), ".png"),
               function(path) save_plot_png(p, path, width = sz + 2, height = sz))
  })

  # ── Outliers ───────────────────────────────────────────────────────────────
  output$outlier_ui <- renderUI({
    df <- loaded(); req(df); oc <- outlier_cols(); nc <- numeric_cols()
    if (length(nc) == 0)
      return(div(class = "corr-empty", "\u25ca No numeric columns found."))
    if (length(oc) == 0)
      return(div(class = "corr-empty", "\u2714 No outliers detected in any numeric column.", br(), br(),
                 span(style = "color:#4A6070;font-size:0.9rem;",
                      "All values fall within Tukey IQR fences.")))
    tagList(fluidRow(
      column(4,
        div(class = "section-tag", "Select column"),
        selectInput("outlier_col", label = NULL,
                    choices  = labelled_choices(oc),
                    selected = effective_outlier_col()),
        div(class = "alert-outlier", style = "margin-top:12px;",
            div(class = "alert-outlier-title", style = "font-size:0.78rem;", "\u25ca Flagged columns"),
            tags$ul(class = "alert-list", lapply(oc, function(col) {
              n <- col_summary()$N_Outliers[col_summary()$Column == col]
              tags$li(paste0(col_label(col, label_map()), " \u2014 ",
                             n, " outlier", if (n != 1) "s" else ""))
            })))),
      column(8,
        plot_dl_btn("dl_outlier"),
        plotOutput("outlier_plot", height = "480px"))
    ))
  })
  output$outlier_plot <- renderPlot({
    df <- loaded(); col <- effective_outlier_col()
    req(df, col, col %in% names(df))
    withProgress(message = "Rendering...", plot_outlier_detail(df[[col]], col, lmap = label_map()))
  }, bg = "#FFFFFF")
  observeEvent(input$dl_outlier, {
    df <- loaded(); col <- effective_outlier_col(); req(df, col, col %in% names(df))
    lm <- label_map()
    p  <- plot_outlier_detail(df[[col]], col, lmap = lm)
    if (is.null(p)) { showNotification("Nothing to save.", type = "warning"); return() }
    start_save(paste0("outlier_", col, "_", format(Sys.Date(), "%Y%m%d"), ".png"),
               function(path) save_plot_png(p, path))
  })

  # ── Distributions ──────────────────────────────────────────────────────────
  output$col_picker <- renderUI({
    df <- loaded(); req(df); nc <- numeric_cols(); cc <- cat_cols()
    selectInput("dist_col", label = NULL,
                choices  = list(Numeric     = labelled_choices(nc),
                                Categorical = labelled_choices(cc)),
                selected = if (length(nc) > 0) nc[1] else names(df)[1])
  })
  output$dist_plot <- renderPlot({
    df <- loaded(); col <- effective_dist_col()
    req(df, col, col %in% names(df))
    if (is.numeric(df[[col]])) plot_distribution(df, col, lmap = label_map())
    else                        plot_categories(df, col, lmap = label_map())
  }, bg = "#FFFFFF")
  observeEvent(input$dl_dist, {
    df <- loaded(); col <- effective_dist_col(); req(df, col, col %in% names(df))
    lm <- label_map()
    p  <- if (is.numeric(df[[col]])) plot_distribution(df, col, lmap = lm)
           else                       plot_categories(df, col, lmap = lm)
    if (is.null(p)) { showNotification("Nothing to save.", type = "warning"); return() }
    start_save(paste0("dist_", col, "_", format(Sys.Date(), "%Y%m%d"), ".png"),
               function(path) save_plot_png(p, path))
  })

  # ── Group Analysis ─────────────────────────────────────────────────────────
  output$group_ui <- renderUI({
    df <- loaded(); req(df); nc <- numeric_cols(); cc <- cat_cols()
    if (length(nc) == 0)
      return(div(class = "corr-empty", "\u25ca Group analysis requires at least one numeric column."))
    if (length(cc) == 0)
      return(div(class = "corr-empty", "\u25ca Group analysis requires at least one categorical column."))
    tagList(fluidRow(
      column(3,
        div(class = "control-panel",
            div(class = "control-label", "Numeric variable"),
            selectInput("grp_num_col", label = NULL, choices = labelled_choices(nc), selected = nc[1])),
        div(class = "control-panel",
            div(class = "control-label", "Group by"),
            selectInput("grp_cat_col", label = NULL, choices = labelled_choices(cc), selected = cc[1])),
        div(class = "alert-info",
            div(class = "alert-info-title", style = "font-size:0.82rem;", "About this chart"),
            p(style = "font-size:0.88rem;color:#4A6070;margin:0;",
              "Boxes show median and IQR. Points are individual observations.
               Groups ordered by median. Kruskal-Wallis tests whether distributions
               differ significantly across groups."))
      ),
      column(9,
        plot_dl_btn("dl_group"),
        plotOutput("group_plot", height = "500px"))
    ))
  })
  output$group_plot <- renderPlot({
    df <- loaded(); req(df)
    nc <- input$grp_num_col; gc <- input$grp_cat_col
    req(nc, gc, nc %in% names(df), gc %in% names(df))
    withProgress(message = "Rendering...", plot_group_box(df, nc, gc, lmap = label_map()))
  }, bg = "#FFFFFF")
  observeEvent(input$dl_group, {
    df <- loaded(); req(df)
    nc <- input$grp_num_col; gc <- input$grp_cat_col; req(nc, gc); lm <- label_map()
    p  <- plot_group_box(df, nc, gc, lmap = lm)
    if (is.null(p)) { showNotification("Nothing to save.", type = "warning"); return() }
    start_save(paste0("group_", format(Sys.Date(), "%Y%m%d"), ".png"),
               function(path) save_plot_png(p, path, width = 12, height = 7))
  })

  # ── Timeline ───────────────────────────────────────────────────────────────
  output$timeline_ui <- renderUI({
    df <- loaded(); req(df); dc <- date_cols(); all_cols <- names(df)
    if (length(dc) == 0)
      return(div(class = "corr-empty",
                 "\U0001f4c5 No date columns detected.", br(), br(),
                 span(style = "color:#4A6070;font-size:0.9rem;",
                      "Supported formats: YYYY-MM-DD, DD/MM/YYYY, MM/DD/YYYY, YYYYMMDD, and variants.")))
    non_date <- if (length(setdiff(all_cols, dc)) > 0) setdiff(all_cols, dc) else all_cols
    period_choices <- list("Week","Month","Quarter","Year")
    tagList(
      fluidRow(
        column(3,
          div(class = "control-panel",
              div(class = "control-label", "Date column"),
              selectInput("tl_date_col", label = NULL, choices = labelled_choices(dc), selected = dc[1])),
          div(class = "control-panel",
              div(class = "control-label", "Period"),
              selectInput("tl_period", label = NULL, choices = period_choices, selected = "Month")),
          div(class = "alert-info",
              div(class = "alert-info-title", style = "font-size:0.82rem;", "Record density"),
              p(style = "font-size:0.88rem;color:#4A6070;margin:0;",
                "Bar height = number of records with dates in that period.
                 Gaps flag periods with no data."))
        ),
        column(9,
          plot_dl_btn("dl_tl_density"),
          plotOutput("tl_density_plot", height = "340px"))
      ),
      tags$hr(style = "margin:24px 0;border-color:#CDD5DF;"),
      fluidRow(
        column(3,
          div(class = "control-panel",
              div(class = "control-label", "Track completeness of"),
              selectInput("tl_track_col", label = NULL, choices = labelled_choices(non_date),
                          selected = non_date[1])),
          div(class = "alert-info",
              div(class = "alert-info-title", style = "font-size:0.82rem;", "Completeness trend"),
              p(style = "font-size:0.88rem;color:#4A6070;margin:0;",
                "% non-missing values aggregated by period.
                 Point size reflects record count.
                 The 80% line flags periods of poor coverage."))
        ),
        column(9,
          plot_dl_btn("dl_tl_complete"),
          plotOutput("tl_completeness_plot", height = "340px"))
      )
    )
  })
  output$tl_density_plot <- renderPlot({
    df <- loaded(); req(df)
    dc <- input$tl_date_col; per <- input$tl_period; req(dc, per, dc %in% names(df))
    withProgress(message = "Rendering timeline...",
                 plot_timeline_density(df, dc, per, lmap = label_map()))
  }, bg = "#FFFFFF")
  output$tl_completeness_plot <- renderPlot({
    df <- loaded(); req(df)
    dc <- input$tl_date_col; tc <- input$tl_track_col; per <- input$tl_period
    req(dc, tc, per, dc %in% names(df), tc %in% names(df))
    withProgress(message = "Rendering completeness trend...",
                 plot_timeline_completeness(df, dc, tc, per, lmap = label_map()))
  }, bg = "#FFFFFF")
  observeEvent(input$dl_tl_density, {
    df <- loaded(); req(df)
    dc <- input$tl_date_col; per <- input$tl_period; req(dc, per); lm <- label_map()
    p  <- plot_timeline_density(df, dc, per, lmap = lm)
    if (is.null(p)) { showNotification("Nothing to save.", type = "warning"); return() }
    start_save(paste0("timeline_density_", format(Sys.Date(), "%Y%m%d"), ".png"),
               function(path) save_plot_png(p, path, width = 12, height = 6))
  })
  observeEvent(input$dl_tl_complete, {
    df <- loaded(); req(df)
    dc <- input$tl_date_col; tc <- input$tl_track_col; per <- input$tl_period
    req(dc, tc, per); lm <- label_map()
    p  <- plot_timeline_completeness(df, dc, tc, per, lmap = lm)
    if (is.null(p)) { showNotification("Nothing to save.", type = "warning"); return() }
    start_save(paste0("timeline_completeness_", format(Sys.Date(), "%Y%m%d"), ".png"),
               function(path) save_plot_png(p, path, width = 12, height = 6))
  })

  # ── Compare tab ────────────────────────────────────────────────────────────
  output$compare_ui <- renderUI({
    df_a <- loaded(); df_b <- loaded_b()

    if (is.null(df_a))
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001f4c4"),
        h4("No Dataset A loaded"),
        p(style = "color:#4A6070;", "Load a primary dataset using Browse Workspace Files.")))

    if (is.null(df_b))
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001f4ca"),
        h4("Load Dataset B to compare"),
        p(style = "color:#4A6070; max-width:420px; margin:0 auto;",
          "Use the ", strong("Compare Dataset (B)"), " section in the sidebar to load a
           second CSV. Columns with matching names will be overlaid and compared.")))

    common  <- intersect(names(df_a), names(df_b))
    only_a  <- setdiff(names(df_a), names(df_b))
    only_b  <- setdiff(names(df_b), names(df_a))
    nc_com  <- common[sapply(common, function(cn) is.numeric(df_a[[cn]]) || is.numeric(df_b[[cn]]))]
    cat_com <- common[sapply(common, function(cn)
      (is.character(df_a[[cn]]) || is.factor(df_a[[cn]])) &&
      (is.character(df_b[[cn]]) || is.factor(df_b[[cn]])))]
    sel_com <- if (length(nc_com) > 0) nc_com else if (length(cat_com) > 0) cat_com else common

    lab_a <- if (!is.null(loaded_path())) basename(loaded_path()) else "Dataset A"
    lab_b <- if (!is.null(loaded_path_b())) basename(loaded_path_b()) else "Dataset B"

    tagList(

      # ── Match summary cards ──────────────────────────────────────────────────
      div(class = "section-tag", "Column match summary"),
      div(class = "stat-row",
          div(class = "stat-card",
              div(class = "stat-number", nrow(df_a)),
              div(class = "stat-label",  paste0("Rows - ", lab_a))),
          div(class = "stat-card",
              div(class = "stat-number", nrow(df_b)),
              div(class = "stat-label",  paste0("Rows - ", lab_b))),
          div(class = "stat-card",
              div(class = "stat-number", length(common)),
              div(class = "stat-label",  "Matched columns")),
          div(class = "stat-card",
              div(class = "stat-number", length(only_a)),
              div(class = "stat-label",  paste0("Only in A"))),
          div(class = "stat-card",
              div(class = "stat-number", length(only_b)),
              div(class = "stat-label",  paste0("Only in B")))),

      if (length(only_a) > 0)
        div(class = "alert-missing",
            div(class = "alert-missing-title", "\u26a0  Columns only in Dataset A"),
            p(style = "font-size:0.88rem; margin:0;", paste(only_a, collapse = ", "))),
      if (length(only_b) > 0)
        div(class = "alert-outlier",
            div(class = "alert-outlier-title", "\u26a0  Columns only in Dataset B"),
            p(style = "font-size:0.88rem; margin:0;", paste(only_b, collapse = ", "))),

      tags$hr(style = "margin:24px 0; border-color:#CDD5DF;"),

      # ── Distribution overlay ─────────────────────────────────────────────────
      fluidRow(
        column(3,
          div(class = "control-panel",
              div(class = "control-label", "Column to compare"),
              selectInput("cmp_col", label = NULL,
                          choices  = as.list(stats::setNames(sel_com,
                            vapply(sel_com, function(n) col_label_with_name(n, label_map()), character(1)))),
                          selected = sel_com[1])),
          div(class = "alert-info",
              div(class = "alert-info-title", style = "font-size:0.82rem;", "Overlay chart"),
              p(style = "font-size:0.88rem; color:#4A6070; margin:0;",
                "Numeric columns: overlapping density curves with median lines.
                 Categorical columns: proportional grouped bar chart.
                 Dashed lines mark medians for each dataset."))
        ),
        column(9,
          plot_dl_btn("dl_cmp_overlay"),
          plotOutput("cmp_overlay_plot", height = "420px"))
      ),

      tags$hr(style = "margin:24px 0; border-color:#CDD5DF;"),

      # ── Statistics comparison table ──────────────────────────────────────────
      div(class = "section-tag", "Side-by-side statistics for matched columns"),
      DTOutput("cmp_stats_tbl")
    )
  })

  output$cmp_overlay_plot <- renderPlot({
    df_a <- loaded(); df_b <- loaded_b()
    req(df_a, df_b)
    col  <- input$cmp_col; req(col, col %in% names(df_a), col %in% names(df_b))
    lab_a <- if (!is.null(loaded_path())) basename(loaded_path()) else "Dataset A"
    lab_b <- if (!is.null(loaded_path_b())) basename(loaded_path_b()) else "Dataset B"
    withProgress(message = "Rendering comparison...",
      if (is.numeric(df_a[[col]]) || is.numeric(df_b[[col]]))
        plot_density_overlay(df_a, df_b, col, lab_a, lab_b, lmap = label_map())
      else
        plot_category_compare(df_a, df_b, col, lab_a, lab_b, lmap = label_map())
    )
  }, bg = "#FFFFFF")

  observeEvent(input$dl_cmp_overlay, {
    df_a <- loaded(); df_b <- loaded_b(); req(df_a, df_b)
    col  <- input$cmp_col; req(col, col %in% names(df_a), col %in% names(df_b))
    lm   <- label_map()
    lab_a <- if (!is.null(loaded_path()))   basename(loaded_path())   else "Dataset A"
    lab_b <- if (!is.null(loaded_path_b())) basename(loaded_path_b()) else "Dataset B"
    p <- if (is.numeric(df_a[[col]]) || is.numeric(df_b[[col]]))
           plot_density_overlay(df_a, df_b, col, lab_a, lab_b, lmap = lm)
         else
           plot_category_compare(df_a, df_b, col, lab_a, lab_b, lmap = lm)
    if (is.null(p)) { showNotification("Nothing to save.", type = "warning"); return() }
    start_save(paste0("compare_", format(Sys.Date(), "%Y%m%d"), ".png"),
               function(path) save_plot_png(p, path))
  })

  output$cmp_stats_tbl <- renderDT({
    df_a <- loaded(); df_b <- loaded_b(); req(df_a, df_b)
    lab_a <- if (!is.null(loaded_path())) basename(loaded_path()) else "A"
    lab_b <- if (!is.null(loaded_path_b())) basename(loaded_path_b()) else "B"
    tbl   <- compare_stats_tbl(df_a, df_b, lab_a, lab_b)
    req(!is.null(tbl))
    datatable(tbl, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE, dom = "ftip"),
              colnames = names(tbl)) %>%
      formatStyle(columns = seq_along(tbl), backgroundColor = "#FFFFFF", color = "#1A2A3A") %>%
      formatStyle("Type", fontWeight = "bold")
  })

  # ══ RELATIONAL (MULTI-TABLE) - PHASE 1 ══════════════════════════════════════
  tables_rv     <- reactiveVal(NULL)              # named list of data frames
  tables_dir_rv <- reactiveVal(NULL)              # folder the tables came from
  folder_dir    <- reactiveVal(WORKSPACE_FILES)   # current dir in the folder picker
  active_tbl_rv <- reactiveVal(NULL)              # name of the active (single-file) table

  observeEvent(input$open_folder,   { folder_dir(WORKSPACE_FILES); open_folder_modal(WORKSPACE_FILES) })
  observeEvent(input$folder_nav_to, { folder_dir(input$folder_nav_to); open_folder_modal(input$folder_nav_to) })

  # Copy a chosen table from the relational set into the single-file engine, so
  # every existing tab (Overview, Distributions, ...) profiles it unchanged.
  set_active_table <- function(nm) {
    res <- tables_rv(); if (is.null(res) || is.null(res[[nm]])) return()
    loaded(res[[nm]]); load_error(NULL)
    p <- file.path(tables_dir_rv() %||% WORKSPACE_FILES, paste0(nm, ".csv"))
    loaded_path(p); selected_file(p); active_tbl_rv(nm)
  }

  observeEvent(input$load_folder_confirm, {
    dir <- folder_dir(); removeModal()
    files <- list.files(dir, pattern = FILE_PATTERN, ignore.case = TRUE, full.names = TRUE)
    if (length(files) == 0) {
      showNotification("No CSV files in that folder.", type = "warning", duration = 5); return()
    }
    res <- list(); failed <- character(0)
    withProgress(message = "Loading tables...", value = 0, {
      for (i in seq_along(files)) {
        nm <- tools::file_path_sans_ext(basename(files[i]))
        df <- tryCatch(readr::read_csv(files[i], show_col_types = FALSE), error = function(e) NULL)
        if (is.null(df)) failed <- c(failed, basename(files[i])) else res[[nm]] <- df
        incProgress(1 / length(files), detail = basename(files[i]))
      }
    })
    if (length(res) == 0) {
      showNotification("None of the CSV files could be read.", type = "error", duration = 6); return()
    }
    tables_rv(res); tables_dir_rv(dir)
    if (length(failed) > 0)
      showNotification(paste0("Loaded ", length(res), " of ", length(files),
                              " files. Skipped: ", paste(failed, collapse = ", ")),
                       type = "warning", duration = 8)
    else
      showNotification(paste0("Loaded ", length(res), " table",
                              if (length(res) != 1) "s" else "", "."),
                       type = "message", duration = 4)
    # Make the largest table active so the single-file tabs populate immediately.
    active <- names(res)[which.max(vapply(res, nrow, integer(1)))]
    set_active_table(active)
    updateSelectInput(session, "active_table", choices = names(res), selected = active)
  })

  observeEvent(input$active_table, {
    req(input$active_table); set_active_table(input$active_table)
  }, ignoreInit = TRUE)

  observeEvent(input$clear_tables, { tables_rv(NULL); tables_dir_rv(NULL); active_tbl_rv(NULL) })

  output$tables_status_ui <- renderUI({
    res <- tables_rv()
    if (is.null(res))
      return(div(class = "label-badge-none", style = "margin-top:8px;", "No folder loaded"))
    tot <- sum(vapply(res, nrow, integer(1)))
    div(class = "fb-selected-badge",
        div(class = "fb-selected-label", "RELATIONAL SET"),
        div(class = "fb-selected-name", paste0(length(res), " tables")),
        div(class = "fb-selected-meta", format(tot, big.mark = ","), " rows total \u00b7 ",
            basename(tables_dir_rv() %||% "")))
  })

  output$active_table_ui <- renderUI({
    res <- tables_rv(); req(res)
    tags$div(style = "margin-top:8px;",
      div(class = "sidebar-section-label", style = "margin-top:4px;", "Active table (single-file tabs)"),
      selectInput("active_table", label = NULL, choices = names(res),
                  selected = active_tbl_rv() %||% names(res)[1]))
  })

  output$clear_tables_ui <- renderUI({
    req(tables_rv())
    tags$div(style = "margin-top:6px;",
      actionButton("clear_tables", label = tagList(icon("xmark"), " Clear Relational Set"),
                   class = "btn-open-file",
                   style = "font-size:0.82rem !important; padding:6px 10px !important;"))
  })

  # ── Shared-column / link detection reactives ───────────────────────────────
  shared_cols <- reactive({ res <- tables_rv(); req(res); detect_shared_columns(res) })

  link_norm <- reactive({
    sel <- input$link_col_sel
    if (!is.null(sel) && nzchar(sel)) return(sel)
    sc <- shared_cols(); if (is.null(sc) || nrow(sc) == 0) return(NULL)
    sc$norm_name[1]
  })

  classification <- reactive({
    res <- tables_rv(); req(res); ln <- link_norm(); req(ln); classify_link(res, ln)
  })

  child_report <- reactive({
    res <- tables_rv(); cls <- classification(); req(res, cls)
    if (length(cls$children) == 0) return(NULL)
    linkage_child_report(res, cls)
  })

  drift_report <- reactive({
    res <- tables_rv(); cls <- classification(); req(res, cls); linkage_format_drift(res, cls)
  })

  # ── Relationships tab ──────────────────────────────────────────────────────
  output$relationships_ui <- renderUI({
    res <- tables_rv()
    if (is.null(res))
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001f5c2\ufe0f"),
        h4("No relational dataset loaded"),
        p(style = "color:#4A6070; max-width:460px; margin:0 auto;",
          "Use ", strong("Load Folder of CSVs"), " in the sidebar to load a set of linked tables.
           The app detects the common identifier, works out how the files join, and draws the
           relationships.")))
    if (length(res) < 2)
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001f4c4"), h4("Only one table loaded"),
        p(style = "color:#4A6070;",
          "Relationship mapping needs at least two CSV files in the folder.")))
    sc <- shared_cols()
    if (is.null(sc) || nrow(sc) == 0)
      return(div(class = "alert-missing",
        div(class = "alert-missing-title", "\u26a0  No shared columns found"),
        p(style = "margin:0;",
          "None of the loaded tables share a column name, so they cannot be linked through a
           common identifier. Check that the ID column is named consistently across files.")))

    link_choices <- stats::setNames(sc$norm_name,
      sprintf("%s  (in %d tables: %s)", sc$column, sc$n_tables, sc$tables))
    tagList(
      div(class = "section-tag", "Common identifier"),
      fluidRow(
        column(6,
          div(class = "control-panel",
              div(class = "control-label", "Link tables on"),
              selectInput("link_col_sel", label = NULL, choices = link_choices, selected = link_norm()),
              p(style = "font-size:0.82rem; color:#4A6070; margin:6px 0 0;",
                "Detected automatically. Change it if a different column is the shared subject ID."))),
        column(6, uiOutput("spine_note_ui"))
      ),
      tags$hr(style = "margin:18px 0; border-color:#CDD5DF;"),
      div(class = "section-tag", "Detected keys & link column per table"),
      DTOutput("keys_tbl"),
      tags$hr(style = "margin:18px 0; border-color:#CDD5DF;"),
      plot_dl_btn("dl_er"),
      plotOutput("er_plot", height = "480px")
    )
  })

  output$spine_note_ui <- renderUI({
    cls <- classification(); req(cls)
    clean <- cls$spine_is_clean
    div(class = if (clean) "alert-info" else "alert-missing",
        div(class = if (clean) "alert-info-title" else "alert-missing-title",
            if (clean) "\U0001f511  Subject spine" else "\u26a0  No clean spine"),
        if (clean)
          p(style = "font-size:0.9rem; margin:0;",
            tags$strong(cls$spine), " holds the identifier as a unique, complete key \u2014 it is the
             subject spine. ", tags$strong(length(cls$children)), " child table",
            if (length(cls$children) != 1) "s" else "", " reference it.")
        else
          p(style = "font-size:0.9rem; margin:0;",
            "No table holds the identifier as a unique, complete key, so there is no clean subject
             spine. ", tags$strong(cls$spine), " was used as the reference (most distinct IDs).
             Duplicate or missing IDs there will distort the checks \u2014 see the Linkage tab."))
  })

  output$keys_tbl <- renderDT({
    res <- tables_rv(); req(res); ln <- link_norm()
    rows <- do.call(rbind, lapply(names(res), function(tn) {
      df <- res[[tn]]; pk <- best_primary_key(df)
      lc <- if (!is.null(ln)) resolve_col(df, ln) else NA_character_
      has_link <- !is.na(lc)
      uniq_here <- if (has_link) { x <- df[[lc]]
        (sum(is.na(x)) == 0 && length(unique(x)) == nrow(df) && nrow(df) > 0) } else NA
      data.frame(
        Table = tn, Rows = format(nrow(df), big.mark = ","), Columns = ncol(df),
        Primary_key = pk %||% "(none unique)",
        Has_link_col = if (has_link) "yes" else "no",
        Link_is_unique = if (!has_link) "\u2014" else if (isTRUE(uniq_here)) "yes (spine)" else "no (child)",
        stringsAsFactors = FALSE)
    }))
    datatable(rows, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "tp")) %>%
      formatStyle(columns = seq_along(rows), backgroundColor = "#FFFFFF", color = "#1A2A3A") %>%
      formatStyle("Primary_key", fontWeight = "bold")
  })

  output$er_plot <- renderPlot({
    res <- tables_rv(); cls <- classification(); req(res, cls)
    plot_er_diagram(res, cls, child_report())
  }, bg = "#FFFFFF")

  observeEvent(input$dl_er, {
    res <- tables_rv(); cls <- classification(); req(res, cls)
    p <- plot_er_diagram(res, cls, child_report())
    start_save(paste0("relationships_", format(Sys.Date(), "%Y%m%d"), ".png"),
               function(f) save_plot_png(p, f, width = 11, height = 7))
  })

  # ── Linkage tab ────────────────────────────────────────────────────────────
  output$linkage_ui <- renderUI({
    res <- tables_rv()
    if (is.null(res))
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001f517"), h4("No relational dataset loaded"),
        p(style = "color:#4A6070; max-width:460px; margin:0 auto;",
          "Load a folder of linked CSVs in the sidebar to run linkage-integrity checks: key
           uniqueness, orphaned rows, subject coverage, cardinality, and key format drift.")))
    if (length(res) < 2)
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001f4c4"), h4("Only one table loaded"),
        p(style = "color:#4A6070;", "Linkage checks need at least two linked CSV files.")))
    cls <- classification()
    if (is.null(cls))
      return(div(class = "alert-missing",
        div(class = "alert-missing-title", "\u26a0  No common identifier"),
        p(style = "margin:0;", "Pick a shared link column in the Relationships tab first.")))

    cr <- child_report()
    spine_col <- cls$info[[cls$spine]]$col
    n_ref     <- cls$info[[cls$spine]]$n_unique
    dup_spine <- cls$info[[cls$spine]]$n - cls$info[[cls$spine]]$n_unique - cls$info[[cls$spine]]$n_na
    types     <- vapply(cls$holders, function(h) cls$info[[h]]$type, character(1))
    type_drift    <- length(unique(types)) > 1
    total_orphans <- if (!is.null(cr)) sum(cr$n_orphan) else 0
    any_fail      <- (!cls$spine_is_clean) || total_orphans > 0 || type_drift

    tagList(
      div(class = if (any_fail) "alert-missing" else "alert-info",
          div(class = if (any_fail) "alert-missing-title" else "alert-info-title",
              if (any_fail) "\u26a0  Linkage issues detected" else "\u2714  Linkage looks clean"),
          p(style = "margin:0; font-size:0.92rem;",
            if (any_fail)
              "One or more checks below would block a clean relational load. A database would reject
               these; review them before linking the tables for analysis."
            else
              "The identifier is a clean key on the spine, every child row resolves to a subject, and
               the key format is consistent across tables.")),

      div(class = "section-tag", style = "margin-top:18px;", "Linkage summary"),
      div(class = "stat-row",
          div(class = "stat-card",
              div(class = "stat-number", format(n_ref, big.mark = ",")),
              div(class = "stat-label", paste0("Subjects (", cls$spine, ")"))),
          div(class = "stat-card",
              div(class = if (!cls$spine_is_clean) "stat-number-warn" else "stat-number",
                  format(max(0, dup_spine), big.mark = ",")),
              div(class = "stat-label", "Duplicate spine IDs")),
          div(class = "stat-card",
              div(class = if (total_orphans > 0) "stat-number-warn" else "stat-number",
                  format(total_orphans, big.mark = ",")),
              div(class = "stat-label", "Orphaned child rows")),
          div(class = "stat-card",
              div(class = "stat-number", length(cls$children)),
              div(class = "stat-label", "Child tables"))),

      div(class = "section-tag", style = "margin-top:8px;", "1 \u00b7 Key uniqueness & completeness"),
      if (!cls$spine_is_clean)
        div(class = "alert-outlier",
            div(class = "alert-outlier-title", "\u25ca  A PRIMARY KEY constraint would fail"),
            p(style = "margin:0; font-size:0.9rem;",
              "On ", tags$strong(paste0(cls$spine, ".", spine_col)), ": ",
              format(max(0, dup_spine), big.mark = ","), " duplicate and ",
              format(cls$info[[cls$spine]]$n_na, big.mark = ","),
              " missing identifier value(s). The spine cannot uniquely identify subjects until these
               are resolved.")),
      DTOutput("linkage_key_tbl"),

      if (!is.null(cr)) tagList(
        div(class = "section-tag", style = "margin-top:18px;", "2 \u00b7 Referential integrity (orphans)"),
        if (total_orphans > 0)
          div(class = "alert-outlier",
              div(class = "alert-outlier-title", "\u25ca  A FOREIGN KEY constraint would reject rows"),
              p(style = "margin:0; font-size:0.9rem;",
                format(total_orphans, big.mark = ","), " child row(s) reference an identifier that does
                 not exist on the spine. A database foreign key would refuse these inserts."))
        else
          div(class = "alert-info",
              div(class = "alert-info-title", "\u2714  No orphaned rows"),
              p(style = "margin:0; font-size:0.9rem;",
                "Every child row resolves to a subject on the spine.")),
        DTOutput("linkage_orphan_tbl")
      ),

      if (!is.null(cr)) tagList(
        div(class = "section-tag", style = "margin-top:18px;",
            "3 \u00b7 Subject coverage (structural missingness)"),
        div(class = "alert-info",
            div(class = "alert-info-title", "\U0001f4a1  Records that are absent, not null"),
            p(style = "margin:0; font-size:0.9rem;",
              "A subject with no rows in a child table has no value that can be \u201cmissing\u201d \u2014
               this structural gap is invisible to single-file profiling. The percentages below show
               how much of the cohort each child table actually covers.")),
        DTOutput("linkage_coverage_tbl")
      ),

      if (!is.null(cr) && length(cls$children) > 0) tagList(
        div(class = "section-tag", style = "margin-top:18px;", "4 \u00b7 Cardinality (rows per subject)"),
        fluidRow(
          column(4,
            div(class = "control-panel",
                div(class = "control-label", "Child table"),
                selectInput("card_child", label = NULL, choices = cls$children, selected = cls$children[1])),
            div(class = "alert-info",
                div(class = "alert-info-title", style = "font-size:0.82rem;", "Why this matters"),
                p(style = "font-size:0.86rem; color:#4A6070; margin:0;",
                  "Joining the spine to a one-to-many child multiplies rows. Knowing the fan-out up
                   front prevents inflated subject counts after a join."))),
          column(8, plot_dl_btn("dl_card"), plotOutput("card_plot", height = "360px")))
      ),

      div(class = "section-tag", style = "margin-top:18px;", "5 \u00b7 Key format across tables"),
      if (type_drift)
        div(class = "alert-outlier",
            div(class = "alert-outlier-title", "\u25ca  Join would require a CAST"),
            p(style = "margin:0; font-size:0.9rem;",
              "The identifier is stored as different types across tables (",
              paste(unique(types), collapse = ", "),
              "). A database could not join these without an explicit cast, and a naive merge in R may
               match far fewer rows than expected."))
      else
        div(class = "alert-info",
            div(class = "alert-info-title", "\u2714  Consistent key format"),
            p(style = "margin:0; font-size:0.9rem;",
              "The identifier has a consistent type across all tables that hold it.")),
      DTOutput("linkage_drift_tbl"),

      tags$hr(style = "margin:22px 0; border-color:#CDD5DF;"),
      div(class = "alert-info",
          div(class = "alert-info-title", "Next step \u00b7 enforce these relationships"),
          p(style = "margin:0; font-size:0.92rem;",
            "Everything flagged here is something the workspace database would enforce automatically.
             Once the integrity issues are resolved, importing these tables into the database would keep
             the keys unique, reject orphaned rows on insert, and hold the relationships as the data
             grows \u2014 your workspace administrator can help set this up. For day-to-day exploration,
             though, the flat files are fully profiled here.")),
      tags$div(style = "margin-top:10px;",
        actionButton("save_linkage", label = tagList(icon("floppy-disk"), " Save Linkage Report (CSV)"),
                     class = "btn-export", style = "width:auto; display:inline-block;"))
    )
  })

  output$linkage_key_tbl <- renderDT({
    cls <- classification(); req(cls)
    rows <- do.call(rbind, lapply(cls$holders, function(h) data.frame(
      Table = h, Link_column = cls$info[[h]]$col, Type = cls$info[[h]]$type,
      Rows = format(cls$info[[h]]$n, big.mark = ","),
      Distinct_IDs = format(cls$info[[h]]$n_unique, big.mark = ","),
      Missing_IDs = format(cls$info[[h]]$n_na, big.mark = ","),
      Unique_and_complete = if (cls$info[[h]]$is_unique) "yes" else "no",
      Role = if (h == cls$spine) "spine" else "child", stringsAsFactors = FALSE)))
    datatable(rows, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "tp")) %>%
      formatStyle(columns = seq_along(rows), backgroundColor = "#FFFFFF", color = "#1A2A3A") %>%
      formatStyle("Unique_and_complete",
                  color = styleEqual(c("yes", "no"), c("#007A6E", "#B04000")), fontWeight = "bold") %>%
      formatStyle("Role", fontWeight = "bold")
  })

  output$linkage_orphan_tbl <- renderDT({
    res <- tables_rv(); cls <- classification(); cr <- child_report(); req(res, cls, cr)
    rows <- do.call(rbind, lapply(seq_len(nrow(cr)), function(i) {
      ch <- cr$child[i]
      data.frame(
        Child_table = ch, Link_column = cr$link_col[i],
        Rows = format(cr$n_rows[i], big.mark = ","),
        Missing_key = format(cr$n_missing_key[i], big.mark = ","),
        Orphan_rows = format(cr$n_orphan[i], big.mark = ","),
        Pct_orphan = paste0(cr$pct_orphan[i], "%"),
        Sample_orphan_ids = orphan_sample(res, cls, ch), stringsAsFactors = FALSE)
    }))
    datatable(rows, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "tp")) %>%
      formatStyle(columns = seq_along(rows), backgroundColor = "#FFFFFF", color = "#1A2A3A") %>%
      formatStyle("Orphan_rows", fontWeight = "bold")
  })

  output$linkage_coverage_tbl <- renderDT({
    cr <- child_report(); req(cr)
    rows <- data.frame(
      Child_table = cr$child, Subjects_covered = format(cr$subjects_covered, big.mark = ","),
      Pct_of_cohort = paste0(cr$pct_coverage, "%"),
      Subjects_with_no_rows = format(cr$n_childless, big.mark = ","), stringsAsFactors = FALSE)
    datatable(rows, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "tp")) %>%
      formatStyle(columns = seq_along(rows), backgroundColor = "#FFFFFF", color = "#1A2A3A") %>%
      formatStyle("Pct_of_cohort", fontWeight = "bold")
  })

  output$card_plot <- renderPlot({
    res <- tables_rv(); cls <- classification(); req(res, cls)
    ch <- input$card_child %||% (if (length(cls$children) > 0) cls$children[1] else NULL); req(ch)
    v <- cardinality_vector(res, cls, ch); req(length(v) > 0)
    plot_cardinality(v, ch, cls$info[[cls$spine]]$col)
  }, bg = "#FFFFFF")

  observeEvent(input$dl_card, {
    res <- tables_rv(); cls <- classification(); req(res, cls)
    ch <- input$card_child %||% cls$children[1]; req(ch)
    v <- cardinality_vector(res, cls, ch); req(length(v) > 0)
    p <- plot_cardinality(v, ch, cls$info[[cls$spine]]$col)
    start_save(paste0("cardinality_", ch, "_", format(Sys.Date(), "%Y%m%d"), ".png"),
               function(f) save_plot_png(p, f, width = 11, height = 6))
  })

  output$linkage_drift_tbl <- renderDT({
    drift <- drift_report(); req(drift)
    colnames(drift) <- c("Table", "Link_column", "R_type", "ID_width",
                         "Whitespace", "Leading_zeros", "Sample_values")
    datatable(drift, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "tp")) %>%
      formatStyle(columns = seq_along(drift), backgroundColor = "#FFFFFF", color = "#1A2A3A") %>%
      formatStyle("R_type", fontWeight = "bold")
  })

  observeEvent(input$save_linkage, {
    res <- tables_rv(); cls <- classification(); req(res, cls)
    cr <- child_report(); drift <- drift_report()
    start_save(paste0("linkage_report_", format(Sys.Date(), "%Y%m%d"), ".csv"),
               function(f) build_linkage_csv(res, cls, cr, drift, f))
  })

  # ── Validation tab ──────────────────────────────────────────────────────────
  output$validate_ui <- renderUI({
    df <- loaded()
    if (is.null(df))
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001f4cb"),
        h4("No dataset loaded"),
        p(style = "color:#4A6070;", "Load a dataset first using Browse Workspace Files.")))

    nc <- names(df)[sapply(df, is.numeric)]

    tagList(
      fluidRow(
        column(4,
          div(class = "section-tag", "Range rules"),
          div(class = "alert-info",
              div(class = "alert-info-title", style = "font-size:0.82rem;",
                  "How to define rules"),
              tags$div(style = "font-size:0.88rem; color:#4A6070;",
                tags$p("One rule per line:  ", tags$code("column,min,max")),
                tags$p("Leave min or max blank for one-sided bounds:"),
                tags$code("age,0,120"), br(),
                tags$code("bmi,10,"),   br(),
                tags$code("sbp,,300"),  br(),
                tags$p(style = "margin-top:8px;",
                  "Column names are case-sensitive and must match exactly.
                   Only numeric columns are evaluated. Lines starting with # are ignored."))),
          if (length(nc) > 0) {
            div(class = "alert-info", style = "margin-top:12px;",
                div(class = "alert-info-title", style = "font-size:0.82rem;", "Numeric columns"),
                p(style = "font-size:0.78rem; color:#4A6070; margin:0; word-break:break-all;",
                  paste(nc, collapse = ", ")))
          }
        ),
        column(8,
          div(class = "section-tag", "Enter rules"),
          tags$textarea(id = "range_rules_txt",
                        class = "form-control",
                        rows  = 10,
                        style = "font-family:monospace; font-size:0.9rem;
                                 background:#FFFFFF; color:#1A2A3A;
                                 border:1px solid #CDD5DF; border-radius:6px;
                                 padding:10px; width:100%; resize:vertical;",
                        placeholder = "# Example rules:\n# age,0,120\n# bmi,10,80\n# sbp,,300"),
          br(),
          actionButton("run_validation", label = tagList(icon("circle-check"), " Run Validation"),
                       class = "btn-open-file",
                       style = "background:rgba(0,122,110,0.12) !important;
                                border-color:rgba(0,122,110,0.5) !important;
                                color:#007A6E !important; margin-bottom:16px;"),
          uiOutput("validation_results_ui")
        )
      )
    )
  })

  output$validation_results_ui <- renderUI({
    input$run_validation
    isolate({
      df  <- loaded(); req(df)
      txt <- input$range_rules_txt
      if (is.null(txt) || !nzchar(trimws(txt)))
        return(div(class = "label-badge-none", "Enter rules above and click Run Validation."))
      rules <- parse_range_rules(txt)
      if (is.null(rules) || nrow(rules) == 0)
        return(div(class = "label-badge-none", "No valid rules found. Check format: column,min,max"))
      results <- apply_range_rules(df, rules)
      if (is.null(results) || nrow(results) == 0)
        return(div(class = "label-badge", icon("check"), " No violations detected."))

      total_v <- sum(results$N_Violations, na.rm = TRUE)
      tagList(
        if (total_v == 0)
          div(class = "label-badge",
              icon("check"), " All ", nrow(results), " rule",
              if (nrow(results) != 1) "s" else "", " passed - no violations found.")
        else
          div(class = "alert-missing",
              div(class = "alert-missing-title",
                  "\u26a0  ", total_v, " total violation",
                  if (total_v != 1) "s" else "",
                  " across ", sum(results$N_Violations > 0, na.rm = TRUE), " rule",
                  if (sum(results$N_Violations > 0, na.rm = TRUE) != 1) "s" else "")),
        br(),
        DTOutput("validation_tbl")
      )
    })
  })

  output$validation_tbl <- renderDT({
    input$run_validation
    isolate({
      df  <- loaded(); req(df)
      txt <- input$range_rules_txt; req(txt, nzchar(trimws(txt)))
      rules   <- parse_range_rules(txt); req(!is.null(rules))
      results <- apply_range_rules(df, rules); req(!is.null(results))
      datatable(results, rownames = FALSE,
                options = list(pageLength = 20, scrollX = TRUE, dom = "ftip")) %>%
        formatStyle("N_Violations",
                    backgroundColor = styleInterval(0, c("#FFFFFF", "#FFF3E0")),
                    color           = styleInterval(0, c("#1A2A3A", "#C07000")),
                    fontWeight      = styleInterval(0, c("normal",  "bold"))) %>%
        formatStyle(columns = seq_len(ncol(results)),
                    backgroundColor = "#FFFFFF", color = "#1A2A3A")
    })
  })
  output$data_preview <- renderDT({
    df <- loaded(); req(df)
    datatable(df, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE, dom = 'lfrtip')) %>%
      formatStyle(columns = seq_along(df), backgroundColor = '#FFFFFF', color = '#1A2A3A')
  })
}


# ── 8. LAUNCH ─────────────────────────────────────────────────────────────────

message("[ cohort profiler ] launching...")
shinyApp(ui = ui, server = server)
