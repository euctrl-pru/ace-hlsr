# ansp status latex table ----

# col_spec = "@{}p{0.23\\linewidth}@{}p{0.16\\linewidth}@{}p{0.175\\linewidth}@{}p{0.255\\linewidth}@{}p{0.180\\linewidth}@{}"

latex_table_ansp <- function(
    df,
    wrap_raw_block = TRUE,
    n_cols = 5L,
    max_rows = 8L,
    col_spec = "@{}p{0.23\\linewidth}@{}p{0.16\\linewidth}@{}p{0.175\\linewidth}@{}p{0.255\\linewidth}@{}p{0.180\\linewidth}@{}",
    tabcolsep_pt = 2,
    arraystretch = 1.4,
    extrarowheight_pt = 0.6,
    shade = TRUE,
    shade_start_row = 1L,         # 1 shades first data row; 2 leaves first row unshaded
    shade_color = "gray!10",
    vspace_after_table = "0.4em",
    legend_vspace = "0.4em",
    legend_font = "\\footnotesize",
    cell_top_ex = 2.6,            # top padding inside each cell (ex)
    cell_bottom_ex = 1.6          # bottom padding inside each cell (ex)
) {
  required_cols <- c("ANSP_NAME", "status", "country")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("latex_table_ansp(): missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  max_items <- as.integer(n_cols) * as.integer(max_rows)
  if (nrow(df) > max_items) {
    stop(
      "latex_table_ansp(): df has ", nrow(df),
      " rows; max supported is ", max_items, " (", max_rows, "×", n_cols, ")."
    )
  }
  
  latex_escape <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("([{}$&#_%])", "\\\\\\1", x, perl = TRUE)
    x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
    x
  }
  
  make_cell <- function(name, country, status) {
    name <- latex_escape(name)
    country <- latex_escape(country)
    status <- suppressWarnings(as.numeric(status))
    mark <- if (!is.na(status) && status == 1) " \\reviewed" else ""
    sprintf("\\cell{%s%s}{%s}", name, mark, country)
  }
  
  cells <- mapply(make_cell, df$ANSP_NAME, df$country, df$status, USE.NAMES = FALSE)
  if (length(cells) < max_items) {
    cells <- c(cells, rep("", max_items - length(cells)))
  }
  
  # Column-wise packing: item 1 -> (1,1), item 2 -> (2,1), ..., item (max_rows+1) -> (1,2), ...
  grid <- matrix(cells, nrow = max_rows, ncol = n_cols, byrow = FALSE)
  
  row_to_latex <- function(row_cells) {
    paste0(paste(row_cells, collapse = " & "), " \\\\")
  }
  
  body_lines <- vapply(seq_len(max_rows), function(i) row_to_latex(grid[i, ]), FUN.VALUE = character(1))
  
  # Row shading strings
  rowcolors_on <- character(0)
  rowcolors_off <- character(0)
  if (isTRUE(shade)) {
    shade_start_row <- as.integer(shade_start_row)
    if (!shade_start_row %in% c(1L, 2L)) {
      stop("latex_table_ansp(): shade_start_row must be 1 or 2.")
    }
    rowcolors_on <- sprintf("\\rowcolors{%d}{%s}{white}", shade_start_row, shade_color)
    rowcolors_off <- sprintf("\\rowcolors{%d}{}{}", shade_start_row)
  }
  
  # Critical: force real bottom depth inside each cell (so shading doesn't look top-heavy)
  # - first rule provides height (top)
  # - second rule provides depth via negative raise (bottom)
  cell_override <- sprintf(
    "\\renewcommand{\\cell}[2]{\\parbox[t]{\\linewidth}{\\rule{0pt}{%.2fex}#1\\\\{\\small (#2)}\\rule[-%.2fex]{0pt}{%.2fex}}}",
    cell_top_ex, cell_bottom_ex, cell_bottom_ex
  )
  
  latex <- paste(
    "\\noindent",
    "{\\small % 1 point smaller than normalsize in standard classes",
    "",
    "% Tighten spacing so the whole table stays within \\linewidth",
    sprintf("\\setlength{\\tabcolsep}{%spt}", tabcolsep_pt),
    "",
    "% Global row padding",
    sprintf("\\renewcommand{\\arraystretch}{%s}", arraystretch),
    sprintf("\\setlength{\\extrarowheight}{%spt}", extrarowheight_pt),
    "",
    "% Local override to ensure symmetric padding with row shading",
    "\\begingroup",
    cell_override,
    "",
    if (length(rowcolors_on)) rowcolors_on else NULL,
    "",
    sprintf("\\begin{tabular}{%s}", col_spec),
    "\\hline",
    paste(body_lines, collapse = "\n"),
    "\\hline",
    "\\end{tabular}",
    "",
    if (length(rowcolors_off)) rowcolors_off else NULL,
    "\\endgroup",
    "}",
    "",
    sprintf("\\vspace{%s}", vspace_after_table),
    sprintf(
      "\\begingroup\\raggedright%s\\noindent\\reviewed\\ Data submission has been reviewed\\par\\endgroup",
      legend_font
    ),
    sprintf("\\vspace{%s}", legend_vspace),
    sep = "\n"
  )
  
  if (!isTRUE(wrap_raw_block)) return(latex)
  paste0("```{=latex}\n", latex, "\n```")
}


# revenues latex table ----
# file: R/latex_table_revenues.R

latex_table_revenues <- function(
    df,
    wrap_raw_block = TRUE,
    float_placement = "H",
    tabular_width = "\\textwidth",
    stripe_starts_at_row_1 = TRUE
) {
  if (!is.data.frame(df) && !inherits(df, "tbl")) {
    stop("latex_table_revenues(): df must be a data.frame or tibble.")
  }
  if (ncol(df) != 5) stop("latex_table_revenues(): expected exactly 5 columns.")
  if (nrow(df) < 2) stop("latex_table_revenues(): expected at least 2 rows (body + total).")
  
  # escape for LaTeX table cells; keep € as-is (xelatex)
  latex_escape <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("\\\\", "\\\\textbackslash{}", x)                 # prevent accidental LaTeX commands
    x <- gsub("([{}$&#_%])", "\\\\\\1", x, perl = TRUE)         # includes %
    x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
    x
  }
  
  # helpers to emit the exact multicolumn style you used
  mc_header <- function(val, align) {
    sprintf(
      "\\multicolumn{1}{>{\\columncolor{HeaderBlue}\\color{white}\\bfseries}%s}{%s}",
      align, latex_escape(val)
    )
  }
  
  mc_cell <- function(val, color, align, is_x = FALSE) {
    val <- latex_escape(val)
    if (is_x) {
      return(sprintf(
        "\\multicolumn{1}{>{\\columncolor{%s}}>{\\centering\\arraybackslash}X}{%s}",
        color, val
      ))
    }
    sprintf(
      "\\multicolumn{1}{>{\\columncolor{%s}}%s}{%s}",
      color, align, val
    )
  }
  
  build_row <- function(row_vec, color) {
    parts <- c(
      mc_cell(row_vec[[1]], color, "r"),
      mc_cell(row_vec[[2]], color, "r"),
      mc_cell(row_vec[[3]], color, "c", is_x = TRUE),
      mc_cell(row_vec[[4]], color, "r"),
      mc_cell(row_vec[[5]], color, "r")
    )
    paste0(paste(parts, collapse = " & "), " \\\\")
  }
  
  # header from column names
  hdr <- names(df)
  if (length(hdr) != 5) stop("latex_table_revenues(): unexpected header length after names(df).")
  
  # last row is total
  total_row <- df[nrow(df), , drop = FALSE]
  body <- df[-nrow(df), , drop = FALSE]
  
  lines <- character(0)
  
  # header line (alignments: r r c r r)
  header_line <- paste(
    mc_header(hdr[[1]], "r"),
    mc_header(hdr[[2]], "r"),
    mc_header(hdr[[3]], "c"),
    mc_header(hdr[[4]], "r"),
    mc_header(hdr[[5]], "r"),
    sep = " & "
  )
  lines <- c(lines, paste0(header_line, " \\\\"), "")
  
  # zebra body
  if (nrow(body) > 0) {
    for (i in seq_len(nrow(body))) {
      use_stripe <- if (stripe_starts_at_row_1) (i %% 2 == 1) else (i %% 2 == 0)
      color <- if (use_stripe) "Stripe" else "white"
      row_vec <- as.list(body[i, , drop = TRUE])
      lines <- c(lines, build_row(row_vec, color), "")
    }
  }
  
  # total row: center text column intentionally empty (matches your target)
  tot <- as.list(total_row[1, , drop = TRUE])
  total_parts <- c(
    mc_cell(tot[[1]], "Total", "r"),
    mc_cell(tot[[2]], "Total", "r"),
    mc_cell("",       "Total", "c", is_x = TRUE),
    mc_cell(tot[[4]], "Total", "r"),
    mc_cell(tot[[5]], "Total", "r")
  )
  lines <- c(lines, paste0(paste(total_parts, collapse = " & "), " \\\\"))
  
  latex <- paste(
    sprintf("\\begin{table}[%s]", float_placement),
    "\\centering",
    sprintf("\\begin{tabularx}{%s}{r r >{\\centering\\arraybackslash}X r r}", tabular_width),
    "",
    paste(lines, collapse = "\n"),
    "",
    "\\end{tabularx}",
    "\\end{table}",
    sep = "\n"
  )
  
  if (!isTRUE(wrap_raw_block)) return(latex)
  paste0("```{=latex}\n", latex, "\n```")
}
