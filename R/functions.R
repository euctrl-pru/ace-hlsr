# ansp status latex table ----

latex_table_ansp <- function(df, wrap_raw_block = TRUE) {
  required_cols <- c("ANSP_NAME", "status", "country")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("latex_table_ansp(): missing columns: ", paste(missing_cols, collapse = ", "))
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
  
  n_cols <- 5L
  max_rows <- 8L
  max_items <- n_cols * max_rows
  
  if (nrow(df) > max_items) {
    stop(
      "latex_table_ansp(): df has ", nrow(df),
      " rows; max supported is ", max_items, " (", max_rows, "×", n_cols, ")."
    )
  }
  
  cells <- mapply(make_cell, df$ANSP_NAME, df$country, df$status, USE.NAMES = FALSE)
  if (length(cells) < max_items) cells <- c(cells, rep("", max_items - length(cells)))
  
  # Column-wise placement: item 1 -> (1,1), item 2 -> (2,1), ..., item 9 -> (1,2), ...
  grid <- matrix(cells, nrow = max_rows, ncol = n_cols, byrow = FALSE)
  
  row_to_latex <- function(row_cells, is_last_row) {
    line <- paste(row_cells, collapse = " & ")
    pad <- if (is_last_row) " \\\\[1.4em]" else " \\\\[0.75em]"
    paste0(line, pad)
  }
  
  body_lines <- vapply(
    seq_len(max_rows),
    function(i) row_to_latex(grid[i, ], is_last_row = (i == max_rows)),
    FUN.VALUE = character(1)
  )
  
  latex <- paste(
    "\\noindent",
    "{\\small % 1 point smaller than normalsize in standard classes",
    "",
    "% Tighten spacing so the whole table stays within \\linewidth",
    "\\setlength{\\tabcolsep}{2pt}",
    "",
    "% vertical padding inside rows (global)",
    "\\renewcommand{\\arraystretch}{1.4}",
    "\\setlength{\\extrarowheight}{0.6pt}",
    "",
    "\\begin{tabular}{@{}p{0.23\\linewidth}@{}p{0.16\\linewidth}@{}p{0.175\\linewidth}@{}p{0.255\\linewidth}@{}p{0.180\\linewidth}@{}}",
    "\\hline",
    paste(body_lines, collapse = "\n\n"),
    "\\hline",
    "\\end{tabular}",
    "}",
    "",
    "\\vspace{-0.2em}",
    "\\noindent{\\reviewed\\ \\ Data submission has been reviewed}",
    sep = "\n"
  )
  
  if (!wrap_raw_block) return(latex)
  paste0("```{=latex}\n", latex, "\n```")
}
