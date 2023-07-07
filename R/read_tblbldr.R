#' read_tblbldr
#' Import CSV from TableBuilder
#' @param path path to the CSV to be imported
#'
#' @return a dataframe
#' @export
#' @importFrom data.table tstrsplit
#' @importFrom data.table :=
#' @importFrom data.table setDT
#' @importFrom readr read_delim
#' @examples read_tblbldr(path = "path/to/some/file.csv")
read_tblbldr <- function(path){

  df <- readr::read_delim(path,
                          skip_empty_rows = FALSE,
                          delim = "~",
                          show_col_types = FALSE)

  na_pos <- which(is.na(df[,1]))
  na_diffs <- diff(na_pos)
  start <- na_pos[which(na_diffs == max(na_diffs))] + 1
  end <- na_pos[which(na_diffs == max(na_diffs)) + 1] - 1
  df <- df[start:end,]

  names(df) <- "x"
  df2 <- df[-1,]

  data.table::setDT(df2)[, gsub(pattern = " ", replacement = ".", strsplit(df$x, ",")[[1]]) := tstrsplit(df2$x, ",", type.convert = TRUE)]
  df2 <- df2[,-1]
  df2
}

