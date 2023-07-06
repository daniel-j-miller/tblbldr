


#this function will read 'csv string' format tables from tablebuilder and Health Workforce data tool
#detects and skips proceeding and trailing metadata
#
#
#path = location of file to read
#
#
#
#


#' read_tblbldr
#' Import CVS from TableBuilder
#' @param path path to the CSV to be imported
#'
#' @return a dataframe
#' @export
#' @importFrom data.table fread
#' @importFrom dplyr mutate
#' @importFrom dplyr where
#' @importFrom dplyr row_number
#' @importFrom readr read_delim
#' @importFrom readr read_csv
#' @importFrom stringr str_count
#' @examples read_tblbldr(path = "path\to\some\file.csv"
read_tblbldr <- function(path){

  df <- readr::read_delim(path,
                          skip_empty_rows = FALSE,
                          delim = "~",
                          show_col_types = FALSE, )

  df <- dplyr::mutate(df,
                      i = dplyr::row_number(),
                      count = stringr::str_count(dplyr::pull(df,1),","))


  uniqv <- unique(df$count[!is.na(df$count)])

  m <- uniqv[which.max(tabulate(match(df$count, uniqv)))]

  df <- df[df$count==m,]

  s_min <- min(df$i, na.rm = TRUE)

  s_max <- max(df$i, na.rm = TRUE) - s_min + 1

  h <- head(
    as.vector(
      unlist(
        data.table::fread(path, skip = s_min-1, nrows = 1))), -1)

  if (length(h) < m){
    h2 <- tail(
      head(
        as.vector(
          unlist(
            data.table::fread(path, skip = s_min-2, nrows = 1))), -1), -1)

    h <- c(h,h2)
  }

  r <- readr::read_csv(path,
                       skip = s_min,
                       col_names = h,
                       show_col_types = FALSE,
                       n_max = s_max, name_repair = "universal")

  r <- dplyr::select(r, dplyr::where(\(x) any(!is.na(x))))

  r
}





