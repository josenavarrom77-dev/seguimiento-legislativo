`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

collapse_value <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
  paste(stats::na.omit(as.character(x)), collapse = "; ")
}

blank_default <- function(x, default = "") {
  value <- collapse_value(x)
  if (nzchar(trimws(value))) value else default
}

empty_to_na <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(trimws(as.character(x)))) NA_character_ else as.character(x)
}

safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- empty_to_na(x)
  if (is.na(x)) return(as.Date(NA_character_))
  parsed <- tryCatch(
    suppressWarnings(as.Date(x)),
    error = function(e) as.Date(NA_character_)
  )
  if (!is.na(parsed)) return(parsed)

  months_es <- c(
    enero = "01", febrero = "02", marzo = "03", abril = "04", mayo = "05", junio = "06",
    julio = "07", agosto = "08", septiembre = "09", setiembre = "09", octubre = "10",
    noviembre = "11", diciembre = "12"
  )
  normalized <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  for (month_name in names(months_es)) {
    normalized <- gsub(month_name, months_es[[month_name]], normalized, fixed = TRUE)
  }
  normalized <- gsub("[^0-9]+", "-", normalized)
  normalized <- gsub("(^-|-$)", "", normalized)
  parts <- strsplit(normalized, "-", fixed = TRUE)[[1]]
  if (length(parts) == 3 && nchar(parts[3]) == 4) {
    parsed <- tryCatch(
      suppressWarnings(as.Date(sprintf("%s-%02d-%02d", parts[3], as.integer(parts[2]), as.integer(parts[1])))),
      error = function(e) as.Date(NA_character_)
    )
  }
  parsed
}

format_form_date <- function(x, label = "La fecha", required = FALSE) {
  value <- trimws(collapse_value(x))
  if (!nzchar(value)) {
    if (isTRUE(required)) stop(label, " es obligatoria.")
    return("")
  }
  parsed <- safe_date(value)
  if (length(parsed) != 1L || is.na(parsed)) {
    stop(label, " debe escribirse como AAAA-MM-DD.")
  }
  format(parsed, "%Y-%m-%d")
}

truncate_document <- function(text, max_chars = 60000L) {
  text <- paste(text, collapse = "\n")
  if (nchar(text, type = "chars") <= max_chars) return(text)
  head_chars <- floor(max_chars * 0.72)
  tail_chars <- max_chars - head_chars
  paste0(
    substr(text, 1, head_chars),
    "\n\n[... PARTE CENTRAL OMITIDA PARA CONTROLAR EL TAMAÑO ...]\n\n",
    substr(text, nchar(text) - tail_chars + 1, nchar(text))
  )
}

slug_id <- function(number = "", chamber = "") {
  clean <- toupper(gsub("[^A-Za-z0-9]+", "-", paste(number, chamber)))
  clean <- gsub("(^-+|-+$)", "", clean)
  if (!nzchar(clean)) clean <- format(Sys.time(), "%Y%m%d-%H%M%S")
  paste0("PL-", clean)
}

normalize_multiline <- function(x) {
  x <- gsub("\r", "", x %||% "")
  x <- gsub("[ \t]+", " ", x)
  x <- gsub("\n{3,}", "\n\n", x)
  trimws(x)
}
