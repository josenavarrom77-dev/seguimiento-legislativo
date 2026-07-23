packages <- c(
  "shiny", "bslib", "DT", "pdftools", "httr2", "jsonlite", "stringr",
  "DBI", "RSQLite", "RPostgres", "openxlsx", "shinymanager", "tesseract", "xml2", "usethis", "rsconnect"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("Todos los paquetes ya están instalados.")
}
