required_packages <- c(
  "httr2", "jsonlite", "xml2", "pdftools", "tesseract", "DBI",
  "RSQLite", "RPostgres", "stringr"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) stop("Faltan paquetes: ", paste(missing_packages, collapse = ", "))

invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source, local = FALSE))

if (!nzchar(Sys.getenv("DATABASE_URL"))) stop("Falta DATABASE_URL.")
if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) stop("Falta OPENAI_API_KEY.")

result <- run_senate_ingestion()
message(
  "Consulta terminada: ", result$discovered, " encontrados; ",
  result$analyzed, " analizados."
)
