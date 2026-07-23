download_pdf <- function(url, destination = tempfile(fileext = ".pdf")) {
  if (!grepl("^https?://", url, ignore.case = TRUE)) {
    stop("El enlace debe comenzar con http:// o https://")
  }
  safe_url <- utils::URLencode(trimws(url), reserved = FALSE, repeated = FALSE)

  response <- httr2::request(safe_url) |>
    httr2::req_user_agent("SeguimientoLegislativoUTL/0.1") |>
    httr2::req_timeout(60) |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 400) {
    stop("No fue posible descargar el documento. Código HTTP: ", httr2::resp_status(response))
  }

  raw <- httr2::resp_body_raw(response)
  if (length(raw) < 4 || rawToChar(raw[1:4]) != "%PDF") {
    stop("El enlace no entrega un PDF directo. En esta primera versión debes usar el enlace exacto al archivo PDF.")
  }
  writeBin(raw, destination)
  destination
}

extract_pdf_text <- function(path, use_ocr = TRUE, max_ocr_pages = 25L) {
  if (!file.exists(path)) stop("No se encontró el archivo PDF.")

  info <- pdftools::pdf_info(path)
  pages <- pdftools::pdf_text(path)
  text <- normalize_multiline(paste(pages, collapse = "\n\n--- PÁGINA ---\n\n"))
  used_ocr <- FALSE

  sparse_text <- nchar(gsub("\\s+", "", text)) < max(500L, info$pages * 80L)
  if (isTRUE(use_ocr) && sparse_text) {
    if (!requireNamespace("tesseract", quietly = TRUE)) {
      warning("El PDF parece escaneado, pero el paquete 'tesseract' no está instalado. Se continuará con el texto disponible.")
    } else {
      available <- tesseract::tesseract_info()$available
      if (!"spa" %in% available && uses_postgres()) {
        try(tesseract::tesseract_download("spa", model = "fast"), silent = TRUE)
        available <- tesseract::tesseract_info()$available
      }
      language <- if ("spa" %in% available) {
        "spa"
      } else if ("eng" %in% available) {
        "eng"
      } else {
        stop(
          "Falta el idioma de OCR. Detén la app y ejecuta: ",
          "tesseract::tesseract_download('spa', model = 'fast')"
        )
      }
      page_limit <- min(info$pages, as.integer(max_ocr_pages))
      image_dir <- tempfile("ocr_pages_")
      dir.create(image_dir, recursive = TRUE)
      on.exit(unlink(image_dir, recursive = TRUE, force = TRUE), add = TRUE)
      image_files <- file.path(image_dir, sprintf("page-%03d.png", seq_len(page_limit)))
      pdftools::pdf_convert(
        path,
        format = "png",
        pages = seq_len(page_limit),
        dpi = 180,
        filenames = image_files,
        verbose = FALSE
      )
      engine <- tesseract::tesseract(language)
      ocr_pages <- vapply(image_files, tesseract::ocr, character(1), engine = engine)
      text <- normalize_multiline(paste(ocr_pages, collapse = "\n\n--- PÁGINA OCR ---\n\n"))
      used_ocr <- TRUE
    }
  }

  list(
    text = text,
    pages = info$pages,
    used_ocr = used_ocr,
    chars = nchar(text, type = "chars")
  )
}
