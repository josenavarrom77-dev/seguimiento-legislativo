senate_base_url <- "https://leyes.senado.gov.co"

senate_post_json <- function(path, fields) {
  request <- httr2::request(paste0(senate_base_url, path)) |>
    httr2::req_user_agent("SeguimientoLegislativoUTL/1.0") |>
    httr2::req_body_multipart(!!!fields) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3)
  response <- httr2::req_perform(request)
  payload <- httr2::resp_body_json(response, simplifyVector = FALSE)
  if (!isTRUE(payload$success)) {
    stop(payload$message %||% "El Senado no devolvió una respuesta válida.")
  }
  payload$data %||% list()
}

senate_get_text <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent("SeguimientoLegislativoUTL/1.0") |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
}

senate_regex_value <- function(text, pattern) {
  match <- regexec(pattern, text, perl = TRUE, ignore.case = TRUE)
  parts <- regmatches(text, match)[[1]]
  if (length(parts) < 2L) return("")
  trimws(gsub("<[^>]+>", "", parts[[2]]))
}

senate_detail <- function(id) {
  detail_url <- paste0(senate_base_url, "/api/get_detalle_pdly.php?id=", id)
  html <- senate_get_text(detail_url)
  pdf_url <- senate_regex_value(
    html,
    "id=['\"]textoRadicadoBtn['\"][^>]*data-link=['\"]([^'\"]*)['\"]"
  )
  if (!nzchar(pdf_url)) {
    pdf_url <- senate_regex_value(
      html,
      "data-link=['\"]([^'\"]*)['\"][^>]*id=['\"]textoRadicadoBtn['\"]"
    )
  }
  if (nzchar(pdf_url) && !grepl("^https?://", pdf_url, ignore.case = TRUE)) {
    pdf_url <- paste0(senate_base_url, "/", sub("^/+", "", pdf_url))
  }
  list(
    detalle_url = detail_url,
    pdf_url = pdf_url,
    fecha_presentacion = senate_regex_value(
      html,
      "Fecha de Presentaci[oó]n</td>\\s*<td[^>]*>([^<]*)"
    )
  )
}

discover_senate_projects <- function(legislature = Sys.getenv("SENATE_LEGISLATURE", "2026-2027"),
                                     db_path = file.path("data", "proyectos.sqlite")) {
  results <- senate_post_json("/api/search_pdly.php", list(legislatura = legislature))
  for (item in results) {
    id <- collapse_value(item$id)
    detail <- tryCatch(
      senate_detail(id),
      error = function(e) list(
        detalle_url = paste0(senate_base_url, "/api/get_detalle_pdly.php?id=", id),
        pdf_url = "", fecha_presentacion = ""
      )
    )
    upsert_senate_import(list(
      senado_id = id,
      legislatura = legislature,
      numero_senado = item$numero_senado,
      numero_camara = item$numero_camara,
      titulo = item$titulo,
      autor = item$autor,
      comision = item$comision,
      estado_senado = item$estado,
      fecha_presentacion = gsub("/", "-", collapse_value(detail$fecha_presentacion), fixed = TRUE),
      detalle_url = detail$detalle_url,
      pdf_url = detail$pdf_url
    ), db_path)
  }
  length(results)
}

senate_internal_id <- function(number) {
  paste0("PL-SENADO-", gsub("[^0-9]+", "-", trimws(number)))
}

analyze_senate_import <- function(row, db_path = file.path("data", "proyectos.sqlite")) {
  senate_id <- collapse_value(row$senado_id)
  if (!nzchar(collapse_value(row$pdf_url))) {
    update_senate_import(
      senate_id, "Sin PDF", error = "El portal aún no publica el texto radicado.", path = db_path
    )
    return(invisible(FALSE))
  }
  tryCatch({
    pdf_path <- download_pdf(collapse_value(row$pdf_url))
    on.exit(unlink(pdf_path, force = TRUE), add = TRUE)
    extracted <- extract_pdf_text(pdf_path, use_ocr = TRUE)
    analysis <- analyze_project(extracted$text)
    data <- analysis$data
    id_interno <- senate_internal_id(collapse_value(row$numero_senado))
    data$id_interno <- id_interno
    data$numero_proyecto <- paste(collapse_value(row$numero_senado), "Senado")
    data$titulo_oficial <- collapse_value(row$titulo)
    data$autores <- collapse_value(row$autor)
    data$camara_origen <- "Senado"
    data$comision <- collapse_value(row$comision)
    data$fecha_radicacion <- collapse_value(row$fecha_presentacion)
    data$prioridad <- "Por definir"
    data$responsable <- ""
    data$revisor <- ""
    data$estado_revision <- "Pendiente"
    data$fuente_oficial <- collapse_value(row$pdf_url)
    data$archivo_origen <- basename(collapse_value(row$pdf_url))
    data$metodo_extraccion <- paste0(analysis$method, " · importación automática")
    save_project(data, db_path, "robot_senado")
    save_tracking(list(
      id_interno = id_interno,
      estado_tramite = blank_default(row$estado_senado, "Radicado"),
      ponentes = "", gaceta = "", proxima_actuacion = "", fecha_proxima = ""
    ), db_path, "robot_senado")
    update_senate_import(senate_id, "Analizado", id_interno = id_interno, path = db_path)
    invisible(TRUE)
  }, error = function(e) {
    update_senate_import(senate_id, "Error", error = conditionMessage(e), path = db_path)
    message("No se pudo analizar ", row$numero_senado, ": ", conditionMessage(e))
    invisible(FALSE)
  })
}

run_senate_ingestion <- function(
  legislature = Sys.getenv("SENATE_LEGISLATURE", "2026-2027"),
  commission = Sys.getenv("SENATE_COMMISSION", "SEPTIMA"),
  max_analyses = as.integer(Sys.getenv("SENATE_MAX_ANALYSES", "3")),
  analyze = TRUE,
  db_path = file.path("data", "proyectos.sqlite")
) {
  discovered <- discover_senate_projects(legislature, db_path)
  processed <- 0L
  if (isTRUE(analyze) && max_analyses > 0L) {
    queue <- list_senate_imports("Nuevo", 500L, db_path)
    if (nzchar(trimws(commission)) && nrow(queue)) {
      normalize_commission <- function(x) {
        toupper(trimws(iconv(x, to = "ASCII//TRANSLIT")))
      }
      queue <- queue[
        normalize_commission(queue$comision) == normalize_commission(commission),
        ,
        drop = FALSE
      ]
    }
    if (nrow(queue) > max_analyses) {
      queue <- queue[seq_len(max_analyses), , drop = FALSE]
    }
    if (nrow(queue)) {
      for (i in seq_len(nrow(queue))) {
        processed <- processed + as.integer(isTRUE(
          analyze_senate_import(as.list(queue[i, , drop = FALSE]), db_path)
        ))
      }
    }
  }
  list(discovered = discovered, analyzed = processed)
}
