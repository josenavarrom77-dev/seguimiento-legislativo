project_schema <- function() {
  string_field <- function(description) list(type = "string", description = description)
  string_array <- function(description) list(type = "array", items = list(type = "string"), description = description)

  properties <- list(
    numero_proyecto = string_field("Número del proyecto, año y cámara cuando estén disponibles."),
    titulo_oficial = string_field("Título oficial completo."),
    titulo_corto = string_field("Nombre corto descriptivo, máximo 100 caracteres."),
    tipo_iniciativa = string_field("Proyecto de ley, proyecto de ley estatutaria, acto legislativo u otro."),
    camara_origen = string_field("Senado, Cámara de Representantes o vacío si no consta."),
    fecha_radicacion = string_field("Fecha ISO YYYY-MM-DD o cadena vacía."),
    autores = string_array("Autores expresamente identificados."),
    partido_bancada = string_field("Partidos o bancadas expresamente identificados; no inferir."),
    comision = string_field("Comisión asignada o mencionada; vacío si aún no consta."),
    tema_principal = string_field("Tema principal de política pública."),
    subtema = string_field("Subtema específico."),
    objeto = string_field("Objeto jurídico y material del proyecto."),
    resumen_ejecutivo = string_field("Resumen ejecutivo claro de 150 a 300 palabras."),
    poblacion_territorio = string_field("Población, sectores y territorios afectados."),
    entidades_competentes = string_array("Entidades con funciones, obligaciones o competencias."),
    normas_modificadas = string_array("Leyes, decretos o artículos modificados o citados como objeto de reforma."),
    impacto_fiscal = string_field("Sí, No, Potencial o Por determinar."),
    hallazgos_fiscales = string_field("Costos, beneficios, fuentes de financiación, obligaciones y alertas de Ley 819."),
    riesgos_juridicos = string_array("Riesgos constitucionales, de competencia, unidad de materia o técnica legislativa."),
    riesgos_implementacion = string_array("Riesgos institucionales, presupuestales, regulatorios u operativos."),
    oportunidades = string_array("Oportunidades de mejora o aporte del senador y la UTL."),
    articulos_clave = string_array("Artículos que requieren especial revisión, con una razón breve."),
    recomendacion_preliminar = string_field("Recomendación técnica preliminar, no una decisión política definitiva."),
    alertas_revision = string_array("Datos dudosos, faltantes o que deben verificarse contra fuente oficial."),
    confianza_extraccion = string_field("Alta, Media o Baja según la claridad del documento.")
  )

  list(
    type = "object",
    properties = properties,
    required = names(properties),
    additionalProperties = FALSE
  )
}

response_output_text <- function(response_json) {
  output <- response_json$output %||% list()
  parts <- character()
  for (item in output) {
    if (!identical(item$type %||% "", "message")) next
    for (content in item$content %||% list()) {
      if (identical(content$type %||% "", "output_text") && nzchar(content$text %||% "")) {
        parts <- c(parts, content$text)
      }
    }
  }
  paste(parts, collapse = "\n")
}

analyze_with_openai <- function(text, api_key = Sys.getenv("OPENAI_API_KEY"), model = Sys.getenv("OPENAI_MODEL", "gpt-5.6")) {
  if (!nzchar(api_key)) stop("No se encontró OPENAI_API_KEY. La aplicación usará extracción básica hasta que configures la clave.")

  document <- truncate_document(text)
  instructions <- paste(
    "Eres un analista legislativo colombiano que apoya una Unidad de Trabajo Legislativo.",
    "Extrae únicamente información sustentada por el documento. No inventes autores, partidos, fechas, comisiones ni fuentes.",
    "Distingue hechos del texto y evaluación preliminar. Si un dato no aparece, usa cadena vacía o lista vacía.",
    "Analiza impacto fiscal, implementación, técnica legislativa y oportunidades de modificación.",
    "La recomendación es un borrador técnico sujeto a revisión humana."
  )

  body <- list(
    model = model,
    instructions = instructions,
    input = paste0("Analiza el siguiente proyecto de ley colombiano:\n\n", document),
    text = list(format = list(
      type = "json_schema",
      name = "ficha_proyecto_ley",
      strict = TRUE,
      schema = project_schema()
    )),
    store = FALSE
  )

  response <- httr2::request("https://api.openai.com/v1/responses") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE, null = "null") |>
    httr2::req_timeout(180) |>
    httr2::req_retry(max_tries = 2) |>
    httr2::req_perform()

  payload <- httr2::resp_body_json(response, simplifyVector = FALSE)
  output_text <- response_output_text(payload)
  if (!nzchar(output_text)) stop("La API respondió sin una ficha de texto utilizable.")
  jsonlite::fromJSON(output_text, simplifyVector = FALSE)
}

analyze_project <- function(text) {
  if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    tryCatch(
      list(data = analyze_with_openai(text), method = "OpenAI", note = ""),
      error = function(e) {
        message <- conditionMessage(e)
        if (grepl("429", message, fixed = TRUE)) {
          message <- paste0(
            message,
            " Revisa el saldo y los límites de la API; si acabas de pagar, espera unos minutos."
          )
        }
        list(
          data = heuristic_analysis(text),
          method = "Reglas básicas",
          note = paste("La IA no respondió:", message, "Se aplicó extracción básica.")
        )
      }
    )
  } else {
    list(
      data = heuristic_analysis(text),
      method = "Reglas básicas",
      note = "No se encontró OPENAI_API_KEY; se aplicó extracción básica."
    )
  }
}
