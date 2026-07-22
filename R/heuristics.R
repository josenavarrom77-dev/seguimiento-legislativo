first_match <- function(text, patterns, group = 1L) {
  for (pattern in patterns) {
    match <- stringr::str_match(text, stringr::regex(pattern, ignore_case = TRUE, dotall = TRUE))
    if (nrow(match) > 0 && !is.na(match[1, group + 1])) {
      value <- trimws(gsub("\\s+", " ", match[1, group + 1]))
      if (nzchar(value)) return(value)
    }
  }
  ""
}

heuristic_analysis <- function(text) {
  compact <- substr(normalize_multiline(text), 1, 45000)
  upper <- toupper(iconv(compact, to = "ASCII//TRANSLIT"))

  number <- first_match(compact, c(
    "PROYECTO\\s+DE\\s+LEY(?:\\s+ESTATUTARIA)?\\s+(?:N[ÚU]MERO|NO\\.?|N[°º])?\\s*([0-9]{1,4}(?:\\s+DE\\s+[0-9]{4})?(?:\\s+(?:SENADO|C[ÁA]MARA))?)",
    "PROYECTO\\s+DE\\s+ACTO\\s+LEGISLATIVO\\s+(?:NO\\.?|N[°º])?\\s*([0-9]{1,4}(?:\\s+DE\\s+[0-9]{4})?)"
  ))
  title <- first_match(compact, c(
    "PROYECTO\\s+DE\\s+LEY[^\n]{0,120}\n+[\"“]?([^\n]{20,500}?)[\"”]?\n+(?:EL\\s+CONGRESO|EXPOSICI[ÓO]N|ART[ÍI]CULO)",
    "(?:POR\\s+MEDIO\\s+DE\\s+LA\\s+CUAL|POR\\s+LA\\s+CUAL|MEDIANTE\\s+LA\\s+CUAL)(.{20,450}?)(?:\n|EL\\s+CONGRESO)"
  ))
  object <- first_match(compact, c(
    "ART[ÍI]CULO\\s+1[°º]?[\\.:-]?\\s*(?:OBJETO[\\.:-]?)?\\s*(.{30,1500}?)(?=ART[ÍI]CULO\\s+2|$)",
    "OBJETO\\s+DEL\\s+PROYECTO[\\.:-]?\\s*(.{30,1500}?)(?=\n[A-ZÁÉÍÓÚÑ ]{6,}|$)"
  ))
  date_text <- first_match(compact, c(
    "(?:BOGOT[ÁA][^\n]{0,30})?([0-3]?[0-9]\\s+DE\\s+(?:ENERO|FEBRERO|MARZO|ABRIL|MAYO|JUNIO|JULIO|AGOSTO|SEPTIEMBRE|OCTUBRE|NOVIEMBRE|DICIEMBRE)\\s+DE\\s+20[0-9]{2})",
    "(20[0-9]{2}-[01][0-9]-[0-3][0-9])"
  ))

  chamber <- if (grepl("SENADO DE LA REPUBLICA", upper, fixed = TRUE)) {
    "Senado"
  } else if (grepl("CAMARA DE REPRESENTANTES", upper, fixed = TRUE)) {
    "Cámara de Representantes"
  } else {
    ""
  }

  type <- if (grepl("PROYECTO DE ACTO LEGISLATIVO", upper, fixed = TRUE)) {
    "Acto legislativo"
  } else if (grepl("PROYECTO DE LEY ESTATUTARIA", upper, fixed = TRUE)) {
    "Proyecto de ley estatutaria"
  } else {
    "Proyecto de ley"
  }

  list(
    numero_proyecto = number,
    titulo_oficial = title,
    titulo_corto = if (nzchar(title)) substr(title, 1, 100) else "",
    tipo_iniciativa = type,
    camara_origen = chamber,
    fecha_radicacion = if (!is.na(safe_date(date_text))) format(safe_date(date_text), "%Y-%m-%d") else "",
    autores = character(),
    partido_bancada = "",
    comision = "",
    tema_principal = "Por clasificar",
    subtema = "",
    objeto = object,
    resumen_ejecutivo = if (nzchar(object)) object else substr(compact, 1, 1500),
    poblacion_territorio = "",
    entidades_competentes = character(),
    normas_modificadas = character(),
    impacto_fiscal = "Por determinar",
    hallazgos_fiscales = "Requiere revisión humana o análisis por IA.",
    riesgos_juridicos = character(),
    riesgos_implementacion = character(),
    oportunidades = character(),
    articulos_clave = character(),
    recomendacion_preliminar = "Pendiente de análisis técnico y político.",
    alertas_revision = c("Ficha generada mediante reglas básicas; verifique el documento original."),
    confianza_extraccion = "Baja"
  )
}
