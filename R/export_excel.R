export_projects_xlsx <- function(path, db_path = file.path("data", "proyectos.sqlite")) {
  projects <- list_projects(db_path)
  tracking <- list_tracking(db_path)
  actuations <- list_actuations(path = db_path)
  tasks <- list_tasks(path = db_path)
  audit <- list_audit(1000L, db_path)
  display_names <- c(
    id_interno = "ID interno", numero_proyecto = "No. del proyecto", titulo_corto = "Título corto",
    titulo_oficial = "Título oficial", tipo_iniciativa = "Tipo de iniciativa", tema_principal = "Tema principal",
    subtema = "Subtema", autores = "Autor(es)", partido_bancada = "Partido / bancada",
    camara_origen = "Cámara de origen", comision = "Comisión", fecha_radicacion = "Fecha de radicación",
    objeto = "Objeto", resumen_ejecutivo = "Resumen ejecutivo", poblacion_territorio = "Población / territorio",
    entidades_competentes = "Entidades competentes", normas_modificadas = "Normas modificadas",
    impacto_fiscal = "Impacto fiscal", hallazgos_fiscales = "Hallazgos fiscales",
    riesgos_juridicos = "Riesgos jurídicos", riesgos_implementacion = "Riesgos de implementación",
    oportunidades = "Oportunidades", articulos_clave = "Artículos clave",
    recomendacion_preliminar = "Recomendación preliminar", alertas_revision = "Alertas de revisión",
    confianza_extraccion = "Confianza", prioridad = "Prioridad", responsable = "Responsable",
    revisor = "Revisor", estado_revision = "Estado de revisión", fuente_oficial = "Fuente oficial",
    archivo_origen = "Archivo de origen", metodo_extraccion = "Método de extracción", updated_at = "Última actualización"
  )
  keep <- intersect(names(display_names), names(projects))
  output <- projects[, keep, drop = FALSE]
  names(output) <- unname(display_names[keep])

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Proyectos")
  if (nrow(output)) {
    openxlsx::writeDataTable(wb, "Proyectos", output, tableStyle = "TableStyleMedium2", withFilter = TRUE)
  } else {
    openxlsx::writeData(wb, "Proyectos", output, withFilter = TRUE)
  }
  openxlsx::freezePane(wb, "Proyectos", firstRow = TRUE, firstCol = TRUE)
  openxlsx::setColWidths(wb, "Proyectos", cols = seq_along(output), widths = "auto")
  wide <- which(names(output) %in% c("Título oficial", "Objeto", "Resumen ejecutivo", "Hallazgos fiscales", "Riesgos jurídicos", "Riesgos de implementación", "Oportunidades", "Recomendación preliminar", "Alertas de revisión"))
  if (length(wide)) openxlsx::setColWidths(wb, "Proyectos", cols = wide, widths = 35)
  wrap <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  if (nrow(output)) openxlsx::addStyle(wb, "Proyectos", wrap, rows = 2:(nrow(output) + 1), cols = seq_along(output), gridExpand = TRUE)

  write_sheet <- function(sheet, data) {
    openxlsx::addWorksheet(wb, sheet)
    if (nrow(data)) {
      openxlsx::writeDataTable(wb, sheet, data, tableStyle = "TableStyleMedium2", withFilter = TRUE)
    } else {
      openxlsx::writeData(wb, sheet, data, withFilter = TRUE)
    }
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    if (ncol(data)) openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(data)), widths = "auto")
    if (nrow(data) && ncol(data)) {
      openxlsx::addStyle(wb, sheet, wrap, rows = 2:(nrow(data) + 1), cols = seq_len(ncol(data)), gridExpand = TRUE)
    }
  }

  tracking_keep <- intersect(c("id_interno", "estado_tramite", "ponentes", "gaceta", "proxima_actuacion", "fecha_proxima", "updated_at"), names(tracking))
  tracking_out <- tracking[, tracking_keep, drop = FALSE]
  names(tracking_out) <- c("ID interno", "Estado del trámite", "Ponentes", "Gaceta", "Próxima actuación", "Fecha prevista", "Actualización")[seq_along(tracking_keep)]
  write_sheet("Seguimiento", tracking_out)

  act_keep <- intersect(c("id_interno", "fecha", "tipo", "etapa", "descripcion", "resultado", "fuente", "created_at"), names(actuations))
  act_out <- actuations[, act_keep, drop = FALSE]
  names(act_out) <- c("ID interno", "Fecha", "Tipo", "Etapa", "Descripción", "Resultado", "Fuente", "Registro")[seq_along(act_keep)]
  write_sheet("Actuaciones", act_out)

  task_keep <- intersect(c("id_interno", "tarea", "responsable", "fecha_limite", "prioridad", "estado", "notas", "updated_at"), names(tasks))
  task_out <- tasks[, task_keep, drop = FALSE]
  names(task_out) <- c("ID interno", "Tarea", "Responsable", "Fecha límite", "Prioridad", "Estado", "Notas", "Actualización")[seq_along(task_keep)]
  write_sheet("Tareas", task_out)

  audit_keep <- intersect(c("created_at", "usuario", "accion", "tipo_entidad", "entidad_id", "detalle"), names(audit))
  audit_out <- audit[, audit_keep, drop = FALSE]
  names(audit_out) <- c("Fecha", "Usuario", "Acción", "Elemento", "ID", "Detalle")[seq_along(audit_keep)]
  write_sheet("Auditoría", audit_out)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
