project_columns <- c(
  "id_interno", "numero_proyecto", "titulo_corto", "titulo_oficial", "tipo_iniciativa",
  "tema_principal", "subtema", "autores", "partido_bancada", "camara_origen", "comision",
  "fecha_radicacion", "objeto", "resumen_ejecutivo", "poblacion_territorio", "entidades_competentes",
  "normas_modificadas", "impacto_fiscal", "hallazgos_fiscales", "riesgos_juridicos",
  "riesgos_implementacion", "oportunidades", "articulos_clave", "recomendacion_preliminar",
  "alertas_revision", "confianza_extraccion", "prioridad", "responsable", "revisor",
  "estado_revision", "fuente_oficial", "archivo_origen", "metodo_extraccion"
)

database_state <- new.env(parent = emptyenv())
database_state$ready <- FALSE

uses_postgres <- function() {
  nzchar(Sys.getenv("DATABASE_URL"))
}

connect_database <- function(path = file.path("data", "proyectos.sqlite")) {
  if (!uses_postgres()) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    return(DBI::dbConnect(RSQLite::SQLite(), path))
  }

  parsed <- httr2::url_parse(Sys.getenv("DATABASE_URL"))
  if (!parsed$scheme %in% c("postgres", "postgresql") || !nzchar(parsed$hostname %||% "")) {
    stop("DATABASE_URL no es una conexión válida de PostgreSQL.")
  }
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = parsed$hostname,
    port = as.integer(parsed$port %||% 5432L),
    dbname = sub("^/", "", parsed$path),
    user = parsed$username,
    password = parsed$password,
    sslmode = parsed$query$sslmode %||% "require"
  )
}

prepare_parameter_sql <- function(con, sql) {
  if (!inherits(con, "PqConnection")) return(sql)
  index <- 0L
  while (grepl("?", sql, fixed = TRUE)) {
    index <- index + 1L
    sql <- sub("?", paste0("$", index), sql, fixed = TRUE)
  }
  sql
}

db_execute <- function(con, sql, params = NULL) {
  if (is.null(params)) return(DBI::dbExecute(con, sql))
  DBI::dbExecute(con, prepare_parameter_sql(con, sql), params = unname(params))
}

db_query <- function(con, sql, params = NULL) {
  if (is.null(params)) return(DBI::dbGetQuery(con, sql))
  DBI::dbGetQuery(con, prepare_parameter_sql(con, sql), params = unname(params))
}

log_audit_connection <- function(con, action, entity_type, entity_id, actor = "", details = "") {
  db_execute(con, "
    INSERT INTO auditoria (accion, tipo_entidad, entidad_id, usuario, detalle, created_at)
    VALUES (?, ?, ?, ?, ?, ?)", list(
      collapse_value(action), collapse_value(entity_type), collapse_value(entity_id),
      blank_default(actor, "usuario_local"), collapse_value(details),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
}

init_database <- function(path = file.path("data", "proyectos.sqlite"), force = FALSE) {
  if (isTRUE(database_state$ready) && !isTRUE(force)) return(invisible(path))
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  serial_id <- if (inherits(con, "PqConnection")) "BIGSERIAL PRIMARY KEY" else "INTEGER PRIMARY KEY AUTOINCREMENT"
  fields <- paste(sprintf("%s TEXT", project_columns), collapse = ",\n")

  DBI::dbExecute(con, sprintf(
    "CREATE TABLE IF NOT EXISTS proyectos (
       record_id %s,
       %s,
       created_at TEXT NOT NULL,
       updated_at TEXT NOT NULL,
       UNIQUE(id_interno)
     )", serial_id, fields
  ))

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS seguimiento_proyectos (
      id_interno TEXT PRIMARY KEY,
      estado_tramite TEXT NOT NULL DEFAULT 'Radicado',
      ponentes TEXT,
      gaceta TEXT,
      proxima_actuacion TEXT,
      fecha_proxima TEXT,
      updated_at TEXT NOT NULL
    )")

  DBI::dbExecute(con, sprintf("
    CREATE TABLE IF NOT EXISTS actuaciones (
      actuacion_id %s,
      id_interno TEXT NOT NULL,
      fecha TEXT NOT NULL,
      tipo TEXT NOT NULL,
      etapa TEXT,
      descripcion TEXT NOT NULL,
      resultado TEXT,
      fuente TEXT,
      created_at TEXT NOT NULL
    )", serial_id))

  DBI::dbExecute(con, sprintf("
    CREATE TABLE IF NOT EXISTS tareas (
      tarea_id %s,
      id_interno TEXT NOT NULL,
      tarea TEXT NOT NULL,
      responsable TEXT NOT NULL,
      fecha_limite TEXT,
      prioridad TEXT NOT NULL DEFAULT 'Media',
      estado TEXT NOT NULL DEFAULT 'Pendiente',
      notas TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )", serial_id))

  DBI::dbExecute(con, sprintf("
    CREATE TABLE IF NOT EXISTS auditoria (
      auditoria_id %s,
      accion TEXT NOT NULL,
      tipo_entidad TEXT NOT NULL,
      entidad_id TEXT,
      usuario TEXT NOT NULL,
      detalle TEXT,
      created_at TEXT NOT NULL
    )", serial_id))

  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_actuaciones_proyecto ON actuaciones(id_interno, fecha)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_tareas_proyecto ON tareas(id_interno, estado, fecha_limite)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_auditoria_fecha ON auditoria(created_at)")
  database_state$ready <- TRUE
  invisible(path)
}

save_project <- function(project, path = file.path("data", "proyectos.sqlite"), actor = "") {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  row <- as.list(setNames(rep("", length(project_columns)), project_columns))
  for (name in intersect(names(project), project_columns)) row[[name]] <- collapse_value(project[[name]])
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  row$created_at <- now
  row$updated_at <- now

  columns <- c(project_columns, "created_at", "updated_at")
  placeholders <- paste(rep("?", length(columns)), collapse = ",")
  update_columns <- setdiff(columns, c("id_interno", "created_at"))
  updates <- paste(sprintf("%s = excluded.%s", update_columns, update_columns), collapse = ",")
  sql <- sprintf(
    "INSERT INTO proyectos (%s) VALUES (%s)
     ON CONFLICT(id_interno) DO UPDATE SET %s",
    paste(columns, collapse = ","), placeholders, updates
  )
  db_execute(con, sql, unname(row[columns]))
  log_audit_connection(con, "guardar", "proyecto", project$id_interno, actor, project$titulo_corto)
  invisible(project$id_interno)
}

list_projects <- function(path = file.path("data", "proyectos.sqlite")) {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  db_query(con, "SELECT * FROM proyectos ORDER BY updated_at DESC")
}

get_project <- function(id_interno, path = file.path("data", "proyectos.sqlite")) {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  result <- db_query(con, "SELECT * FROM proyectos WHERE id_interno = ?", list(id_interno))
  if (!nrow(result)) return(NULL)
  as.list(result[1, , drop = FALSE])
}

save_tracking <- function(tracking, path = file.path("data", "proyectos.sqlite"), actor = "") {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  values <- list(
    collapse_value(tracking$id_interno), blank_default(tracking$estado_tramite, "Radicado"),
    collapse_value(tracking$ponentes), collapse_value(tracking$gaceta),
    collapse_value(tracking$proxima_actuacion), collapse_value(tracking$fecha_proxima),
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  db_execute(con, "
    INSERT INTO seguimiento_proyectos
      (id_interno, estado_tramite, ponentes, gaceta, proxima_actuacion, fecha_proxima, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(id_interno) DO UPDATE SET
      estado_tramite = excluded.estado_tramite,
      ponentes = excluded.ponentes,
      gaceta = excluded.gaceta,
      proxima_actuacion = excluded.proxima_actuacion,
      fecha_proxima = excluded.fecha_proxima,
      updated_at = excluded.updated_at", values)
  log_audit_connection(con, "actualizar", "seguimiento", tracking$id_interno, actor, tracking$estado_tramite)
  invisible(tracking$id_interno)
}

get_tracking <- function(id_interno, path = file.path("data", "proyectos.sqlite")) {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  result <- db_query(con, "SELECT * FROM seguimiento_proyectos WHERE id_interno = ?", list(id_interno))
  if (!nrow(result)) return(NULL)
  as.list(result[1, , drop = FALSE])
}

list_tracking <- function(path = file.path("data", "proyectos.sqlite")) {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  db_query(con, "SELECT * FROM seguimiento_proyectos ORDER BY updated_at DESC")
}

save_actuation <- function(actuation, path = file.path("data", "proyectos.sqlite"), actor = "") {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  db_execute(con, "
    INSERT INTO actuaciones
      (id_interno, fecha, tipo, etapa, descripcion, resultado, fuente, created_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)", list(
      collapse_value(actuation$id_interno), collapse_value(actuation$fecha),
      collapse_value(actuation$tipo), collapse_value(actuation$etapa),
      collapse_value(actuation$descripcion), collapse_value(actuation$resultado),
      collapse_value(actuation$fuente), format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
  log_audit_connection(con, "crear", "actuacion", actuation$id_interno, actor, actuation$descripcion)
  invisible(TRUE)
}

list_actuations <- function(id_interno = NULL, path = file.path("data", "proyectos.sqlite")) {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (is.null(id_interno) || !nzchar(id_interno)) {
    return(db_query(con, "SELECT * FROM actuaciones ORDER BY fecha DESC, actuacion_id DESC"))
  }
  db_query(con, "SELECT * FROM actuaciones WHERE id_interno = ? ORDER BY fecha DESC, actuacion_id DESC", list(id_interno))
}

save_task <- function(task, path = file.path("data", "proyectos.sqlite"), actor = "") {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  db_execute(con, "
    INSERT INTO tareas
      (id_interno, tarea, responsable, fecha_limite, prioridad, estado, notas, created_at, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)", list(
      collapse_value(task$id_interno), collapse_value(task$tarea),
      collapse_value(task$responsable), collapse_value(task$fecha_limite),
      blank_default(task$prioridad, "Media"), blank_default(task$estado, "Pendiente"),
      collapse_value(task$notas), now, now
    ))
  log_audit_connection(con, "crear", "tarea", task$id_interno, actor, task$tarea)
  invisible(TRUE)
}

list_tasks <- function(id_interno = NULL, path = file.path("data", "proyectos.sqlite")) {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  order_sql <- "ORDER BY CASE WHEN estado = 'Completada' THEN 1 ELSE 0 END, fecha_limite, tarea_id DESC"
  if (is.null(id_interno) || !nzchar(id_interno)) {
    return(db_query(con, paste("SELECT * FROM tareas", order_sql)))
  }
  db_query(con, paste("SELECT * FROM tareas WHERE id_interno = ?", order_sql), list(id_interno))
}

complete_task <- function(tarea_id, path = file.path("data", "proyectos.sqlite"), actor = "") {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  db_execute(con, "UPDATE tareas SET estado = 'Completada', updated_at = ? WHERE tarea_id = ?",
             list(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), as.integer(tarea_id)))
  log_audit_connection(con, "completar", "tarea", tarea_id, actor)
  invisible(TRUE)
}

list_audit <- function(limit = 100L, path = file.path("data", "proyectos.sqlite")) {
  init_database(path)
  con <- connect_database(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  limit <- max(1L, min(as.integer(limit), 1000L))
  db_query(con, sprintf("SELECT * FROM auditoria ORDER BY created_at DESC, auditoria_id DESC LIMIT %d", limit))
}
