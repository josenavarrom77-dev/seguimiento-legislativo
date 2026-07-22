required <- c("DBI", "RSQLite", "RPostgres", "httr2", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Faltan paquetes: ", paste(missing, collapse = ", "))
if (!nzchar(Sys.getenv("DATABASE_URL"))) stop("Primero configura DATABASE_URL en .Renviron.")

source(file.path("R", "utils.R"), local = FALSE)
source(file.path("R", "database.R"), local = FALSE)

sqlite_path <- file.path("data", "proyectos.sqlite")
if (!file.exists(sqlite_path)) stop("No se encontró data/proyectos.sqlite.")

source_con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
on.exit(DBI::dbDisconnect(source_con), add = TRUE)
init_database(force = TRUE)

read_source <- function(table) {
  if (!DBI::dbExistsTable(source_con, table)) return(data.frame())
  DBI::dbReadTable(source_con, table)
}

projects <- read_source("proyectos")
if (nrow(projects)) {
  for (i in seq_len(nrow(projects))) {
    save_project(as.list(projects[i, , drop = FALSE]), actor = "migracion_inicial")
  }
}

tracking <- read_source("seguimiento_proyectos")
if (nrow(tracking)) {
  for (i in seq_len(nrow(tracking))) {
    save_tracking(as.list(tracking[i, , drop = FALSE]), actor = "migracion_inicial")
  }
}

actuations <- read_source("actuaciones")
if (nrow(actuations)) {
  for (i in seq_len(nrow(actuations))) {
    save_actuation(as.list(actuations[i, , drop = FALSE]), actor = "migracion_inicial")
  }
}

tasks <- read_source("tareas")
if (nrow(tasks)) {
  for (i in seq_len(nrow(tasks))) {
    save_task(as.list(tasks[i, , drop = FALSE]), actor = "migracion_inicial")
  }
}

message(
  "Migración terminada: ", nrow(projects), " proyectos, ",
  nrow(tracking), " seguimientos, ", nrow(actuations), " actuaciones y ",
  nrow(tasks), " tareas."
)
