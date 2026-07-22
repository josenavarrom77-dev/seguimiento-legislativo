authentication_enabled <- function() {
  nzchar(Sys.getenv("APP_USERS"))
}

load_app_credentials <- function() {
  raw <- Sys.getenv("APP_USERS")
  if (!nzchar(raw)) return(NULL)
  credentials <- tryCatch(
    jsonlite::fromJSON(raw, simplifyDataFrame = TRUE),
    error = function(e) stop("APP_USERS no contiene un JSON válido.")
  )
  if (!is.data.frame(credentials) || !all(c("user", "password") %in% names(credentials))) {
    stop("APP_USERS debe incluir user y password para cada usuario.")
  }
  credentials$user <- trimws(as.character(credentials$user))
  credentials$password <- as.character(credentials$password)
  if (any(!nzchar(credentials$user)) || any(!nzchar(credentials$password))) {
    stop("Todos los usuarios deben tener nombre y contraseña.")
  }
  if (anyDuplicated(credentials$user)) stop("Los nombres de usuario en APP_USERS no pueden repetirse.")
  if (!"admin" %in% names(credentials)) credentials$admin <- FALSE
  credentials$admin <- as.logical(credentials$admin)
  credentials
}
