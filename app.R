required_packages <- c("shiny", "bslib", "DT", "pdftools", "httr2", "jsonlite", "stringr", "xml2", "DBI", "RSQLite", "RPostgres", "openxlsx", "shinymanager")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Faltan paquetes: ", paste(missing_packages, collapse = ", "), ". Ejecuta install_packages.R y reinicia RStudio.")
}

# Shiny admite aproximadamente 5 MB por defecto. Los proyectos escaneados
# suelen ser mucho más pesados, por lo que permitimos archivos de hasta 100 MB.
options(shiny.maxRequestSize = 100 * 1024^2)
options(shinymanager.pwd_failure_limit = 5)

invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source, local = FALSE))

db_path <- file.path("data", "proyectos.sqlite")
init_database(db_path)

theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0F6B66",
  secondary = "#17324D"
)

app_credentials <- load_app_credentials()

app_ui <- shiny::navbarPage(
  id = "main_tabs",
  title = "Seguimiento legislativo UTL",
  theme = theme,
  header = shiny::tags$head(shiny::includeCSS("www/styles.css")),

  shiny::tabPanel(
    "Tablero",
    shiny::fluidPage(
      shiny::br(),
      shiny::fluidRow(
        shiny::column(8, shiny::h2("Panorama de seguimiento")),
        shiny::column(4, shiny::uiOutput("session_user"))
      ),
      shiny::p(class = "text-muted", "Prioridades, próximos compromisos y carga de trabajo del equipo."),
      shiny::uiOutput("dashboard_cards"),
      shiny::fluidRow(
        shiny::column(7, bslib::card(
          bslib::card_header("Tareas pendientes y vencimientos"),
          DT::DTOutput("dashboard_tasks")
        )),
        shiny::column(5, bslib::card(
          bslib::card_header("Próximas actuaciones"),
          DT::DTOutput("dashboard_next")
        ))
      ),
      shiny::br(),
      bslib::card(
        bslib::card_header("Actividad reciente del equipo"),
        DT::DTOutput("audit_table")
      )
    )
  ),

  shiny::tabPanel(
    "Bandeja Senado",
    shiny::fluidPage(
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          8,
          shiny::h2("Proyectos detectados en el Senado"),
          shiny::p(
            class = "text-muted",
            "Los proyectos analizados entran al tablero como pendientes de revisión humana."
          )
        ),
        shiny::column(
          4,
          shiny::actionButton("sync_senate", "Buscar nuevos ahora", class = "btn-primary w-100")
        )
      ),
      shiny::uiOutput("senate_summary"),
      DT::DTOutput("senate_table")
    )
  ),

  shiny::tabPanel(
    "Cargar y analizar",
    shiny::fluidPage(
      shiny::br(),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::h4("Documento de entrada"),
          shiny::fileInput("pdf_file", "Subir proyecto en PDF", accept = ".pdf"),
          shiny::textInput("pdf_url", "O enlace directo al PDF", placeholder = "https://.../proyecto.pdf"),
          shiny::checkboxInput("use_ocr", "Aplicar OCR si el PDF está escaneado", TRUE),
          shiny::actionButton("analyze", "Leer y analizar proyecto", class = "btn-primary w-100"),
          shiny::hr(),
          shiny::uiOutput("api_status")
        ),
        bslib::card(
          bslib::card_header("Resultado de la lectura"),
          shiny::uiOutput("document_stats"),
          shiny::verbatimTextOutput("text_preview", placeholder = TRUE)
        )
      )
    )
  ),

  shiny::tabPanel(
    "Seguimiento",
    shiny::fluidPage(
      shiny::br(),
      shiny::fluidRow(
        shiny::column(8, shiny::selectInput("tracking_project", "Proyecto para seguimiento", choices = character(0))),
        shiny::column(4, shiny::uiOutput("tracking_badge"))
      ),
      bslib::card(
        bslib::card_header("Estado general del trámite"),
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput("estado_tramite", "Estado del trámite", c(
            "Radicado", "Publicación en Gaceta", "Pendiente de ponencia", "Ponencia publicada",
            "Primer debate", "Segundo debate", "Tercer debate", "Cuarto debate",
            "Conciliación", "Sanción presidencial", "Ley de la República", "Archivado", "Retirado"
          ))),
          shiny::column(4, shiny::textInput("ponentes", "Ponente(s)")),
          shiny::column(4, shiny::textInput("gaceta", "Gaceta(s)"))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput("proxima_actuacion", "Próxima actuación esperada")),
          shiny::column(3, shiny::textInput("fecha_proxima", "Fecha prevista", placeholder = "AAAA-MM-DD")),
          shiny::column(3, shiny::br(), shiny::actionButton("save_tracking", "Guardar estado", class = "btn-primary w-100"))
        )
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(6, bslib::card(
          bslib::card_header("Registrar actuación"),
          shiny::fluidRow(
            shiny::column(5, shiny::textInput("act_fecha", "Fecha", value = format(Sys.Date(), "%Y-%m-%d"))),
            shiny::column(7, shiny::selectInput("act_tipo", "Tipo", c("Radicación", "Gaceta", "Ponencia", "Debate", "Aprobación", "Conciliación", "Sanción", "Concepto", "Reunión", "Otra")))
          ),
          shiny::selectInput("act_etapa", "Etapa", c("Trámite inicial", "Comisión de origen", "Plenaria de origen", "Comisión de la otra cámara", "Plenaria de la otra cámara", "Conciliación", "Sanción presidencial", "Seguimiento")),
          shiny::textAreaInput("act_descripcion", "Descripción de la actuación", rows = 3),
          shiny::textInput("act_resultado", "Resultado / decisión"),
          shiny::textInput("act_fuente", "Enlace o fuente"),
          shiny::actionButton("add_actuation", "Agregar al historial", class = "btn-success")
        )),
        shiny::column(6, bslib::card(
          bslib::card_header("Asignar tarea"),
          shiny::textInput("task_name", "Tarea o producto"),
          shiny::fluidRow(
            shiny::column(6, shiny::textInput("task_responsible", "Responsable")),
            shiny::column(6, shiny::textInput("task_due", "Fecha límite", placeholder = "AAAA-MM-DD"))
          ),
          shiny::selectInput("task_priority", "Prioridad", c("Crítica", "Alta", "Media", "Baja"), selected = "Media"),
          shiny::textAreaInput("task_notes", "Notas", rows = 2),
          shiny::actionButton("add_task", "Asignar tarea", class = "btn-success")
        ))
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(6, shiny::h3("Línea de tiempo"), shiny::uiOutput("timeline")),
        shiny::column(
          6,
          shiny::h3("Tareas del proyecto"),
          DT::DTOutput("tasks_table"),
          shiny::br(),
          shiny::actionButton("complete_task", "Marcar tarea seleccionada como completada", class = "btn-outline-success")
        )
      ),
      shiny::br()
    )
  ),

  shiny::tabPanel(
    "Revisar ficha",
    shiny::fluidPage(
      shiny::br(),
      shiny::div(class = "review-banner", "La ficha es un borrador. Verifique datos, análisis y recomendación contra el documento oficial."),
      shiny::fluidRow(
        shiny::column(4, shiny::textInput("id_interno", "ID interno"), shiny::textInput("numero_proyecto", "Número del proyecto")),
        shiny::column(4, shiny::selectInput("tipo_iniciativa", "Tipo", c("Proyecto de ley", "Proyecto de ley estatutaria", "Acto legislativo", "Otro")), shiny::selectInput("camara_origen", "Cámara de origen", c("", "Senado", "Cámara de Representantes"))),
        shiny::column(
          4,
          shiny::textInput(
            "fecha_radicacion",
            "Fecha de radicación",
            value = "",
            placeholder = "AAAA-MM-DD"
          ),
          shiny::textInput("comision", "Comisión")
        )
      ),
      shiny::textInput("titulo_corto", "Título corto"),
      shiny::textAreaInput("titulo_oficial", "Título oficial", rows = 2),
      shiny::fluidRow(
        shiny::column(6, shiny::textInput("tema_principal", "Tema principal")),
        shiny::column(6, shiny::textInput("subtema", "Subtema"))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::textAreaInput("autores", "Autores", rows = 2)),
        shiny::column(6, shiny::textAreaInput("partido_bancada", "Partido / bancada", rows = 2))
      ),
      shiny::textAreaInput("objeto", "Objeto", rows = 4),
      shiny::textAreaInput("resumen_ejecutivo", "Resumen ejecutivo", rows = 7),
      shiny::textAreaInput("poblacion_territorio", "Población y territorio", rows = 3),
      shiny::fluidRow(
        shiny::column(6, shiny::textAreaInput("entidades_competentes", "Entidades competentes", rows = 3)),
        shiny::column(6, shiny::textAreaInput("normas_modificadas", "Normas modificadas", rows = 3))
      ),
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput("impacto_fiscal", "Impacto fiscal", c("Por determinar", "Sí", "No", "Potencial"))),
        shiny::column(3, shiny::selectInput("confianza_extraccion", "Confianza", c("Baja", "Media", "Alta"))),
        shiny::column(3, shiny::selectInput("prioridad", "Prioridad", c("Por definir", "Crítica", "Alta", "Media", "Baja"))),
        shiny::column(3, shiny::selectInput("estado_revision", "Estado de revisión", c("Pendiente", "En revisión", "Aprobado", "Requiere ajustes")))
      ),
      shiny::textAreaInput("hallazgos_fiscales", "Hallazgos fiscales", rows = 4),
      shiny::fluidRow(
        shiny::column(6, shiny::textAreaInput("riesgos_juridicos", "Riesgos jurídicos", rows = 5)),
        shiny::column(6, shiny::textAreaInput("riesgos_implementacion", "Riesgos de implementación", rows = 5))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::textAreaInput("oportunidades", "Oportunidades de aporte", rows = 5)),
        shiny::column(6, shiny::textAreaInput("articulos_clave", "Artículos clave", rows = 5))
      ),
      shiny::textAreaInput("recomendacion_preliminar", "Recomendación preliminar", rows = 5),
      shiny::textAreaInput("alertas_revision", "Alertas para revisión humana", rows = 4),
      shiny::fluidRow(
        shiny::column(4, shiny::textInput("responsable", "Responsable")),
        shiny::column(4, shiny::textInput("revisor", "Revisor")),
        shiny::column(4, shiny::textInput("fuente_oficial", "Fuente oficial"))
      ),
      shiny::actionButton("save_project", "Guardar proyecto", class = "btn-success btn-lg"),
      shiny::br(), shiny::br()
    )
  ),

  shiny::tabPanel(
    "Proyectos guardados",
    shiny::fluidPage(
      shiny::br(),
      shiny::fluidRow(
        shiny::column(8, shiny::h3("Base de proyectos")),
        shiny::column(4, shiny::downloadButton("download_excel", "Descargar matriz en Excel", class = "btn-primary float-end"))
      ),
      DT::DTOutput("projects_table"),
      shiny::br(),
      shiny::actionButton("open_tracking", "Abrir seguimiento del proyecto seleccionado", class = "btn-outline-primary")
    )
  )
)

ui <- if (authentication_enabled()) {
  shinymanager::secure_app(
    app_ui,
    tags_top = shiny::tags$div(
      class = "login-brand",
      shiny::tags$h3("Seguimiento legislativo UTL"),
      shiny::tags$p("Acceso exclusivo para el equipo autorizado")
    ),
    enable_admin = FALSE,
    language = "es"
  )
} else {
  app_ui
}

server <- function(input, output, session) {
  auth <- if (authentication_enabled()) {
    shinymanager::secure_server(
      check_credentials = shinymanager::check_credentials(app_credentials),
      timeout = 60
    )
  } else {
    shiny::reactive(list(user = "usuario_local", result = TRUE))
  }

  current_user <- shiny::reactive({
    blank_default(auth()$user, "usuario_local")
  })

  state <- shiny::reactiveValues(
    text = "",
    pages = 0L,
    chars = 0L,
    used_ocr = FALSE,
    method = "",
    source = "",
    file_name = ""
  )
  refresh <- shiny::reactiveVal(0L)

  output$session_user <- shiny::renderUI({
    shiny::div(
      class = "session-user",
      shiny::span("Sesión iniciada como"),
      shiny::strong(current_user())
    )
  })

  output$api_status <- shiny::renderUI({
    if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
      shiny::div(class = "api-ok", paste("IA habilitada · modelo", Sys.getenv("OPENAI_MODEL", "gpt-5.6")))
    } else {
      shiny::div(class = "api-warning", "IA no configurada. Se usará extracción básica hasta definir OPENAI_API_KEY.")
    }
  })

  output$document_stats <- shiny::renderUI({
    if (!nzchar(state$text)) return(shiny::p(class = "text-muted", "Aún no se ha leído un documento."))
    shiny::div(
      class = "stats-row",
      shiny::span(shiny::strong(state$pages), " páginas"),
      shiny::span(
        shiny::strong(formatC(
          as.integer(state$chars),
          format = "d",
          big.mark = ".",
          decimal.mark = ","
        )),
        " caracteres"
      ),
      shiny::span(if (state$used_ocr) "OCR aplicado" else "Texto digital"),
      shiny::span(paste("Método:", state$method))
    )
  })

  output$text_preview <- shiny::renderText({
    if (!nzchar(state$text)) return("Sube un PDF para comenzar.")
    substr(state$text, 1, 9000)
  })

  set_form <- function(data) {
    shiny::updateTextInput(session, "numero_proyecto", value = collapse_value(data$numero_proyecto))
    shiny::updateTextInput(session, "id_interno", value = slug_id(data$numero_proyecto, data$camara_origen))
    shiny::updateSelectInput(session, "tipo_iniciativa", selected = blank_default(data$tipo_iniciativa, "Proyecto de ley"))
    shiny::updateSelectInput(session, "camara_origen", selected = collapse_value(data$camara_origen))
    date_value <- safe_date(data$fecha_radicacion)
    shiny::updateTextInput(
      session,
      "fecha_radicacion",
      value = if (length(date_value) == 1L && !is.na(date_value)) {
        format(date_value, "%Y-%m-%d")
      } else {
        ""
      }
    )
    shiny::updateTextInput(session, "comision", value = collapse_value(data$comision))
    for (id in c("titulo_corto", "tema_principal", "subtema", "responsable", "revisor")) {
      shiny::updateTextInput(session, id, value = collapse_value(data[[id]]))
    }
    for (id in c("titulo_oficial", "autores", "partido_bancada", "objeto", "resumen_ejecutivo",
                 "poblacion_territorio", "entidades_competentes", "normas_modificadas", "hallazgos_fiscales",
                 "riesgos_juridicos", "riesgos_implementacion", "oportunidades", "articulos_clave",
                 "recomendacion_preliminar", "alertas_revision")) {
      shiny::updateTextAreaInput(session, id, value = collapse_value(data[[id]]))
    }
    shiny::updateSelectInput(session, "impacto_fiscal", selected = blank_default(data$impacto_fiscal, "Por determinar"))
    shiny::updateSelectInput(session, "confianza_extraccion", selected = blank_default(data$confianza_extraccion, "Baja"))
    shiny::updateSelectInput(session, "prioridad", selected = "Por definir")
    shiny::updateSelectInput(session, "estado_revision", selected = "Pendiente")
    shiny::updateTextInput(session, "fuente_oficial", value = state$source)
  }

  shiny::observeEvent(input$analyze, {
    if (is.null(input$pdf_file) && !nzchar(input$pdf_url)) {
      shiny::showNotification("Sube un PDF o pega un enlace directo.", type = "error")
      return(invisible(NULL))
    }

    completed <- FALSE
    tryCatch({
      shiny::withProgress(message = "Procesando proyecto de ley", value = 0, {
        path <- NULL
        if (!is.null(input$pdf_file)) {
          path <- input$pdf_file$datapath
          state$file_name <- input$pdf_file$name
          state$source <- ""
        } else {
          shiny::incProgress(0.1, detail = "Descargando PDF")
          path <- download_pdf(input$pdf_url)
          state$file_name <- basename(path)
          state$source <- input$pdf_url
        }

        shiny::incProgress(0.25, detail = "Extrayendo texto")
        extracted <- extract_pdf_text(path, use_ocr = isTRUE(input$use_ocr))
        if (!nzchar(extracted$text)) stop("No se pudo extraer texto del documento.")
        state$text <- extracted$text
        state$pages <- extracted$pages
        state$chars <- extracted$chars
        state$used_ocr <- extracted$used_ocr

        shiny::incProgress(0.55, detail = "Generando ficha")
        analysis <- analyze_project(extracted$text)
        state$method <- analysis$method
        set_form(analysis$data)
        if (nzchar(analysis$note %||% "")) {
          shiny::showNotification(analysis$note, type = "warning", duration = 15)
        }
        shiny::incProgress(1, detail = "Ficha lista")
        completed <- TRUE
      })
    }, error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
    })

    if (completed) {
      shiny::showNotification("Proyecto leído. Revisa la ficha antes de guardarla.", type = "message")
      shiny::updateNavbarPage(session, "main_tabs", selected = "Revisar ficha")
    }
  }, ignoreInit = TRUE)

  collect_form <- function() {
    date_value <- safe_date(input$fecha_radicacion)

    list(
      id_interno = input$id_interno,
      numero_proyecto = input$numero_proyecto,
      titulo_corto = input$titulo_corto,
      titulo_oficial = input$titulo_oficial,
      tipo_iniciativa = input$tipo_iniciativa,
      tema_principal = input$tema_principal,
      subtema = input$subtema,
      autores = input$autores,
      partido_bancada = input$partido_bancada,
      camara_origen = input$camara_origen,
      comision = input$comision,
      fecha_radicacion = if (length(date_value) == 1L && !is.na(date_value)) {
        format(date_value, "%Y-%m-%d")
      } else {
        ""
      },
      objeto = input$objeto,
      resumen_ejecutivo = input$resumen_ejecutivo,
      poblacion_territorio = input$poblacion_territorio,
      entidades_competentes = input$entidades_competentes,
      normas_modificadas = input$normas_modificadas,
      impacto_fiscal = input$impacto_fiscal,
      hallazgos_fiscales = input$hallazgos_fiscales,
      riesgos_juridicos = input$riesgos_juridicos,
      riesgos_implementacion = input$riesgos_implementacion,
      oportunidades = input$oportunidades,
      articulos_clave = input$articulos_clave,
      recomendacion_preliminar = input$recomendacion_preliminar,
      alertas_revision = input$alertas_revision,
      confianza_extraccion = input$confianza_extraccion,
      prioridad = input$prioridad,
      responsable = input$responsable,
      revisor = input$revisor,
      estado_revision = input$estado_revision,
      fuente_oficial = input$fuente_oficial,
      archivo_origen = state$file_name,
      metodo_extraccion = state$method
    )
  }

  shiny::observeEvent(input$save_project, {
    project <- collect_form()
    shiny::validate(
      shiny::need(nzchar(project$id_interno), "El ID interno es obligatorio."),
      shiny::need(nzchar(project$titulo_oficial), "El título oficial es obligatorio.")
    )
    save_project(project, db_path, current_user())
    refresh(refresh() + 1L)
    shiny::showNotification("Proyecto guardado correctamente.", type = "message")
    shiny::updateNavbarPage(session, "main_tabs", selected = "Proyectos guardados")
  }, ignoreInit = TRUE)

  projects_data <- shiny::reactive({
    refresh()
    list_projects(db_path)
  })

  tracking_data <- shiny::reactive({
    refresh()
    list_tracking(db_path)
  })

  all_tasks <- shiny::reactive({
    refresh()
    list_tasks(path = db_path)
  })

  audit_data <- shiny::reactive({
    refresh()
    list_audit(100L, db_path)
  })

  senate_data <- shiny::reactive({
    refresh()
    list_senate_imports(path = db_path)
  })

  shiny::observeEvent(input$sync_senate, {
    tryCatch({
      shiny::withProgress(message = "Consultando el portal del Senado", value = 0.2, {
        count <- discover_senate_projects(
          Sys.getenv("SENATE_LEGISLATURE", "2026-2027"), db_path
        )
        shiny::incProgress(0.8)
        refresh(refresh() + 1L)
        shiny::showNotification(
          paste(count, "proyectos encontrados. La automatización analizará los nuevos."),
          type = "message"
        )
      })
    }, error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  }, ignoreInit = TRUE)

  output$senate_summary <- shiny::renderUI({
    data <- senate_data()
    if (!nrow(data)) {
      return(shiny::div(
        class = "review-banner",
        "La bandeja está vacía. Pulsa “Buscar nuevos ahora” para la primera consulta."
      ))
    }
    counts <- table(factor(
      data$estado_importacion,
      levels = c("Nuevo", "Analizado", "Sin PDF", "Error")
    ))
    shiny::div(
      class = "metric-grid",
      shiny::div(class = "metric-card metric-total", shiny::span("Detectados"), shiny::strong(nrow(data))),
      shiny::div(class = "metric-card metric-pending", shiny::span("Por analizar"), shiny::strong(counts[["Nuevo"]])),
      shiny::div(class = "metric-card metric-ok", shiny::span("Analizados"), shiny::strong(counts[["Analizado"]])),
      shiny::div(
        class = "metric-card metric-danger",
        shiny::span("Con novedad"),
        shiny::strong(counts[["Sin PDF"]] + counts[["Error"]])
      )
    )
  })

  output$senate_table <- DT::renderDT({
    data <- senate_data()
    if (!nrow(data)) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay proyectos importados todavía."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    show <- data[, c(
      "numero_senado", "titulo", "autor", "comision", "estado_senado",
      "fecha_presentacion", "estado_importacion", "ultimo_error"
    ), drop = FALSE]
    names(show) <- c(
      "N.º Senado", "Título", "Autor", "Comisión", "Estado Senado",
      "Fecha", "Importación", "Observación"
    )
    DT::datatable(
      show, rownames = FALSE, filter = "top",
      options = list(
        pageLength = 15, scrollX = TRUE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/es-ES.json")
      )
    )
  })

  shiny::observe({
    data <- projects_data()
    if (!nrow(data)) {
      shiny::updateSelectInput(session, "tracking_project", choices = character(0), selected = character(0))
      return()
    }
    labels <- paste0(data$numero_proyecto, " · ", ifelse(nzchar(data$titulo_corto), data$titulo_corto, data$titulo_oficial))
    choices <- stats::setNames(data$id_interno, labels)
    current <- shiny::isolate(input$tracking_project %||% "")
    selected <- if (current %in% data$id_interno) current else data$id_interno[[1]]
    shiny::updateSelectInput(session, "tracking_project", choices = choices, selected = selected)
  })

  shiny::observeEvent(input$tracking_project, {
    shiny::req(nzchar(input$tracking_project))
    data <- get_tracking(input$tracking_project, db_path)
    if (is.null(data)) {
      data <- list(
        estado_tramite = "Radicado", ponentes = "", gaceta = "",
        proxima_actuacion = "", fecha_proxima = ""
      )
    }
    shiny::updateSelectInput(session, "estado_tramite", selected = blank_default(data$estado_tramite, "Radicado"))
    shiny::updateTextInput(session, "ponentes", value = collapse_value(data$ponentes))
    shiny::updateTextInput(session, "gaceta", value = collapse_value(data$gaceta))
    shiny::updateTextInput(session, "proxima_actuacion", value = collapse_value(data$proxima_actuacion))
    shiny::updateTextInput(session, "fecha_proxima", value = collapse_value(data$fecha_proxima))
  }, ignoreInit = FALSE)

  output$tracking_badge <- shiny::renderUI({
    refresh()
    id <- input$tracking_project %||% ""
    if (!nzchar(id)) return(shiny::div(class = "tracking-empty", "Guarda un proyecto para comenzar."))
    tracking <- get_tracking(id, db_path)
    status <- if (is.null(tracking)) "Sin seguimiento" else blank_default(tracking$estado_tramite, "Radicado")
    shiny::div(class = "tracking-badge", shiny::span("Estado actual"), shiny::strong(status))
  })

  shiny::observeEvent(input$save_tracking, {
    id <- input$tracking_project %||% ""
    if (!nzchar(id)) {
      shiny::showNotification("Selecciona un proyecto.", type = "error")
      return()
    }
    tryCatch({
      save_tracking(list(
        id_interno = id,
        estado_tramite = input$estado_tramite,
        ponentes = input$ponentes,
        gaceta = input$gaceta,
        proxima_actuacion = input$proxima_actuacion,
        fecha_proxima = format_form_date(input$fecha_proxima, "La fecha prevista")
      ), db_path, current_user())
      refresh(refresh() + 1L)
      shiny::showNotification("Estado del trámite actualizado.", type = "message")
    }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$add_actuation, {
    id <- input$tracking_project %||% ""
    if (!nzchar(id) || !nzchar(trimws(input$act_descripcion %||% ""))) {
      shiny::showNotification("Selecciona un proyecto y describe la actuación.", type = "error")
      return()
    }
    tryCatch({
      save_actuation(list(
        id_interno = id,
        fecha = format_form_date(input$act_fecha, "La fecha de la actuación", required = TRUE),
        tipo = input$act_tipo,
        etapa = input$act_etapa,
        descripcion = input$act_descripcion,
        resultado = input$act_resultado,
        fuente = input$act_fuente
      ), db_path, current_user())
      refresh(refresh() + 1L)
      shiny::updateTextAreaInput(session, "act_descripcion", value = "")
      shiny::updateTextInput(session, "act_resultado", value = "")
      shiny::updateTextInput(session, "act_fuente", value = "")
      shiny::showNotification("Actuación agregada al historial.", type = "message")
    }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$add_task, {
    id <- input$tracking_project %||% ""
    if (!nzchar(id) || !nzchar(trimws(input$task_name %||% "")) || !nzchar(trimws(input$task_responsible %||% ""))) {
      shiny::showNotification("La tarea y el responsable son obligatorios.", type = "error")
      return()
    }
    tryCatch({
      save_task(list(
        id_interno = id,
        tarea = input$task_name,
        responsable = input$task_responsible,
        fecha_limite = format_form_date(input$task_due, "La fecha límite"),
        prioridad = input$task_priority,
        estado = "Pendiente",
        notas = input$task_notes
      ), db_path, current_user())
      refresh(refresh() + 1L)
      shiny::updateTextInput(session, "task_name", value = "")
      shiny::updateTextAreaInput(session, "task_notes", value = "")
      shiny::showNotification("Tarea asignada correctamente.", type = "message")
    }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
  }, ignoreInit = TRUE)

  project_tasks <- shiny::reactive({
    refresh()
    id <- input$tracking_project %||% ""
    if (!nzchar(id)) return(data.frame())
    list_tasks(id, db_path)
  })

  output$tasks_table <- DT::renderDT({
    data <- project_tasks()
    if (!nrow(data)) return(DT::datatable(data.frame(Mensaje = "No hay tareas asignadas."), rownames = FALSE, options = list(dom = "t")))
    due <- suppressWarnings(as.Date(data$fecha_limite))
    data$Semáforo <- ifelse(
      data$estado == "Completada", "Completada",
      ifelse(!is.na(due) & due < Sys.Date(), "Vencida",
             ifelse(!is.na(due) & due <= Sys.Date() + 3, "Próxima", "En plazo"))
    )
    show <- data[, c("tarea_id", "tarea", "responsable", "fecha_limite", "prioridad", "estado", "Semáforo"), drop = FALSE]
    names(show) <- c("ID", "Tarea", "Responsable", "Fecha límite", "Prioridad", "Estado", "Semáforo")
    table <- DT::datatable(show, rownames = FALSE, selection = "single", options = list(pageLength = 8, scrollX = TRUE))
    DT::formatStyle(
      table,
      "Semáforo",
      backgroundColor = DT::styleEqual(
        c("Vencida", "Próxima", "En plazo", "Completada"),
        c("#f8d7da", "#fff3cd", "#dbeafe", "#d1e7dd")
      )
    )
  })

  shiny::observeEvent(input$complete_task, {
    selected <- input$tasks_table_rows_selected
    data <- project_tasks()
    if (length(selected) != 1L || !nrow(data)) {
      shiny::showNotification("Selecciona una tarea de la tabla.", type = "warning")
      return()
    }
    complete_task(data$tarea_id[[selected]], db_path, current_user())
    refresh(refresh() + 1L)
    shiny::showNotification("Tarea marcada como completada.", type = "message")
  }, ignoreInit = TRUE)

  output$timeline <- shiny::renderUI({
    refresh()
    id <- input$tracking_project %||% ""
    if (!nzchar(id)) return(shiny::p(class = "text-muted", "Selecciona un proyecto."))
    data <- list_actuations(id, db_path)
    if (!nrow(data)) return(shiny::p(class = "text-muted", "Aún no hay actuaciones registradas."))
    items <- lapply(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      source <- collapse_value(row$fuente)
      source_ui <- if (grepl("^https?://", source, ignore.case = TRUE)) {
        shiny::tags$a("Abrir fuente", href = source, target = "_blank", rel = "noopener noreferrer")
      } else if (nzchar(source)) {
        shiny::small(paste("Fuente:", source))
      }
      shiny::div(
        class = "timeline-item",
        shiny::div(class = "timeline-date", collapse_value(row$fecha)),
        shiny::strong(paste(collapse_value(row$tipo), "·", collapse_value(row$etapa))),
        shiny::p(collapse_value(row$descripcion)),
        if (nzchar(collapse_value(row$resultado))) shiny::div(class = "timeline-result", paste("Resultado:", collapse_value(row$resultado))),
        source_ui
      )
    })
    shiny::div(class = "timeline", items)
  })

  output$dashboard_cards <- shiny::renderUI({
    projects <- projects_data()
    tasks <- all_tasks()
    pending <- if (nrow(tasks)) tasks$estado != "Completada" else logical(0)
    due <- if (nrow(tasks)) suppressWarnings(as.Date(tasks$fecha_limite)) else as.Date(character(0))
    overdue <- sum(pending & !is.na(due) & due < Sys.Date())
    high <- if (nrow(projects)) sum(projects$prioridad %in% c("Crítica", "Alta")) else 0L
    cards <- list(
      c("Proyectos", nrow(projects), "total"),
      c("Prioridad alta", high, "high"),
      c("Tareas pendientes", sum(pending), "pending"),
      c("Tareas vencidas", overdue, if (overdue > 0) "danger" else "ok")
    )
    shiny::div(class = "metric-grid", lapply(cards, function(card) {
      shiny::div(class = paste("metric-card", paste0("metric-", card[[3]])),
                 shiny::span(card[[1]]), shiny::strong(card[[2]]))
    }))
  })

  output$dashboard_tasks <- DT::renderDT({
    tasks <- all_tasks()
    tasks <- tasks[tasks$estado != "Completada", , drop = FALSE]
    if (!nrow(tasks)) return(DT::datatable(data.frame(Mensaje = "No hay tareas pendientes."), rownames = FALSE, options = list(dom = "t")))
    projects <- projects_data()[, c("id_interno", "numero_proyecto"), drop = FALSE]
    data <- merge(tasks, projects, by = "id_interno", all.x = TRUE, sort = FALSE)
    due <- suppressWarnings(as.Date(data$fecha_limite))
    data$Alerta <- ifelse(!is.na(due) & due < Sys.Date(), "Vencida", ifelse(!is.na(due) & due <= Sys.Date() + 3, "Próxima", "En plazo"))
    data <- data[order(match(data$Alerta, c("Vencida", "Próxima", "En plazo")), due), , drop = FALSE]
    show <- data[, c("numero_proyecto", "tarea", "responsable", "fecha_limite", "prioridad", "Alerta"), drop = FALSE]
    names(show) <- c("Proyecto", "Tarea", "Responsable", "Límite", "Prioridad", "Alerta")
    table <- DT::datatable(show, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
    DT::formatStyle(
      table,
      "Alerta",
      backgroundColor = DT::styleEqual(c("Vencida", "Próxima", "En plazo"), c("#f8d7da", "#fff3cd", "#dbeafe"))
    )
  })

  output$dashboard_next <- DT::renderDT({
    data <- tracking_data()
    data <- data[nzchar(data$proxima_actuacion) | nzchar(data$fecha_proxima), , drop = FALSE]
    if (!nrow(data)) return(DT::datatable(data.frame(Mensaje = "No hay próximas actuaciones registradas."), rownames = FALSE, options = list(dom = "t")))
    projects <- projects_data()[, c("id_interno", "numero_proyecto"), drop = FALSE]
    data <- merge(data, projects, by = "id_interno", all.x = TRUE, sort = FALSE)
    data <- data[order(suppressWarnings(as.Date(data$fecha_proxima)), na.last = TRUE), , drop = FALSE]
    show <- data[, c("numero_proyecto", "estado_tramite", "proxima_actuacion", "fecha_proxima"), drop = FALSE]
    names(show) <- c("Proyecto", "Estado", "Próxima actuación", "Fecha")
    DT::datatable(show, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })

  output$audit_table <- DT::renderDT({
    data <- audit_data()
    if (!nrow(data)) return(DT::datatable(data.frame(Mensaje = "Aún no hay actividad registrada."), rownames = FALSE, options = list(dom = "t")))
    show <- data[, c("created_at", "usuario", "accion", "tipo_entidad", "entidad_id", "detalle"), drop = FALSE]
    names(show) <- c("Fecha", "Usuario", "Acción", "Elemento", "ID", "Detalle")
    DT::datatable(show, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })

  output$projects_table <- DT::renderDT({
    data <- projects_data()
    columns <- intersect(c("id_interno", "numero_proyecto", "titulo_corto", "tipo_iniciativa", "tema_principal", "camara_origen", "comision", "impacto_fiscal", "prioridad", "responsable", "estado_revision", "updated_at"), names(data))
    DT::datatable(
      data[, columns, drop = FALSE],
      rownames = FALSE,
      filter = "top",
      selection = "single",
      options = list(pageLength = 15, scrollX = TRUE, language = list(url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/es-ES.json"))
    )
  })

  shiny::observeEvent(input$open_tracking, {
    selected <- input$projects_table_rows_selected
    data <- projects_data()
    if (length(selected) != 1L || !nrow(data)) {
      shiny::showNotification("Selecciona un proyecto de la tabla.", type = "warning")
      return()
    }
    shiny::updateSelectInput(session, "tracking_project", selected = data$id_interno[[selected]])
    shiny::updateNavbarPage(session, "main_tabs", selected = "Seguimiento")
  }, ignoreInit = TRUE)

  output$download_excel <- shiny::downloadHandler(
    filename = function() paste0("matriz_proyectos_", Sys.Date(), ".xlsx"),
    content = function(file) export_projects_xlsx(file, db_path)
  )
}

shiny::shinyApp(ui, server)
