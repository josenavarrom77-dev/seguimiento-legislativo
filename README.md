# Seguimiento legislativo UTL — versión operativa en Shiny

Aplicación para cargar un proyecto de ley en PDF, extraer su texto, generar una ficha estructurada, administrar su trámite legislativo, asignar tareas al equipo y exportar la matriz completa a Excel.

Funciona en dos modalidades: local con SQLite y web multiusuario con PostgreSQL. La guía completa de publicación está en `WEB_DEPLOYMENT.md`.

## Importación automática del Senado

La aplicación consulta la fuente oficial del Senado, registra los proyectos nuevos en
`importaciones_senado`, descarga el texto radicado disponible y analiza una cantidad
limitada por ejecución. El flujo programado está en
`.github/workflows/senate-ingestion.yml`.

Secretos requeridos en GitHub Actions:

- `DATABASE_URL`
- `OPENAI_API_KEY`

Variables opcionales:

- `OPENAI_MODEL` (por defecto `gpt-5.6`)
- `SENATE_LEGISLATURE` (por defecto `2026-2027`)
- `SENATE_MAX_ANALYSES` (por defecto `3`)

Requiere R 4.1 o posterior y una versión reciente de RStudio.

## 1. Abrir el proyecto

Descarga y descomprime la carpeta. En RStudio, abre la carpeta `seguimiento-legislativo` como proyecto o establece esa carpeta como directorio de trabajo.

## 2. Instalar paquetes

Ejecuta una vez:

```r
source("install_packages.R")
```

El OCR es opcional. En macOS, si `tesseract` presenta problemas, instala primero el componente del sistema:

```bash
brew install tesseract tesseract-lang
```

También puedes descargar el modelo de español directamente desde R:

```r
tesseract::tesseract_download("spa", model = "fast")
tesseract::tesseract_info()$available
```

## 3. Configurar la IA

La aplicación funciona sin IA, pero solo hará una extracción básica. Para habilitar el análisis completo, ejecuta en RStudio:

```r
usethis::edit_r_environ()
```

Agrega:

```text
OPENAI_API_KEY=tu_clave_aqui
OPENAI_MODEL=gpt-5.6
```

Guarda el archivo y reinicia RStudio. No escribas la clave directamente en `app.R` y no compartas el archivo `.Renviron`.

## 4. Ejecutar

```r
shiny::runApp()
```

La aplicación se abrirá en el navegador. Puedes subir un PDF o pegar un enlace directo que termine entregando un archivo PDF.

## Funciones incluidas

- Lee PDF digitales con `pdftools`.
- Puede aplicar OCR a PDF escaneados con `tesseract`.
- Usa extracción básica si no hay clave de IA.
- Con la API configurada, solicita una respuesta estructurada y auditable.
- Permite revisar y corregir todos los campos antes de guardar.
- Guarda los registros en `data/proyectos.sqlite`.
- Registra ponentes, gacetas, estado del trámite y próxima actuación.
- Construye una línea de tiempo acumulativa de actuaciones.
- Permite asignar tareas, responsables, prioridades y fechas límite.
- Muestra alertas para tareas vencidas o próximas a vencer.
- Incluye un tablero general para la coordinación del equipo.
- Registra en una auditoría qué usuario realizó cada cambio.
- Exporta proyectos, seguimiento, actuaciones, tareas y auditoría en hojas separadas de Excel.

## Actualizar sin perder información

Si vienes de una versión anterior, copia el archivo `data/proyectos.sqlite` a la carpeta nueva antes de ejecutar la aplicación. Al iniciar, la app crea automáticamente las tablas adicionales sin modificar las fichas ya guardadas.

## Controles importantes

- La recomendación de la IA es preliminar y nunca reemplaza la revisión jurídica, fiscal o política.
- Confirma autores, número, comisión, fechas y estado contra la fuente oficial.
- La app envía a la API el texto extraído del documento cuando la IA está habilitada.
- En la siguiente etapa se pueden agregar los conectores específicos para detectar nuevas radicaciones en Senado y Cámara.

La integración usa la Responses API y Structured Outputs:

- https://developers.openai.com/api/docs/guides/text
- https://developers.openai.com/api/docs/guides/structured-outputs

## Estructura

```text
seguimiento-legislativo/
├── app.R
├── install_packages.R
├── README.md
├── .Renviron.example
├── R/
│   ├── database.R
│   ├── auth.R
│   ├── export_excel.R
│   ├── heuristics.R
│   ├── openai_analysis.R
│   ├── pdf_reader.R
│   └── utils.R
├── data/
└── www/
    └── styles.css
```

Para preparar la versión web también se incluyen `migrate_to_postgres.R`, `prepare_deployment.R` y `WEB_DEPLOYMENT.md`.

## Próxima etapa sugerida

Agregar comparación inteligente entre el texto original, las ponencias y los textos aprobados; posteriormente, crear conectores para detectar nuevas radicaciones en Senado y Cámara.
