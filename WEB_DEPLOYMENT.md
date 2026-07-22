# Publicación web compartida

La aplicación usa dos servicios:

- **Posit Connect Cloud:** ejecuta la aplicación y entrega el enlace web.
- **Neon PostgreSQL:** guarda una sola base de datos para todos los usuarios.

Las claves no deben escribirse en el código ni subirse a GitHub. Se configuran como variables secretas al publicar.

## 1. Crear la base compartida

1. Crea una cuenta en <https://console.neon.tech/>.
2. Crea un proyecto y una base de datos.
3. En **Connect**, copia la cadena de conexión de PostgreSQL. Comienza por `postgresql://`.
4. En RStudio ejecuta `usethis::edit_r_environ()` y agrega temporalmente:

```text
DATABASE_URL=postgresql://usuario:contraseña@servidor/base?sslmode=require
```

5. Reinicia RStudio y comprueba:

```r
nzchar(Sys.getenv("DATABASE_URL"))
```

Debe responder `TRUE`.

## 2. Migrar la información local

Con el archivo actual en `data/proyectos.sqlite`, ejecuta una sola vez:

```r
source("migrate_to_postgres.R")
```

La consola indicará cuántos proyectos, seguimientos, actuaciones y tareas fueron migrados. No vuelvas a ejecutar el migrador sobre los mismos datos porque las actuaciones y tareas podrían duplicarse.

## 3. Definir usuarios

La variable `APP_USERS` contiene los usuarios autorizados. Ejemplo para dos personas:

```json
[{"user":"jose","password":"CAMBIAR_CLAVE_1","admin":true},{"user":"jefe","password":"CAMBIAR_CLAVE_2","admin":false}]
```

Usa contraseñas distintas, largas y difíciles de adivinar. Esta variable se guardará como secreto en Posit; no debe agregarse a `.Renviron.example`, GitHub ni mensajes públicos.

## 4. Preparar GitHub

1. Crea un repositorio en GitHub sin incluir información sensible.
2. En RStudio ejecuta:

```r
source("prepare_deployment.R")
```

3. Sube el proyecto, incluido `manifest.json`.
4. Verifica que **no** se hayan subido `.Renviron` ni `data/proyectos.sqlite`.

## 5. Publicar en Posit Connect Cloud

1. Crea una cuenta en <https://connect.posit.cloud/>.
2. Selecciona **Publish** y conecta el repositorio de GitHub.
3. Selecciona el archivo `manifest.json` como contenido a publicar.
4. En **Secret variables**, agrega:

```text
DATABASE_URL
OPENAI_API_KEY
OPENAI_MODEL
APP_USERS
```

Para `OPENAI_MODEL` usa el modelo configurado en la versión local. En `APP_USERS`, pega el JSON completo de usuarios.

5. Pulsa **Publish**. Cuando finalice, abre el enlace y prueba el ingreso con cada usuario.

## 6. Comprobaciones antes de compartir

- Crea un proyecto de prueba.
- Ingresa con el segundo usuario y confirma que pueda verlo.
- Asigna una tarea y comprueba que aparezca en ambos usuarios.
- Revisa la sección **Actividad reciente del equipo**.
- Descarga el Excel y verifica sus cinco hojas.
- No compartas el enlace junto con las contraseñas en el mismo mensaje.

## Respaldo

La base principal estará en Neon. Conserva también una copia del archivo SQLite original hasta comprobar que toda la migración quedó correcta.
