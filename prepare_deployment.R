if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

rsconnect::writeManifest(appDir = ".")
message("manifest.json creado. Ya puedes subir el proyecto a GitHub y publicarlo en Posit Connect Cloud.")
