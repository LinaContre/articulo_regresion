# =============================================================================
# anal_descrip.R
# Análisis descriptivo de la base de datos "Datos.xlsx"
# Repositorio: https://github.com/LinaContre/articulo_regresion

# ---- 0. Paquetes -------------------------------------------------------------
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl", repos = "https://cloud.r-project.org")
}
library(readxl)

# ---- 1. Lectura de la base de datos ------------------------------------------
url_datos <- "https://github.com/LinaContre/articulo_regresion/raw/main/Datos.xlsx"

# Busca el archivo localmente (ignorando mayúsculas/minúsculas) y, si no está,
# lo descarga del repositorio.
candidatos <- list.files(pattern = "^datos\\.xlsx$", ignore.case = TRUE,
                         recursive = TRUE, full.names = TRUE)

if (length(candidatos) > 0) {
  ruta <- candidatos[1]
  message("Leyendo base de datos local: ", ruta)
} else {
  ruta <- tempfile(fileext = ".xlsx")
  message("No se encontró Datos.xlsx localmente. Descargando desde GitHub...")
  download.file(url_datos, destfile = ruta, mode = "wb", quiet = TRUE)
}

datos <- as.data.frame(read_excel(ruta, sheet = 1))

# Limpieza básica: quitar filas y columnas completamente vacías
datos <- datos[rowSums(!is.na(datos)) > 0, , drop = FALSE]
datos <- datos[, colSums(!is.na(datos)) > 0, drop = FALSE]

# Variables de tipo texto o lógicas -> factor (categóricas)
for (v in names(datos)) {
  if (is.character(datos[[v]]) || is.logical(datos[[v]])) {
    datos[[v]] <- as.factor(datos[[v]])
  }
}

# ---- 2. Estructura general ---------------------------------------------------
cat("\n==================== ESTRUCTURA DE LOS DATOS ====================\n")
cat("Número de observaciones:", nrow(datos), "\n")
cat("Número de variables    :", ncol(datos), "\n\n")
str(datos)

cat("\nPrimeras filas:\n")
print(head(datos))

# Clasificación de variables
es_num <- sapply(datos, is.numeric)
vars_num <- names(datos)[es_num]
vars_cat <- names(datos)[!es_num]

cat("\nVariables numéricas   :", ifelse(length(vars_num) > 0,
    paste(vars_num, collapse = ", "), "ninguna"), "\n")
cat("Variables categóricas :", ifelse(length(vars_cat) > 0,
    paste(vars_cat, collapse = ", "), "ninguna"), "\n")

# ---- 3. Datos faltantes ------------------------------------------------------
cat("\n==================== DATOS FALTANTES ====================\n")
faltantes <- data.frame(
  variable   = names(datos),
  n_faltante = sapply(datos, function(x) sum(is.na(x))),
  porcentaje = round(100 * sapply(datos, function(x) mean(is.na(x))), 2),
  row.names  = NULL
)
print(faltantes)

# ---- 4. Resumen de variables numéricas ---------------------------------------
if (length(vars_num) > 0) {
  cat("\n==================== VARIABLES NUMÉRICAS ====================\n")

  moda <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    t <- table(x)
    as.numeric(names(t)[which.max(t)])
  }
  asimetria <- function(x) {
    x <- x[!is.na(x)]; n <- length(x)
    if (n < 3 || sd(x) == 0) return(NA)
    sum((x - mean(x))^3) / n / (sd(x)^3 * ((n - 1) / n)^1.5)
  }
  curtosis <- function(x) {
    x <- x[!is.na(x)]; n <- length(x)
    if (n < 4 || sd(x) == 0) return(NA)
    sum((x - mean(x))^4) / n / (sd(x)^4 * ((n - 1) / n)^2) - 3
  }

  resumen_num <- t(sapply(datos[vars_num], function(x) {
    c(n        = sum(!is.na(x)),
      media    = mean(x, na.rm = TRUE),
      mediana  = median(x, na.rm = TRUE),
      moda     = moda(x),
      desv_est = sd(x, na.rm = TRUE),
      varianza = var(x, na.rm = TRUE),
      CV_pct   = 100 * sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE)),
      minimo   = min(x, na.rm = TRUE),
      Q1       = unname(quantile(x, 0.25, na.rm = TRUE)),
      Q3       = unname(quantile(x, 0.75, na.rm = TRUE)),
      maximo   = max(x, na.rm = TRUE),
      rango    = diff(range(x, na.rm = TRUE)),
      IQR      = IQR(x, na.rm = TRUE),
      asimetria = asimetria(x),
      curtosis  = curtosis(x))
  }))
  print(round(resumen_num, 3))
}

# ---- 5. Resumen de variables categóricas -------------------------------------
if (length(vars_cat) > 0) {
  cat("\n==================== VARIABLES CATEGÓRICAS ====================\n")
  for (v in vars_cat) {
    cat("\nVariable:", v, "\n")
    tab <- table(datos[[v]], useNA = "ifany")
    print(data.frame(categoria  = names(tab),
                     frecuencia = as.vector(tab),
                     porcentaje = round(100 * as.vector(tab) / sum(tab), 2)),
          row.names = FALSE)
  }
}

# ---- 6. Correlaciones --------------------------------------------------------
if (length(vars_num) >= 2) {
  cat("\n==================== CORRELACIONES (Pearson) ====================\n")
  matriz_cor <- cor(datos[vars_num], use = "pairwise.complete.obs")
  print(round(matriz_cor, 3))
}

# ---- 7. Gráficos -------------------------------------------------------------
# Se muestran en pantalla y además se guardan en "graficos_descriptivo.pdf".
graficar <- function() {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  # Histogramas y boxplots de las variables numéricas
  for (v in vars_num) {
    x <- datos[[v]][!is.na(datos[[v]])]
    if (length(x) < 2) next
    par(mfrow = c(1, 2))
    hist(x, main = paste("Histograma de", v), xlab = v, ylab = "Frecuencia",
         col = "steelblue", border = "white")
    boxplot(x, main = paste("Boxplot de", v), ylab = v, col = "lightblue")
  }

  # Gráficos de barras de las variables categóricas
  for (v in vars_cat) {
    par(mfrow = c(1, 1))
    barplot(table(datos[[v]]), main = paste("Frecuencias de", v),
            xlab = v, ylab = "Frecuencia", col = "steelblue", las = 2)
  }

  # Boxplots de cada variable numérica según cada variable categórica
  # (solo si la categórica tiene entre 2 y 10 niveles)
  for (vc in vars_cat) {
    if (nlevels(droplevels(datos[[vc]])) < 2 ||
        nlevels(droplevels(datos[[vc]])) > 10) next
    for (vn in vars_num) {
      par(mfrow = c(1, 1))
      boxplot(datos[[vn]] ~ datos[[vc]], main = paste(vn, "según", vc),
              xlab = vc, ylab = vn, col = "lightblue", las = 2)
    }
  }

  # Matriz de dispersión (hasta 8 variables numéricas para que sea legible)
  if (length(vars_num) >= 2) {
    par(mfrow = c(1, 1))
    pairs(datos[vars_num[seq_len(min(8, length(vars_num)))]],
          main = "Matriz de dispersión", pch = 19,
          col = rgb(0.27, 0.51, 0.71, 0.6))
  }
}

graficar()  # en pantalla

ok <- tryCatch({
  pdf("graficos_descriptivo.pdf", width = 10, height = 6)
  graficar()
  dev.off()
  TRUE
}, error = function(e) {
  while (!is.null(dev.list()) && names(dev.cur()) == "pdf") dev.off()
  FALSE
})
if (ok) message("Gráficos guardados en: ", file.path(getwd(), "graficos_descriptivo.pdf"))

cat("\n==================== FIN DEL ANÁLISIS DESCRIPTIVO ====================\n")

