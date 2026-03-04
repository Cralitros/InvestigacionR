# ============================================================================
# AUDITORÍA DE CALIDAD DE HISTORIAS CLÍNICAS - VERSIÓN PARA SHINYAPPS.IO
# CON DATOS PRE-CARGADOS Y OPCIÓN DE UPLOAD
# ============================================================================

# 1. CARGA DE LIBRERÍAS
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(writexl)
library(plotly)
library(corrplot)
library(DT)
library(shinyWidgets)

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

truncar_texto <- function(texto, max_caracteres = 40) {
  ifelse(nchar(texto) > max_caracteres,
         paste0(substr(texto, 1, max_caracteres), "..."),
         texto)
}

# ============================================================================
# DATOS ESTÁTICOS: TABLA DE PESOS
# ============================================================================
pesos_db <- tribble(
  ~SECCION, ~VARIABLE, ~PESO_COMPLETO, ~PESO_INCOMPLETO, ~PESO_NO_EXISTE, ~PESO_NO_APLICA,
  "ANAMNESIS_FILIACION", "Numero de historia clinica", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Nombres y apellidos del paciente", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Tipo seguro", 0.125, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Nro Seguro", 0.125, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Lugar de nacimiento", 0.125, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Fecha nacimiento", 0.125, 0, 0, 0,
  "ANAMNESIS_FILIACION", "edad", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "sexo", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "domicilio actual", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Luqar de Procedencia", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Documento de identificacion", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Estado Civil", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Grado de instrucción", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Ocupacion", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Religion", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Telefono", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Acompañante", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Domicilio y/o tetefono de la persona responsable", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Fecha de Ingreso", 0.25, 0, 0, 0,
  "ANAMNESIS_FILIACION", "Fecha de elaboración de historia cllnica", 0.25, 0, 0, 0,
  "ENFERMEDAD_ACTUAL", "Signos y Sintomas principales", 1, 0, 0, 0,
  "ENFERMEDAD_ACTUAL", "Tiempo de enfermedad", 1, 0, 0, 0,
  "ENFERMEDAD_ACTUAL", "Forma de inicio", 1, 0, 0, 0,
  "ENFERMEDAD_ACTUAL", "Curso de la enfermedad", 1, 0, 0, 0,
  "ENFERMEDAD_ACTUAL", "Relato Cronologico de la enfermedad", 3, 1, 0, 0,
  "ENFERMEDAD_ACTUAL", "Funciones Bioloqicas", 1, 0.5, 0, 0,
  "ENFERMEDAD_ACTUAL", "Antecedentes", 2, 1, 0, 0,
  "EXAMEN_CLINICO", "Funciones Vitales: Temperatura (T°). Frecuencia respiratoria (FR), Frecuencia cardiaca (FC), Presion arterial (PA)", 1, 0.5, 0, 0,
  "EXAMEN_CLINICO", "Peso, Talla, IMC", 1, 0.5, 0, 0,
  "EXAMEN_CLINICO", "Estado general, estado de hidratación, estado de nutricion, estado de conciencia, piel y anexos.", 1, 0.5, 0, 0,
  "EXAMEN_CLINICO", "Examen Clinico Regional", 4, 2, 0, 0,
  "DIAGNOSTICOS", "a) Presuntivo coherente y concordante.", 8, 4, 0, 0,
  "DIAGNOSTICOS", "b) Definitivo coherente y concordante", 8, 4, 0, 0,
  "DIAGNOSTICOS", "c) Uso del CIE 10", 4, 2, 0, 0,
  "PLAN_TRABAJO", "Examenes de Patologia Clinica pertinentes", 4, 2, 0, 0,
  "PLAN_TRABAJO", "Examenes de Diagnostico por imagenes pertinentes", 3, 1.5, 0, 0,
  "PLAN_TRABAJO", "Interconsultas pertinentes", 4, 2, 0, 0,
  "PLAN_TRABAJO", "Referencias Oportunas", 4, 2, 0, 0,
  "PLAN_TRABAJO", "Procedimientos diagnosticos y/o terapeuticos pertinentes", 4, 2, 0, 0,
  "TRATAMIENTO", "Regimen higienico-dietetico y medidas generales concordantes y coherentes", 4, 2, 0, 0,
  "TRATAMIENTO", "Nombre de medicamentos coherentes y concordantes con Denominacion Comun Internacional (DCI).", 4, 2, 0, 0,
  "TRATAMIENTO", "Consigna presentacion", 1, 0, 0, 0,
  "TRATAMIENTO", "Dosis del medicamento", 1, 0, 0, 0,
  "TRATAMIENTO", "Frecuencia del medicamento", 1, 0, 0, 0,
  "TRATAMIENTO", "Via de administracion", 1, 0, 0, 0,
  "TRATAMIENTO", "Cuidados de Enfermeria y otros profesionales", 2, 0, 0, 0,
  "NOTAS_EVOLUCION", "Fecha de evolucion", 0.25, 0, 0, 0,
  "NOTAS_EVOLUCION", "Hora de evolucion", 0.25, 0, 0, 0,
  "NOTAS_EVOLUCION", "Apreciacibn subjetiva", 0.5, 0, 0, 0,
  "NOTAS_EVOLUCION", "Apreciacion objetiva", 0.5, 0, 0, 0,
  "NOTAS_EVOLUCION", "Verificacion de tratamiento y dieta", 0.5, 0, 0, 0,
  "NOTAS_EVOLUCION", "Interpretacion de examenees de apoyo al diagnostico y comentario", 0.5, 0.25, 0, 0,
  "NOTAS_EVOLUCION", "Plan diagnostico", 0.5, 0, 0, 0,
  "NOTAS_EVOLUCION", "Plan terapeutico", 0.5, 0, 0, 0,
  "NOTAS_EVOLUCION", "Firma del medico que evoluciona", 0.25, 0, 0, 0,
  "NOTAS_EVOLUCION", "sello del medico que evoluciona", 0.25, 0, 0, 0,
  "REGISTROS_ENFERMERIA", "Notas de Ingreso de enfermeria/obstetricia", 1, 0, 0, 0,
  "REGISTROS_ENFERMERIA", "Notas de Evolucion de enfermeria/obstetricia", 1, 0.5, 0, 0,
  "REGISTROS_ENFERMERIA", "Hoja de Grafica de Signos vitales", 1, 0.5, 0, 0,
  "REGISTROS_ENFERMERIA", "Hoja de balance hidrico", 1, 0, 0, 0,
  "REGISTROS_ENFERMERIA", "Kardex", 1, 0, 0, 0,
  "REGISTROS_ENFERMERIA", "Firma del Profesional", 0.5, 0, 0, 0,
  "REGISTROS_ENFERMERIA", "Sello del Profesional", 0.5, 0, 0, 0,
  "INDICACIONES_ALTA", "Informe de Alta", 1, 0, 0, 0,
  "INDICACIONES_ALTA", "Medicamentos prescritos", 1, 0, 0, 0,
  "INDICACIONES_ALTA", "Cuidados generales e indicaciones de reevaluacion posterior por consulta externa", 1, 0, 0, 0,
  "ATRIBUTOS_HC", "Firma del medico tratante", 0.5, 0, 0, 0,
  "ATRIBUTOS_HC", "Sello del medico tratante", 0.5, 0, 0, 0,
  "ATRIBUTOS_HC", "Orden cronologico de las hojas de la historia clinica", 1, 0, 0, 0,
  "ATRIBUTOS_HC", "Pulcritud", 1, 0, 0, 0,
  "ATRIBUTOS_HC", "Legibilidad", 1, 0, 0, 0,
  "ATRIBUTOS_HC", "No uso de abreviaturas", 1, 0, 0, 0,
  "FORMATOS_ESPECIALES", "Formato de interconsulta", 0.5, 0.25, 0, 0,
  "FORMATOS_ESPECIALES", "Formato de orden de intervencion quirurgica", 0.5, 0, 0, 0,
  "FORMATOS_ESPECIALES", "Reporte operatorio", 0.5, 0, 0, 0,
  "FORMATOS_ESPECIALES", "Hoia de evolucion pre anestesica", 0.5, 0, 0, 0,
  "FORMATOS_ESPECIALES", "Lista de verificacion de seguridad de la cirugia", 1, 0, 0, 0,
  "FORMATOS_ESPECIALES", "Hoja de anestesia", 0.5, 0, 0, 0,
  "FORMATOS_ESPECIALES", "Hoja post anestesica", 0.5, 0, 0, 0,
  "FORMATOS_ESPECIALES", "Formatos de patologia clinica formato de diagnostico por imágenes", 0.5, 0, 0, 0,
  "FORMATOS_ESPECIALES", "Formato de anatomia patologica", 0.5, 0.25, 0, 0,
  "FORMATOS_ESPECIALES", "Formato de consentimiento informado", 1, 0.25, 0, 0,
  "FORMATOS_ESPECIALES", "Formato de retiro voluntano ", 0.5, 0, 0, 0,
  "FORMATOS_ESPECIALES", "Epicrisis", 1, 0, 0, 0
)

nombres_secciones <- c(
  "ANAMNESIS_FILIACION" = "📋 Anamnesis y Filiación",
  "ENFERMEDAD_ACTUAL" = "🏥 Enfermedad Actual",
  "EXAMEN_CLINICO" = "🩺 Examen Clínico",
  "DIAGNOSTICOS" = "🔬 Diagnósticos",
  "PLAN_TRABAJO" = "📝 Plan de Trabajo",
  "TRATAMIENTO" = "💊 Tratamiento",
  "NOTAS_EVOLUCION" = "📓 Notas de Evolución",
  "REGISTROS_ENFERMERIA" = "👩‍⚕️ Registros Enfermería",
  "INDICACIONES_ALTA" = "🏠 Indicaciones de Alta",
  "ATRIBUTOS_HC" = "📄 Atributos de la HC",
  "FORMATOS_ESPECIALES" = "📑 Formatos Especiales"
)

# ============================================================================
# FUNCIONES DE CARGA DE DATOS
# ============================================================================

# Función para procesar un archivo individual
procesar_archivo <- function(ruta, seccion, es_url = FALSE) {
  
  if(es_url) {
    df <- tryCatch({
      read_csv2(ruta, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    }, error = function(e) NULL)
  } else {
    df <- tryCatch({
      read_csv2(ruta, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    }, error = function(e) NULL)
  }
  
  if(is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>%
    pivot_longer(cols = -ITEM, names_to = "VARIABLE", values_to = "CALIFICACION") %>%
    mutate(
      SECCION = seccion,
      CALIFICACION = as.numeric(CALIFICACION),
      CALIFICACION_TXT = case_when(
        CALIFICACION == 1 ~ "COMPLETO",
        CALIFICACION == 2 ~ "INCOMPLETO",
        CALIFICACION == 3 ~ "NO EXISTE",
        CALIFICACION == 4 ~ "NO APLICA",
        TRUE ~ "OTRO"
      )
    ) %>%
    filter(!is.na(CALIFICACION), CALIFICACION %in% c(1, 2, 3, 4)) %>%
    select(ITEM, SECCION, VARIABLE, CALIFICACION, CALIFICACION_TXT)
}

# Cargar datos pre-definidos desde carpeta data/
cargar_datos_predefinidos <- function() {
  
  archivos <- list(
    "ANAMNESIS_FILIACION" = "data/ANAMNESIS FILIACION.csv",
    "ENFERMEDAD_ACTUAL" = "data/ENFERMEDAD ACTUAL Y ANTECEDENTES.csv",
    "EXAMEN_CLINICO" = "data/EXAMEN CLINICO GENERAL.csv",
    "DIAGNOSTICOS" = "data/DIAGNOSTICOS.csv",
    "PLAN_TRABAJO" = "data/PLAN DE TRABAJO.csv",
    "TRATAMIENTO" = "data/TRATAMIENTO.csv",
    "NOTAS_EVOLUCION" = "data/NOTAS DE EVOLUCION.csv",
    "REGISTROS_ENFERMERIA" = "data/REGISTROS DE ENFERMERIA OBSTETRICIA.csv",
    "ATRIBUTOS_HC" = "data/ATRIBUTOS DE LA HISTORIA CLINICA.csv",
    "FORMATOS_ESPECIALES" = "data/FORMATOS ESPECIALES.csv",
    "INDICACIONES_ALTA" = "data/ESPECIFICA INDICACIONES DE ALTA.csv"
  )
  
  lista_datos <- lapply(names(archivos), function(seccion) {
    procesar_archivo(archivos[[seccion]], seccion, es_url = FALSE)
  })
  
  datos_crudos <- bind_rows(lista_datos)
  
  if(is.null(datos_crudos) || nrow(datos_crudos) == 0) return(NULL)
  
  procesar_datos_completos(datos_crudos)
}

# Cargar desde archivos subidos por el usuario
cargar_desde_upload <- function(archivos) {
  
  procesar_archivo_upload <- function(ruta, nombre_archivo) {
    seccion <- NA
    if(grepl("ANAMNESIS", nombre_archivo, ignore.case = TRUE)) seccion <- "ANAMNESIS_FILIACION"
    else if(grepl("ENFERMEDAD", nombre_archivo, ignore.case = TRUE)) seccion <- "ENFERMEDAD_ACTUAL"
    else if(grepl("EXAMEN", nombre_archivo, ignore.case = TRUE)) seccion <- "EXAMEN_CLINICO"
    else if(grepl("DIAGNOSTICO", nombre_archivo, ignore.case = TRUE)) seccion <- "DIAGNOSTICOS"
    else if(grepl("PLAN", nombre_archivo, ignore.case = TRUE)) seccion <- "PLAN_TRABAJO"
    else if(grepl("TRATAMIENTO", nombre_archivo, ignore.case = TRUE)) seccion <- "TRATAMIENTO"
    else if(grepl("EVOLUCION", nombre_archivo, ignore.case = TRUE)) seccion <- "NOTAS_EVOLUCION"
    else if(grepl("ENFERMERIA", nombre_archivo, ignore.case = TRUE)) seccion <- "REGISTROS_ENFERMERIA"
    else if(grepl("ATRIBUTO", nombre_archivo, ignore.case = TRUE)) seccion <- "ATRIBUTOS_HC"
    else if(grepl("FORMATO", nombre_archivo, ignore.case = TRUE)) seccion <- "FORMATOS_ESPECIALES"
    else if(grepl("ALTA", nombre_archivo, ignore.case = TRUE)) seccion <- "INDICACIONES_ALTA"
    
    if(is.na(seccion)) return(NULL)
    
    procesar_archivo(ruta, seccion, es_url = FALSE)
  }
  
  lista_datos <- lapply(1:nrow(archivos), function(i) {
    procesar_archivo_upload(archivos$datapath[i], archivos$name[i])
  })
  
  datos_crudos <- bind_rows(lista_datos)
  
  if(is.null(datos_crudos) || nrow(datos_crudos) == 0) return(NULL)
  
  procesar_datos_completos(datos_crudos)
}

# Procesamiento común de datos
procesar_datos_completos <- function(datos_crudos) {
  
  datos_con_pesos <- datos_crudos %>%
    left_join(pesos_db, by = c("SECCION", "VARIABLE")) %>%
    mutate(
      PESO_COMPLETO = ifelse(is.na(PESO_COMPLETO), 0, PESO_COMPLETO),
      PESO_INCOMPLETO = ifelse(is.na(PESO_INCOMPLETO), 0, PESO_INCOMPLETO),
      PESO_NO_EXISTE = ifelse(is.na(PESO_NO_EXISTE), 0, PESO_NO_EXISTE),
      PESO_NO_APLICA = ifelse(is.na(PESO_NO_APLICA), 0, PESO_NO_APLICA),
      PUNTAJE_OBTENIDO = case_when(
        CALIFICACION == 1 ~ PESO_COMPLETO,
        CALIFICACION == 2 ~ PESO_INCOMPLETO,
        CALIFICACION == 3 ~ PESO_NO_EXISTE,
        CALIFICACION == 4 ~ PESO_NO_APLICA,
        TRUE ~ 0
      ),
      PUNTAJE_MAXIMO = ifelse(CALIFICACION != 4, PESO_COMPLETO, 0)
    )
  
  score_por_hc <- datos_con_pesos %>%
    group_by(ITEM) %>%
    summarise(
      PUNTAJE_TOTAL_OBTENIDO = sum(PUNTAJE_OBTENIDO, na.rm = TRUE),
      PUNTAJE_TOTAL_MAXIMO = sum(PUNTAJE_MAXIMO, na.rm = TRUE),
      PORCENTAJE_COMPLETUD = ifelse(PUNTAJE_TOTAL_MAXIMO > 0, 
                                    round((PUNTAJE_TOTAL_OBTENIDO / PUNTAJE_TOTAL_MAXIMO) * 100, 2), 0),
      CALIDAD_GLOBAL = case_when(
        PORCENTAJE_COMPLETUD >= 90 ~ "EXCELENTE",
        PORCENTAJE_COMPLETUD >= 70 ~ "ACEPTABLE",
        PORCENTAJE_COMPLETUD >= 50 ~ "DEFICIENTE",
        TRUE ~ "INADECUADA"
      ),
      .groups = "drop"
    )
  
  resumen_general <- score_por_hc %>%
    summarise(
      TOTAL_HISTORIAS = n(),
      PROMEDIO_PUNTAJE = round(mean(PORCENTAJE_COMPLETUD), 2),
      EXCELENTES = sum(CALIDAD_GLOBAL == "EXCELENTE"),
      ACEPTABLES = sum(CALIDAD_GLOBAL == "ACEPTABLE"),
      DEFICIENTES = sum(CALIDAD_GLOBAL == "DEFICIENTE"),
      INADECUADAS = sum(CALIDAD_GLOBAL == "INADECUADA")
    ) %>%
    mutate(
      PORC_EXCELENTES = round((EXCELENTES / TOTAL_HISTORIAS) * 100, 2),
      PORC_ACEPTABLES = round((ACEPTABLES / TOTAL_HISTORIAS) * 100, 2),
      PORC_DEFICIENTES = round((DEFICIENTES / TOTAL_HISTORIAS) * 100, 2),
      PORC_INADECUADAS = round((INADECUADAS / TOTAL_HISTORIAS) * 100, 2)
    )
  
  resumen_por_seccion <- datos_con_pesos %>%
    group_by(SECCION) %>%
    summarise(
      PUNTAJE_OBTENIDO_SECCION = sum(PUNTAJE_OBTENIDO, na.rm = TRUE),
      PUNTAJE_MAXIMO_SECCION = sum(PUNTAJE_MAXIMO, na.rm = TRUE),
      PORCENTAJE_SECCION = ifelse(PUNTAJE_MAXIMO_SECCION > 0,
                                  round((PUNTAJE_OBTENIDO_SECCION / PUNTAJE_MAXIMO_SECCION) * 100, 2), 0),
      .groups = "drop"
    ) %>%
    arrange(desc(PORCENTAJE_SECCION))
  
  historias_fallando <- score_por_hc %>%
    filter(CALIDAD_GLOBAL %in% c("DEFICIENTE", "INADECUADA")) %>%
    arrange(PORCENTAJE_COMPLETUD)
  
  items_mas_criticos <- datos_con_pesos %>%
    filter(ITEM %in% historias_fallando$ITEM) %>%
    filter(CALIFICACION %in% c(2, 3)) %>%
    group_by(SECCION, VARIABLE) %>%
    summarise(TOTAL_FALLAS = n(), .groups = "drop") %>%
    arrange(desc(TOTAL_FALLAS)) %>%
    head(20)
  
  list(
    datos_con_pesos = datos_con_pesos,
    score_por_hc = score_por_hc,
    resumen_general = resumen_general,
    resumen_por_seccion = resumen_por_seccion,
    historias_fallando = historias_fallando,
    items_mas_criticos = items_mas_criticos
  )
}

# ============================================================================
# UI (INTERFAZ DE USUARIO)
# ============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Auditoría Calidad HC", titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("📂 Cargar Datos", tabName = "carga", icon = icon("folder-open")),
      menuItem("📊 Dashboard General", tabName = "dashboard", icon = icon("chart-pie")),
      menuItem("🏥 Análisis por Sección", tabName = "secciones", icon = icon("hospital")),
      menuItem("📈 Gráficos por Sección", tabName = "graficos_seccion", icon = icon("chart-bar")),
      menuItem("⚠️ Items Críticos", tabName = "criticos", icon = icon("exclamation-triangle")),
      menuItem("📥 Exportar Reporte", tabName = "exportar", icon = icon("file-excel"))
    ),
    
    box(
      title = "Información",
      width = NULL,
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      HTML("Seleccione cómo cargar los datos para comenzar el análisis.")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box {
          height: 100%;
          display: flex;
          flex-direction: column;
        }
        .box-body {
          flex: 1;
          overflow: auto;
          padding: 10px;
        }
        .box-title {
          padding: 10px 15px;
          font-size: 14px;
          font-weight: bold;
        }
        .plotly {
          height: 100% !important;
        }
        .dataTables_wrapper {
          font-size: 11px;
          padding: 5px;
        }
        .dataTables_wrapper .dataTables_scrollBody {
          max-height: 450px !important;
        }
        table.dataTable {
          font-size: 11px;
          width: 100% !important;
        }
        table.dataTable th, table.dataTable td {
          padding: 6px 10px;
          white-space: nowrap;
        }
        .info-box {
          min-height: 80px;
          margin-bottom: 15px;
        }
        .btn {
          margin: 5px;
        }
        .fluid-row {
          margin-left: 0;
          margin-right: 0;
        }
        .col-sm-7, .col-sm-5, .col-sm-6, .col-sm-4 {
          padding-left: 10px;
          padding-right: 10px;
        }
        .box-body {
          overflow-x: auto;
        }
      "))
    ),
    
    tabItems(
      # PESTAÑA 1: CARGA - HÍBRIDA (PRE-CARGADO + UPLOAD)
      tabItem(tabName = "carga",
              fluidRow(
                box(width = 12, title = "📂 Opciones de Carga", status = "primary", solidHeader = TRUE,
                    radioButtons("tipo_carga", "Seleccione cómo cargar los datos:",
                                 choices = c("📁 Usar datos pre-cargados" = "predefinido",
                                             "📤 Subir archivos nuevos" = "upload"),
                                 selected = "predefinido",
                                 inline = TRUE)
                )
              ),
              fluidRow(
                box(width = 12, title = "📤 Subir Archivos CSV", status = "warning", solidHeader = TRUE,
                    conditionalPanel(
                      condition = "input.tipo_carga == 'upload'",
                      fileInput("archivos_hc", "Seleccione los archivos CSV de la auditoría:",
                                multiple = TRUE,
                                accept = c(".csv"),
                                buttonLabel = "Seleccionar Archivos"),
                      p(class = "text-muted", "💡 Suba los 11 archivos CSV para procesar nuevos datos.")
                    ),
                    conditionalPanel(
                      condition = "input.tipo_carga == 'predefinido'",
                      div(class = "alert alert-info",
                          icon("info-circle"), " Usando datos pre-cargados del sistema. ",
                          "Para usar otros datos, seleccione 'Subir archivos nuevos'."
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "📊 Estado de Carga", status = "success",
                    uiOutput("mensaje_carga")
                )
              )
      ),
      
      # PESTAÑA 2: DASHBOARD
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("kpi_total", width = 3),
                valueBoxOutput("kpi_promedio", width = 3),
                valueBoxOutput("kpi_excelentes", width = 3),
                valueBoxOutput("kpi_criticos", width = 3)
              ),
              fluidRow(
                box(width = 4, title = "🍩 Distribución de Calidad", status = "success", solidHeader = TRUE,
                    plotlyOutput("plot_donut", height = "400px")
                ),
                box(width = 4, title = "📊 Indicador General", status = "success", solidHeader = TRUE,
                    plotlyOutput("plot_gauge", height = "400px")
                ),
                box(width = 4, title = "📈 Histograma de Puntajes", status = "success", solidHeader = TRUE,
                    plotlyOutput("plot_hist", height = "400px")
                )
              ),
              fluidRow(
                box(width = 12, title = "📊 Desempeño por Sección (Ranking)", status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_barras_seccion", height = "550px")
                )
              )
      ),
      
      # PESTAÑA 3: ANÁLISIS POR SECCIÓN
      tabItem(tabName = "secciones",
              fluidRow(
                box(width = 6, title = "📈 Perfil de Calidad (Líneas)", status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_perfil_lineas", height = "450px")
                ),
                box(width = 6, title = "🔥 Mapa de Calor", status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_heatmap", height = "450px")
                )
              ),
              fluidRow(
                box(width = 12, title = "🎻 Distribución (Violín + Boxplot)", status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_violin", height = "550px")
                )
              ),
              fluidRow(
                box(width = 12, title = "🗺️ Treemap: Peso vs Calidad", status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_treemap", height = "550px")
                )
              )
      ),
      
      # PESTAÑA 4: GRÁFICOS POR SECCIÓN CON TABLAS
      tabItem(tabName = "graficos_seccion",
              fluidRow(
                box(width = 4, title = "⚙️ Configuración", status = "primary", solidHeader = TRUE,
                    selectInput("seccion_seleccionada", 
                                "Seleccione la sección:",
                                choices = nombres_secciones,
                                selected = "TRATAMIENTO",
                                selectize = TRUE,
                                width = "100%"),
                    sliderInput("longitud_texto", 
                                "Longitud del texto:",
                                min = 20, max = 80, value = 35, step = 5,
                                width = "100%")
                ),
                box(width = 8, title = "📊 Resumen de la Sección", status = "info", solidHeader = TRUE,
                    uiOutput("resumen_seccion_ui")
                )
              ),
              
              fluidRow(
                box(width = 7, title = "📊 Distribución de Calificaciones por Ítem", status = "success", solidHeader = TRUE,
                    plotlyOutput("plot_barras_seccion_detalle", height = "650px")
                ),
                box(width = 5, title = "📋 Tabulación de Datos", status = "warning", solidHeader = TRUE,
                    DT::dataTableOutput("tabla_distribucion_items"),
                    div(style = "padding: 10px;",
                        downloadButton("download_tabla_distribucion", "📥 Descargar CSV", class = "btn-success")
                    )
                )
              ),
              
              fluidRow(
                box(width = 6, title = "📈 Porcentaje de Completitud", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_completitud_items", height = "550px")
                ),
                box(width = 6, title = "🔴 Items con Más Problemas", status = "danger", solidHeader = TRUE,
                    plotlyOutput("plot_items_problemas", height = "550px")
                )
              ),
              
              fluidRow(
                box(width = 12, title = "📋 Tabulación Completa - Completitud y Problemas", status = "info", solidHeader = TRUE,
                    DT::dataTableOutput("tabla_completitud_problemas")
                )
              )
      ),
      
      # PESTAÑA 5: ITEMS CRÍTICOS
      tabItem(tabName = "criticos",
              fluidRow(
                box(width = 8, title = "⚠️ Top 20 Ítems con Más Problemas", status = "danger", solidHeader = TRUE,
                    plotlyOutput("plot_top_criticos", height = "650px")
                ),
                box(width = 4, title = "📋 Historias con Mayor Riesgo", status = "danger", solidHeader = TRUE,
                    DT::dataTableOutput("tabla_historias_fallando")
                )
              ),
              fluidRow(
                box(width = 12, title = "🔗 Matriz de Correlación entre Secciones", status = "warning", solidHeader = TRUE,
                    plotOutput("plot_corr", height = "650px")
                )
              )
      ),
      
      # PESTAÑA 6: EXPORTAR
      tabItem(tabName = "exportar",
              fluidRow(
                box(width = 12, title = "💾 Descargar Resultados", status = "success", solidHeader = TRUE,
                    p("Descargue el archivo Excel con todas las hojas de cálculo procesadas y los datos crudos."),
                    br(),
                    downloadButton("download_excel", "📥 Descargar Excel Completo", class = "btn-lg btn-success", icon = icon("file-excel")),
                    br(), br(),
                    downloadButton("download_graficos_seccion", "📥 Descargar Gráficos (PDF)", class = "btn-lg btn-primary", icon = icon("file-pdf"))
                )
              )
      )
    )
  )
)

# ============================================================================
# SERVER (LÓGICA)
# ============================================================================

server <- function(input, output, session) {
  
  # DATOS PROCESADOS - HÍBRIDO (PRE-CARGADO O UPLOAD)
  datos_procesados <- reactive({
    
    if(input$tipo_carga == "predefinido") {
      # CARGAR DATOS PRE-DEFINIDOS
      datos <- cargar_datos_predefinidos()
      if(is.null(datos)) {
        showNotification("⚠️ No se encontraron los archivos en la carpeta data/. Verifique la estructura.", type = "warning", duration = 10)
      }
      return(datos)
      
    } else {
      # CARGAR DESDE ARCHIVOS SUBIDOS
      req(input$archivos_hc)
      return(cargar_desde_upload(input$archivos_hc))
    }
  })
  
  # MENSAJE DE CARGA
  output$mensaje_carga <- renderUI({
    
    if(input$tipo_carga == "predefinido") {
      tryCatch({
        d <- datos_procesados()
        if(is.null(d)) {
          return(tagList(
            p("❌ No se encontraron los archivos pre-cargados."),
            p(class = "text-muted", "Verifique que la carpeta 'data/' exista con los 11 archivos CSV.")
          ))
        }
        tagList(
          p("✅ Datos pre-cargados correctamente."),
          p(strong("📊 Total Historias: "), d$resumen_general$TOTAL_HISTORIAS),
          p(strong("📈 Promedio General: "), paste0(d$resumen_general$PROMEDIO_PUNTAJE, "%"))
        )
      }, error = function(e) {
        p("❌ Error interno: ", e$message)
      })
      
    } else {
      if(is.null(input$archivos_hc)) {
        return(p("⏳ Esperando archivos..."))
      }
      tryCatch({
        d <- datos_procesados()
        if(is.null(d)) {
          return(p("❌ Error al procesar. Verifique los archivos."))
        }
        tagList(
          p("✅ Datos cargados correctamente."),
          p(strong("📊 Total Historias: "), d$resumen_general$TOTAL_HISTORIAS),
          p(strong("📈 Promedio General: "), paste0(d$resumen_general$PROMEDIO_PUNTAJE, "%"))
        )
      }, error = function(e) {
        p("❌ Error interno: ", e$message)
      })
    }
  })
  
  # KPIS
  output$kpi_total <- renderValueBox({
    req(datos_procesados())
    d <- datos_procesados()
    valueBox(d$resumen_general$TOTAL_HISTORIAS, "Historias Evaluadas", icon = icon("file-medical"), color = "blue")
  })
  
  output$kpi_promedio <- renderValueBox({
    req(datos_procesados())
    d <- datos_procesados()
    color <- ifelse(d$resumen_general$PROMEDIO_PUNTAJE >= 80, "green", ifelse(d$resumen_general$PROMEDIO_PUNTAJE >= 60, "yellow", "red"))
    valueBox(paste0(d$resumen_general$PROMEDIO_PUNTAJE, "%"), "Calidad Promedio", icon = icon("chart-line"), color = color)
  })
  
  output$kpi_excelentes <- renderValueBox({
    req(datos_procesados())
    d <- datos_procesados()
    valueBox(d$resumen_general$EXCELENTES, "Historias Excelentes", icon = icon("star"), color = "green")
  })
  
  output$kpi_criticos <- renderValueBox({
    req(datos_procesados())
    d <- datos_procesados()
    valueBox(d$resumen_general$INADECUADAS, "Historias Inadecuadas", icon = icon("exclamation-circle"), color = "red")
  })
  
  # GRÁFICOS DASHBOARD
  output$plot_donut <- renderPlotly({
    req(datos_procesados())
    d <- datos_procesados()
    datos_anillo <- data.frame(
      Categoria = c("EXCELENTE", "ACEPTABLE", "DEFICIENTE", "INADECUADA"),
      Cantidad = c(d$resumen_general$EXCELENTES, d$resumen_general$ACEPTABLES, 
                   d$resumen_general$DEFICIENTES, d$resumen_general$INADECUADAS)
    )
    
    fig <- plot_ly(datos_anillo, labels = ~Categoria, values = ~Cantidad, type = 'pie', hole = 0.4,
                   marker = list(colors = c("#2ecc71", "#3498db", "#f39c12", "#e74c3c")),
                   textinfo = 'label+percent', hoverinfo = 'label+value+percent')
    fig %>% layout(title = "Distribución de Calidad", showlegend = TRUE, margin = list(t = 40, b = 40))
  })
  
  output$plot_gauge <- renderPlotly({
    req(datos_procesados())
    d <- datos_procesados()
    valor <- d$resumen_general$PROMEDIO_PUNTAJE
    
    fig <- plot_ly() %>%
      add_trace(
        type = 'indicator',
        mode = "gauge+number+delta",
        value = valor,
        domain = list(x = c(0, 1), y = c(0, 1)),
        title = list(text = "Calidad General", font = list(size = 14)),
        gauge = list(
          axis = list(range = list(NULL, 100), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          bgcolor = "white",
          borderwidth = 2,
          bordercolor = "gray",
          steps = list(
            list(range = c(0, 50), color = "#e74c3c"),
            list(range = c(50, 70), color = "#f39c12"),
            list(range = c(70, 90), color = "#3498db"),
            list(range = c(90, 100), color = "#2ecc71")
          ),
          threshold = list(
            line = list(color = "red", width = 4),
            thickness = 0.75,
            value = 90
          )
        )
      )
    
    fig %>% layout(margin = list(l = 20, r = 20, t = 50, b = 20))
  })
  
  output$plot_hist <- renderPlotly({
    req(datos_procesados())
    d <- datos_procesados()
    fig <- plot_ly(x = d$score_por_hc$PORCENTAJE_COMPLETUD, type = 'histogram', nbinsx = 20,
                   marker = list(color = '#3498db', line = list(color = 'white', width = 1)))
    fig %>% layout(title = "Distribución de Puntajes", xaxis = list(title = "Porcentaje"), yaxis = list(title = "Frecuencia"),
                   margin = list(t = 40, b = 40))
  })
  
  output$plot_barras_seccion <- renderPlotly({
    req(datos_procesados())
    d <- datos_procesados()
    fig <- plot_ly(d$resumen_por_seccion, x = ~PORCENTAJE_SECCION, y = ~SECCION, type = 'bar', orientation = 'h',
                   marker = list(color = ~PORCENTAJE_SECCION, colorscale = 'RdYlGn', showscale = TRUE))
    fig %>% layout(title = "Desempeño por Sección", xaxis = list(title = "% Completitud"), yaxis = list(title = ""),
                   margin = list(l = 150, t = 40, b = 40))
  })
  
  output$plot_perfil_lineas <- renderPlotly({
    req(datos_procesados())
    d <- datos_procesados()
    d$resumen_por_seccion$SECCION_SHORT <- substr(d$resumen_por_seccion$SECCION, 1, 15)
    
    fig <- plot_ly(d$resumen_por_seccion, x = ~SECCION_SHORT, y = ~PORCENTAJE_SECCION, type = 'scatter', mode = 'lines+markers',
                   line = list(color = '#3498db', width = 3), marker = list(size = 10))
    fig %>% layout(title = "Perfil de Calidad", yaxis = list(range = c(0, 100)),
                   xaxis = list(tickangle = -45), margin = list(t = 40, b = 80))
  })
  
  output$plot_heatmap <- renderPlotly({
    req(datos_procesados())
    d <- datos_procesados()
    datos_heatmap <- d$datos_con_pesos %>%
      group_by(SECCION, CALIFICACION_TXT) %>% summarise(CUENTOS = n(), .groups = 'drop') %>%
      pivot_wider(names_from = CALIFICACION_TXT, values_from = CUENTOS, values_fill = 0)
    
    cols_num <- names(datos_heatmap)[!names(datos_heatmap) %in% c("SECCION")]
    matriz <- as.matrix(datos_heatmap[, cols_num])
    rownames(matriz) <- datos_heatmap$SECCION
    
    fig <- plot_ly(z = matriz, x = cols_num, y = rownames(matriz), type = 'heatmap', colors = 'RdYlGn')
    fig %>% layout(title = "Frecuencia de Calificaciones", margin = list(l = 150, t = 40, b = 40))
  })
  
  output$plot_violin <- renderPlotly({
    req(datos_procesados())
    d <- datos_procesados()
    score_seccion_hc <- d$datos_con_pesos %>%
      group_by(ITEM, SECCION) %>%
      summarise(PCT_SECCION = ifelse(sum(PUNTAJE_MAXIMO) > 0, sum(PUNTAJE_OBTENIDO) / sum(PUNTAJE_MAXIMO) * 100, 0), .groups = 'drop')
    
    fig <- plot_ly(score_seccion_hc, y = ~PCT_SECCION, x = ~SECCION, type = 'violin',
                   box = list(visible = TRUE), meanline = list(visible = TRUE))
    fig %>% layout(title = "Distribución y Variabilidad por Sección", yaxis = list(title = "% Completitud"),
                   xaxis = list(tickangle = -45), margin = list(t = 40, b = 80))
  })
  
  output$plot_treemap <- renderPlotly({
    req(datos_procesados())
    d <- datos_procesados()
    peso_seccion <- pesos_db %>% group_by(SECCION) %>% summarise(PESO_MAXIMO = sum(PESO_COMPLETO), .groups = 'drop')
    treemap_data <- peso_seccion %>% left_join(d$resumen_por_seccion, by = "SECCION")
    treemap_data$Calidad <- cut(treemap_data$PORCENTAJE_SECCION, breaks = c(0, 50, 70, 90, 100), labels = c("Inadecuada", "Deficiente", "Aceptable", "Excelente"))
    
    fig <- plot_ly(treemap_data, labels = ~SECCION, parents = ~"", values = ~PESO_MAXIMO, color = ~Calidad,
                   type = 'treemap', hoverinfo = 'label+value+percent parent')
    fig %>% layout(title = "Peso e Importancia de Secciones", margin = list(t = 40, b = 20))
  })
  
  # RESUMEN DE SECCIÓN
  output$resumen_seccion_ui <- renderUI({
    req(datos_procesados())
    req(input$seccion_seleccionada)
    
    d <- datos_procesados()
    seccion_codigo <- names(nombres_secciones)[nombres_secciones == input$seccion_seleccionada]
    
    resumen_seccion <- d$resumen_por_seccion %>% filter(SECCION == seccion_codigo)
    
    if(nrow(resumen_seccion) == 0) {
      return(p("No hay datos para esta sección"))
    }
    
    tagList(
      fluidRow(
        column(4, 
               infoBox("Puntaje Obtenido", round(resumen_seccion$PUNTAJE_OBTENIDO_SECCION, 2), 
                       icon = icon("check-circle"), color = "green", fill = TRUE)
        ),
        column(4,
               infoBox("Puntaje Máximo", round(resumen_seccion$PUNTAJE_MAXIMO_SECCION, 2), 
                       icon = icon("star"), color = "blue", fill = TRUE)
        ),
        column(4,
               infoBox("Porcentaje", paste0(resumen_seccion$PORCENTAJE_SECCION, "%"), 
                       icon = icon("chart-line"), 
                       color = ifelse(resumen_seccion$PORCENTAJE_SECCION >= 80, "green", 
                                      ifelse(resumen_seccion$PORCENTAJE_SECCION >= 60, "yellow", "red")), 
                       fill = TRUE)
        )
      )
    )
  })
  
  # GRÁFICO 1: Distribución de calificaciones
  output$plot_barras_seccion_detalle <- renderPlotly({
    req(datos_procesados())
    req(input$seccion_seleccionada)
    
    d <- datos_procesados()
    seccion_codigo <- names(nombres_secciones)[nombres_secciones == input$seccion_seleccionada]
    max_car <- input$longitud_texto
    
    items_unicos <- d$datos_con_pesos %>%
      filter(SECCION == seccion_codigo) %>%
      distinct(VARIABLE) %>%
      pull(VARIABLE)
    
    tratamiento_detalle <- d$datos_con_pesos %>%
      filter(SECCION == seccion_codigo) %>%
      group_by(VARIABLE, CALIFICACION_TXT) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      tidyr::complete(VARIABLE = items_unicos,
                      CALIFICACION_TXT = c("COMPLETO", "INCOMPLETO", "NO EXISTE", "NO APLICA"),
                      fill = list(Cantidad = 0)) %>%
      mutate(
        ITEM_NUM = match(VARIABLE, items_unicos),
        VARIABLE_CORTA = paste0(ITEM_NUM, ". ", truncar_texto(VARIABLE, max_car)),
        VARIABLE_COMPLETA = VARIABLE,
        CALIFICACION_TXT = factor(CALIFICACION_TXT, 
                                  levels = c("COMPLETO", "INCOMPLETO", "NO EXISTE", "NO APLICA"))
      ) %>%
      arrange(ITEM_NUM, CALIFICACION_TXT)
    
    num_items <- length(items_unicos)
    altura_grafico <- max(500, num_items * 80)
    
    fig <- plot_ly(tratamiento_detalle, 
                   x = ~VARIABLE_CORTA, 
                   y = ~Cantidad, 
                   color = ~CALIFICACION_TXT,
                   type = 'bar',
                   colors = c("#2ecc71", "#f39c12", "#e74c3c", "#95a5a6"),
                   hoverinfo = "text",
                   hovertext = ~paste0(
                     "<b>Item ", ITEM_NUM, ":</b> ", VARIABLE_COMPLETA, "<br>",
                     "<b>", CALIFICACION_TXT, "</b>: ", Cantidad, " historias"
                   )) %>%
      layout(title = paste("Distribución de Calificaciones -", input$seccion_seleccionada),
             xaxis = list(
               title = "Ítems Evaluados", 
               tickangle = -45,
               tickfont = list(size = 8)
             ),
             yaxis = list(title = "Cantidad de Historias"),
             barmode = 'stack',
             legend = list(orientation = 'h', y = -0.15, x = 0.5, xanchor = 'center'),
             margin = list(l = 50, r = 50, t = 60, b = 150),
             height = 600,
             bargap = 0.1)
    
    fig
  })
  
  # TABLA 1: Distribución de datos - VERSIÓN ULTRA ROBUSTA
  output$tabla_distribucion_items <- DT::renderDataTable({
    req(datos_procesados())
    req(input$seccion_seleccionada)
    
    d <- datos_procesados()
    seccion_codigo <- names(nombres_secciones)[nombres_secciones == input$seccion_seleccionada]
    
    tryCatch({
      datos_seccion <- d$datos_con_pesos %>%
        filter(SECCION == seccion_codigo)
      
      todas_categorias <- expand.grid(
        VARIABLE = unique(datos_seccion$VARIABLE),
        CALIFICACION_TXT = c("COMPLETO", "INCOMPLETO", "NO EXISTE", "NO APLICA"),
        stringsAsFactors = FALSE
      )
      
      conteo_real <- datos_seccion %>%
        group_by(VARIABLE, CALIFICACION_TXT) %>%
        summarise(Cantidad = n(), .groups = "drop")
      
      tabla_completa <- todas_categorias %>%
        left_join(conteo_real, by = c("VARIABLE", "CALIFICACION_TXT")) %>%
        mutate(Cantidad = ifelse(is.na(Cantidad), 0, Cantidad))
      
      tabla_pivot <- tabla_completa %>%
        tidyr::pivot_wider(
          names_from = CALIFICACION_TXT,
          values_from = Cantidad,
          values_fill = 0
        )
      
      tabla_datos <- tabla_pivot %>%
        mutate(
          Total = COMPLETO + INCOMPLETO + `NO EXISTE` + `NO APLICA`,
          Porcentaje_Completitud = ifelse(Total > 0, round((COMPLETO / Total) * 100, 1), 0),
          ITEM = row_number()
        ) %>%
        select(ITEM, VARIABLE, COMPLETO, INCOMPLETO, `NO EXISTE`, `NO APLICA`, Total, Porcentaje_Completitud) %>%
        arrange(ITEM)
      
      if(nrow(tabla_datos) == 0) {
        return(data.frame(Mensaje = "No hay datos disponibles para esta sección"))
      }
      
      DT::datatable(tabla_datos,
                    options = list(
                      pageLength = 15,
                      scrollX = TRUE,
                      scrollY = "550px",
                      autoWidth = TRUE
                    ),
                    rownames = FALSE,
                    class = 'table table-striped table-bordered table-condensed',
                    filter = 'top',
                    escape = FALSE)
      
    }, error = function(e) {
      data.frame(Error = paste("Error al procesar tabla:", e$message))
    })
  }, server = FALSE)
  
  # Descarga de tabla 1
  output$download_tabla_distribucion <- downloadHandler(
    filename = function() {
      paste0("Tabla_Distribucion_", gsub("[^a-zA-Z0-9]", "_", input$seccion_seleccionada), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(datos_procesados())
      d <- datos_procesados()
      seccion_codigo <- names(nombres_secciones)[nombres_secciones == input$seccion_seleccionada]
      
      datos_seccion <- d$datos_con_pesos %>%
        filter(SECCION == seccion_codigo)
      
      todas_categorias <- expand.grid(
        VARIABLE = unique(datos_seccion$VARIABLE),
        CALIFICACION_TXT = c("COMPLETO", "INCOMPLETO", "NO EXISTE", "NO APLICA"),
        stringsAsFactors = FALSE
      )
      
      conteo_real <- datos_seccion %>%
        group_by(VARIABLE, CALIFICACION_TXT) %>%
        summarise(Cantidad = n(), .groups = "drop")
      
      tabla_completa <- todas_categorias %>%
        left_join(conteo_real, by = c("VARIABLE", "CALIFICACION_TXT")) %>%
        mutate(Cantidad = ifelse(is.na(Cantidad), 0, Cantidad))
      
      tabla_pivot <- tabla_completa %>%
        tidyr::pivot_wider(
          names_from = CALIFICACION_TXT,
          values_from = Cantidad,
          values_fill = 0
        )
      
      tabla_datos <- tabla_pivot %>%
        mutate(
          Total = COMPLETO + INCOMPLETO + `NO EXISTE` + `NO APLICA`,
          Porcentaje_Completitud = ifelse(Total > 0, round((COMPLETO / Total) * 100, 1), 0)
        ) %>%
        select(VARIABLE, COMPLETO, INCOMPLETO, `NO EXISTE`, `NO APLICA`, Total, Porcentaje_Completitud)
      
      write.csv(tabla_datos, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # GRÁFICO 2: Completitud
  output$plot_completitud_items <- renderPlotly({
    req(datos_procesados())
    req(input$seccion_seleccionada)
    
    d <- datos_procesados()
    seccion_codigo <- names(nombres_secciones)[nombres_secciones == input$seccion_seleccionada]
    max_car <- input$longitud_texto
    
    completitud_items <- d$datos_con_pesos %>%
      filter(SECCION == seccion_codigo, CALIFICACION != 4) %>%
      group_by(VARIABLE) %>%
      summarise(
        Total = n(),
        Completos = sum(CALIFICACION == 1),
        Porcentaje = round((Completos / Total) * 100, 2),
        .groups = "drop"
      ) %>%
      arrange(desc(Porcentaje)) %>%
      mutate(
        ITEM_NUM = row_number(),
        VARIABLE_CORTA = paste0(ITEM_NUM, ". ", truncar_texto(VARIABLE, max_car)),
        VARIABLE_COMPLETA = VARIABLE
      )
    
    num_items <- nrow(completitud_items)
    altura_grafico <- max(450, num_items * 70)
    
    fig <- plot_ly(completitud_items, 
                   x = ~Porcentaje, 
                   y = ~VARIABLE_CORTA, 
                   type = 'bar', 
                   orientation = 'h',
                   marker = list(
                     color = ~Porcentaje, 
                     colorscale = list(c(0, "#e74c3c"), c(0.5, "#f39c12"), c(1, "#2ecc71")),
                     showscale = TRUE
                   ),
                   hoverinfo = "text",
                   hovertext = ~paste0(
                     "<b>", VARIABLE_COMPLETA, "</b><br>",
                     "Completitud: ", Porcentaje, "%<br>",
                     "Completos: ", Completos, " de ", Total
                   )) %>%
      layout(title = "Porcentaje de Completitud",
             xaxis = list(title = "% Completitud", range = c(0, 100)),
             yaxis = list(title = "Ítems", tickfont = list(size = 9)),
             margin = list(l = 300, r = 50, t = 40, b = 50),
             height = 500)
    
    fig
  })
  
  # GRÁFICO 3: Items con problemas
  output$plot_items_problemas <- renderPlotly({
    req(datos_procesados())
    req(input$seccion_seleccionada)
    
    d <- datos_procesados()
    seccion_codigo <- names(nombres_secciones)[nombres_secciones == input$seccion_seleccionada]
    max_car <- input$longitud_texto
    
    items_problemas <- d$datos_con_pesos %>%
      filter(SECCION == seccion_codigo, CALIFICACION %in% c(2, 3)) %>%
      group_by(VARIABLE) %>%
      summarise(
        Total_Fallas = n(),
        Incompletos = sum(CALIFICACION == 2),
        No_Existe = sum(CALIFICACION == 3),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Fallas)) %>%
      head(10) %>%
      mutate(
        ITEM_NUM = row_number(),
        VARIABLE_CORTA = paste0(ITEM_NUM, ". ", truncar_texto(VARIABLE, max_car)),
        VARIABLE_COMPLETA = VARIABLE
      )
    
    if(nrow(items_problemas) == 0) {
      return(plot_ly() %>% layout(title = "✅ No hay items con problemas", height = 300))
    }
    
    num_items <- nrow(items_problemas)
    altura_grafico <- max(450, num_items * 80)
    
    fig <- plot_ly(items_problemas, 
                   x = ~Total_Fallas, 
                   y = ~VARIABLE_CORTA, 
                   type = 'bar', 
                   orientation = 'h',
                   marker = list(color = '#e74c3c'),
                   hoverinfo = "text",
                   hovertext = ~paste0(
                     "<b>", VARIABLE_COMPLETA, "</b><br>",
                     "Total fallas: ", Total_Fallas, "<br>",
                     "Incompletos: ", Incompletos, "<br>",
                     "No existe: ", No_Existe
                   )) %>%
      layout(title = "Top 10 con Más Problemas",
             xaxis = list(title = "Número de Fallas"),
             yaxis = list(title = "Ítems", tickfont = list(size = 9), autorange = "reversed"),
             margin = list(l = 300, r = 50, t = 40, b = 50),
             height = 500)
    
    fig
  })
  
  # TABLA 2: Completitud y problemas
  output$tabla_completitud_problemas <- DT::renderDataTable({
    req(datos_procesados())
    req(input$seccion_seleccionada)
    
    d <- datos_procesados()
    seccion_codigo <- names(nombres_secciones)[nombres_secciones == input$seccion_seleccionada]
    
    tryCatch({
      tabla_completitud <- d$datos_con_pesos %>%
        filter(SECCION == seccion_codigo, CALIFICACION != 4) %>%
        group_by(VARIABLE) %>%
        summarise(
          Total_Evaluados = n(),
          Completos = sum(CALIFICACION == 1),
          Incompletos = sum(CALIFICACION == 2),
          No_Existe = sum(CALIFICACION == 3),
          Porcentaje_Completitud = round((Completos / Total_Evaluados) * 100, 1),
          Total_Problemas = Incompletos + No_Existe,
          .groups = "drop"
        ) %>%
        mutate(
          ITEM = row_number(),
          Estado = case_when(
            Porcentaje_Completitud >= 90 ~ "Excelente",
            Porcentaje_Completitud >= 70 ~ "Aceptable",
            Porcentaje_Completitud >= 50 ~ "Deficiente",
            TRUE ~ "Inadecuado"
          )
        ) %>%
        select(ITEM, VARIABLE, Total_Evaluados, Completos, Incompletos, No_Existe, 
               Porcentaje_Completitud, Total_Problemas, Estado) %>%
        arrange(desc(Porcentaje_Completitud))
      
      if(nrow(tabla_completitud) == 0) {
        return(data.frame(Mensaje = "No hay datos disponibles"))
      }
      
      DT::datatable(tabla_completitud,
                    options = list(
                      pageLength = 20,
                      scrollX = TRUE,
                      scrollY = "500px",
                      autoWidth = TRUE
                    ),
                    rownames = FALSE,
                    class = 'table table-striped table-bordered table-condensed',
                    filter = 'top',
                    escape = FALSE)
      
    }, error = function(e) {
      data.frame(Error = paste("Error al procesar tabla:", e$message))
    })
  }, server = FALSE)
  
  # GRÁFICOS CRÍTICOS
  output$plot_top_criticos <- renderPlotly({
    req(datos_procesados())
    d <- datos_procesados()
    fig <- plot_ly(d$items_mas_criticos, x = ~TOTAL_FALLAS, y = ~VARIABLE, type = 'bar', orientation = 'h',
                   marker = list(color = '#e74c3c'))
    fig %>% layout(title = "Ítems con Más Fallas", yaxis = list(autorange = "reversed"),
                   margin = list(l = 400, t = 40, b = 40))
  })
  
  output$plot_corr <- renderPlot({
    req(datos_procesados())
    d <- datos_procesados()
    matriz_correlacion <- d$datos_con_pesos %>%
      group_by(ITEM, SECCION) %>%
      summarise(PCT = sum(PUNTAJE_OBTENIDO)/sum(PUNTAJE_MAXIMO)*100, .groups="drop") %>%
      pivot_wider(names_from = SECCION, values_from = PCT, values_fill = 0) %>%
      select(-ITEM) %>% cor()
    
    corrplot(matriz_correlacion, method = "circle", type = "upper", tl.col = "black", tl.srt = 45,
             col = colorRampPalette(c("#e74c3c", "white", "#2ecc71"))(200),
             title = "Correlación entre Secciones", mar = c(0,0,2,0))
  })
  
  output$tabla_historias_fallando <- DT::renderDataTable({
    req(datos_procesados())
    d <- datos_procesados()
    DT::datatable(d$historias_fallando, 
                  options = list(pageLength = 10, scrollX = TRUE, scrollY = "550px"),
                  class = 'table table-striped table-bordered table-condensed')
  })
  
  # DESCARGAS
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("Reporte_Auditoria_HC_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(datos_procesados())
      d <- datos_procesados()
      
      lista_export <- list(
        "01_RESUMEN_GENERAL" = as.data.frame(d$resumen_general),
        "02_RESUMEN_POR_SECCION" = as.data.frame(d$resumen_por_seccion),
        "03_SCORE_POR_HC" = as.data.frame(d$score_por_hc),
        "04_HISTORIAS_FALLANDO" = as.data.frame(d$historias_fallando),
        "05_ITEMS_CRITICOS" = as.data.frame(d$items_mas_criticos),
        "06_DATOS_CON_PESOS" = as.data.frame(d$datos_con_pesos)
      )
      
      writexl::write_xlsx(lista_export, file)
    }
  )
  
  output$download_graficos_seccion <- downloadHandler(
    filename = function() {
      paste0("Graficos_Por_Seccion_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(datos_procesados())
      d <- datos_procesados()
      
      pdf(file, width = 14, height = 10)
      
      for(seccion in unique(d$datos_con_pesos$SECCION)) {
        tratamiento_detalle <- d$datos_con_pesos %>%
          filter(SECCION == seccion) %>%
          group_by(VARIABLE, CALIFICACION_TXT) %>%
          summarise(Cantidad = n(), .groups = "drop")
        
        if(nrow(tratamiento_detalle) > 0) {
          p <- ggplot(tratamiento_detalle, aes(x = VARIABLE, y = Cantidad, fill = CALIFICACION_TXT)) +
            geom_bar(stat = "identity", position = "stack") +
            geom_text(aes(label = Cantidad), position = position_stack(vjust = 0.5), size = 3, color = "white") +
            labs(
              title = paste("Distribución de Calificaciones -", seccion),
              subtitle = paste("N =", d$resumen_general$TOTAL_HISTORIAS, "historias"),
              x = "Ítems", y = "Cantidad", fill = "Calificación"
            ) +
            scale_fill_manual(values = c("COMPLETO" = "#2ecc71", "INCOMPLETO" = "#f39c12", 
                                         "NO EXISTE" = "#e74c3c", "NO APLICA" = "#95a5a6")) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                  legend.position = "bottom")
          
          print(p)
        }
      }
      
      dev.off()
    }
  )
}

# ============================================================================
# EJECUTAR APP
# ============================================================================
shinyApp(ui = ui, server = server)