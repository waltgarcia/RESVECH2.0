# Load required libraries
library(shiny)
library(shinyjs)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(bslib)

# Keep uploads lightweight for ShinyApps.io (5 MB max).
options(shiny.maxRequestSize = 5 * 1024^2)

# RESVECH 2.0 Scoring Function
calculate_resvech <- function(dimensions, depth, edges, tissue, exudate, infection_items) {
  
  dim_score <- as.numeric(dimensions)
  depth_score <- as.numeric(depth)
  edges_score <- as.numeric(edges)
  tissue_score <- as.numeric(tissue)
  exudate_score <- as.numeric(exudate)
  infection_score <- sum(as.numeric(infection_items))
  
  total_score <- dim_score + depth_score + edges_score + tissue_score + exudate_score + infection_score
  
  return(list(
    dimension = dim_score,
    depth = depth_score,
    edges = edges_score,
    tissue = tissue_score,
    exudate = exudate_score,
    infection = infection_score,
    total = total_score
  ))
}

# Get clinical suggestion based on score
get_suggestion <- function(score) {
  if (score <= 5) {
    suggestion <- "✅ HERIDA CERCA DE LA CICATRIZACIÓN\n\nRecomendaciones:\n• Continuar con curación en ambiente húmedo\n• Proteger la piel perilesional\n• Controlar cada 7-10 días\n• Valorar alta si persiste mejoría"
    color <- "success"
  } else if (score <= 10) {
    suggestion <- "⚠️ RETRASO EN LA CICATRIZACIÓN\n\nRecomendaciones:\n• Revisar plan de cuidados actual\n• Evaluar carga bacteriana\n• Considerar terapia de presión negativa o apósitos avanzados\n• Control cada 3-5 días\n• Descartar infección"
    color <- "warning"
  } else if (score <= 15) {
    suggestion <- "🔴 DETERIORO SIGNIFICATIVO\n\nRecomendaciones:\n• Evaluar infección activa\n• Tomar cultivo si hay signos de infección\n• Considerar desbridamiento\n• Valorar tratamiento antibiótico\n• Interconsulta a especialista\n• Control cada 24-48 horas"
    color <- "danger"
  } else {
    suggestion <- "🚨 HERIDA CRÍTICA\n\nRecomendaciones:\n• REQUIERE EVALUACIÓN URGENTE\n• Interconsulta a cirugía vascular o dermatología\n• Descartar osteomielitis\n• Considerar hospitalización\n• Tratamiento multidisciplinario\n• Control diario"
    color <- "danger"
  }
  
  return(list(text = suggestion, color = color))
}

# Create radar chart function
create_radar_chart <- function(scores) {
  data <- data.frame(
    r = c(scores$dimension, scores$depth, scores$edges, scores$tissue, scores$exudate, scores$infection),
    theta = c("Dimensiones", "Profundidad", "Bordes", "Tejido", "Exudado", "Infección")
  )
  
  plot_ly(
    type = 'scatterpolar',
    mode = 'lines+markers',
    fill = 'toself'
  ) %>%
    add_trace(
      r = c(data$r, data$r[1]),
      theta = c(data$theta, data$theta[1]),
      line = list(color = '#3498db', width = 3),
      marker = list(size = 10, color = '#2c3e50'),
      fillcolor = 'rgba(52, 152, 219, 0.3)'
    ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 6)
        )
      ),
      showlegend = FALSE,
      title = "Perfil de la Herida"
    )
}

# Define themes
light_theme <- bs_theme(
  version = 4,
  bg = "#FFFFFF",
  fg = "#2C3E50",
  primary = "#3498DB",
  secondary = "#95A5A6",
  success = "#27AE60",
  info = "#3498DB",
  warning = "#F39C12",
  danger = "#E74C3C"
)

dark_theme <- bs_theme(
  version = 4,
  bg = "#1a1e24",
  fg = "#ECF0F1",
  primary = "#3498DB",
  secondary = "#7F8C8D",
  success = "#27AE60",
  info = "#3498DB",
  warning = "#F39C12",
  danger = "#E74C3C"
)

# UI
ui <- fluidPage(
  theme = light_theme,
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .container-fluid {
        max-width: 1400px;
        margin: 0 auto;
        padding: 0 15px;
      }
      .title-panel {
        background: linear-gradient(135deg, #2c3e50, #3498db);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin: 15px 0 20px 0;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .score-box {
        background-color: white;
        border-radius: 10px;
        padding: 20px;
        margin: 15px 0;
        border: 1px solid #e1e8ed;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .total-score {
        font-size: 56px;
        font-weight: bold;
        color: #2c3e50;
        text-align: center;
        line-height: 1.2;
      }
      .suggestion-box {
        border-radius: 10px;
        padding: 20px;
        margin: 15px 0;
        font-size: 15px;
        white-space: pre-line;
      }
      .bg-success { 
        background-color: #d4edda; 
        border-left: 5px solid #28a745; 
        color: #155724;
      }
      .bg-warning { 
        background-color: #fff3cd; 
        border-left: 5px solid #ffc107; 
        color: #856404;
      }
      .bg-danger { 
        background-color: #f8d7da; 
        border-left: 5px solid #dc3545; 
        color: #721c24;
      }
      .well {
        background-color: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }
      .btn-action {
        background-color: #3498db;
        border-color: #3498db;
        color: white;
        font-weight: 600;
        padding: 10px 20px;
        border-radius: 20px;
        margin: 3px;
        font-size: 14px;
      }
      .btn-action:hover {
        background-color: #2980b9;
        border-color: #2980b9;
      }
      .info-icon {
        display: inline-block;
        margin-left: 5px;
        color: #7f8c8d;
        cursor: help;
        font-size: 16px;
      }
      .history-item {
        background-color: #f8f9fa;
        border-radius: 8px;
        padding: 8px;
        margin: 3px 0;
        border-left: 3px solid #3498db;
        text-align: center;
      }
      .badge-score {
        background-color: #3498db;
        color: white;
        padding: 4px 8px;
        border-radius: 15px;
        font-size: 13px;
        margin: 0 3px;
        display: inline-block;
      }
      .nav-tabs {
        margin-bottom: 15px;
      }
      .nav-tabs > li > a {
        padding: 10px 15px;
        font-size: 14px;
      }
      .photo-preview-box {
        border: 1px dashed #bdc3c7;
        border-radius: 8px;
        background: #ffffff;
        padding: 10px;
      }
    "))
  ),
  
  # Title with action buttons
  fluidRow(
    column(8,
           div(class = "title-panel",
               h1("🏥 RESVECH 2.0", style = "margin: 0; font-size: 32px;"),
               h5("Evaluación de Heridas Crónicas", style = "margin: 5px 0 0 0; opacity: 0.9;")
           )
    ),
    column(4,
           br(),
           div(style = "float: right;",
               actionButton("dark_mode", "🌙", class = "btn-action", 
                            style = "background-color: #34495e; width: 45px;",
                            title = "Modo oscuro/claro"),
               actionButton("new_eval", "➕ Nueva", class = "btn-action", 
                            style = "background-color: #27ae60;",
                            title = "Nueva evaluación"),
               actionButton("save_history", "💾 Guardar", class = "btn-action",
                            style = "background-color: #3498db;",
                            title = "Guardar evaluación")
           )
    )
  ),
  
  # Main tabs
  tabsetPanel(id = "main_tabs",
              # Tab 1: Evaluación
              tabPanel("📋 Evaluación", value = "eval",
                       fluidRow(
                         column(6,
                                wellPanel(
                                  h5("1. Dimensiones de la lesión", 
                                     span(class = "info-icon", title = "Área de la herida en cm²", "ⓘ")),
                                  radioButtons("dimensions", NULL,
                                               choices = c("Superficie = 0 cm²" = 0,
                                                           "Superficie < 4 cm²" = 1,
                                                           "Superficie = 4 - < 16 cm²" = 2,
                                                           "Superficie = 16 - < 36 cm²" = 3,
                                                           "Superficie = 36 - < 64 cm²" = 4,
                                                           "Superficie = 64 - < 100 cm²" = 5,
                                                           "Superficie ≥ 100 cm²" = 6),
                                               selected = 0, width = "100%"),
                                  
                                  h5("2. Profundidad/Tejidos afectados",
                                     span(class = "info-icon", title = "Nivel de tejido afectado", "ⓘ")),
                                  radioButtons("depth", NULL,
                                               choices = c("Piel intacta cicatrizada" = 0,
                                                           "Afectación de la dermis-epidermis" = 1,
                                                           "Afectación del tejido subcutáneo" = 2,
                                                           "Afectación del músculo" = 3,
                                                           "Afectación de hueso o tejidos anexos" = 4),
                                               selected = 0, width = "100%")
                                )
                         ),
                         column(6,
                                wellPanel(
                                  h5("3. Bordes",
                                     span(class = "info-icon", title = "Característica de los bordes", "ⓘ")),
                                  radioButtons("edges", NULL,
                                               choices = c("No distinguibles" = 0,
                                                           "Difusos" = 1,
                                                           "Delimitados" = 2,
                                                           "Dañados" = 3,
                                                           "Engrosados" = 4),
                                               selected = 0, width = "100%"),
                                  
                                  h5("4. Tipo de tejido en el lecho de la herida",
                                     span(class = "info-icon", title = "Tejido predominante en el lecho", "ⓘ")),
                                  radioButtons("tissue", NULL,
                                               choices = c("Cerrada/cicatrización" = 0,
                                                           "Tejido epitelial" = 1,
                                                           "Tejido de granulación" = 2,
                                                           "Tejido necrótico o esfacelos" = 3,
                                                           "Necrótico (escara negra)" = 4),
                                               selected = 0, width = "100%")
                                )
                         )
                       ),
                       fluidRow(
                         column(6,
                                wellPanel(
                                  h5("5. Exudado",
                                     span(class = "info-icon", title = "Cantidad y tipo de exudado", "ⓘ")),
                                  radioButtons("exudate", NULL,
                                               choices = c("Seco" = 3,
                                                           "Húmedo" = 0,
                                                           "Mojado" = 1,
                                                           "Saturado" = 2,
                                                           "Con fuga de exudado" = 3),
                                               selected = 0, width = "100%"),
                                  p(style = "color: #7f8c8d; font-size: 12px; margin-top: 5px;",
                                    "Nota: Seco (3), Húmedo (0), Mojado (1), Saturado (2), Con fuga (3)")
                                )
                         ),
                         column(6,
                                wellPanel(
                                  h5("6. Infección/inflamación",
                                     span(class = "info-icon", title = "Marque todos los presentes", "ⓘ")),
                                  fluidRow(
                                    column(6,
                                           checkboxInput("inf_pain", "6.1. Dolor que va en aumento", value = FALSE),
                                           checkboxInput("inf_erythema", "6.2. Eritema perilesional", value = FALSE),
                                           checkboxInput("inf_edema", "6.3. Edema perilesional", value = FALSE),
                                           checkboxInput("inf_temp", "6.4. Aumento de temperatura", value = FALSE),
                                           checkboxInput("inf_exudate_inc", "6.5. Exudado que va en aumento", value = FALSE),
                                           checkboxInput("inf_purulent", "6.6. Exudado purulento", value = FALSE),
                                           checkboxInput("inf_friable", "6.7. Tejido friable o sangra con facilidad", value = FALSE)
                                    ),
                                    column(6,
                                           checkboxInput("inf_stalled", "6.8. Herida estancada, que no progresa", value = FALSE),
                                           checkboxInput("inf_biofilm", "6.9. Tejido compatible con biopelícula", value = FALSE),
                                           checkboxInput("inf_odor", "6.10. Olor", value = FALSE),
                                           checkboxInput("inf_hypergran", "6.11. Hipergranulación", value = FALSE),
                                           checkboxInput("inf_size_inc", "6.12. Aumento del tamaño de la herida", value = FALSE),
                                           checkboxInput("inf_satellite", "6.13. Lesiones satélite", value = FALSE),
                                           checkboxInput("inf_pallor", "6.14. Palidez del tejido", value = FALSE)
                                    )
                                  )
                                )
                         )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  h5("Foto clínica de la herida (opcional)",
                                     span(class = "info-icon", title = "JPG/PNG. Tamaño máximo: 5 MB", "ⓘ")),
                                  div(id = "photo_upload_form",
                                      fileInput(
                                        "wound_photo",
                                        label = NULL,
                                        accept = c("image/jpeg", "image/png", "image/jpg")
                                      )
                                  ),
                                  p(style = "color: #7f8c8d; font-size: 12px; margin-top: 5px;",
                                    "Recomendación: usar imágenes de 1280 px aprox. para conservar calidad y reducir consumo."),
                                  div(class = "photo-preview-box",
                                      uiOutput("photo_upload_status"),
                                      imageOutput("photo_preview_eval", height = "260px")
                                  )
                                )
                         )
                       ),
                       fluidRow(
                         column(12,
                                div(style = "text-align: center; margin-top: 25px;",
                                    actionButton("clear_eval", "🗑️ Limpiar", class = "btn-action",
                                                 style = "background-color: #e74c3c; margin-right: 10px;",
                                                 width = "150px"),
                                    actionButton("calculate_eval", "📊 Calcular", class = "btn-action",
                                                 style = "background-color: #27ae60; margin-left: 10px;",
                                                 width = "150px")
                                )
                         )
                       )
              ),
              
              # Tab 2: Resultados
              tabPanel("📈 Resultados", value = "results",
                       fluidRow(
                         column(12,
                                wellPanel(
                                  h4("📊 RESULTADO DE LA EVALUACIÓN", style = "text-align: center; margin-top: 0;"),
                                  p(textOutput("result_datetime"), style = "text-align: center; color: #7f8c8d; font-size: 12px;"),
                                  p(textOutput("result_photo_info"), style = "text-align: center; color: #7f8c8d; font-size: 12px;"),
                                  imageOutput("result_photo_preview", height = "260px"),
                                  hr(),
                                  fluidRow(
                                    column(6,
                                           div(style = "text-align: center;",
                                               div(class = "total-score", textOutput("result_score"))
                                           )
                                    ),
                                    column(6,
                                           div(style = "padding: 20px;",
                                               div(class = "history-item", h5("Clasificación"), textOutput("result_classification"))
                                           )
                                    )
                                  ),
                                  hr(),
                                  h5("Desglose de puntajes:"),
                                  fluidRow(
                                    column(2, div(class = "history-item", "Dimensiones", h5(textOutput("res_dim")))),
                                    column(2, div(class = "history-item", "Profundidad", h5(textOutput("res_depth")))),
                                    column(2, div(class = "history-item", "Bordes", h5(textOutput("res_edges")))),
                                    column(2, div(class = "history-item", "Tejido", h5(textOutput("res_tissue")))),
                                    column(2, div(class = "history-item", "Exudado", h5(textOutput("res_exudate")))),
                                    column(2, div(class = "history-item", "Infección", h5(textOutput("res_infection"))))
                                  ),
                                  hr(),
                                  uiOutput("result_suggestion_ui"),
                                  hr(),
                                  div(style = "text-align: center;",
                                      downloadButton("export_pdf", "📄 Exportar PDF", class = "btn-action",
                                                     style = "background-color: #e74c3c; margin-right: 10px;"),
                                      downloadButton("export_txt", "📝 Exportar Texto", class = "btn-action",
                                                     style = "background-color: #f39c12; margin: 0 10px;"),
                                      actionButton("save_history_btn", "💾 Guardar en Historial", class = "btn-action",
                                                   style = "background-color: #3498db; margin-left: 10px;")
                                  )
                                )
                         )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  h5("📉 Gráfico de Perfil"),
                                  plotlyOutput("result_radar_plot", height = "400px")
                                )
                         )
                       )
              ),
              
              # Tab 3: Histórico
              tabPanel("📊 Histórico", value = "history",
                       fluidRow(
                         column(12,
                                wellPanel(
                                  h5("📈 Evolución de Puntajes RESVECH 2.0"),
                                  plotlyOutput("history_trend_plot", height = "400px")
                                )
                         )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  h5("📋 Tabla de Evaluaciones"),
                                  DTOutput("history_table"),
                                  actionButton("clear_history", "🗑️ Limpiar Historial", class = "btn-action",
                                               style = "background-color: #e74c3c; margin-top: 15px;")
                                )
                         )
                       )
              ),
              
              # Tab 4: Bibliografía
              tabPanel("📚 Bibliografía", value = "bibliography",
                       fluidRow(
                         column(12,
                                wellPanel(
                                  h4("📚 Referencias en Formato APA", style = "margin-top: 0;"),
                                  tags$ul(
                                    tags$li("Restrepo-Medrano, J. C., Medina-Pérez, F., & Guzmán-Camargo, S. (2021). Adaptación cultural y validación del índice RESVECH 2.0 en población mexicana. Revista Iberoamericana de Heridas y Cicatrización, 2(1), 45-58."),
                                    tags$li("Restrepo-Medrano, J. C., & Verdú Soriano, J. (2011). Desarrollo de un índice de medida de la evolución hacia la cicatrización de las heridas crónicas. Gerokomos, 22(4), 176-183.")
                                  )
                                )
                         )
                       )
              )
  ),
  
  # Results panel (hidden, used for storage)
  div(id = "results_placeholder", style = "display: none;",
      uiOutput("result_panel")
  )
)

# Get classification text and color
get_classification <- function(score) {
  if (score <= 5) {
    list(text = "✅ CICATRIZACIÓN ADECUADA", color = "success", badge = "primary")
  } else if (score <= 10) {
    list(text = "⚠️ RETRASO EN CICATRIZACIÓN", color = "warning", badge = "warning")
  } else if (score <= 15) {
    list(text = "🔴 DETERIORO SIGNIFICATIVO", color = "danger", badge = "danger")
  } else {
    list(text = "🚨 HERIDA CRÍTICA", color = "danger", badge = "danger")
  }
}

# Export to PDF function
export_to_pdf <- function(result, datetime, photo_info = NULL) {
  temp_file <- tempfile(fileext = ".pdf")

  classification <- get_classification(result$total)$text
  suggestion <- get_suggestion(result$total)$text

  # Remove UI icons so the printable report stays clean.
  clean_classification <- gsub("[✅⚠️🔴🚨]", "", classification)
  clean_suggestion <- gsub("[✅⚠️🔴🚨]", "", suggestion)
  clean_suggestion <- gsub("•", "-", clean_suggestion)

  grDevices::pdf(temp_file, width = 8.27, height = 11.69, paper = "special", family = "Helvetica")
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(mar = c(0.6, 0.6, 0.6, 0.6))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))

  y_pos <- 0.97

  new_page <- function() {
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    y_pos <<- 0.97
  }

  write_wrapped <- function(text, indent = 0.06, cex = 0.9, font = 1, line_height = 0.024) {
    text_lines <- unlist(strsplit(text, "\\n", fixed = FALSE))

    for (line in text_lines) {
      wrapped <- strwrap(line, width = 95)
      if (length(wrapped) == 0) {
        y_pos <<- y_pos - line_height
      } else {
        for (wline in wrapped) {
          if (y_pos < 0.06) {
            new_page()
          }
          graphics::text(indent, y_pos, labels = wline, adj = c(0, 1), cex = cex, font = font)
          y_pos <<- y_pos - line_height
        }
      }
    }
  }

  section_title <- function(title) {
    if (y_pos < 0.12) {
      new_page()
    }
    graphics::rect(0.04, y_pos - 0.028, 0.96, y_pos, col = "#F2F2F2", border = "#BDBDBD")
    graphics::text(0.06, y_pos - 0.006, labels = title, adj = c(0, 1), font = 2, cex = 0.95)
    y_pos <<- y_pos - 0.042
  }

  graphics::rect(0.04, 0.905, 0.96, 0.985, col = "#E9F1FA", border = "#4A6FA5", lwd = 1.4)
  graphics::text(0.5, 0.968, labels = "REPORTE CLINICO RESVECH 2.0", cex = 1.24, font = 2)
  graphics::text(0.5, 0.942, labels = "Documento para impresion y anexo al expediente", cex = 0.86)
  y_pos <- 0.885

  section_title("Datos del reporte")
  write_wrapped(paste0("Fecha y hora de evaluacion: ", datetime))
  write_wrapped(paste0("Generado por la aplicacion: ", format(Sys.time(), "%d/%m/%Y %H:%M:%S")))

  y_pos <- y_pos - 0.01
  section_title("Registro fotografico")
  if (is.null(photo_info)) {
    write_wrapped("Foto clinica adjunta: No")
  } else {
    write_wrapped("Foto clinica adjunta: Si")
    write_wrapped(paste0("Archivo: ", photo_info$name))
    write_wrapped(paste0("Tamano: ", photo_info$size_label))
  }

  y_pos <- y_pos - 0.01
  section_title("Identificacion del paciente")
  write_wrapped("Nombre del paciente: ________________________________")
  write_wrapped("No. de expediente: _________________________________")
  write_wrapped("Servicio / Unidad: _________________________________")
  write_wrapped("Profesional que evalua: ____________________________")

  y_pos <- y_pos - 0.01
  section_title("Resultado RESVECH 2.0")
  write_wrapped(paste0("Puntaje total: ", result$total, " puntos"), cex = 1.04, font = 2)
  write_wrapped(paste0("Clasificacion clinica: ", trimws(clean_classification)))

  y_pos <- y_pos - 0.01
  section_title("Desglose de puntuacion")
  write_wrapped(paste0("1. Dimensiones de la lesion: ", result$dimension, " puntos"))
  write_wrapped(paste0("2. Profundidad / tejidos afectados: ", result$depth, " puntos"))
  write_wrapped(paste0("3. Bordes: ", result$edges, " puntos"))
  write_wrapped(paste0("4. Tipo de tejido en el lecho: ", result$tissue, " puntos"))
  write_wrapped(paste0("5. Exudado: ", result$exudate, " puntos"))
  write_wrapped(paste0("6. Infeccion / inflamacion: ", result$infection, " puntos"))

  y_pos <- y_pos - 0.01
  section_title("Indicaciones clinicas sugeridas")
  write_wrapped(trimws(clean_suggestion))

  if (y_pos < 0.08) {
    new_page()
  }
  graphics::segments(0.04, 0.06, 0.96, 0.06, col = "#BDBDBD")
  graphics::text(0.04, 0.045,
                 labels = "Este reporte es un apoyo clinico y debe interpretarse junto con valoracion profesional integral.",
                 adj = c(0, 1), cex = 0.75, col = "#555555")

  temp_file
}

# Export to text function  
export_to_text <- function(result, datetime, photo_info = NULL) {
  temp_file <- tempfile(fileext = ".txt")

  photo_text <- "Sin foto clínica adjunta"
  if (!is.null(photo_info)) {
    photo_text <- paste0("Foto adjunta: ", photo_info$name, " (", photo_info$size_label, ")")
  }
  
  report_content <- sprintf(
    "REPORTE RESVECH 2.0\n\nFecha/Hora: %s\n%s\n\n=== RESULTADO ===\nPuntuación Total: %d puntos\nClasificación: %s\n\n=== DESGLOSE DE PUNTAJES ===\nDimensiones: %d puntos\nProfundidad: %d puntos\nBordes: %d puntos\nTejido: %d puntos\nExudado: %d puntos\nInfección/Inflamación: %d puntos\n\n=== RECOMENDACIONES CLÍNICAS ===\n%s\n\n---\nGenerado: %s\nREVSECH 2.0 Calculator v1.0",
    datetime,
    photo_text,
    result$total,
    get_classification(result$total)$text,
    result$dimension,
    result$depth,
    result$edges,
    result$tissue,
    result$exudate,
    result$infection,
    get_suggestion(result$total)$text,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  writeLines(report_content, temp_file)
  temp_file
}
# Server
server <- function(input, output, session) {

  format_size_mb <- function(bytes) {
    if (is.null(bytes) || is.na(bytes)) {
      return("N/D")
    }
    paste0(format(round(bytes / (1024^2), 2), nsmall = 2), " MB")
  }
  
  # Reactive values
  values <- reactiveValues(
    current_result = NULL,
    result_datetime = NULL,
    photo_info = NULL,
    photo_path = NULL,
    history = data.frame(
      Fecha_Hora = as.POSIXct(character()),
      Puntuacion = numeric(),
      Clasificacion = character(),
      stringsAsFactors = FALSE
    ),
    dark_mode = FALSE
  )
  
  # Clear evaluation form
  observeEvent(input$clear_eval, {
    updateCheckboxInput(session, "inf_pain", value = FALSE)
    updateCheckboxInput(session, "inf_erythema", value = FALSE)
    updateCheckboxInput(session, "inf_edema", value = FALSE)
    updateCheckboxInput(session, "inf_temp", value = FALSE)
    updateCheckboxInput(session, "inf_exudate_inc", value = FALSE)
    updateCheckboxInput(session, "inf_purulent", value = FALSE)
    updateCheckboxInput(session, "inf_friable", value = FALSE)
    updateCheckboxInput(session, "inf_stalled", value = FALSE)
    updateCheckboxInput(session, "inf_biofilm", value = FALSE)
    updateCheckboxInput(session, "inf_odor", value = FALSE)
    updateCheckboxInput(session, "inf_hypergran", value = FALSE)
    updateCheckboxInput(session, "inf_size_inc", value = FALSE)
    updateCheckboxInput(session, "inf_satellite", value = FALSE)
    updateCheckboxInput(session, "inf_pallor", value = FALSE)
    
    updateRadioButtons(session, "dimensions", selected = "0")
    updateRadioButtons(session, "depth", selected = "0")
    updateRadioButtons(session, "edges", selected = "0")
    updateRadioButtons(session, "tissue", selected = "0")
    updateRadioButtons(session, "exudate", selected = "0")

    shinyjs::reset("photo_upload_form")
    values$photo_info <- NULL
    values$photo_path <- NULL
    
    showNotification("Formulario limpiado", type = "message", duration = 2)
  })

  observeEvent(input$wound_photo, {
    if (is.null(input$wound_photo)) {
      values$photo_info <- NULL
      values$photo_path <- NULL
      return()
    }

    file_size <- input$wound_photo$size
    file_type <- input$wound_photo$type
    is_valid_type <- grepl("^image/", file_type)

    if (!is_valid_type) {
      showNotification("Archivo inválido. Solo se permiten imágenes JPG o PNG.", type = "error", duration = 4)
      shinyjs::reset("photo_upload_form")
      values$photo_info <- NULL
      values$photo_path <- NULL
      return()
    }

    if (!is.null(file_size) && file_size > (5 * 1024^2)) {
      showNotification("La imagen supera 5 MB. Usa una foto más ligera.", type = "error", duration = 4)
      shinyjs::reset("photo_upload_form")
      values$photo_info <- NULL
      values$photo_path <- NULL
      return()
    }

    file_ext <- tools::file_ext(input$wound_photo$name)
    saved_path <- file.path(tempdir(), paste0("wound_", as.integer(Sys.time()), ".", file_ext))
    file.copy(input$wound_photo$datapath, saved_path, overwrite = TRUE)

    values$photo_path <- saved_path
    values$photo_info <- list(
      name = input$wound_photo$name,
      size_label = format_size_mb(file_size),
      mime = file_type
    )
  })

  output$photo_upload_status <- renderUI({
    if (is.null(values$photo_info)) {
      return(p("Sin foto cargada", style = "margin: 0; color: #7f8c8d;"))
    }

    p(
      paste0("Foto seleccionada: ", values$photo_info$name, " (", values$photo_info$size_label, ")"),
      style = "margin: 0 0 8px 0; color: #2c3e50;"
    )
  })

  output$photo_preview_eval <- renderImage({
    req(values$photo_path)
    list(src = values$photo_path, contentType = values$photo_info$mime, alt = "Foto de herida")
  }, deleteFile = FALSE)
  
  # Calculate and show results
  observeEvent(input$calculate_eval, {
    infection_items <- c(input$inf_pain, input$inf_erythema, input$inf_edema,
                         input$inf_temp, input$inf_exudate_inc, input$inf_purulent,
                         input$inf_friable, input$inf_stalled, input$inf_biofilm,
                         input$inf_odor, input$inf_hypergran, input$inf_size_inc,
                         input$inf_satellite, input$inf_pallor)
    
    values$current_result <- calculate_resvech(
      dimensions = input$dimensions,
      depth = input$depth,
      edges = input$edges,
      tissue = input$tissue,
      exudate = input$exudate,
      infection_items = infection_items
    )
    
    values$result_datetime <- format(Sys.time(), "%d/%m/%Y - %H:%M:%S")
    
    # Go to results tab
    updateTabsetPanel(session, "main_tabs", selected = "results")
  })
  
  # Results panel outputs
  output$result_datetime <- renderText({
    req(values$result_datetime)
    values$result_datetime
  })
  
  output$result_score <- renderText({
    req(values$current_result)
    as.character(values$current_result$total)
  })
  
  output$result_classification <- renderText({
    req(values$current_result)
    get_classification(values$current_result$total)$text
  })

  output$result_photo_info <- renderText({
    if (is.null(values$photo_info)) {
      return("Foto clínica: no adjunta")
    }
    paste0("Foto clínica: ", values$photo_info$name, " (", values$photo_info$size_label, ")")
  })

  output$result_photo_preview <- renderImage({
    req(values$photo_path)
    list(src = values$photo_path, contentType = values$photo_info$mime, alt = "Foto de herida")
  }, deleteFile = FALSE)
  
  output$res_dim <- renderText({ req(values$current_result); values$current_result$dimension })
  output$res_depth <- renderText({ req(values$current_result); values$current_result$depth })
  output$res_edges <- renderText({ req(values$current_result); values$current_result$edges })
  output$res_tissue <- renderText({ req(values$current_result); values$current_result$tissue })
  output$res_exudate <- renderText({ req(values$current_result); values$current_result$exudate })
  output$res_infection <- renderText({ req(values$current_result); values$current_result$infection })
  
  output$result_suggestion_ui <- renderUI({
    req(values$current_result)
    suggestion <- get_suggestion(values$current_result$total)
    div(class = paste("suggestion-box bg-", suggestion$color),
        h5("🎯 RECOMENDACIÓN CLÍNICA:"),
        p(suggestion$text, style = "font-size: 14px;")
    )
  })
  
  output$result_radar_plot <- renderPlotly({
    req(values$current_result)
    create_radar_chart(values$current_result)
  })
  
  # Export to PDF
  output$export_pdf <- downloadHandler(
    filename = function() {
      paste0("RESVECH_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      req(values$current_result)
      file.copy(export_to_pdf(values$current_result, values$result_datetime, values$photo_info), file)
    }
  )
  
  # Export to text
  output$export_txt <- downloadHandler(
    filename = function() {
      paste0("RESVECH_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      req(values$current_result)
      file.copy(export_to_text(values$current_result, values$result_datetime, values$photo_info), file)
    }
  )
  
  # Save to history
  observeEvent(input$save_history_btn, {
    req(values$current_result)
    
    new_entry <- data.frame(
      Fecha_Hora = Sys.time(),
      Puntuacion = values$current_result$total,
      Clasificacion = get_classification(values$current_result$total)$text,
      stringsAsFactors = FALSE
    )
    
    values$history <- rbind(values$history, new_entry)
    
    showNotification("✅ Evaluación guardada en el historial", type = "message", duration = 3)
  })
  
  # Render history table
  output$history_table <- renderDT({
    if(nrow(values$history) > 0) {
      datatable(
        values$history %>% 
          mutate(Fecha_Hora = format(Fecha_Hora, "%d/%m/%Y %H:%M:%S")) %>%
          select(Fecha_Hora, Puntuacion, Clasificacion),
        colnames = c("Fecha/Hora", "Puntuación", "Clasificación"),
        options = list(
          pageLength = 10,
          order = list(list(0, "desc")),
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
          )
        ),
        rownames = FALSE
      )
    } else {
      datatable(
        data.frame(Mensaje = "No hay evaluaciones guardadas aún"),
        rownames = FALSE,
        options = list(dom = 't')
      )
    }
  })
  
  # Render history trend plot
  output$history_trend_plot <- renderPlotly({
    if(nrow(values$history) > 0) {
      plot_data <- values$history %>%
        arrange(Fecha_Hora) %>%
        mutate(Index = row_number())
      
      plot_ly(plot_data, x = ~Index, y = ~Puntuacion,
              type = 'scatter', mode = 'lines+markers',
              line = list(color = '#3498db', width = 3),
              marker = list(size = 10, color = '#2c3e50')) %>%
        layout(
          title = "Evolución de Puntajes RESVECH 2.0",
          xaxis = list(title = "Evaluación #"),
          yaxis = list(title = "Puntuación", range = c(0, 34)),
          hovermode = "closest",
          shapes = list(
            list(type = "line", x0 = 0, x1 = nrow(plot_data), y0 = 5, y1 = 5,
                 line = list(color = "rgba(39, 174, 96, 0.3)", width = 2, dash = "dash")),
            list(type = "line", x0 = 0, x1 = nrow(plot_data), y0 = 10, y1 = 10,
                 line = list(color = "rgba(243, 156, 18, 0.3)", width = 2, dash = "dash")),
            list(type = "line", x0 = 0, x1 = nrow(plot_data), y0 = 15, y1 = 15,
                 line = list(color = "rgba(231, 76, 60, 0.3)", width = 2, dash = "dash"))
          )
        ) %>%
        add_annotations(
          x = nrow(plot_data) * 0.05, y = 5.5,
          text = "Cicatrización", showarrow = FALSE,
          font = list(size = 10, color = "rgba(39, 174, 96, 0.5)")
        ) %>%
        add_annotations(
          x = nrow(plot_data) * 0.05, y = 10.5,
          text = "Retraso", showarrow = FALSE,
          font = list(size = 10, color = "rgba(243, 156, 18, 0.5)")
        ) %>%
        add_annotations(
          x = nrow(plot_data) * 0.05, y = 15.5,
          text = "Deterioro", showarrow = FALSE,
          font = list(size = 10, color = "rgba(231, 76, 60, 0.5)")
        )
    } else {
      plot_ly() %>%
        layout(
          title = "No hay datos de historial aún",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        ) %>%
        add_text(
          x = 0.5, y = 0.5,
          text = "📊 Complete evaluaciones para ver la evolución",
          showarrow = FALSE,
          font = list(size = 16, color = "#7f8c8d")
        )
    }
  })
  
  # Clear history
  observeEvent(input$clear_history, {
    showModal(modalDialog(
      title = "⚠️ Confirmar",
      "¿Desea eliminar todo el historial de evaluaciones? Esta acción no se puede deshacer.",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirm_clear_history", "Sí, eliminar", class = "btn btn-danger")
      ),
      size = "m"
    ))
  })
  
  observeEvent(input$confirm_clear_history, {
    values$history <- data.frame(
      Fecha_Hora = as.POSIXct(character()),
      Puntuacion = numeric(),
      Clasificacion = character(),
      stringsAsFactors = FALSE
    )
    removeModal()
    showNotification("✅ Historial eliminado", type = "message", duration = 2)
  })
  
  # Dark mode
  observeEvent(input$dark_mode, {
    values$dark_mode <- !values$dark_mode
    if(values$dark_mode) {
      session$setCurrentTheme(dark_theme)
    } else {
      session$setCurrentTheme(light_theme)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)