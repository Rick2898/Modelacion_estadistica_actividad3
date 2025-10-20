#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
  library(ggplot2)
  library(GGally)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(broom)
  library(car)
  library(lmtest)
  library(performance)
})

# ----- Utilidades -----
safe_read <- function(path) {
  if (file.exists(path)) {
    read.csv(path, sep = ";")
  } else {
    NULL
  }
}

# Carga de datos desde archivos locales por defecto (ajusta si los tienes en otra ruta)
por_default <- safe_read("student-por.csv")
mat_default <- safe_read("student-mat.csv")

# Si ejecutas este app en este entorno (donde compartiste archivos), también intentamos esta ruta:
if (is.null(por_default) && file.exists("/mnt/data/student-por.csv")) {
  por_default <- read.csv("/mnt/data/student-por.csv", sep = ";")
}
if (is.null(mat_default) && file.exists("/mnt/data/student-mat.csv")) {
  mat_default <- read.csv("/mnt/data/student-mat.csv", sep = ";")
}

# Columnas candidatas cuantitativas típicas del dataset UCI Student Performance
quant_candidates <- c("G1","G2","age","absences","studytime","failures","famrel",
                      "freetime","goout","Dalc","Walc","health","traveltime")

# ------- UI -------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Actividad 3 · Regresión lineal simple para predecir G3 (MAT & POR)"),
  sidebarLayout(
    sidebarPanel(
      h4("Fuente de datos"),
      tabsetPanel(type = "pills", id = "tabs_data",
                  tabPanel("Usar CSV locales",
                           helpText("Coloca en la misma carpeta del app: student-mat.csv y student-por.csv (separador ';')."),
                           verbatimTextOutput("status_local", placeholder = TRUE)
                  ),
                  tabPanel("Cargar CSV manualmente",
                           fileInput("file_mat", "student-mat.csv", accept = c(".csv")),
                           fileInput("file_por", "student-por.csv", accept = c(".csv")),
                           helpText("Ambos CSV deben tener separador ';' y columna G3.")
                  )
      ),
      hr(),
      h4("Opciones de análisis"),
      radioButtons("curso", "Curso (variable respuesta fija: G3)", choices = c("MAT","POR"), inline = TRUE),
      uiOutput("sel_predictor"),
      uiOutput("sel_excluir"),
      checkboxInput("usar_top2", "Mostrar solo las 2 mejores variables cuantitativas por |r|", TRUE),
      actionButton("ajustar", "Ajustar / Actualizar", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(id = "tabs_main", type = "tabs",
                  tabPanel("Correlaciones",
                           br(),
                           fluidRow(
                             column(6, h4("Matriz de correlación (Pearson)"), plotOutput("plot_corr")),
                             column(6, h4("Top correlaciones con G3"), DTOutput("tabla_corr"))
                           ),
                           br(),
                           p(em("Nota: solo se consideran variables cuantitativas; G2 suele ser candidata fuerte."))
                  ),
                  tabPanel("Resumen del modelo",
                           br(),
                           h4("Ecuación estimada"),
                           verbatimTextOutput("modelo_eq"),
                           br(),
                           h4("Coeficientes (con pruebas t)"),
                           DTOutput("tabla_coef"),
                           br(),
                           fluidRow(
                             column(6, h4("Métricas globales"), DTOutput("tabla_glance")),
                             column(6, h4("ANOVA (prueba F global)"), DTOutput("tabla_anova"))
                           ),
                           br(),
                           h4("Intervalos de confianza de coeficientes"),
                           DTOutput("tabla_confint"),
                           br(),
                           h4("Ajuste gráfico"),
                           plotOutput("plot_fit", height = "380px")
                  ),
                  tabPanel("Diagnóstico",
                           br(),
                           h4("Gráficos base de diagnóstico"),
                           plotOutput("plot_diag", height = "520px"),
                           br(),
                           h4("Pruebas de supuestos"),
                           DTOutput("tabla_tests"),
                           br(),
                           h4("Influencia: Cook vs. leverage"),
                           plotOutput("plot_influence", height = "420px"),
                           h4("Top observaciones por distancia de Cook"),
                           DTOutput("tabla_cook")
                  ),
                  tabPanel("Predicción",
                           br(),
                           fluidRow(
                             column(4, uiOutput("num_pred")),
                             column(4, sliderInput("nivel_ic", "Nivel de confianza:", min = 0.80, max = 0.99, value = 0.95, step = 0.01)),
                             column(4, sliderInput("nivel_ip", "Nivel de predicción:", min = 0.80, max = 0.99, value = 0.95, step = 0.01))
                           ),
                           br(),
                           h4("Predicción puntual, IC y IP para el valor seleccionado"),
                           verbatimTextOutput("pred_text"),
                           br(),
                           h4("Bandas: IC (media) vs IP (individual)"),
                           plotOutput("plot_bands", height = "420px")
                  )
      )
    )
  )
)

# ------- SERVER -------
server <- function(input, output, session) {
  
  # Reactivos de datos según fuente seleccionada
  dat_por <- reactive({
    req(input$tabs_data)
    if (!is.null(input$file_por)) {
      read.csv(input$file_por$datapath, sep = ";")
    } else if (!is.null(por_default)) {
      por_default
    } else {
      validate(need(FALSE, "No se encontró student-por.csv. Cárgalo manualmente en la pestaña correspondiente."))
    }
  })
  
  dat_mat <- reactive({
    req(input$tabs_data)
    if (!is.null(input$file_mat)) {
      read.csv(input$file_mat$datapath, sep = ";")
    } else if (!is.null(mat_default)) {
      mat_default
    } else {
      validate(need(FALSE, "No se encontró student-mat.csv. Cárgalo manualmente en la pestaña correspondiente."))
    }
  })
  
  output$status_local <- renderText({
    paste0(
      "POR local: ", ifelse(is.null(por_default), "No encontrado", "OK"), "\n",
      "MAT local: ", ifelse(is.null(mat_default), "No encontrado", "OK")
    )
  })
  
  # Dataset activo por curso
  dat_active <- reactive({
    if (input$curso == "POR") dat_por() else dat_mat()
  })
  
  # Variables cuantitativas candidatas presentes en dataset
  vars_quant <- reactive({
    intersect(quant_candidates, names(dat_active()))
  })
  
  # Ordenar por |correlación| con G3 y ofrecer top 2 (o todas)
  sorted_predictors <- reactive({
    df <- dat_active()
    vq <- vars_quant()
    vq <- setdiff(vq, "G3")
    df_num <- df[, intersect(c("G3", vq), names(df)), drop = FALSE]
    df_num <- df_num[, sapply(df_num, is.numeric), drop = FALSE]
    if (!"G3" %in% names(df_num)) return(character(0))
    C <- suppressWarnings(cor(na.omit(df_num), use = "pairwise.complete.obs", method = "pearson"))
    r <- sort(abs(C[,"G3"][setdiff(colnames(C), "G3")]), decreasing = TRUE)
    names(r)
  })
  
  output$sel_predictor <- renderUI({
    choices <- sorted_predictors()
    if (length(choices) == 0) choices <- vars_quant()
    if (input$usar_top2 && length(choices) >= 2) {
      choices <- choices[1:2]
    }
    selectInput("xvar", "Variable predictora (cuantitativa):", choices = choices, selected = choices[1])
  })
  
  # Exclusión de observaciones por índice
  output$sel_excluir <- renderUI({
    df <- dat_active()
    checkboxGroupInput("excluir", "Excluir observaciones (por índice):",
                       inline = FALSE,
                       choices = as.character(1:nrow(df)),
                       selected = character(0))
  })
  
  # Data filtrada por exclusión
  dat_filtered <- reactive({
    df <- dat_active()
    exc <- as.integer(input$excluir)
    if (length(exc)) {
      df <- df[-exc, , drop = FALSE]
    }
    df
  })
  
  # Modelo reactivo
  fit_model <- eventReactive(input$ajustar, {
    df <- dat_filtered()
    req(input$xvar, "G3" %in% names(df), input$xvar %in% names(df))
    # Asegurar numérica
    df <- df %>% mutate(across(all_of(c("G3", input$xvar)), as.numeric))
    lm(as.formula(paste("G3 ~", input$xvar)), data = df)
  }, ignoreInit = FALSE)
  
  # ---- Correlaciones ----
  output$plot_corr <- renderPlot({
    df <- dat_filtered()
    cols <- intersect(c("G3", vars_quant()), names(df))
    cols <- cols[sapply(df[, cols, drop = FALSE], is.numeric)]
    validate(need(length(cols) >= 2, "No hay suficientes variables numéricas para correlaciones."))
    GGally::ggcorr(df[, cols], method = c("pairwise", "pearson"), label = TRUE) +
      ggtitle(paste0(input$curso, " — Correlaciones (Pearson)"))
  })
  
  output$tabla_corr <- renderDT({
    df <- dat_filtered()
    cols <- intersect(c("G3", vars_quant()), names(df))
    cols <- cols[sapply(df[, cols, drop = FALSE], is.numeric)]
    if (!"G3" %in% cols) return(DT::datatable(data.frame(Mensaje="No se encuentra G3"), options = list(dom='t')))
    C <- suppressWarnings(cor(na.omit(df[, cols, drop = FALSE]), use = "pairwise.complete.obs", method = "pearson"))
    r <- sort(C[,"G3"][setdiff(colnames(C), "G3")], decreasing = TRUE)
    DT::datatable(data.frame(Variable = names(r), Pearson_r = round(as.numeric(r), 3)),
                  options = list(pageLength = 5, dom = 'tp'))
  })
  
  # ---- Resumen del modelo ----
  output$modelo_eq <- renderPrint({
    fit <- fit_model()
    co <- coef(fit)
    cat(sprintf("G3 = %.3f + %.3f × %s\n", co[1], co[2], input$xvar))
  })
  
  output$tabla_coef <- renderDT({
    fit <- fit_model()
    co <- broom::tidy(fit)
    DT::datatable(co, options = list(dom='t', paging = FALSE))
  })
  
  output$tabla_glance <- renderDT({
    fit <- fit_model()
    gl <- broom::glance(fit) %>%
      transmute(R2 = r.squared, R2_ajustado = adj.r.squared, Sigma = sigma,
                F = statistic, p_value_F = p.value, gl = df)
    DT::datatable(round(gl, 5), options = list(dom='t', paging = FALSE))
  })
  
  output$tabla_anova <- renderDT({
    fit <- fit_model()
    an <- anova(fit) %>% as.data.frame()
    DT::datatable(round(an, 5), options = list(dom='t', paging = FALSE))
  })
  
  output$tabla_confint <- renderDT({
    fit <- fit_model()
    ci <- confint(fit) %>% as.data.frame()
    ci$Parametro <- rownames(ci)
    ci <- ci %>% select(Parametro, `2.5 %`, `97.5 %`)
    DT::datatable(round(ci, 5), options = list(dom='t', paging = FALSE))
  })
  
  output$plot_fit <- renderPlot({
    fit <- fit_model()
    df <- model.frame(fit)
    ggplot(df, aes_string(x = names(df)[2], y = "G3")) +
      geom_point(alpha = .75) +
      geom_smooth(method = "lm", se = TRUE, color = "steelblue", fill = "pink") +
      labs(title = paste0(input$curso, ": G3 ~ ", input$xvar), x = input$xvar, y = "G3") +
      theme_light()
  })
  
  # ---- Diagnóstico ----
  output$plot_diag <- renderPlot({
    fit <- fit_model()
    par(mfrow = c(2,2)); plot(fit); par(mfrow = c(1,1))
  })
  
  output$tabla_tests <- renderDT({
    fit <- fit_model()
    res <- residuals(fit)
    p_shapiro <- tryCatch(shapiro.test(res)$p.value, error = function(e) NA_real_)
    p_bp <- tryCatch(bptest(fit)$p.value, error = function(e) NA_real_)
    p_dw <- tryCatch(dwtest(fit)$p.value, error = function(e) NA_real_)
    out <- data.frame(
      Hipotesis = c("Normalidad de errores (Shapiro–Wilk)",
                    "Homocedasticidad (Breusch–Pagan)",
                    "No autocorrelación (Durbin–Watson)"),
      p_value = round(c(p_shapiro, p_bp, p_dw), 5)
    )
    DT::datatable(out, options = list(dom='t', paging = FALSE))
  })
  
  output$plot_influence <- renderPlot({
    fit <- fit_model()
    car::influencePlot(fit, main = "Distancia de Cook vs leverage", sub = paste0(input$curso, " — ", input$xvar))
  })
  
  output$tabla_cook <- renderDT({
    fit <- fit_model()
    cd <- cooks.distance(fit)
    ord <- order(cd, decreasing = TRUE)
    top <- data.frame(Obs = ord[1:min(10, length(ord))], Cook = cd[ord][1:min(10, length(ord))])
    DT::datatable(round(top, 5), options = list(dom='t', paging = FALSE))
  })
  
  # ---- Predicción ----
  output$num_pred <- renderUI({
    fit <- fit_model()
    xname <- names(model.frame(fit))[2]
    rng <- range(model.frame(fit)[[xname]], na.rm = TRUE)
    numericInput("x0", label = paste0("Valor de ", xname, " para predecir G3:"),
                 value = mean(rng), min = rng[1], max = rng[2], step = 1)
  })
  
  output$pred_text <- renderPrint({
    fit <- fit_model()
    xname <- names(model.frame(fit))[2]
    newd <- setNames(data.frame(input$x0), xname)
    ic <- predict(fit, newdata = newd, interval = "confidence", level = input$nivel_ic)
    ip <- predict(fit, newdata = newd, interval = "prediction", level = input$nivel_ip)
    cat("IC (media) nivel", input$nivel_ic, ":\n")
    print(ic)
    cat("\nIP (observación individual) nivel", input$nivel_ip, ":\n")
    print(ip)
  })
  
  output$plot_bands <- renderPlot({
    fit <- fit_model()
    df <- model.frame(fit)
    xname <- names(df)[2]
    xseq <- seq(min(df[[xname]], na.rm = TRUE), max(df[[xname]], na.rm = TRUE), length.out = 200)
    nd <- setNames(data.frame(xseq), xname)
    ic <- predict(fit, newdata = nd, interval = "confidence", level = input$nivel_ic)
    ip <- predict(fit, newdata = nd, interval = "prediction", level = input$nivel_ip)
    plot_df <- data.frame(x = xseq, fit = ic[,"fit"],
                          lwr_c = ic[,"lwr"], upr_c = ic[,"upr"],
                          lwr_p = ip[,"lwr"], upr_p = ip[,"upr"])
    ggplot() +
      geom_point(data = df, aes_string(x = xname, y = "G3"), alpha = 0.7) +
      geom_line(data = plot_df, aes(x = x, y = fit), color = "steelblue", linewidth = 1) +
      geom_ribbon(data = plot_df, aes(x = x, ymin = lwr_c, ymax = upr_c, fill = "IC media"), alpha = 0.25) +
      geom_ribbon(data = plot_df, aes(x = x, ymin = lwr_p, ymax = upr_p, fill = "IP individual"), alpha = 0.12) +
      scale_fill_manual(values = c("IC media"="steelblue","IP individual"="red")) +
      labs(title = paste0(input$curso, ": IC vs IP — G3 ~ ", input$xvar), x = xname, y = "G3") +
      theme_light()
  })
  
}

shinyApp(ui, server)

