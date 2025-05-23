library(shiny)
library(bslib)
library(yaml)
library(tools)
library(shiny)
cargar_todos_yaml <- function(dir_path) {
  archivos <- list.files(dir_path, pattern = "\\.yaml$", full.names = TRUE)
  lista <- list()
  for (arch in archivos) {
    nombre <- basename(arch)
    lista[[nombre]] <- yaml.load_file(arch)$choices
  }
  return(lista)
}

yaml_2_R <- function(choices){
  
  # Extraer los vectores
  vector_choice_external <- sapply(choices, function(x) x$choice_external)
  vector_choice_internal <- sapply(choices, function(x) x$choice_internal)
  vector_files <- sapply(choices, function(x) x$file)
  vector_file_names <- tools::file_path_sans_ext(vector_files)
  
  # Crear una lista con los vectores
  resultado <- list(
    choice_external = vector_choice_external,
    choice_internal = vector_choice_internal,
    vector_files = vector_files,
    vector_file_names = vector_file_names
  )
  
  return(resultado)
  
}

dir_path <- file.path("inst", "yaml")
list_yaml <- cargar_todos_yaml(dir_path)
list_R    <- lapply(list_yaml, yaml_2_R)
list_R    <- lapply(list_R, function(x){
  mi_list <- x
  vector_files <- x$vector_files
  vector_file_names <- x$vector_file_names
  
  vector_files[vector_files == ""] <- NA
  vector_file_names[vector_file_names == ""] <- NA
  
  mi_list$vector_files <- vector_files
  mi_list$vector_file_names <- vector_file_names
  mi_list
})
list_df_R <- lapply(list_R, data.frame)
names(list_df_R) <- tools::file_path_sans_ext(names(list_df_R))
# # Simulamos tu lista de marcos de datos para el ejemplo
# list_df_R <- list(
#   menu00 = data.frame(
#     choice_external = c("01 - Estadística Descriptiva", "02 - Estadísticos Clásicos", 
#                         "03 - Estadística Multivariada", "04 - Machine Learning"),
#     choice_internal = c("opt_01", "opt_02", "opt_03", "opt_04"),
#     vector_files = c("menu01.yaml", "menu02.yaml", "menu03.yaml", "menu04.yaml"),
#     vector_file_names = c("menu01", "menu02", "menu03", "menu04"),
#     stringsAsFactors = FALSE
#   ),
#   
#   menu01 = data.frame(
#     choice_external = c("01 - Estadística descriptiva 1Q", "02 - Estadística descriptiva 1C",
#                         "02 - Estadística descriptiva 2C independientes", 
#                         "02 - Estadística descriptiva 2C apareadas",
#                         "02 - Estadística descriptiva QC"),
#     choice_internal = c("opt_01", "opt_02", "opt_03", "opt_04", "opt_05"),
#     vector_files = NA,
#     vector_file_names = NA,
#     stringsAsFactors = FALSE
#   ),
#   
#   menu02 = data.frame(
#     choice_external = c("01 - Test para 1Q", "02 - Test para 1C", "03 - Test para 2Q",
#                         "04 - Test para 2C independientes", "05 - Test para 2C apareadas",
#                         "06 - Test para QC", "07 - Modelos Lineales Generales",
#                         "08 - Modelos Lineales Generales Mixtos", "09 - Modelos Lineales Generalizados",
#                         "10 - Modelos Lineales Generalizados Mixtos", 
#                         "11 - Modelos Lineales Generalizados Mixtos",
#                         "12 - Estadistica de distribucion libre"),
#     choice_internal = c("opt_01", "opt_02", "opt_03", "opt_04", "opt_05", "opt_06", "opt_07",
#                         "opt_08", "opt_09", "opt_10", "opt_11", "opt_12"),
#     vector_files = c(NA, NA, NA, NA, NA, NA, "menu02_07.yaml", NA, NA, NA, NA, NA),
#     vector_file_names = c(NA, NA, NA, NA, NA, NA, "menu02_07", NA, NA, NA, NA, NA),
#     stringsAsFactors = FALSE
#   ),
#   
#   menu02_07 = data.frame(
#     choice_external = c("01 - Fixed", "02 - Random", "03 - Mixed"),
#     choice_internal = c("opt_01", "opt_02", "opt_03"),
#     vector_files = c("menu_02_07_01.yaml", NA, NA),
#     vector_file_names = c("menu_02_07_01", NA, NA),
#     stringsAsFactors = FALSE
#   ),
#   
#   menu_02_07_01 = data.frame(
#     choice_external = c("01 - Anova", "02 - Ancova", "03 - Dummy", "04 - Linear Regresion"),
#     choice_internal = c("opt_01", "opt_02", "opt_03", "opt_04"),
#     vector_files = c("menu_02_07_01_01.yaml", NA, NA, NA),
#     vector_file_names = c("menu_02_07_01_01", NA, NA, NA),
#     stringsAsFactors = FALSE
#   ),
#   
#   menu_02_07_01_01 = data.frame(
#     choice_external = c("01 - Anova 1 way", "02 - Anova 1 way and 1 block", 
#                         "03 - Anova 2 ways", "04 - Anova 3 ways", "05 - Anova Multifactorial"),
#     choice_internal = c("opt_01", "opt_02", "opt_03", "opt_04", "opt_05"),
#     vector_files = NA,
#     vector_file_names = NA,
#     stringsAsFactors = FALSE
#   )
# )
# 
# # Añadimos menús adicionales para completar el ejemplo
# list_df_R$menu03 = data.frame(
#   choice_external = c("01 - PCA", "02 - Análisis factorial", "03 - Clustering"),
#   choice_internal = c("opt_01", "opt_02", "opt_03"),
#   vector_files = NA,
#   vector_file_names = NA,
#   stringsAsFactors = FALSE
# )
# 
# list_df_R$menu04 = data.frame(
#   choice_external = c("01 - Clasificación", "02 - Regresión", "03 - Clustering"),
#   choice_internal = c("opt_01", "opt_02", "opt_03"),
#   vector_files = NA,
#   vector_file_names = NA,
#   stringsAsFactors = FALSE
# )

ui <- page_sidebar(
  title = "Generador dinámico de selectInput",
  sidebar = sidebar(
    h3("Selección de opciones"),
    uiOutput("todos_selects")
  ),
  card(
    card_header("Resultados"),
    h4("Selección Actual:"),
    verbatimTextOutput("mostrar_seleccion"),
    h4("Información detallada:"),
    verbatimTextOutput("mostrar_detalles")
  )
)

server <- function(input, output, session) {
  # Iniciamos con el menú raíz
  menu_actual <- reactiveVal("menu00")
  
  # ReactiveVal para almacenar los menús que deben mostrarse
  menus_a_mostrar <- reactiveVal(c("menu00"))
  
  # ReactiveVal para recopilar todas las selecciones
  selecciones <- reactiveVal(list())
  
  # Generar UI para todos los menús que deben mostrarse
  output$todos_selects <- renderUI({
    menus <- menus_a_mostrar()
    
    # Lista para almacenar los elementos UI
    ui_elements <- list()
    
    # Recorremos cada menú y creamos un selectInput para cada uno
    for (i in seq_along(menus)) {
      menu_name <- menus[i]
      
      # Si el menú no existe en nuestra lista, lo omitimos
      if (!menu_name %in% names(list_df_R)) {
        next
      }
      
      # Obtenemos los datos del menú
      menu_data <- list_df_R[[menu_name]]
      
      # Creamos las opciones para el selectInput
      # Usamos choice_internal como valor y choice_external como nombre visible
      choices <- menu_data$choice_internal
      names(choices) <- menu_data$choice_external
      
      # Valor seleccionado actual (si existe)
      selected_value <- NULL
      selection_list <- selecciones()
      if (menu_name %in% names(selection_list)) {
        selected_value <- selection_list[[menu_name]]
      }
      
      # Creamos el selectInput
      ui_elements[[i]] <- selectInput(
        inputId = paste0("sel_", menu_name),
        label = paste("Nivel", i, "-", menu_name),
        choices = choices,
        selected = selected_value
      )
    }
    
    # Devolvemos todos los elementos UI
    do.call(tagList, ui_elements)
  })
  
  # Observar cambios en cualquier selectInput y actualizar la lista de menús
  observe({
    # Obtener todos los inputs de tipo "sel_*"
    sel_inputs <- names(input)[grep("^sel_", names(input))]
    
    if (length(sel_inputs) == 0) return()
    
    # Lista actual de menús
    current_menus <- menus_a_mostrar()
    
    # Creamos una nueva lista de selecciones, solo con los menús activos
    current_selections <- list()
    
    # Añadimos solo las selecciones que corresponden a menús actualmente visibles
    for (menu_name in current_menus) {
      sel_input <- paste0("sel_", menu_name)
      if (sel_input %in% sel_inputs && !is.null(input[[sel_input]])) {
        current_selections[[menu_name]] <- input[[sel_input]]
      }
    }
    
    # Verificar cambios en los inputs existentes
    for (sel_input in sel_inputs) {
      # Extraer el nombre del menú desde el inputId
      menu_name <- sub("^sel_", "", sel_input)
      
      # Saltamos si este menú no está en la lista actual de menús
      if (!menu_name %in% current_menus) next
      
      # Verificar si la selección ha cambiado
      old_selections <- selecciones()
      if (is.null(old_selections[[menu_name]]) || 
          old_selections[[menu_name]] != input[[sel_input]]) {
        
        # Encontramos la posición del menú en la lista actual
        menu_pos <- match(menu_name, current_menus)
        if (is.na(menu_pos)) next
        
        # Si cambiamos un nivel intermedio, cortamos los menús a partir de ahí
        if (menu_pos < length(current_menus)) {
          current_menus <- current_menus[1:menu_pos]
          
          # También actualizamos las selecciones para incluir solo los menús que permanecen
          temp_selections <- list()
          for (active_menu in current_menus) {
            active_sel_input <- paste0("sel_", active_menu)
            if (active_sel_input %in% sel_inputs && !is.null(input[[active_sel_input]])) {
              temp_selections[[active_menu]] <- input[[active_sel_input]]
            }
          }
          current_selections <- temp_selections
        }
        
        # Verificamos si hay un menú hijo para esta selección
        menu_data <- list_df_R[[menu_name]]
        selection <- input[[sel_input]]
        
        selected_row <- which(menu_data$choice_internal == selection)
        if (length(selected_row) > 0) {
          next_menu_name <- menu_data$vector_file_names[selected_row]
          
          # Si hay un menú hijo válido, lo añadimos
          if (!is.na(next_menu_name) && next_menu_name %in% names(list_df_R)) {
            current_menus <- c(current_menus, next_menu_name)
          }
        }
        
        # Actualizamos la lista de menús
        menus_a_mostrar(current_menus)
        
        # Terminamos después de procesar el primer cambio detectado
        break
      }
    }
    
    # Guardamos las selecciones actualizadas (solo las activas)
    selecciones(current_selections)
  })
  
  # Mostrar todas las selecciones en algún output
  output$mostrar_seleccion <- renderPrint({
    selections <- selecciones()
    if (length(selections) == 0) {
      cat("No hay selecciones todavía.\n")
    } else {
      cat("Selecciones actuales:\n")
      for (menu_name in names(selections)) {
        selection_value <- selections[[menu_name]]
        menu_data <- list_df_R[[menu_name]]
        selection_text <- menu_data$choice_external[menu_data$choice_internal == selection_value]
        cat(menu_name, ": ", selection_text, " (", selection_value, ")\n", sep = "")
      }
    }
  })
  
  # Mostrar información detallada sobre la última selección
  output$mostrar_detalles <- renderPrint({
    menus <- menus_a_mostrar()
    selections <- selecciones()
    
    if (length(menus) == 0) {
      cat("No hay menús configurados.\n")
      return()
    }
    
    last_menu <- tail(menus, 1)
    
    if (!last_menu %in% names(selections)) {
      cat("No hay selección para el último menú: ", last_menu, "\n", sep="")
      return()
    }
    
    selection <- selections[[last_menu]]
    menu_data <- list_df_R[[last_menu]]
    
    selected_row <- which(menu_data$choice_internal == selection)
    if (length(selected_row) == 0) {
      cat("No se encontró la selección en el menú.\n")
      return()
    }
    
    cat("Información detallada sobre la última selección:\n\n")
    cat("Menú: ", last_menu, "\n", sep="")
    cat("Selección: ", menu_data$choice_external[selected_row], " (", selection, ")\n\n", sep="")
    
    cat("Datos completos de la fila seleccionada:\n")
    print(menu_data[selected_row, ])
    
    next_menu <- menu_data$vector_file_names[selected_row]
    if (!is.na(next_menu) && next_menu %in% names(list_df_R)) {
      cat("\nEsta selección tiene un submenú asociado: ", next_menu, "\n", sep="")
    } else {
      cat("\nEsta es una selección final (sin submenús).\n")
    }
  })
}


shinyApp(ui, server)