
#' @export
MASTER_module_tools_ui <- function(id) {
  ns <- NS(id)
  
  # En tu UI
  div(
    h3("Selección de opciones"),
    fluidRow(
      column(6, uiOutput(ns("todos_selects"))),
      column(6, uiOutput(ns("zocalo_tools")))
    ),
    uiOutput(ns("show_dev"))
    
    
     
  )
  
  
}

#' @export
MASTER_module_tools_server <- function(id, show_dev = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$show_dev <- renderUI({
      req(show_dev)
      div(
        class = "card",
        style = "background-color: #4A6D8C; border: 1px solid #2F4F6F;",  # azul oscuro suave
        tags$div(
          class = "card-header",
          style = "background-color: #2F4F6F; color: black;", # azul muy oscuro, letras negras
          "Información Interna"
        ),
        tags$div(
          class = "card-body",
          # Contenido interior
          div(
            h4("Selección Actual:"),
            verbatimTextOutput(ns("mostrar_seleccion"))
          ),
          div(
            h4("Información detallada (última):"),
            verbatimTextOutput(ns("mostrar_detalles"))
          ),
          div(
            h4("Detalle de todas las selecciones:"),
            verbatimTextOutput(ns("mostrar_todas_selecciones"))
          ),
          div(
            h4("Código de la herramienta:"),
            verbatimTextOutput(ns("mostrar_selected_tool"))
          )
        )
      )
      
      
      
      
    })
    dir_path <- fn_PK_folder_path_yaml()
    list_df_R <- fn_SUPER_mod_better_df(dir_path)
    ###############################################
    df_user_selection_tools <- reactiveVal(NULL)
    selected_tool <- reactiveVal(NULL)

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
        choices <- c("Select one..." = "", choices)
        
        # Valor seleccionado actual (si existe)
        selected_value <- NULL
        selection_list <- selecciones()
        if (menu_name %in% names(selection_list)) {
          selected_value <- selection_list[[menu_name]]
        }
        
        # Creamos el selectInput
        ui_elements[[i]] <- selectInput(
          inputId = ns(paste0("sel_", menu_name)),
          label = paste("Nivel", i, "-", menu_name),
          choices = choices,
          selected = selected_value,
          width = "50%"
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
    
    
    
    # Dentro de tu módulo server, justo después del render de mostrar_detalles:
    # Dentro del server
    df_tool_selection <- reactive({
      menus <- menus_a_mostrar()
      selections <- selecciones()
      
      if (length(selections) == 0) {
        return(NULL) # O un data frame vacío
      }
      
      df_resultado <- data.frame()
      
      for (menu_name in names(selections)) {
        sel_val <- selections[[menu_name]]
        df_menu <- list_df_R[[menu_name]]
        fila <- df_menu[df_menu$choice_internal == sel_val, , drop = FALSE]
        if (nrow(fila) == 0) next
        df_resultado <- rbind(df_resultado, fila)
      }
      
      # Invertir orden si quieres
      # df_resultado <- df_resultado[nrow(df_resultado):1, ]
      if(nrow(df_resultado) >= 1) rownames(df_resultado) <- paste0("label", 1:nrow(df_resultado))
      return(df_resultado)
    })
    
    selected_tool <- reactive({
      req(df_tool_selection())
      the_selected_tools <- df_tool_selection()[nrow(df_tool_selection()), "choice_internal"]
      the_selected_tools
    })
    
    output$mostrar_todas_selecciones <- renderPrint({
      df <- df_tool_selection()
      if (is.null(df) || nrow(df) == 0) {
        cat("No hay seleccionados aún.\n")
      } else {
        print(df)
      }
    })
    
    output$mostrar_selected_tool <- renderPrint({
      req(selected_tool())
      selected_tool()
    })
    
    output$zocalo_tools <- renderUI({
      req(df_tool_selection())
      req(ncol(df_tool_selection())>= 1)
      
      fn_infoUI_zocalo_tools(df_data_obj = df_tool_selection())
    })
    
    return(reactive(list(selected_tool = selected_tool(), 
                         df_tool_selection = df_tool_selection())))
  })
}

