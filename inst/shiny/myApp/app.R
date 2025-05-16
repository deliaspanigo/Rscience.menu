library(shiny)
library(bslib)
library(shinyjs)
library(quarto)
library(shinyjs)
library(plotly)
library(htmlwidgets)
library(knitr)

library(shiny)
library(bslib)
library(shinyjs)
library(shinyAce)
library(yaml)

# 
# source("global.R")
# 
# # Cargar los módulos
# source("modulos.R")
# source("modules_model_selection.R")
# source("modules_variables_anova.R")
# source("modules_quarto.R")


# Definir la interfaz de usuario principal
ui <- page_fluid( # Cambiado de page_sidebar a page_fluid para más flexibilidad
  #theme = bs_theme(version = 5, bootswatch = "minty"),
  theme = bs_theme(version = 5),
  
  # Necesario para manipular clases de CSS
  useShinyjs(),
  
  div(
    MASTER_module_tools_ui(id ="module_tools"),
    uiOutput("show_dev2")
    )
  
)

# Definir el servidor
server <- function(input, output, session) {
  
  list_user_tool_selection <- MASTER_module_tools_server(id ="module_tools", show_dev = T)
  
  # observe(print(list_user_tool_selection()))
  
  output$mostrar_list <- renderPrint({
    obj <- list_user_tool_selection()
    print(obj)
  })
  
  output$show_dev2 <- renderUI({

    
    div(
      class = "card",
      style = "background-color: #D2B48C; border: 1px solid #A0522D;", # fondo marrón claro y borde más oscuro
      tags$div(
        class = "card-header",
        style = "background-color: #A0522D; color: black;", # marrón oscuro, letras negras
        "Desde afuera..."
      ),
      tags$div(
        class = "card-body",
        verbatimTextOutput("mostrar_list")
      )
    )
    
    
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
