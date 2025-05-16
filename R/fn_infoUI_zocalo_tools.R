#' @export
fn_infoUI_zocalo_tools <- function(df_data_obj) {
  req(df_data_obj)
  
  df_safe <- tryCatch(df_data_obj, error = function(e) NULL)
  if (is.null(df_safe)) return("Sin datos.")
  
  vector_labels <- rownames(df_safe)
  vector_external <- df_safe[, "choice_external"]
  
  
  div(
    class = "mb-3 p-2 rounded",
    style = "background-color: rgba(25, 135, 84, 0.05); border-left: 4px solid #198754;",
    
    h5(class = "text-success", icon("tools", class = "me-2"), "Selected tool"),
      
      # Aquí se generan para cada par
      div(
        class = "d-flex flex-column", # o 'flex-wrap' si quieres varias en línea
        lapply(seq_along(vector_labels), function(i) {
          tags$div(
            style = "display: flex; align-items: center; margin-bottom: 5px;",
            tags$b(style = "padding-left: 10px; width: 150px;", paste0(vector_labels[i], ":")),
            span(vector_external[i], style = "font-family: monospace;")
          )
        })
      )
    )
  
  
}
