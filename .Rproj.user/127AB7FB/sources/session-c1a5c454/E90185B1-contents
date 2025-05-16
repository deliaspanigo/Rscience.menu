#' @export
fn_yaml_2_R <- function(choices){
  
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