#' @export
fn_load_all_files_yaml <- function(dir_path) {
  archivos <- list.files(dir_path, pattern = "\\.yaml$", full.names = TRUE)
  lista <- list()
  
  for (arch in archivos) {
    nombre <- basename(arch)
    
    obj_label   <- yaml::yaml.load_file(arch)$label
    obj_choices <- yaml::yaml.load_file(arch)$choices
    
    lista[[nombre]] <- list(obj_label = obj_label,
                            obj_choices = obj_choices)
  }
  return(lista)
}