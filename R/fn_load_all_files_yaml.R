#' @export
fn_load_all_files_yaml <- function(dir_path) {
  archivos <- list.files(dir_path, pattern = "\\.yaml$", full.names = TRUE)
  lista <- list()
  for (arch in archivos) {
    nombre <- basename(arch)
    lista[[nombre]] <- yaml::yaml.load_file(arch)$choices
  }
  return(lista)
}