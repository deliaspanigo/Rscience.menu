#' @export
fn_SUPER_mod_better_df <- function(dir_path){
  
  list_yaml <- fn_load_all_files_yaml(dir_path)
  list_R    <- lapply(list_yaml, fn_yaml_2_R)
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
  
  return(list_df_R)
}