
#' @export
fn_PK_folder_path_yaml <- function(){
  

  
  # # Local ---------------------------------------------------------------------- # RETIRAR AL FINAL
  # the_local_folder <- file.path(getwd(), "../../../", "inst")
  # 
  # if(dir.exists(the_local_folder)){
  #   the_folder_path <- file.path(the_local_folder, "yaml")  
  # } 
  # ----------------------------------------------------------------------------
  # if(!dir.exists(the_local_folder)){
    #From packages
  
    # the_package_path <- find.package("Rscience.menu")
    # the_folder_path <- file.path(the_package_path, "yaml") # NO DETALLAR "inst"!!!!
    
  # }
  # ----------------------------------------------------------------------------
  
    the_package <- "Rscience.menu"
    the_folder <- "yaml"
    
    the_package_path <- find.package(the_package)
    vector_folder_paths <- list.dirs(path = the_package_path, recursive = T)
    dt_selected_folder <- grepl(the_folder, vector_folder_paths, ignore.case = TRUE)
    selected_folder_path <- vector_folder_paths[dt_selected_folder]
    
    if(length(selected_folder_path)>1) selected_folder_path <- selected_folder_path[1]
    
    selected_folder_path    
  
  
  
  return(selected_folder_path)
}


