
#' @export
fn_PK_folder_path_yaml <- function(){
  

  
  # Local ---------------------------------------------------------------------- # RETIRAR AL FINAL
  the_local_folder <- file.path(getwd(), "../../../", "inst")
  
  if(dir.exists(the_local_folder)){
    the_folder_path <- file.path(the_local_folder, "yaml")  
  } 
  # ----------------------------------------------------------------------------
  if(!dir.exists(the_local_folder)){
    #From packages
    the_package_path <- find.package("Rscience.menu")
    the_folder_path <- file.path(the_package_path, "yaml") # NO DETALLAR "inst"!!!!
    
  }
  # ----------------------------------------------------------------------------
  
  

  
  
  
  return(the_folder_path)
}


