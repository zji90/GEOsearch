#' GEOSearchUI
#' 
#' Launch the GEOsearch graphical user interface
#'
#' This function will automatically launch the GEOsearch user interface in a web browser. 
#' The user interface provides many powerful functions (e.g. second-round search) which is not available by command line programming.
#' It also provides a much easier and more convenient way to perform all functions available in command lines.
#' The user interface can also be accessed by http://zhiji.shinyapps.io/GEOsearch. Neither R nor any packages are required in this online version.
#' However, it is highly recommended that the user interface be launched locally for faster running speed.
#' 
#' @export
#' @author Zhicheng Ji, Hongkai Ji <zji4@@zji4.edu>
#' @examples
#' if(interactive()) {
#'    GEOSearchUI()
#' }

GEOSearchUI <- function() {
      shiny::runApp(system.file("shiny",package="GEOsearch"))
}

