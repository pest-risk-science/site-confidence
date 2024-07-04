#' Install and load packages
#'
#' @description
#' Helper function to install and load needed packages.
#'
#' @details
#' Determines which packages are installed, then installs needed packages first
#' from github and other repositories, then from CRAN.  Then loads all packages
#' into the environment.
#'
#' @return Nothing
#' @export
load_my_packages <- function() {

  # Packages to load
  pkg_list <- c("trapDetect")

  # Packages already installed
  my_installed_packages <- installed.packages()[,1]

  # Installing Packages from Github
  if(!("trapDetect" %in% my_installed_packages)) {
    devtools::install_github("dangladish/trapDetect")
  }

  # Installing Packages from CRAN
  to_install <- pkg_list[which(is.na(match(pkg_list,my_installed_packages)))]
  if(length(to_install)>0) {
    install.packages(to_install)
  }

  # Loading Packages
  suppressMessages(eval(parse(text=paste("library(",pkg_list,")"))))

}


