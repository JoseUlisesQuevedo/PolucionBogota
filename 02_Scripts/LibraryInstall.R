# List of libraries to check and install
libraries_to_install <- c("rmarkdown", "ggplot2", "dplyr","lubridate",
                          "tidyr","zoo","leaflet","sf","scales","leaflet",
                          "RColorBrewer","haven",
                          "readxl",
                          "devtools")

# Function to check and install libraries
check_and_install_libraries <- function(libs) {
  missing_libs <- libs[!(libs %in% installed.packages()[,"Package"])]
  
  if (length(missing_libs) > 0) {
    message("Installing missing libraries: ", paste(missing_libs, collapse = ", "))
    install.packages(missing_libs, dependencies = TRUE)
  } else {
    message("All libraries are already installed.")
  }
}

# Call the function with the list of libraries
check_and_install_libraries(libraries_to_install)

if(!("emo" %in% installed.packages()[,"Package"])){
  devtools::install_github("hadley/emo")
}
