############
# Preamble #
############

# Directories
root_dir <- getwd()
func_dir <- file.path(root_dir, "R")


# Load Functions
invisible(sapply(list.files(func_dir, "\\.[Rr]$", full.names = TRUE),
                 source, encoding = "UTF-8"))

# Install and load packages
load_my_packages()

############
# Analysis #
############
