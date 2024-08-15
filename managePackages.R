required_packages <- c('mFilter', "tseries", "ggplot2", "lubridate", "vars")

for (pkg in required_packages) {
  if (!pkg %in% rownames(installed.packages())) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}