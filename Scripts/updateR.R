# Update R
install.packages("installr")
library(installr)
updateR()

# Update installed packages
update.packages(ask = FALSE, checkBuilt = TRUE)