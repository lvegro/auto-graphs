packages <- c(
"tidyverse",
"here",
"lubridate",
"ggthemes",
"rjson"
)

install.packages(packages)

# Run all scripts in parent folder
files <- list.files(path="./src", pattern="*.R", full.names=T, recursive=FALSE)
sapply(files, source)
