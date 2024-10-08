# In global.R or at the top of app_server.R
library(yaml)
config <- yaml::read_yaml("inst/tab-config.yml")
