# All packages used

listpaquets <- c('DBI', 'RPostgreSQL', 'data.table', 'dplyr', 'dygraphs', 'ggplot2',
    'ggtern', 'httr', 'leaflet', 'plotly', 'pool', 'readr', 'reshape',
    'shiny', 'stringr', 'suncalc', 'wesanderson', 'xts', 'yaml')
sapply(listpaquets,usethis::use_package)

# Ignore these files
usethis::use_build_ignore("devtools_history.R")
usethis::use_build_ignore("Readme.md")
git remote add origin https://github.com/Rosalien/toolboxApps.git
