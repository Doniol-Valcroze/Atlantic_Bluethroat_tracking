###
# See https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html
###

library(GeoPressureR)

## OPTION 1: Run workflow step-by-step for a single tag
id <- "CS759" # Run a single tag
geopressuretemplate_config(id)
tag <- geopressuretemplate_tag(id)
graph <- geopressuretemplate_graph(id)
geopressuretemplate_pressurepath(id)


rdata_path <- paste0("C:/Users/pdoniolvalcroze/Desktop/Admin_MNHN/LUSSVENAM_Short_Com/Data_Code_LUSSVENAM/Multi_sensor_data_analysis_LUSSVENAM/data/interim/", id, ".RData")

if (file.exists(rdata_path)) {load(rdata_path)}


## OPTION 2: Run entire workflow for all tags
list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)

for (id in list_id){
  cli::cli_h1("Run for {id}")
  geopressuretemplate(id)
}


## OPTION 3: All tracks, step-by-step

# 1. Compute likelihood map
for (id in list_id){
  cli::cli_h1("Run tag for {id}")
  geopressuretemplate_tag(id)
}

# 2. (optional) Manual check of labeling
# geopressureviz("18LX")
# write.csv(path_geopressureviz, glue::glue("./data/interim/geopressureviz_{id}.csv", row.names = FALSE))

# 3. (optional) Add wind if not done before
for (id in list_id){
  cli::cli_h1("Run tag_download_wind for {id}")
  load(glue::glue("./data/interim/{id}.RData"))
  tag_download_wind(tag)
}

# 4. Run graph
for (id in list_id){
  cli::cli_h1("Run graph for {id}")
  geopressuretemplate_graph(id)
}

# 5. Run pressurepath
for (id in list_id){
  cli::cli_h1("Run pressurepath for {id}")
  geopressuretemplate_pressurepath(id)
}
