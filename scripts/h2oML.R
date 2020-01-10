pacman::p_load(h2o, data.table, dplyr)

# connecting to h2o cluster
h2o.init()

# loading an ML dataset (the data.table way)
adults <- fread("data/adult.data", stringsAsFactors = T); adults %>% glimpse

# changing from characters to factors using data.table (optional, if not using stringsAsFactors = T in fread())
chars <- names(which(lapply(adults, class) == "character"))

adults[, lapply(.SD, as.factor), .SDcols = chars] %>% glimpse
adults[, (chars) := lapply(.SD, as.factor), .SDcols = chars][] %>% glimpse

# converting into h2oframe
adults_h2o <- as.h2o(adults)
h2o.describe(adults_h2o)

# training the dataset using different algorithms
h2o.deeplearning(x = paste0("V", 1:14), y = "V15", adults_h2o)
h2o.automl(x = paste0("V", 1:14), y = "V15", training_frame = adults_h2o, max_runtime_secs = 60)