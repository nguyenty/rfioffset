source("readdata.R")
source("design_list.R")
source("fit_model.R")
source("simulation_criteria.R")
vnf <- c("Line", "Block", "RFI", "RINb", "Concb", 
          "RINa", "Conca", "neut", "lymp", 
          "mono", "eosi", "baso")
vnf <- c("Line")
fm <- as.formula(paste(" ~ ", paste(vnf, collapse = "+")))
vntest <- c("Line")
model_th <- 1
criteria <- 1
fit_model(vnf, vntest, criteria,  counts, model_th)
