source("readdata.R")
source("design_list.R")
source("fit_model.R")
source("simulation_criteria.R")
vnf <- c("Line", "Block", "RFI", "RINb", "Concb", 
          "RINa", "Conca", "neut", "lymp", 
          "mono", "eosi", "baso")
fm <- as.formula(paste(" ~ 0 + ", paste(vnf[-c(1:2)], collapse = "+")))
full_model <- scale(model.matrix(fm))
u <- svd(full_model)$u
colnames(u) <- paste("Prc", 1:10, sep = "")
for (i in 1:dim(u)[2]) assign(paste("Prc", i, sep =''), u[,i])
vnf <- c("Line", "Block", paste("Prc", 1:10, sep = ''))
vnf <- c("Line", "Block")
vntest <- vnf

fit_model(vnf, vntest, criteria,  counts, model_th)
criteria <- 1
model_th <- 1
repeat{
out_model <- fit_model(vnf, vntest, criteria, counts, model_th)
#out_model <- s[[i]]
assign(paste("ms_criteria", model_th, sep = "_" ),out_model)
ms_val <- get(paste("ms_criteria", model_th, sep = "_" ))
cov_del <- ms_val[1,criteria] # cov_del <- 14; i <- 1# model_th <- 2 # criteria <- 1

cov_set <- list_model(vnf, vntest)$test.mat # dim(cov_set)
res <- data.frame(criteria = colnames(ms_val)[criteria], 
                  model = model_th, 
                  cov_del = rownames(cov_set)[ cov_del])
out_pairedend_cbc <- rbind(out_pairedend_cbc, res)
if (cov_del ==1) break
block_ind <- grep("Block2", colnames(full_model))
blockorder_ind <-grep("Blockorder", colnames(full_model))
indicator <- FALSE
if(length(block_ind)!=0){
  if(cov_del+1 == cov_set["Block",2])
  {
    full_model <- full_model[, -c(block_ind, block_ind+1, block_ind+2)]
    indicator <- TRUE
  }
}

if(length(blockorder_ind)!=0){
  if(cov_del+1 == cov_set["Blockorder", 2])
  {
    full_model <- full_model[, -blockorder_ind] 
    indicator <- TRUE
  }
}

if (indicator == FALSE) full_model <- full_model[, -(cov_del +1)] 
  model_th <- model_th +1
}

# write.csv(out_pairedend_cbc, "out_pairedend_cbc.csv", row.names = F)
proc.time() -pm1