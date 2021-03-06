NBDev <- function(counts, design, log.offset, nb.disp, method, print.progress = TRUE) {

    n <- ncol(counts)
    
    if (is.null(log.offset)) 
        log.offset <- rep(0, ncol(counts))
    est.offset <- exp(log.offset)

  SAT.LIKE <- function(counts, disp) {
        means <- counts
        like <- disp * log(disp/(disp + means))
        like[counts != 0] <- like[counts != 0] + counts[counts != 
            0] * log(means[counts != 0]/(disp + means[counts != 
            0]))
        -sum(like)
    }
    LIKE <- function(parms, design, counts, disp, est.offset) {
        means <- as.vector(exp(design %*% parms) * est.offset)
        like <- disp * log(disp/(disp + means))
        like[counts != 0] <- like[counts != 0] + counts[counts != 
            0] * log(means[counts != 0]/(disp + means[counts != 
            0]))
        -sum(like)
    }
    GRAD <- function(parms, design, counts, disp, est.offset) {
        means <- as.vector(exp(design %*% parms) * est.offset)
        colSums(-(counts - means * (disp + counts)/(disp + means)) * 
            design)
    }

    
    deviance.vector <- rep(NA, nrow(counts))
    means <- matrix(NA, nrow(counts), ncol(counts))
    parms <- matrix(NA, nrow(counts), ncol(design))

    ### For each gene and given design matrix, fit glm using provided negative binomial dispersion estimate
    for (gn in 1:nrow(counts)) {
        ### If wanted, provide running progress update (eventually once every 5000 genes)
        if (gn %in% c(2, 10, 100, 500, 1000, 2500, 5000 * (1:200)) & print.progress) 
            print(paste("Analyzing Gene #", gn))
        
if(length(grep(method,"optim"))==1){
        if (ncol(design) > 1) 
            init.parms <- lm(log(counts[gn, ] + 1) ~ design[, 
                -1], offset = log(est.offset))$coefficients
        if (ncol(design) == 1) 
            init.parms <- lm(log(counts[gn, ] + 1) ~ 1, offset = log(est.offset))$coefficients
        opt <- optim(init.parms, fn = LIKE, gr = GRAD, method = "BFGS", 
            design = design, control = list(reltol = 1e-25, maxit = 1000), 
            counts = counts[gn, ], disp = 1/nb.disp[gn], est.offset = est.offset)
        means[gn, ] <- as.vector(exp(design %*% opt$par) * est.offset)
        parms[gn, ] <- opt$par
        deviance.vector[gn] <- 2 * (opt$value - SAT.LIKE(counts[gn, 
            ], 1/nb.disp[gn]))
    }

if(length(grep(method,"glm"))==1){
        #### For 2000 Fly genes, glm takes roughly 9 seconds (optim took roughly 21 seconds)
        glm.fitted <- glm(formula = counts[gn, ] ~ . - 1 + offset(log.offset), family = negbin.br("log", nb.disp[gn]), 
            data = as.data.frame(design), control = glm.control(epsilon = 1e-08, maxit = 100, trace = FALSE))
        parms[gn, ] <- coefficients(glm.fitted)
        
        ### Save optimized means (used in Pearson's dispersion estimator)
        means[gn, ] <- as.vector(exp(design %*% coefficients(glm.fitted)) * est.offset)
        
        ### Save deviance (used to compute LRT for comparing models and also deviance dispersion estimator)
        deviance.vector[gn] <- glm.fitted$deviance
        
        ### When a group has only zero counts, the MLE doesn't exist.  For these genes, we use the method Kosmidis & Firth(2009)
        ### to moderate the parameter estimates.
        ### For 2000 Fly genes, fbr takes roughly 41 seconds
        if (any(counts[gn, ] == 0) && any(abs(coefficients(glm.fitted)) > 3)) {
            fbrflm.fit <- fbrglm(formula = counts[gn, ] ~ . - 1 + offset(log.offset), data = as.data.frame(design), start=coefficients(glm.fitted),
                family = negbin.br("log", nb.disp[gn]), control = glm.control(epsilon = 1e-08, maxit = 100, trace = FALSE))
            parms[gn, ] <- coefficients(fbrflm.fit)
        }
    }
      }
    return(list(dev = deviance.vector, means = means, parms = parms))
} 

