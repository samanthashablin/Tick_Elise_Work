# function to get boostraping "percentile" CI - only one random effect is allowed
# we use the case boostrap (fully non-parametric)


#' Title
#'
#' @param model: put a lmer model 
#' @param bootN: the number of boostrapping (defaul = 1000), but you could increase to 10,000 for your real results 
#'
#' @return
#' @export
#'
#' @examples
#' 
nonpara_boot <- function(model, bootN = 1000){
  
  # helper function for boostrap()
  my_get <- function(model) { 
    beta <- getME(model, "beta")
    sigma <- getME(model, "sigma")
    sigma2 <- sigma^2
    tau2 <- unname(sigma * getME(model, "theta"))^2
    rep <- tau2/(tau2 + sigma2)
    c(beta = beta, sigma2 = sigma2,  tau2 = tau2, rep = rep) 
  }
  
  # boostrapping - type = "case" fully
  # we are only respmpleing "individual" - resample = c(TRUE, FALSE)
  #  for why - see "Van der Leeden, R., Meijer, E. and Busing F. M. (2008) Resampling multilevel models. In J. de Leeuw and E. Meijer, editors, Handbook of Multilevel Analysis, pages 401–433. New York: Springer."
  # The book chapter says "The cases bootstrap, finally, requires minimal assumptions: Only the hierarchical dependency in the data is assumed to be specified correctly."
  boot_res <- lmeresampler::bootstrap(model = model, fn = my_get, type = "case", B = bootN, resample = c(TRUE, FALSE))
  # get the CI for everything
  paraN <- length(boot_res$t0)
  
  # percetile CI does not assume normality and has other good properties
  ind_fun <- function(index){boot.ci(boot_res, index = index, type = c("norm", "basic", "perc"))}
  # get the list of CI results
  ci_list <- map(1:paraN, ind_fun)
  # get values
  ci_value <- map(ci_list, "normal")
  
  # getting lower and upper limits
  ci_low <- flatten_dbl(map(ci_value, 2))
  ci_upr <- flatten_dbl(map(ci_value, 3))
  
  # getting names of the parameters
  Name <- c(names(fixef(model)), names(boot_res$t0)[-(1:length(names(fixef(model))))])
  
  # putting all togther
  res <- tibble(Name = Name, Est = as.numeric(boot_res$t0) , CI_L = ci_low, CI_U =  ci_upr)
  return(res)
  
}

Model1<-lmer(Total.Time.Spent.Questing~Life.Stage+Infection.Status+Day+(1|Combined.ID),data=TotaltimeQuesters)


new_res <- nonpara_boot(model = Model1, bootN = 1000)
summary(new_res)
new_res


Model2<-lmer(Questing.Height.Average~Life.Stage+Infection.Status+Day+(1|Combined.ID),data=TotaltimeQuesters)


new_res2 <- nonpara_boot(model = Model2, bootN = 1000)
summary(new_res2)
new_res2
#moderate repeatability is not significant in questing time and height









