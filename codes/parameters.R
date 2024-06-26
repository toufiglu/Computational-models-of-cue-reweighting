RESET_MODELS = F
get_path <- function(filename) return(paste0("", filename))
result.panel.size = 0.98
duo.panel.key <- unit(0.2, 'cm')
SET_SEED = T
cues <- c("VOT_centered", "f0_centered")
prior_kappa.plot = 4^(1:6) 
prior_nu.plot = 4^(1:6) 
n.subject = 1
example_label = "Cue_reweighting"
# set parameter ranges considered by the optimization algorithm
## parameters for representations model
range.prior_kappa <- c(1, 10000)
range.prior_nu <- c(4, 10000) 
## parameters for bias model
range.lapse_rate <- c(0, 1)
range.beta_pi <- c(10^-3, 10^2)
## parameters for normalization model
range.prior_kappa.normalization <- c(1, 10000)
categories.IH <- c("/b/", "/p/")
maxRep = 1000