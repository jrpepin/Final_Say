

m_tidy <- tidy(fe_mheM[[1]])

ame_stata <- NULL # create empty df for test results

stakes  <- c("High-stakes", "High-stakes", "Low-stakes", "Low-stakes")
decider <- c("He decided", "She decided", "He decided", "She decided")
data <- data.frame(race, decider)




avg_slopes(fe_mheM[[1]], variables = c("dum"), by = c("decision" * "per"))





s1M <- modelsummary(ame_mheM, shape = decision + model ~ relinc + per,
                    gof_map = NA, stars = c("*"=.05, "**"=.01, "***"=0.001), output = "huxtable") 

