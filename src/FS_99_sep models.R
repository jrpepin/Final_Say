polr3.spWM  <- polr(gdsp ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                  data = subset(data, racesex=="White men"), weights = svyweight, Hess = T)
polr3.spWF  <- polr(gdsp ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="White women"), weights = svyweight, Hess = T)
polr3.spBM  <- polr(gdsp ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="Black men"), weights = svyweight, Hess = T)
polr3.spBW  <- polr(gdsp ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="Black women"), weights = svyweight, Hess = T)

polr3.paWM  <- polr(gdpa ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="White men"), weights = svyweight, Hess = T)
polr3.paWF  <- polr(gdpa ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="White women"), weights = svyweight, Hess = T)
polr3.paBM  <- polr(gdpa ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="Black men"), weights = svyweight, Hess = T)
polr3.paBW  <- polr(gdpa ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="Black women"), weights = svyweight, Hess = T)

polr3.wkWM  <- polr(gdwk ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="White men"), weights = svyweight, Hess = T)
polr3.wkWF  <- polr(gdwk ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="White women"), weights = svyweight, Hess = T)
polr3.wkBM  <- polr(gdwk ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="Black men"), weights = svyweight, Hess = T)
polr3.wkBW  <- polr(gdwk ~ year.c + I(year.c^2) + momed + region + religion + famstru.d,
                    data = subset(data, racesex=="Black women"), weights = svyweight, Hess = T)

tidy1.spWM <- broom::tidy(polr3.spWM)
tidy1.spWF <- broom::tidy(polr3.spWF)
tidy1.spBM <- broom::tidy(polr3.spBM)
tidy1.spBW <- broom::tidy(polr3.spBW)

tidy1.paWM <- broom::tidy(polr3.paWM)
tidy1.paWF <- broom::tidy(polr3.paWF)
tidy1.paBM <- broom::tidy(polr3.paBM)
tidy1.paBW <- broom::tidy(polr3.paBW)

tidy1.wkWM <- broom::tidy(polr3.wkWM)
tidy1.wkWF <- broom::tidy(polr3.wkWF)
tidy1.wkBM <- broom::tidy(polr3.wkBM)
tidy1.wkBW <- broom::tidy(polr3.wkBW)


## Create Odds Ratios
give_me_ORs <- function(tidy_model){
  tidy_model <- tidy_model %>%
    mutate(z_scores = estimate/std.error,
           p.value  = round(2 * (1 - pnorm(abs(z_scores))), 3),
           estimate = case_when(
             coef.type == "coefficient" ~ exp(estimate),
             coef.type == "scale"       ~ estimate))
}

tidy1.spWM <- give_me_ORs(tidy1.spWM)
tidy1.spWF <- give_me_ORs(tidy1.spWF)
tidy1.spBM <- give_me_ORs(tidy1.spBM)
tidy1.spBW <- give_me_ORs(tidy1.spBW)

tidy1.paWM <- give_me_ORs(tidy1.paWM)
tidy1.paWF <- give_me_ORs(tidy1.paWF)
tidy1.paBM <- give_me_ORs(tidy1.paBM)
tidy1.paBW <- give_me_ORs(tidy1.paBW)

tidy1.wkWM <- give_me_ORs(tidy1.wkWM)
tidy1.wkWF <- give_me_ORs(tidy1.wkWF)
tidy1.wkBM <- give_me_ORs(tidy1.wkBM)
tidy1.wkBW <- give_me_ORs(tidy1.wkBW)

