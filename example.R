source("binscatter.R")
data("Guns", package = "AER")
Guns$ln_violent <- log(Guns$violent)
Guns$ln_prisoners <- log(Guns$prisoners)

binscatter(formula="ln_violent ~ ln_prisoners", key_var = "ln_prisoners",
           data=Guns, bins=10, partial=FALSE)

binscatter(formula="ln_violent ~ ln_prisoners + year | state | 0 | state" , key_var = "ln_prisoners",
           data=Guns, bins=10, partial=TRUE)

data("CPS1988", package = "AER")
CPS1988$ln_wage <- log(CPS1988$wage)
CPS1988$experience2 <- (CPS1988$experience)^2

binscatter(formula=" ln_wage ~ education" , key_var = "education",
           data=CPS1988, bins=10, partial=FALSE)

binscatter(formula=" ln_wage ~ education + ethnicity" , key_var = "education",
           data=CPS1988, bins=10, partial=TRUE)

binscatter(formula=" ln_wage ~ education + ethnicity | experience + experience2" , key_var = "education",
           data=CPS1988, bins=10, partial=TRUE)

