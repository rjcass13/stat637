# Section 4.5 of the book

load(url('https://byu.box.com/shared/static/ok29pvry4dep5hb0wilutz0e09n8nk2i.rdata'))

##################
##### Part 1 #####
##################

# eta_ij = alpha_0 + gamma_j
options(contrasts=c("contr.sum", "contr.poly"))
moda <- glm(sat.bin~is.very.dark, family = binomial, data = crabs)

# eta_i = beta_0 + I(x_i = True)beta_1
options(contrasts=c("contr.treatment", "contr.poly"))
modb <- glm(sat.bin~is.very.dark, family = binomial, data = crabs)

coef(moda)
alpha_hat_0 <- coef(moda)[1] # Intercept is alpha_hat_0
gamma_hat_1 <- coef(moda)[2]# is.very.dark1 is gamma_hat_1
gamma_hat_2 <- -1 * gamma_hat_1 # so gamma_hat_2 = - gamma_hat_1

coef(modb)
beta_hat_0 <- coef(modb)[1] # Intercept is beta_hat_0
beta_hat_1 <- coef(modb)[2] # is.very.darkTRUE is beta_hat_1

# Notice that beta_hat_0 = alpha_hat_0 + gamma_hat_1
round(beta_hat_0, 4) == round(alpha_hat_0 + gamma_hat_1, 4)
# This is also log(prob(sat | not very dark)/(1-prob))

# beta_hat_0 + beta_hat_1 = alpha_hat_0 + gamma_hat_2
round(beta_hat_0 + beta_hat_1, 4) == round(alpha_hat_0 + gamma_hat_2, 4)
# = log(prob(sat | very dark)/(1-prob))


##################
##### Part 2 #####
##################
mod2 <- glm(sat.bin~color, data = crabs, family=binomial)
summary(mod2) # p-value for dark med is .04
anova(mod2) # at least one of the betas is not 0
# Test if light-medium and medium-dark are different
cont <- c(0, 0, -1, 1)
betadiff <- cont%*%coef(mod2) # estimte of difference
betadiffvar <- cont%*%summary(mod2)$cov.unscaled%*%cont # variance of difference
zdiff <- betadiff/sqrt(betadiffvar)
zdiff
pnorm(abs(zdiff), lower.tail=FALSE)*2 # p-value

##################
##### Part 3 #####
##################
# Collinearity is still a problem in GLMs
mod3 <- glm(sat.bin~width+weight, family = binomial, data = crabs)
summary(mod3)
mod3b <- glm(sat.bin~width+weight, family = binomial, data = crabs)
summary(mod3b)
# VIF
mod3c <- glm(sat.bin~weight, family = binomial, data = crabs)
anova(mod3c)
car::vif(mod3b)


##################
##### Part 4 #####
##################

# You can do stepwise selection on GLM using the step() function
# Selection gets more complicated when you consider testing/choosing links as well
# Some common methods for link selection as well:
# 1. Pick the link that makes the most sense/is interpretable, etc.
# 2. Pick a link, do variable selection, then test links with the seelcted variables
# etc. Do something you understand and can defend

# Order matters when doing the ANOVA test
mod4a <- glm(sat.bin ~ width + weight + color + spine, data = crabs, family = binomial)
# Given that width in the model, is weight worth being in the model? No
anova(mod4a)

# Remove weight
mod4b <- glm(sat.bin ~ width + color + spine, data = crabs, family = binomial)
# Given that width in the model, is color worth being in the model? No
anova(mod4b)

mod4d <- glm(sat.bin ~ width + is.very.dark, data = crabs, family = binomial)
anova(mod4d)
summary(mod4d)

