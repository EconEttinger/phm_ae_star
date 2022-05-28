###https://www.nature.com/articles/srep31893#citeas 

library(Epi)


pop <- 1000000

b <- rnorm(pop, mean = 0.689, sd = 0.173)


### mild disease ###

mild <- (sum(b >= 0.80) / pop) * pop

## CI of the proportion
  mp_mild <- glm(cbind(mild, pop-mild) ~ 1, family = binomial)
  mild_CI <- round(ci.pred(mp_mild)*100,4)

### moderate disease ###

moderate<- (sum(b >= 0.5 & b< 0.8) / pop) * pop
mp_moderate <- glm(cbind(moderate, pop-moderate) ~ 1, family = binomial)
moderate_CI <- round(ci.pred(mp_moderate)*100,4)


###severe disease ###
severe <- (sum(b >= 0.3 & b < 0.5) / pop) * pop
mp_severe <- glm(cbind(severe, pop-severe) ~ 1, family = binomial)
severe_CI <- round(ci.pred(mp_severe)*100,4)

### very severe disease ####
very_severe <- (sum(b < 0.3) / pop) * pop
mp_very_severe <- glm(cbind(very_severe, pop-very_severe) ~ 1, family = binomial)
very_severe_CI <- round(ci.pred(mp_very_severe)*100,4)

### DO we need confiendence intervals?