#LOGISTIC MODELS
#Isabela Pereira Lima Dias
set_packages <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","stargazer","lmtest","caret","pROC","ROCR","nnet",
             "magick","cowplot","globals","equatiomatic")


if(sum(as.numeric(!set_packages %in% installed.packages())) != 0){
  install_packages <- set_packages[!set_packages %in% installed.packages()]
  for(i in 1:length(install_packages)) {
    install.packages(install_packages, dependencies = T)
    break()}
  sapply(set_packages, require, character = T) 
} else {
  sapply(set_packages, require, character = T) 
}

#With this dataset we study theoretical concepts such as ▪ Probability, Chance (Odds), Logit

#Probability - Logit: natural logarithm of the chance of occurrence of a response of the type “yes”.
prob <- function(z){
  prob = 1/(1+ exp(-z))
}

data.frame(z= -5:5) %>%
  ggplot() +
  stat_function(aes(x = z, color = "Prob. Event"),
                fun = prob,
                size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_color_manual("Legend:",
                     values = "deeppink4") +
  labs(x = "Logit z",
       y = "Probability") +
  theme_bw()
#load the dataset 
load(file = "Late.RData")

late <- Atrasado
rm(Atrasado)


colnames(late)[1] <- "student"
colnames(late)[2] <- "late"
colnames(late)[3] <- "distance"
colnames(late)[4] <- "n_trafficlights"


late %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

summary(late)

table(late)


#Logistic model 

model_late <- glm(formula= late ~ distance + n_trafficlights,
                  data = late,
                  family = "binomial")

summary(late)




