# Definindo o projeto
# Pergunta: O que afeta a qualidade do ar e como?

library(Ecdat)

data(Airq)

names(Airq)

# Análise descritiva

summary(Airq)

plot(airq ~ vala, data = Airq)

# Modelo estatístico linear

# ------------------------------------------- #

m1 <- lm(formula = airq ~ vala, data = Airq)

summary(m1)

# Não influencia

# ------------------------------------------- #

library(fastDummies)

Airq <- dummy_cols(Airq, select_columns = "coas")

m2 <- lm(formula = airq ~ coas_yes, data = Airq)

summary(m2)

plot(airq ~ coas_yes, data = Airq)

# ------------------------------------------- #

m3 <- lm(formula = airq ~ medi, data = Airq)

summary(m3)

plot(airq ~ medi, data = Airq)


m4 <- lm(formula = airq ~ rain, data = Airq)

summary(m4)

plot(airq ~ rain, data = Airq)


m5 <- lm(formula = airq ~ dens, data = Airq)

summary(m5)

plot(airq ~ dens, data = Airq)

# --------------------------------------- #

plot(airq ~ medi, data = Airq,
     xlab = "Renda", ylab = "Qualidade do Ar",
     pch = 16, col = "blue", main = "Renda média")

curve(9.936e+01+5.638e-04*x, add = TRUE, lwd = 2,
      lty = 2)

# --------------------------------------- #


# Regressão multipla

mm1 <- lm(formula = airq ~ vala + coas_yes, data = Airq)

summary(mm1)

plot(airq ~ vala, data = Airq)

curve(1.171e+02 + 1.999e-03 * x , add = T)
curve(1.171e+02 + 1.999e-03 * x + (-2.968e+01) , lty = 2 , add = T)
legend("bottomright", c("Não-Costeira","Costeira"), pch = 1, lty = c(1,2), bty = "n")


mm2 <- lm(formula = airq ~ vala + coas_yes + dens, data = Airq)
summary(mm2)

# Constraste de modelos


modelo_completo <- lm(formula = airq ~ vala + coas_yes + dens, data = Airq)
summary(modelo_completo)

modelo_anova <- lm(formula = airq ~ vala + coas_yes, data = Airq)
summary(modelo_anova)

# Esses modelos são iguais? 

# Se P_valor >= 0.05 não existe diferença então ficamos com o modelo mais simples

anova(modelo_completo, modelo_anova)










