library(gRim)
library(gRain)
library(gRbase)
library(ggplot2)
library(bnlearn)
library(igraph)
library(Rgraphviz)
# Dati GSS con focus su l'opinioni relative alle leggi sulle armi (GUNLAW),
# pena di morte (CAPPUN) e aborto in caso di stupro (ABRAPE).

# Caricamento del dataset
load("GSS.RData")
# Visualizzazione delle variabili contenute nel DS
# insieme al sommario statistico
summary(GSS)
#Visualizzazione dela struttura del DS
str(GSS)


#Eliminazione dei valori mancanti/non validi
GSS_cleaned <- na.omit(GSS)
#Visualizzazione del DS pulito
str(GSS_cleaned)

#Trasformazione delle variabili in fattori
#con etichette per migliorare la leggibilità

#CAPPUN
GSS_cleaned$CAPPUN <- factor(GSS_cleaned$CAPPUN,
                             levels = c(1, 2),
                             labels = c("Favorevole", "Contrario"))

# GUNLAW
GSS_cleaned$GUNLAW <- factor(GSS_cleaned$GUNLAW,
                             levels = c(1, 2),
                             labels = c("Favorevole", "Contrario"))

# SEX
GSS_cleaned$SEX <- factor(GSS_cleaned$SEX,
                          levels = c(1, 2),
                          labels = c("Maschio", "Femmina"))

# ABRAPE: pena per stupro
GSS_cleaned$ABRAPE <- factor(GSS_cleaned$ABRAPE,
                             levels = c(1, 2),
                             labels = c("Favorevole", "Contrario"))

# CONFINAN: fiducia nel governo
GSS_cleaned$CONFINAN <- factor(GSS_cleaned$CONFINAN,
                               levels = c(1, 2, 3),
                               labels = c("Fiducioso", "Neutrale", "Scettico"))

# SATJOB: soddisfazione per il lavoro
GSS_cleaned$SATJOB <- factor(GSS_cleaned$SATJOB,
                             levels = c(1, 2, 3),
                             labels = c("Soddisfatto",
                                        "Neutrale",
                                        "Insoddisfatto"))
#Visualizzazione del DS dopo la fattorizzazione
str(GSS_cleaned)



#Visualizzaione grafico a barre

ggplot(GSS_cleaned, aes(x = CAPPUN)) +
  geom_bar(fill = "purple3") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribuzione di CUPPUN",
       x = "Opinione", y = "Frequenza") +
  theme_minimal()



#Selezione delle variabili di confronto
variabili_confronto <- c("SEX", "SATJOB", "ABRAPE", "GUNLAW", "CONFINAN")
#Ciclo per visualizzazione della matrice di contingenza per ogni variabile
for (var in variabili_confronto) {
  
  tab <- table(GSS_cleaned$CAPPUN, GSS_cleaned[[var]])
  dimnames(tab)[[1]] <- levels(GSS_cleaned$CAPPUN)
  dimnames(tab)[[2]] <- levels(GSS_cleaned[[var]])
  names(dimnames(tab)) <- c("CAPPUN", var)
  
  #Matrice totale
  cat("\n========================================\n")
  cat("Matrice di contingenza assoluta: CAPPUN vs", var, "\n")
  cat("========================================\n")
  print(tab)
  
  #Matrice percentuale
  prop_tab <- round(prop.table(tab, margin = 2) * 100, 1)
  
  cat("\n----------------------------------------\n")
  cat("Distribuzione % per colonna: CAPPUN vs", var, "\n")
  cat("----------------------------------------\n")
  print(prop_tab)
}

#Visualizzazione heatmap
for (var in variabili_confronto) {
  
  # Tabella di contingenza
  tab <- table(GSS_cleaned$CAPPUN, GSS_cleaned[[var]])
  
  # Calcolo percentuali colonna per colonna
  tab_perc <- round(prop.table(tab, margin = 2) * 100, 1)
  
  # Conversione
  df_heat <- as.data.frame(tab_perc)
  names(df_heat) <- c("CAPPUN", "Variabile", "Percentuale")
  df_heat$CAPPUN <- factor(df_heat$CAPPUN, levels = c("Contrario", "Favorevole"))
  
  # Heatmap con percentuali
  print(
  ggplot(df_heat, aes(x = Variabile, y = CAPPUN, fill = Percentuale)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(Percentuale, "%")), color = "black", size = 4) +
    scale_fill_gradient(low = "white", high = "firebrick") +
    labs(title = paste("Heatmap percentuale: CAPPUN vs", var),
         x = var, y = "CAPPUN") +
    theme_minimal()
  )
}



#Creazione del modello saturo
model_sat <- dmod(~.^., GSS_cleaned)

#Creazione del modello di indipendenza
model_ind <- dmod(~.^1, GSS_cleaned)

#Grafo backward con penalizzazione AIC (Default)
backAIC_model <- stepwise(model_sat)
plot(backAIC_model)
title(main="UG backward AIC")

#Grafo backward con penalizzazione BIC
backBIC_model <- stepwise(model_sat, k = log(nrow(GSS_cleaned)))
plot(backBIC_model)
title(main="UG backward BIC")

#Grafo forward con penalizzazione AIC
forwardAIC_model <- stepwise(model_ind, direction="forward")
plot(forwardAIC_model)
title(main="UG forward AIC")

#Grafo forward con penalizzazione BIC
forwardBIC_model <- stepwise(model_ind, k = log(nrow(GSS_cleaned)), direction="forward")
plot(forwardBIC_model)
title(main="UG forward BIC")


#Grafo both con penalizzazione AIC
bothAIC_model <- stepwise(model_ind, direction="both")
plot(bothAIC_model)
title(main = "UG both AIC")

#Grafo both con penalizzazione BIC 
bothBIC_model <- stepwise(model_ind, k = log(nrow(GSS_cleaned)), direction = "both" )
plot(bothBIC_model)
title(main = "UG both BIC")



#Grafi diretti - Reti bayesiane

model_bnstd <- hc(GSS_cleaned)
dag_bnstd <-as.igraph(model_bnstd)
plot(
  dag_bnstd,
  vertex.size = 20,
  vertex.label.cex = 0.7,
  edge.arrow.size = 0.6,
  main = "Rete Bayesiana"
  
)


backgnd_vars <- c("SEX", "SATJOB", "CONFINAN")
target_vars <- c("CAPPUN", "GUNLAW", "ABRAPE")
blacklist <- expand.grid(from = target_vars, to = backgnd_vars)
target_model_bn <- hc(GSS_cleaned, blacklist = blacklist)
dag_target_bn <- as.igraph(target_model_bn)
plot(
  dag_target_bn,
  vertex.size = 20,
  vertex.label.cex = 0.7,
  edge.arrow.size = 0.5,
  main = "Rete Bayesiana con variabile target"
)

#Focus su CAPPUN
GSS_sub_CAPPUN <- GSS_cleaned[,c("CAPPUN","SEX", "SATJOB", "CONFINAN")]
blacklist_CAPPUN <- expand.grid(from = "CAPPUN", to = backgnd_vars)
blacklist_SEX <- expand.grid(from = setdiff(c("CAPPUN", backgnd_vars), "SEX"), to = "SEX")
blacklist_total <- rbind(blacklist_CAPPUN, blacklist_SEX)
CAPPUN_model_bn <- hc(GSS_sub_CAPPUN, score = "aic", blacklist = blacklist_total)
dag_CAPPUN_bn<- as.igraph(CAPPUN_model_bn)
plot(
  dag_CAPPUN_bn,
  vertex.size = 20,
  vertex.label.cex = 0.7,
  edge.arrow.size = 0.5,
  main = "Rete Bayesiana con target CAPPUN"
)



#Stima delle distribuzioni condizionate per ogni variabile
bn_fit <- bn.fit(target_model_bn, data=GSS_cleaned)
#Immissione in un oggetto grain
bn_grain <- as.grain(bn_fit)
#Interrogazione della rete
#Marginali
querygrain(bn_grain, nodes = "CAPPUN", type="marginal")
#Condizionate
querygrain(bn_grain, nodes = c("CAPPUN", "SEX"), type="conditional")
#Congiunta
querygrain(bn_grain, nodes = c("CONFINAN", "SATJOB"), type="joint")

graphviz.chart(bn_fit,
               type = "barprob",
               bar.col = "brown4",
               )

#Impostazione delle evidenze - SEX

GSS_male <- subset(GSS_cleaned, SEX == "Maschio")
bn_fit_male <- bn.fit(target_model_bn, data = GSS_male)
GSS_female <- subset(GSS_cleaned, SEX == "Femmina")
bn_fit_female <- bn.fit(target_model_bn, data = GSS_female)
graphviz.chart(bn_fit_male,
               type = "barprob",
               bar.col = "sienna3"
)
graphviz.chart(bn_fit_female,
               type = "barprob",
               bar.col = "mediumorchid3"
)


GSS_cappun_F <- subset(GSS_cleaned, CAPPUN == "Favorevole")
bn_fit_cappun_F <- bn.fit(target_model_bn, data = GSS_cappun_F)
GSS_cappun_C <- subset(GSS_cleaned, CAPPUN == "Contrario")
bn_fit_cappun_C <- bn.fit(target_model_bn, data = GSS_cappun_C)
graphviz.chart(bn_fit_cappun_F,
               type = "barprob",
               bar.col = "springgreen3"
)
graphviz.chart(bn_fit_cappun_C,
               type = "barprob",
               bar.col = "royalblue3"
)

GSS_abrape_F <- subset(GSS_cleaned, ABRAPE == "Favorevole")
bn_fit_abrape_F <- bn.fit(target_model_bn, data = GSS_abrape_F)
GSS_abrape_C <- subset(GSS_cleaned, ABRAPE == "Contrario")
bn_fit_abrape_C <- bn.fit(target_model_bn, data = GSS_abrape_C)
graphviz.chart(bn_fit_abrape_F,
               type = "barprob",
               bar.col = "purple3"
)
graphviz.chart(bn_fit_abrape_C,
               type = "barprob",
               bar.col = "hotpink"
)


GSS_cappun_F_SEX_M <- subset(GSS_cleaned, CAPPUN == "Favorevole" & SEX == "Maschio")
bn_fit_cappun_F_SEX_M <- bn.fit(target_model_bn, data = GSS_cappun_F_SEX_M)
graphviz.chart(bn_fit_cappun_F_SEX_M,
               type = "barprob",
               bar.col = "orangered1"
)
GSS_cappun_F_SEX_F <- subset(GSS_cleaned, CAPPUN == "Favorevole" & SEX == "Femmina")
bn_fit_cappun_F_SEX_F <- bn.fit(target_model_bn, data = GSS_cappun_F_SEX_F)
graphviz.chart(bn_fit_cappun_F_SEX_F,
               type = "barprob",
               bar.col = "purple2"
)
GSS_cappun_C_SEX_M <- subset(GSS_cleaned, CAPPUN == "Contrario" & SEX == "Maschio")
bn_fit_cappun_C_SEX_M <- bn.fit(target_model_bn, data = GSS_cappun_C_SEX_M)
graphviz.chart(bn_fit_cappun_C_SEX_M,
               type = "barprob",
               bar.col = "magenta2"
)
GSS_cappun_C_SEX_F <- subset(GSS_cleaned, CAPPUN == "Contrario" & SEX == "Femmina")
bn_fit_cappun_C_SEX_F <- bn.fit(target_model_bn, data = GSS_cappun_C_SEX_F)
graphviz.chart(bn_fit_cappun_C_SEX_F,
               type = "barprob",
               bar.col = "forestgreen"
)


#Regrazzione logistica
null_model <- glm(GUNLAW ~ 1, data = GSS_cleaned, family = binomial)
full_model <- glm(GUNLAW ~.^2, data = GSS_cleaned, family = binomial)
scope <- list(lower=formula(null_model), upper = formula(full_model))

back_model_AIC <- step(full_model, scope = scope, direction = "backward", k = 2, trace = TRUE)
summary(back_model_AIC)

#Per il metodo BIC utilizziamo k = log(n)
back_model_BIC <- step(full_model, scope = scope, direction = "backward", k = log(nrow(GSS_cleaned)))
summary(back_model_BIC)

forward_model_AIC <- step(null_model, scope = scope, direction = "forward", k = 2)
summary(forward_model_AIC)

forward_model_BIC <- step(null_model, scope = scope, direction = "forward", k = log(nrow(GSS_cleaned)))
summary(forward_model_BIC)


both_model_AIC <- step(null_model, scope = scope, direction = "both", k = 2)
summary(both_model_AIC)

both_model_BIC <- step(null_model, scope = scope, direction = "both", k = log(nrow(GSS_cleaned)))
summary(both_model_BIC)




























