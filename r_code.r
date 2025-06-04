# Pacotes necessários
library(ggplot2)
library(AER)
library(dplyr)
library(GGally)
library(scales)
library(MASS)
librqary(pscl)

# Dados
data("RecreationDemand")

# --------------------
# Plote da head (Figura 1)
# --------------------
head(RecreationDemand)

# --------------------
# Summary da variável trips ( Figura 2)
# --------------------
summary(RecreationDemand$trips)

# --------------------
# Distribuição de frequências da variável trips (Figura 3)
# --------------------

#INSERIR

# --------------------
# Mapa de correlação (Figura 4)
# --------------------
df <- RecreationDemand %>%
    mutate(ski = factor(ifelse(ski == "yes", "Ski", "No Ski"))) %>%
    select(trips, quality, income, costS, costC, costH, ski, userfee)

pair_plot <- ggpairs(
    df,
    columns = 1:6,
    mapping = aes(color = ski, alpha = 0.5),
    upper = list(continuous = wrap("cor", size = 3)),
    lower = list(continuous = wrap("points", alpha = 0.5, size = 1)),
    diag = list(continuous = wrap("densityDiag", alpha = 0.5))
) +
    theme_bw() +
    theme(
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "gray90"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
    )

ggsave("pairplot_clean.png", pair_plot, width = 10, height = 8, dpi = 300)


# --------------------
# Histograma por qualidade (Figura 5)
# --------------------
data_filtered <- subset(RecreationDemand, quality > 0)

ggplot(data_filtered, aes(x = trips)) +
    geom_histogram(binwidth = 1, color = "black", fill = "gray") +
    facet_wrap(~quality) +
    xlab("Número de viagens") +
    theme_minimal()


# --------------------
# Log da média de viagens por qualidade e status de esqui (Figura 6)
# --------------------
sumStats_ski <- RecreationDemand %>%
    group_by(quality, ski) %>%
    summarise(
        mean_trips = mean(trips),
        log_mean_trips = log(mean_trips + 1),
        n = n(),
        .groups = 'drop'
    ) %>%
    filter(!is.na(quality))

ggplot(sumStats_ski, aes(x = quality, y = log_mean_trips, shape = ski, color = ski)) +
    geom_point(size = 4, stroke = 1.2) +
    scale_x_continuous(breaks = 0:5, limits = c(-0.5, 5.5)) +
    scale_shape_manual(values = c(16, 17)) +
    scale_color_manual(values = c("#1f77b4", "#ff7f0e")) +
    labs(
        x = "Nível de Qualidade (0-5)",
        y = "Log(média de viagens + 1)",
        title = "Log Média de Viagens por Qualidade e Status de Esqui",
        shape = "Grupo",
        color = "Grupo"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
    )

# --------------------
# Transformando ski e userfee 0 e 1:
# --------------------
RecreationDemand$ski <- ifelse(RecreationDemand$ski == "yes", 1, 0)
RecreationDemand$userfee <- ifelse(RecreationDemand$userfee == "yes", 1, 0)

# --------------------
# Criando data quality_squared com nova  variável quality2
# --------------------
quality_squared <- RecreationDemand %>%
    mutate(quality2 = quality^2)

# --------------------
# Modelo Poisson
# --------------------
poisson_model <- glm(trips ~ quality + quality2 + costS + ski, family = poisson, data = quality_squared)

# --------------------
# Resumo do modelo (Figura 7)
# --------------------

summary(poisson_model)

# --------------------
# Intervalos de confiança para os coeficientes usando o método wald (figura 8)
# --------------------
wald_confint <- function(model) {
    coefs <- coef(model)
    se <- sqrt(diag(vcov(model)))
    Lower_CI <- coefs - 1.96 * se
    Upper_CI <- coefs + 1.96 * se
    data.frame(lower = lower, upper = upper)
}

wald_confint(poisson_model)

# --------------------
# Intervalos de confiança
confint(poisson_model) # Figura 9
exp(confint(poisson_model)) # Figura 10

# --------------------
# Residual Deviance
# --------------------
1 - pchisq(poisson_model$deviance, poisson_model$df.residual)
# Resultado: [1] 0

# --------------------
# Estatística de Pearson
# --------------------
P_disp <- function(x) {
    pr <- sum(residuals(x, type = "pearson")^2)
    dispersion <- pr / x$df.residual
    cat("\nPearson Chi² =", pr, "\nRatio (Dispersion) =", dispersion, "\n")
}

P_disp(poisson_model)

# --------------------
# Probabilidade de zero
# --------------------
lambda <- predict(poisson_model, type = "response")
p_zero <- exp(-lambda)

expected_zeros <- sum(p_zero)

expected_zeros

# Resultado: [1] 337.6309

# ---------------------
# Teste de Sobredispersão de Cameron-Trivedi
# ---------------------

# Obter valores ajustados (mu_hat) e valores observados (Y)
mu_hat <- predict(poisson_model, type = "response")
Y <- quality_squared$trips

# Calcular Z
Z <- ((Y - mu_hat)^2 - Y) / (mu_hat * sqrt(2))

# Regressão sem intercepto
overdispersion_test <- lm(Z ~ mu_hat - 1)

# Resumo do teste (Figura 11)
summary(overdispersion_test)

# --------------------
# Ajuste de Modelo Negativo Binomial
# --------------------

nb_model <- glm.nb(trips ~ quality + quality2 + costS + ski, data = quality_squared)
summary(nb_model) # Figura 12

# --------------------
# Ajuste de Modelo Zero-Inflated Poisson
# --------------------

zip_model <- zeroinfl(trips ~ quality + quality2 + costS + ski | costS, data = quality_squared, dist = "poisson")
summary(zip_model) # Figura 13

# --------------------
# Comparação de Modelos
# --------------------

# Teste de Vuong
vuong(zip_model, poisson_model) # Figura 14

vuong(zip_model, nb_model) # Figura 15

# Comparação de AIC
AIC(zip_model, poisson_model, nb_model) # Figura 16
# --------------------