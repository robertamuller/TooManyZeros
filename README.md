# TooManyZeros

# Análise de Sobredispersão em Modelos de Contagem — RecreationDemand

Este repositório contém um trabalho desenvolvido para a disciplina de **Modelagem Estatística**, ministrada pelo professor **Luiz Max Carvalho** ao **5º período dos cursos de Matemática Aplicada e Ciência de Dados da FGV**, em 2025, como parte da nota da **segunda avaliação da disciplina**, A2

## Descrição do Trabalho

O objetivo do trabalho é aplicar e comparar diferentes modelos estatísticos para dados de contagem com excesso de zeros, utilizando o conjunto de dados `RecreationDemand`, que descreve o número de viagens de recreação de barco, `trips`, em função de variáveis socioeconômicas, como `income`, `costS`, `costH`, `costC` e `userfee` e características dos indivíduos, como `quality` e `ski`.

As análises incluem:

- Análise exploratória dos dados;
- Ajuste do modelo de **Poisson**;
- Verificação de **sobredispersão**;
- Ajuste do modelo **Binomial Negativo**;
- Ajuste do modelo **Zero-Inflated Poisson (ZIP)**;
- Comparação dos modelos utilizando critérios como o **AIC** e o **teste de Vuong**.

## Bibliotecas Utilizadas

- `AER` — Conjuntos de dados;
- `ggplot2` — Visualização de dados;
- `dplyr` — Manipulação de dados;
- `GGally` — Extensão do `ggplot2`;
- `MASS` — Modelos estatísticos, incluindo o Binomial Negativo;
- `pscl` — Modelos de contagem com inflação de zeros.
