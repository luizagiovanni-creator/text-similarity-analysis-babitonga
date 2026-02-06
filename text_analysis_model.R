# Pacotes necessários
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(word2vec)

# Função de pré-processamento de texto
pre_processar_texto <- function(texto) {
  texto %>%
    tolower() %>%
    stringr::str_replace_all("[[:punct:]]", " ") %>%
    stringr::str_squish()
}

# Caminho do arquivo - SUBSTITUA pelo seu caminho real
caminho_arquivo <- "C:\Users\09893099919\OneDrive\Documentos\BABITONGA_GERAL\0_dataframes_correcoes_babitonga\Zona.xlsx"

# Ler e preparar os dados
df <- read_excel(caminho_arquivo) %>%
  clean_names() %>%
  mutate(id = row_number()) %>%
  mutate(
    definicao_valida = if_else(
      is.na(definicoes) |
        str_trim(definicoes) == "" |
        str_detect(definicoes, "^\\[.*\\]$"),
      FALSE,
      TRUE
    ),
    texto_analise = if_else(definicao_valida, definicoes, conteudo),
    texto_analise = pre_processar_texto(texto_analise)
  )

# Gerar pares intermunicipais
df_inter <- df %>%
  rename(id_1 = id, municipio_1 = municipio, lei_1 = lei, texto_1 = texto_analise) %>%
  cross_join(
    df %>%
      rename(id_2 = id, municipio_2 = municipio, lei_2 = lei, texto_2 = texto_analise)
  ) %>%
  filter(municipio_1 != municipio_2, id_1 < id_2)

# Treinar modelo Word2Vec localmente
corpus <- c(df_inter$texto_1, df_inter$texto_2)
modelo <- word2vec(corpus, type = "skip-gram", dim = 100, window = 5, iter = 20)

# Função para vetorizar cada texto
obter_embedding <- function(texto, modelo) {
  palavras <- strsplit(texto, " ")[[1]]
  palavras <- palavras[palavras %in% rownames(as.matrix(modelo))]
  if (length(palavras) == 0) return(rep(0, 100))
  colMeans(as.matrix(modelo)[palavras, , drop = FALSE])
}

# Aplicar vetorização
embeddings_1 <- t(sapply(df_inter$texto_1, obter_embedding, modelo = modelo))
embeddings_2 <- t(sapply(df_inter$texto_2, obter_embedding, modelo = modelo))

# Similaridade do cosseno
cosine_similarity <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

df_inter$similaridade <- mapply(cosine_similarity,
                                split(embeddings_1, seq(nrow(embeddings_1))),
                                split(embeddings_2, seq(nrow(embeddings_2))))

# Classificação interpretativa
df_inter <- df_inter %>%
  mutate(classificacao = case_when(
    similaridade <= 0.30 ~ "Muito Divergente",
    similaridade <= 0.50 ~ "Divergente",
    similaridade <= 0.70 ~ "Pouco Divergente",
    similaridade <= 0.85 ~ "Semelhante",
    similaridade >  0.85 ~ "Muito Semelhante",
    TRUE ~ "Indefinido"
  ))

# Criar diretório de saída se necessário
dir_saida <- "C:/Users/09893099919/OneDrive/Documentos/BABITONGA_GERAL/1_analise_divergencias_corrigidos"
dir.create(dir_saida, showWarnings = FALSE)

# Exportar arquivo Excel com nome desejado
arquivo_saida <- file.path(dir_saida, "Zona_Correto.xlsx")
write_xlsx(df_inter, arquivo_saida)