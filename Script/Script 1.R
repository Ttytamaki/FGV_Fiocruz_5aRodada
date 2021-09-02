# ---
# Relatorio 5a Fase - Profissionais de Saude e Covid-19
# FGV x Fiocruz
# 2021
# ---
# Eduardo Ryo Tamaki
# eduardo.rtamaki@gmail.com
#  
# ---
# 02/09/2021
# ---
#
# PREAMBLE ---------------------------------------------------------------------

# Preparando nosso R -----------------------------------------------------------

# Instalando | Carregando os Pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, Hmisc, expss)

# Limpando nosso Environment:
rm(list = ls())

# Se achar necessário:
options(mc.cores = parallel::detectCores())


# Abrir o banco de dados -------------------------------------------------------
## 1a Vez ----------------------------------------------------------------------

### !!ATENÇÃO!! 
### SE JÁ ABRIU O BANCO ANTES, PODE PULARA PARA A LINHA 61

bd <- readxl::read_excel(file.choose(), 
                         col_names = FALSE)

## Criar nome para as colunas (para as variáveis):

# Padrão "qx" de 1 ao numero total de colunas do bd
names(bd) <- paste0("q", 1:ncol(bd))

# Atribuir a cada coluna um Label:
for(i in 1:ncol(bd)){ 
  label(bd[[paste0("q", i)]]) <- bd[[1, i]]
}

## Exportando o DF --
# write_labelled_xlsx(bd, "bd.xlsx")

# Guardando os Labels para consultar se necessário 
labels <- list(label(bd))

# Remover a primeira linha das colunas:
bd <- bd[-1, ]

# Salvando o banco em RData
# save(bd, file = "5a_fase.RData")


## Abrir direto a versão já salva ----------------------------------------------
load("5a_fase.RData")


# Tratando o Banco -------------------------------------------------------------






# Tratando Variáveis -----------------------------------------------------------

# Faixa de Idade:
## fx_id : q90
### A variável Idade tem dois casos que serão transformados
### em NAs: -33 e 5.1
bd <- bd %>%
  mutate(fx_id = case_when(
    q90 >= 19 & q90 <= 29 ~ "19 a 29 Anos",
    q90 >= 30 & q90 <= 39 ~ "30 a 39 Anos",
    q90 >= 40 & q90 <= 49 ~ "40 a 49 Anos",
    q90 >= 50 & q90 <= 59 ~ "50 a 59 Anos",
    q90 >= 60 ~ "Acima de 60 Anos",
    is.na(q90) ~ NA_character_,
    T ~ NA_character_),
    fx_id = as.factor(fx_id)
  )


# Orientação Sexual, Cor/Raça, Sexo
## orient : q93
## sexo : q91
## cor : q89

bd %>% select(q89, q91, q93) %>%
  map(., ~sum(is.na(.)))

# Orientação Sexual:
bd %<>% mutate(orient = as.character(q93),
               orient = if_else(orient == "Homossexual (gay ou lésbica)", "Homossexual", orient),
               orient = as.factor(orient))

# Cor/Raça:
bd <- bd %>% mutate(cor = case_when(q89 %in% c("Preta", "Parda") ~ "Negra",
                                    q89 == "Branca" ~ "Branca",
                                    T ~ "Outros"),
                    cor = as.factor(cor))
                                    

# Sexo:
bd %<>% mutate(sexo = as.factor(q91))


# Serviço (q43)
sum(is.na(bd$q43))

bd <- bd %>% 
  mutate(servico = case_when(q43 %in% c("Laboratório", "Médico(a)", "Outro", 
                                        "Pesquisa", "Privado", "Vigilância") ~ "Outros",
                                        T ~ as.character(q43)),
         servico = as.factor(servico))


# Profissão (q46)
bd %<>% mutate(profissao = as.factor(q46))


# Tempo de Atuação (q47)
## !! ATENÇÃO !! Até agora é a primeira variável que colocamos NA
bd <- bd %>% mutate(tempo_atuacao = case_when(q47 %in% c("Entre 15 e 20 anos\r\nMais de 20 anos",
                                                         "Entre 5 a 10 anos\r\nEntre 15 e 20 anos\r\nMais de 20 anos",
                                                         "Menos de 5 anos\r\nEntre 5 a 10 anos") ~ NA_character_,
                                              T ~ as.character(q47)),
                    tempo_atuacao = as.factor(tempo_atuacao))



# Região (q99)
bd %<>% mutate(regiao = tolower(as.factor(q99)),
               regiao = tools::toTitleCase(regiao))


save(bd, file = "5a_fase_Ed.RData")



