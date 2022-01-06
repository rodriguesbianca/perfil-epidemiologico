#carregando os pacotes que serão usados

library(scales)
library(tidyverse) #manipulaçõo
library(ggplot2)
library(knitr)
library(kableExtra)

# carregando o banco de dados
tabaco <- readxl::read_excel("tabagismo_lanest.xlsx")

# renomeando variaveis com espaços
tabaco <- tabaco %>% rename(Comecou_fumar = `HT - 4.1 (Idade que começou a fumar)`,
                            Cigarros_di = `Cigarros diários`,
                            Renda_familiar = `Renda Familiar`,
                            Estado_civil = `Estado Civil`,
                            Teste_Fagerstrom = `Teste de Fagerstrom`,
                            EUROQOL_1 = `EUROQOL 1`,
                            EUROQOL_2 = `EUROQOL 2`,
                            EUROQOL_3 = `EUROQOL 3`,
                            HT_4.2 = `HT - 4.2`,
                            HT_4.3 = `HT - 4.3`,
                            HT_4.4 = `HT - 4.4`,
                            HT_4.5 = `HT - 4.5`,
                            HT_4.6 = `HT - 4.6`,
                            HT_4.7 = `HT - 4.7`) %>%
  # organizando os dados por ordem de idade
  arrange(Idade)
#recodificando  as variaveis em fatores

#função para calcular a moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Criando a variavel "Idade_clas" que classifica a Idade por faixa etaria de 1 a 5
# e criando a variavel "Idade_fumar" que classifica a idade em que o paciente começou
#a fumar em: Criança, adolescente e adulto

tabaco <- tabaco %>%
  mutate(Idade_clas =
           if_else(Idade >=18 & Idade <= 29, "18-29 anos",
                   if_else(Idade >=30 & Idade <= 39, "30-39 anos",
                           if_else(Idade >= 40 & Idade <= 49, "40-49 anos",
                                   if_else(Idade >= 50 & Idade <= 59,"50-59 anos",
                                           "60 anos ou mais")))),
         Idade_comecou_fumar =
           if_else(Comecou_fumar < 12, "Criança",
                   if_else(Comecou_fumar >=12 & Comecou_fumar < 18, "Adolescente", "Adulto")))



### Análise dos dados sociodemograficos ###

# nenomeando as categorias da Escolaridade
tabaco$Escolaridade <- factor(tabaco$Escolaridade,
                              labels = c("Ensino fundamental incompleto",
                                         "Ensino fundamental Completo",
                                         "Ensino Médio Incompleto",
                                         "Ensino Médio Completo",
                                         "Ensino Superior Incompleto",
                                         "Ensino Superior Completo"))

# tabela de frequencias escolaridade
tab.escola <- tabaco %>%
  group_by(Escolaridade) %>%
  summarise(freq = n()) %>%
  mutate(prop = round(100*freq/sum(freq), 1))


# visualização da tabela mais bonitinha :)
kbl(tab.escola,
    col.names = c("Escolaridade", "Frequência", "Percentual %"),
    caption = "Tabela 1: Frequência e porcentagens dos participantes no Programa") %>%
  kable_styling(full_width = TRUE)


#------ Idade dos participantes ------#
## criando o grafico de barras sobre a Idade

(graf_idade <- tabaco %>% count(Idade_clas) %>%

    ggplot(aes(x = Idade_clas, y = (n/nrow(tabaco)))) +
    geom_bar(stat='identity', width = 0.75,  size = 0.01,
             colour = "#64808c", fill = "#64808c") +
    scale_x_discrete(limits = rev(levels(tabaco$Idade_clas)))+
    geom_text(
      aes(label = percent(n/nrow(tabaco))),
      color = "#363636",vjust=-0.3, hjust = 0.5) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), # removendo grid
          panel.grid.minor = element_blank(), #removendo grid
          axis.text.y = element_blank()) +# removendo o eixo y
    labs(title = "Percentual dos participantes de acordo com a idade.",
         subtitle = "", x = "", y = "")

)

#------ Renada Escolar ------#
### renomeando as categorias da variável Reanda familiar
tabaco$Renda_familiar <- factor(tabaco$Renda_familiar,
                                labels = c("Menor ou igual a um salário mínimo",
                                           "Entre dois e três salários mínimos",
                                           "Superior a três salários mínimos"))


## gerando a tabela de freq.
tabela.renda <- tabaco %>%
  group_by(Renda_familiar) %>%
  summarise(total = n()) %>%
  mutate(prop = round(100*total/sum(total),1))


## grafico de barras sobre a renda familiar
(graf_renda <- tabaco %>% count(Renda_familiar) %>%

    ggplot(
      aes(x = Renda_familiar, y = n/nrow(tabaco))) +
    geom_bar(stat='identity', width = 0.75,
             colour = "#64808c", fill = "#64808c") +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(tabaco$Renda_familiar))) +
    geom_text(aes(label = percent(n/nrow(tabaco))), color = "white",
              position=position_dodge(width= 3), vjust=0.9, hjust = 1.5) +


    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), #removendo grid
          axis.text.x=element_blank()) + #remover eixo x
    labs(title = "Renda familiar dos participantes do Programa",
         x = "", y = "")
)


#------ Estado civil ------#
### renomeando as categorias
tabaco$Estado_civil <- factor(tabaco$Estado_civil,
                              levels = c("1","2","3","4"),
                              labels = c("Casado(a)", "Solteiro(a)",
                                         "Divorciado (a)", "viúvo (a)"))

## gerando a tabela de frequencias
tabela.estadocivil <- tabaco %>%
  group_by(Estado_civil) %>%
  summarise(total = n()) %>%
  mutate(prop = round(100 * total / sum(total), 1))


## grafico de barras sobre o estado civil
(graf_estacoc <- tabaco %>% count(Estado_civil) %>%

    ggplot(aes(x = Estado_civil, y = (n/nrow(tabaco)))) +
    geom_bar(stat='identity',width = 0.75,
             colour = "#64808c", fill = "#64808c") +
    scale_y_continuous(
      labels = scales::percent) +
    labs(title = "Estado civil dos participantes do Programa",
         subtitle = " " , x = " ", y = " ") +
    geom_text(aes(label = percent(n/nrow(tabaco))), color = "white",
              position=position_dodge(width= 0.9), vjust=2) +
    theme_classic())


### Análise dos dados sobre o Historico do paciente ###


#------ Idade que comeou a fumar------#

## renomeando as categorias
tabaco$Idade_comecou_fumar = factor(tabaco$Idade_comecou_fumar,
                                    levels = c("Criança", "Adolescente", "Adulto"))

# gerando a tabela de freq.
tabela.cfumar <- tabaco %>%
  group_by(Idade_comecou_fumar) %>%
  summarise(total = n()) %>%
  mutate(prop = round(100*total/sum(total), 2))

# grafico de barras
(graf_estacoc <- tabaco %>% count(Idade_comecou_fumar) %>%

    ggplot(aes(x = Idade_comecou_fumar, y = (n/nrow(tabaco)))) +
    geom_bar(stat='identity', width = 0.65,
             colour = "#64808c", fill = "#64808c") +
    scale_x_discrete(labels = c("Criança ", "Adolescente", "Adulato")) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Idade em que o paciente começou a fumar",
         subtitle = " " , x = " ", y = " ") +
    geom_text(aes(label = percent(n/nrow(tabaco))), color = "white",
              position=position_dodge(width= 0.9), vjust=2) +
    theme_classic())


#------ Quantidade de cigarros consumidas diariamente. ------#

# renomeando as categorias.
tabaco$Cigarros_di <- factor(tabaco$Cigarros_di,
                             levels = c("1","2","3","4"),
                             labels = c("Menos de 10", "Entre 11 e 20",
                                        "Entre 21 e 30", "Mais de 31"))

## grafico de barras
(graf_renda <- tabaco %>% count(Cigarros_di) %>%

    ggplot(aes(x = Cigarros_di, y = (n/nrow(tabaco)))) +
    geom_bar(stat='identity', width = 0.75) +
    geom_col(colour = "#64808c", fill = "#64808c") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = rev(levels(tabaco$Cigarros_di)))+
    geom_text(aes(label = percent(n/nrow(tabaco))), color = "white",
              position=position_dodge(width=3), vjust=1.5, hjust = 0.55) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), #removendo grid
          axis.text.y=element_blank()) + #remover eixo y
    labs(title = "Quantidade de cigarros consumida diariamente",
         x = "", y = "")
)


#------ Quantas vezes tentou parar de fumar ------#

#Renomeando as categorias
tabaco$HT_4.7 <- factor(tabaco$HT_4.7,
                        levels = c("0","1","2","3","4","5"),
                        labels = c("Nenhuma vez", "Uma vez", "Duas vezes",
                                   "Três vezes", "Quatro vezes", "Mais de quatro"))


## grafico qunatas vezes tentou parar
(graf_tentouparar <- tabaco %>% count(HT_4.7) %>%

    ggplot(aes(x = HT_4.7, y = (n/nrow(tabaco)))) +
    geom_bar(stat='identity', width = 0.75,
             colour = "#64808c", fill = "#64808c") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Proporções de vezes em que o paciente tentou parar de fumar.",
         subtitle = " " , x = " ", y = " ") +
    geom_text(aes(label = percent(n/nrow(tabaco))), color = "white",
              position=position_dodge(width= 0.9), vjust=2) +
    theme_minimal()) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_line(colour = "black"))
