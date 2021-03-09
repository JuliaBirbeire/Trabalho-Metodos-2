#______________________________________ TRABALHO 1 - METODOS 2 ______________________________________________
# JULIA BIRBEIRE MACHADO - 190090090
# pacotes
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(moments)

banco <- amostra_190090090

#____________ VARIAVEIS QUANTITATIVAS ----
## NOTA_LP ----
#### distribuição de frequencia
d <- banco$NOTA_LP
brk <-  seq(95,335,20) 
classes<-c("95|--115", "115|--135", "135|--155", "155|--175", "175|--195", "195|--215","215|--235",
           "235|--255","255|--275","275|--295","295|--315","315|--335")

dist_freq_lp <- data.frame(table(cut(d,breaks=brk,right=FALSE,labels=classes))) %>%
  mutate(Freq_relativa = round((Freq/sum(Freq))*100,2),
         FAcumulada = cumsum(Freq),
         F_rel_acumulada = cumsum(Freq_relativa))

#### histograma
ggplot(banco, aes(x=NOTA_LP)) + geom_histogram(colour="white", fill="#008080",binwidth=20)+
  labs(x="Nota Lingua Portuguesa", y="Frequência Absoluta") +
  theme_bw() + 
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

ggsave("histlp.png", width = 160, height = 80, units = "mm")

#### boxplot
ggplot(banco, aes(x=factor(""), y=NOTA_LP)) +
  geom_boxplot(fill=c("#008080"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="")+
  theme_bw() +
  theme(panel.border = element_blank())

ggsave("boxlp.png", width = 160, height = 80, units = "mm")

#### medidas de posição
summary(banco$NOTA_LP)
sd(banco$NOTA_LP)
kurtosis(banco$NOTA_LP)
skewness(banco$NOTA_LP)


## NOTA_MT ----
#### distribuição de frequencias
d <- banco$NOTA_MT
brk <-  seq(115,360,20) 
classes<-c("115|--135", "135|--155", "155|--175", "175|--195", "195|--215","215|--235","235|--255",
           "255|--275","275|--295","295|--315","315|--335","335|--358")

dist_freq_mt <- data.frame(table(cut(d,breaks=brk,right=FALSE,labels=classes))) %>%
  mutate(Freq_relativa = round((Freq/sum(Freq))*100,2),
         FAcumulada = cumsum(Freq),
         F_rel_acumulada = cumsum(Freq_relativa))

#### histograma
ggplot(banco, aes(x=NOTA_MT)) + geom_histogram(colour="white", fill="#008080",binwidth=20)+
  labs(x="Nota Matemática", y="Frequência Absoluta") +
  theme_bw() + 
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

ggsave("histmt.png", width = 160, height = 80, units = "mm")


#### boxplot
ggplot(banco, aes(x=factor(""), y=NOTA_MT)) +
  geom_boxplot(fill=c("#008080"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="")+
  theme_bw() +
  theme(panel.border = element_blank())

ggsave("boxmt.png", width = 160, height = 80, units = "mm")

#### medidas de posição
summary(banco$NOTA_MT)
sd(banco$NOTA_MT)
kurtosis(banco$NOTA_MT)
skewness(banco$NOTA_MT)


#____________ VARIAVEIS QUALITATIVAS ----
## REGIAO----
#### mudando o nome das observações
banco$REGIAO <- str_replace_all(banco$REGIAO,"1","Norte")
banco$REGIAO <- str_replace_all(banco$REGIAO,"2","Nordeste")
banco$REGIAO <- str_replace_all(banco$REGIAO,"3","Sudeste")
banco$REGIAO <- str_replace_all(banco$REGIAO,"4","Sul")
banco$REGIAO <- str_replace_all(banco$REGIAO,"5","Centro-Oeste")

#### tabela
tb_regiao <- banco %>% 
  group_by(REGIAO) %>% 
  summarise(Frequencia = n()) %>%
  mutate(Freq_relativa = round((Frequencia/sum(Frequencia))*100,3))

#### grafico
banco %>%
  group_by(REGIAO) %>%
  count(REGIAO)%>%
  ggplot(aes(x = reorder(REGIAO,-n), y = n))+ geom_bar(stat="identity", fill="#008080") +
  labs(x="Região", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

ggsave("regiao.png", width = 160, height = 80, units = "mm")


## COMPUTADOR ----
#### mudando o nome das observações
banco$COMPUTADOR <- str_replace_all(banco$COMPUTADOR,"A","Não tem")
banco$COMPUTADOR <- str_replace_all(banco$COMPUTADOR,"B","Sim, um")
banco$COMPUTADOR <- str_replace_all(banco$COMPUTADOR,"C","Sim, dois")
banco$COMPUTADOR <- str_replace_all(banco$COMPUTADOR,"D","Sim, três")
banco$COMPUTADOR <- str_replace_all(banco$COMPUTADOR,"E","Quatro ou mais")

#### tabela
tb_computador <- banco %>% 
  group_by(COMPUTADOR) %>%
  summarise(Frequencia = n()) 
tb_computador <-na.omit(tb_computador)
tb_computador <- tb_computador %>% 
  mutate(Freq_relativa = round((Frequencia/sum(Frequencia))*100,2)) 

#### grafico
banco %>%
  group_by(COMPUTADOR) %>%
  count(COMPUTADOR)%>%
  ggplot(aes(x = reorder(COMPUTADOR,-n), y = n))+ geom_bar(stat="identity", fill="#008080") +
  labs(x="Computador", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

ggsave("computador.png", width = 160, height = 80, units = "mm")

## MORA COM A MAE ----
#### mudando o nome das observaçoes
banco$MORA_MÃE <- str_replace_all(banco$MORA_MÃE,"A","Sim")
banco$MORA_MÃE <- str_replace_all(banco$MORA_MÃE,"B","Não")
banco$MORA_MÃE <- str_replace_all(banco$MORA_MÃE,"C","Não, mas moro com outra\nmulher responsável por mim")

#### tabela
tb_mae <- banco %>% 
  group_by(MORA_MÃE) %>% 
  summarise(Frequencia = n()) 
tb_mae <- na.omit(tb_mae)
tb_mae<- tb_mae%>%
  mutate(Freq_relativa = round((Frequencia/sum(Frequencia))*100,2)) 


#### grafico
banco %>%
  group_by(MORA_MÃE) %>%
  count(MORA_MÃE)%>%
  ggplot(aes(x = reorder(MORA_MÃE,-n), y = n))+ geom_bar(stat="identity", fill="#008080") +
  labs(x="Mora com a Mãe", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

ggsave("moramae.png", width = 160, height = 80, units = "mm")


## MORA COM O PAI ----
#### mudando o nome das observaçoes
banco$MORA_PAI <- str_replace_all(banco$MORA_PAI,"A","Sim")
banco$MORA_PAI <- str_replace_all(banco$MORA_PAI,"B","Não")
banco$MORA_PAI <- str_replace_all(banco$MORA_PAI,"C","Não, mas moro com outro\nhomem responsável por mim")

#### tabela
tb_pai <- banco %>% 
  group_by(MORA_PAI) %>% 
  summarise(Frequencia = n()) 
tb_pai <- na.omit(tb_pai)
tb_pai<- tb_pai%>%
  mutate(Freq_relativa = round((Frequencia/sum(Frequencia))*100,2)) 

### grafico
banco %>%
  group_by(MORA_PAI) %>%
  count(MORA_PAI)%>%
  ggplot(aes(x = reorder(MORA_PAI,-n), y = n))+ geom_bar(stat="identity", fill="#008080") +
  labs(x="Mora com o Pai", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

ggsave("morapai.png", width = 160, height = 80, units = "mm")


## AFAZERES DOMESTICOS ----
#### mudando o nome das observações
banco$AFAZERES_DOM[banco$AFAZERES_DOM == 'A'] <- 'Menos de 1\nhora'
banco$AFAZERES_DOM[banco$AFAZERES_DOM == 'B'] <- 'Entre 1 e 2\nhoras'
banco$AFAZERES_DOM[banco$AFAZERES_DOM == 'C'] <- 'Mais de\n2 horas,\naté 3 horas'
banco$AFAZERES_DOM[banco$AFAZERES_DOM == 'D'] <- 'Mais de 3\nhoras'
banco$AFAZERES_DOM[banco$AFAZERES_DOM == 'E'] <- 'Não faço\ntrabalhos\ndomésticas'

#### tabela
tb_domesticos <- banco %>% 
  group_by(AFAZERES_DOM) %>% 
  summarise(Frequencia = n()) 
tb_domesticos <- na.omit(tb_domesticos)
tb_domesticos <- tb_domesticos %>%
  mutate(Freq_relativa = round((Frequencia/sum(Frequencia))*100,2))


#### grafico
banco %>%
  group_by(AFAZERES_DOM) %>%
  count(AFAZERES_DOM)%>%
  ggplot(aes(x = reorder(AFAZERES_DOM,-n), y = n))+ geom_bar(stat="identity", fill="#008080") +
  labs(x="Trabalhos Domésticos", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

ggsave("afazeres.png", width = 160, height = 80, units = "mm")


