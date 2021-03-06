# Esse c�digo tem por finalidade selecionar as esp�cies lenhosas da Caatinga presentes nas listas do livro do Siqueira Filho
# e do Marcelo Moro

# Os seguintes passos ser�o realizados:
# a) Receber as listas do livro Siqueira Filho e do Marcelo Moro
# b) Observar e tratar as listas (remo��o de espa�os no nome das esp�cies,jun��o das listas,etc.)
# c) Submeter a lista completa ao TNRS
# d) Submiss�o da lista ao Reflora (por meio do pacote Flora, dispon�vel no R, realizar a capta��o de informa��es 
# sobre as esp�cies, mais especificamente o h�bito, para que seja poss�vel selecionar as esp�cies lenhosas da Caatinga)
# e) An�lise e tratamento do conjunto de dados vindo do Reflora
# f) Sele��o de lenhosas vindas do conjunto de dados que possui h�bito

# Autora: Nicolli Albuquerque
# Modifica��es e acr�scimos:Juliana Stropp

#0.Definindo o diret�rio de trabalho
setwd("C:/Users/MyName")

#0.Pacotes a serem instalados e ativados
install.packages("flora")
library(flora)
install.packages("stringr")
library(stringr)
install.packages("plyr")
library(plyr)

## a) Receber as listas do livro Siqueira Filho e do Marcelo Moro;

#Lista de esp�cies Siqueira Filho
comp.dat<-read.csv("flora_caatinga1.csv", header=TRUE, sep = ";")

#Lista de esp�cies Marcelo Moro
spp_Moro<-readLines("Appendix2_Moro_2.txt")

## b) Observar e tratar as listas (remo��o de espa�os no nome das esp�cies,jun��o das listas, etc...);

##Lista Siqueira-Filho 
head(comp.dat)
str(comp.dat)
comp.dat$Especie2<-as.character(comp.dat$Especie2)

#Uso do pacote stringr para remo��o de espa�os da lista do Siqueira
names<-str_trim(comp.dat$Especie2, side = c("both")) # Remove espacos antes e depois do texto
comp.dat$Especie2<-names # Substitui dados originais (com espaco) por dados sem espaco
names(comp.dat)
comp.dat$fonte<-"Siqueira_Filho"
#submeter_TNRS<-comp.dat[,c("ID", "Especie2")] # Obt�m dados para submeter para TNRS

#Renomear coluna do Siqueira
colnames(comp.dat)[4] <- "species"

#Selecionar colunas do Siqueira
siq1<-comp.dat[,c("species", "fonte")]

##Lista Marcelo Moro
head(spp_Moro)
spp_Moro2<-as.character(spp_Moro)
str(spp_Moro2)
spp_Moro3<-as.data.frame(spp_Moro2)
head(spp_Moro3)
str(spp_Moro3)
spp_Moro3$spp_Moro2<-as.character(spp_Moro3$spp_Moro2)
ss<-strsplit(spp_Moro3$spp_Moro2,'�',fixed=TRUE)
head(ss)
ss[1290]
length(ss)
species_Moro<-as.data.frame(sapply(ss, "[", 1))
colnames(species_Moro)[1]<-'species'
species_Moro$species<-as.character(species_Moro$species)

#Removendo espacos antes e depois do texto atrav�s do pacote stringr
names<-str_trim(species_Moro$species, side = c("both")) 
species_Moro$species<-names
head(species_Moro)
species_Moro$fonte<-"Moro"
str(species_Moro)

##Jun��o  das listas (siq e moro)
998+1692
str(siq1)
str(species_Moro)
spp<-rbind(siq1, species_Moro)

#Remo��o dos duplicados 
spp<-spp[!duplicated(spp[c("species")]),]
nrow(spp)

#Criar uma coluna com um ID para cada esp�cie (usando pacote plyr)
spp$id_species <- id(spp[c("species")], drop = FALSE)
length(unique(spp$species))
length(unique(spp$id_species))

## c) Submeter a lista ao TNRS;

spp[,c(3,2,1)]#colocar id_species como primeira coluna
submeter_TNRS<-spp[,c("id_species", "species")]

#Exportar arquivo para submeter para o TNRS
write.csv(submeter_TNRS, "submeter_tnrs.csv") 

## d) Submiss�o da lista ao Reflora; 
setwd("C:/Users/albuq/Dropbox/UFAL/LACOS XXI/PIBIC 2016-2017 Biogeografia Funcional de Sementes de Esp�cies Lenhosas da Caatinga/Folhas e sementes/Arquivos M�e - N�o Mexer")

#Receber lista do TNRS
submeter_TNRS<-read.csv("tnrs_results_caatinga.csv",header = T,sep=";")
head(submeter_TNRS)
nrow(submeter_TNRS) #3718

#Remover duplicados
submeter_TNRS<-submeter_TNRS[!duplicated(submeter_TNRS[c("Accepted_name")]),]

names(submeter_TNRS)<-c("ID","species") 
head(submeter_TNRS)
names2<-submeter_TNRS$species 
head(names2)
length(names2)

#Uso da fun��o get.taxa do pacote flora
flora_occ<-get.taxa(names2, replace.synonyms = TRUE, suggest.names = TRUE,
                    life.form = TRUE, habitat = TRUE, vernacular = FALSE,
                    states = TRUE, establishment = TRUE, 
                    drop = c("authorship", "genus", "specific.epiteth", "infra.epiteth", "name.status"),
                    suggestion.distance = 0.9, parse = FALSE)

#Resultados do flora para a lista proveniente do TNRS 
head(flora_occ)
nrow(flora_occ)
View(flora_occ)
ncol(flora_occ)

#Remover duplicados
flora_occ<-flora_occ[!duplicated(flora_occ[c("search.str")]),]

##Tratamento do objeto flora_occ 

#Esp�cies que n�o foram encontradas no Reflora
nomes_NA_flora <- flora_occ[(is.na(flora_occ$scientific.name)),]
View(nomes_NA_flora) 
write.csv(nomes_NA_flora,"nomes_NA_flora_conferindo.csv")

#Dados onde o nome foi encontrado no Reflora
flora_nomes_encontrados<-flora_occ[(!is.na(flora_occ$scientific.name)),]
View(flora_nomes_encontrados) 
write.csv(flora_nomes_encontrados,"flora_nomes_encontrados_conferindo.csv")

#Esp�cies onde o h�bito n�o foi retornado do Reflora 
flora_sem_habito<-flora_nomes_encontrados[(is.na(flora_nomes_encontrados$life.form)),]
flora_sem_habito<-flora_sem_habito[flora_sem_habito$taxon.status=="Accepted", ]
View(flora_sem_habito)
write.csv(flora_sem_habito,"flora_sem_habito_conferindo.csv")

#Esp�cies que possuem h�bito
flora_com_habito<-flora_nomes_encontrados[!(is.na(flora_nomes_encontrados$life.form)),] 
View(flora_com_habito)  
write.csv(flora_com_habito,"flora_com_habito_conferindo.csv")

#H�bitos presentes no conjunto de dados 
unique(flora_occ$life.form)

#Salvando arquivo com os dados do flora
write.csv(flora_occ, "flora_Siqueira_Moro_conferindo.csv")

## f) Sele��o de lenhosas vindas do conjunto de dados que possui h�bito;

#Separa��o dos nomes dos h�bitos em colunas, que est�o numa mesma coluna, e classifica��o por meio de 0 ou 1 (verdadeiro
#ou falso)
flora_com_habito$life.form <- as.character(flora_com_habito$life.form)
vv_list<-strsplit(flora_com_habito$life.form,'|',fixed=TRUE)
vv_unique<-unique(unlist(strsplit(flora_com_habito$life.form,'|',fixed=TRUE)))

vv<-matrix(nrow=nrow(flora_com_habito),ncol=length(vv_unique))
colnames(vv)<-vv_unique

for(i in 1:length(vv_unique)){
  for(j in 1:length(vv_list)){
    test<-which(vv_list[[j]]==vv_unique[i])
    vv[j,i]<-ifelse(length(test)>0,1,0)
    
    print(i)
    print(j)
  }
}

vv2<-as.data.frame(vv)
head(vv2)
View(vv2)
habito<-cbind(flora_com_habito, vv2)
names(habito)
View(habito)

#Sele��o das esp�cies lenhosas
lenhosas<-subset(habito,habito$Erva==0 & habito$Suculenta==0)

#Remover duplicados
lenhosas<-lenhosas[!duplicated(lenhosas[c("search.str")]),]
nrow(lenhosas) #1147 especies lenhosas
head(lenhosas)
View(lenhosas) 

#Retorno do arquivo de lenhosas com informa��es
write.csv(lenhosas,"lenhosas_com_info_conferindo.csv")

#Tomando as esp�cies lenhosas que foram substituidas por sin�nimo no flora 
especies_replaced_synonym<-subset(lenhosas,lenhosas$notes=="replaced synonym")
View(especies_replaced_synonym)
write.csv(especies_replaced_synonym,"especies_replaced_synonym_conferindo.csv")
lista_lenhosas_especies<-as.data.frame(lenhosas$search.str) 
View(lista_lenhosas_especies)
colnames(lista_lenhosas_especies)[1]<-"sp_lenhosas"

#Retornando a lista de lenhosas (somente esp�cies)
write.csv(lista_lenhosas_especies, "lista_lenhosas_conferindo.csv")

##Resumo

#Arquivos exportados: 
#1. Lista com o nome de esp�cies que n�o foram encontradas no flora
#2. Lista com esp�cies e seus atributos encontrados no flora
#3. Lista de esp�cies + atributos sem h�bito
#4. Lista de esp�cies + atributos com h�bito
#5. Lista completa de buscas no flora
#6. Lista de plantas lenhosas + atributos
#7. Lista de lenhosas que foram pesquisadas com sin�nimo no flora
#8. Lista com esp�cies lenhosas

