#Esse script tem por finalidade adquirir os registros de ocorrência de espécies lenhosas da Caatinga no GBIF

# Os seguintes passos serão realizados:
# a) Download de registros de ocorrência para Magnoliopsida presentes no GBIF (extração de dados provenientes de um polígono
# que contempla a Caatinga)
# b) Observação e tratamento dos registros de ocorrência (exclusão de espaços em branco e linhas com problemas) 
# c) Seleção da lista de espécies para submissão ao TNRS
# d) Observação e Tratamento da planilha vinda do TNRS (remoção de duplicados, seleção de aceitos, exclusão de campos em
# branco, remoção de gêneros [presentes na coluna de espécies])
# e) Unir os dados registros de ocorrência (dfnovo) com a lista de espécies pós-TNRS (tnrs_GBIF_final)
# f) Seleção das lenhosas do dataset do GBIF (assim temos um conjunto de dados de registros de ocorrência para espécies 
# lenhosas da caatinga)


# Autora: Nicolli Albuquerque
# Modificações e acréscimos: Juliana Stropp 

#0.Definindo o diretório atual
setwd("C:/Users/MyName")

#0.Pacotes a serem instalados e ativados
install.packages("rgbif")
library(rgbif)
install.packages("data.table")
library(data.table)
install.packages("plyr")
library(plyr)
install.packages("sqldf")
library(sqldf)
install.packages("beepr")
library(beepr)
install.packages("stringr")
library(stringr)


## a) Download de registros de ocorrência para Magnoliopsida presentes no GBIF (extração de dados provenientes de um polígono
## que contempla a Caatinga)

# Colocar seu nome de usuário, senha e e-mail, como cadastrado no site do GBIF
options(gbif_user="yourlogin") 
options(gbif_pwd="yourpassword")
options(gbif_email="myemail@email.com")

splist <- c('Magnoliopsida')
keys <- sapply(splist, function(x) name_suggest(x)$key[1], USE.NAMES=FALSE)
beep(1) #toca quando finaliza a aquisição das chaves
keys #a chave é utlizada na função "occ_download" para o argumento "taxonKey"

Mag<-occ_download('taxonKey = 220', 
                  'hasCoordinate = TRUE', 
                  'basisOfRecord = PRESERVED_SPECIMEN',
                  'hasGeospatialIssue = FALSE',
                  'decimalLatitude > -21', 'decimalLatitude < 1', 
                  'decimalLongitude > -52', 'decimalLongitude < -28', type = 'and') #selecão de registros de ocorrência em um polígono que comtempla a caatinga
beep(1) #aviso de finalização do processo

occ_download_meta(Mag) #Baixa metadados do arquivo 

occ_download_get(key="0089268-160910150852091", overwrite = TRUE)
beep(1) #aviso de finalização do processo

unzip("C:/Users/MyName/GBIF0089268-160910150852091.zip") #GBIFkey.zip #onde key é argumento da função "occ_download_get"

## b) Observação e tratamento dos registros de ocorrência (exclusão de espaços em branco e linhas com problemas)

n.lines<-count.fields("occurrence.txt", sep = "\t") #conhecer o número de ocorrências
length(n.lines)
n.x<-max(count.fields("occurrence.txt", sep = "\t")) #conhecer o número de atributos
length(n.x)

#Leitura dos dados de ocorrência
df<-fread("occurrence.txt", na.strings=NULL, sep = '\t',stringsAsFactors=FALSE, dec = ".",  colClasses = NA_character_,header="auto", strip.white = TRUE, data.table = FALSE, integer64=getOption("datatable.integer64"),verbose = TRUE) 
#leitura do arquivo "occurence", que possui nosso conjunto de dados, por meio do pacote data.table, porque dessa maneira o processo é acelerado
beep(1) #aviso de finalização do processo

View(df) #Visão geral do dataset (só dá para visualizar até o centésimo atributo da tabela)

#Excluindo linhas com problemas
dfnovo <- df[-c(791843,709557,804795), ]
nrow(df) 
rm(df)
nrow(dfnovo)

#Removendo os espaços em branco para a coluna Espécies 
dfnovo<-dfnovo[!dfnovo$species=="", ]
nrow(dfnovo) 

## c) Seleção da lista de espécies para submissão ao TNRS

sp.gbif.for.tnrs<-data.frame(unique(dfnovo$species))
head(sp.gbif.for.tnrs)
View(sp.gbif.for.tnrs)
colnames(sp.gbif.for.tnrs)[1]<-"speciesgbif"

#Retirar espécies em branco 
sp.gbif.for.tnrs<-subset(sp.gbif.for.tnrs,!sp.gbif.for.tnrs$speciesgbif=="") 
View(sp.gbif.for.tnrs)
nrow(sp.gbif.for.tnrs) #17.522 espécies

write.table(sp.gbif.for.tnrs, file='spgbiffortnrs.csv', sep=';', dec=',', row.names=FALSE) #exporando lista de espécies

## d) Observação e Tratamento da planilha vinda do TNRS (remoção de duplicados, seleção de aceitos, exclusão de campos em
## branco, remoção de gêneros [presentes na coluna de espécies], inserção de coluna identificadora)

species_gbif<-read.csv("GBIF_TNRS_novo.csv",h=T,sep=";")
View(species_gbif)

#Remoção de duplicados 
tnrs.semdup.GBIF<-species_gbif[!duplicated(species_gbif[c("Accepted_name")]),]
View(tnrs.semdup.GBIF) 

#Seleção dos nomes aceitos
tnrs.aceitos.GBIF<-subset(tnrs.semdup.GBIF,tnrs.semdup.GBIF$Taxonomic_status=="Accepted")
nrow(tnrs.aceitos.GBIF)

#Remoção de "nomes inexistentes" (em branco)
tnrs.GBIF.final<-tnrs.aceitos.GBIF[!tnrs.aceitos.GBIF$Accepted_name=="", ]
nrow(tnrs.GBIF.final)

#Retirar os gêneros
str(tnrs.GBIF.final)
summary(tnrs.GBIF.final)
tnrs_GBIF_final<-subset(tnrs.GBIF.final,tnrs.GBIF.final$Name_matched_rank=="species")
nrow(tnrs_GBIF_final)

#Inserir ID para a lista
tnrs_GBIF_final$id_sp<- id(tnrs_GBIF_final[c("Accepted_name")], drop = FALSE)

View(tnrs_GBIF_final)

## e) Unir os dados registros de ocorrência (dfnovo) com a lista de espécies pós-TNRS (tnrs_GBIF_final)
names(tnrs_GBIF_final)
names(dfnovo)
versp<-data.frame(dfnovo$species)
View(versp)
colnames(tnrs_GBIF_final)[27]<-"Especie"

#Criar ID para o dataset de registros de ocorrência (dfnovo)
dfnovo$id_sp<- id(dfnovo[c("species")], drop = FALSE)

#Unir tnrs_GBIF_final e dfnovo (usando sqldf)
juncao_sqldf<-sqldf('select dfnovo.*, tnrs_GBIF_final.* from dfnovo left outer join tnrs_GBIF_final on dfnovo.id_sp = tnrs_GBIF_final.id_sp') #Temos 732.998 registros de ocorrência
View(juncao_sqldf) #confere!

#Verificar juncao_sqldf
verificar<-data.frame(cbind(juncao_sqldf$family,juncao_sqldf$species,juncao_sqldf$genus,juncao_sqldf$id_sp))
View(verificar)

sp1<-subset(verificar,verificar$X4==5351)
nrow(sp1)

df1<-subset(dfnovo,dfnovo$id_sp==5351)
nrow(df1) 

## f) Seleção das lenhosas do dataset do GBIF (assim temos um conjunto de dados de registros de ocorrência para espécies 
## lenhosas da caatinga)
lenhosas<-read.csv("lenhosas_revisadas.csv",header=T, sep=";")

#Interseção das lenhosas com juncao_sqldf
names(lenhosas)
names(juncao_sqldf)
colnames(lenhosas)[2]<-"species"
colnames(lenhosas)[1]<-"id_sp"
juncao.lenhosas<-merge(x = juncao_sqldf, y = lenhosas, by = "species", all= FALSE) 

#Verificar a quantidade de registros de uma lenhosa no juncao.lenhosas e no juncao_sqldf
qt.a<-subset(juncao.lenhosas,juncao.lenhosas$species=="Acalypha brasiliensis")
nrow(qt.a)
qt.b<-subset(juncao_sqldf,juncao_sqldf$species=="Acalypha brasiliensis")
nrow(qt.b)

#Exportar o conjunto de dados de registros de ocorrência para espécies lenhosas da caatinga
write.table(juncao.lenhosas, file='juncao_lenhosas_GBIF.csv', sep=';', dec=',', row.names=FALSE)


## Resumo do Script 

# Arquivos exportados:
# Dados de ocorrência baixados do GBIF: occurrence.txt
# Lista de espécies dos dados de ocorrência para submissão ao TNRS: spgbiffortnrs.csv
# Conjunto de dados de registros de ocorrência para espécies lenhosas da caatinga: juncao_lenhosas_GBIF.csv 

