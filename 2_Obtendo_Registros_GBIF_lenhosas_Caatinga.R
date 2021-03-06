#Esse script tem por finalidade adquirir os registros de ocorr�ncia de esp�cies lenhosas da Caatinga no GBIF

# Os seguintes passos ser�o realizados:
# a) Download de registros de ocorr�ncia para Magnoliopsida presentes no GBIF (extra��o de dados provenientes de um pol�gono
# que contempla a Caatinga)
# b) Observa��o e tratamento dos registros de ocorr�ncia (exclus�o de espa�os em branco e linhas com problemas) 
# c) Sele��o da lista de esp�cies para submiss�o ao TNRS
# d) Observa��o e Tratamento da planilha vinda do TNRS (remo��o de duplicados, sele��o de aceitos, exclus�o de campos em
# branco, remo��o de g�neros [presentes na coluna de esp�cies])
# e) Unir os dados registros de ocorr�ncia (dfnovo) com a lista de esp�cies p�s-TNRS (tnrs_GBIF_final)
# f) Sele��o das lenhosas do dataset do GBIF (assim temos um conjunto de dados de registros de ocorr�ncia para esp�cies 
# lenhosas da caatinga)


# Autora: Nicolli Albuquerque
# Modifica��es e acr�scimos: Juliana Stropp 

#0.Definindo o diret�rio atual
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


## a) Download de registros de ocorr�ncia para Magnoliopsida presentes no GBIF (extra��o de dados provenientes de um pol�gono
## que contempla a Caatinga)

# Colocar seu nome de usu�rio, senha e e-mail, como cadastrado no site do GBIF
options(gbif_user="yourlogin") 
options(gbif_pwd="yourpassword")
options(gbif_email="myemail@email.com")

splist <- c('Magnoliopsida')
keys <- sapply(splist, function(x) name_suggest(x)$key[1], USE.NAMES=FALSE)
beep(1) #toca quando finaliza a aquisi��o das chaves
keys #a chave � utlizada na fun��o "occ_download" para o argumento "taxonKey"

Mag<-occ_download('taxonKey = 220', 
                  'hasCoordinate = TRUE', 
                  'basisOfRecord = PRESERVED_SPECIMEN',
                  'hasGeospatialIssue = FALSE',
                  'decimalLatitude > -21', 'decimalLatitude < 1', 
                  'decimalLongitude > -52', 'decimalLongitude < -28', type = 'and') #selec�o de registros de ocorr�ncia em um pol�gono que comtempla a caatinga
beep(1) #aviso de finaliza��o do processo

occ_download_meta(Mag) #Baixa metadados do arquivo 

occ_download_get(key="0089268-160910150852091", overwrite = TRUE)
beep(1) #aviso de finaliza��o do processo

unzip("C:/Users/MyName/GBIF0089268-160910150852091.zip") #GBIFkey.zip #onde key � argumento da fun��o "occ_download_get"

## b) Observa��o e tratamento dos registros de ocorr�ncia (exclus�o de espa�os em branco e linhas com problemas)

n.lines<-count.fields("occurrence.txt", sep = "\t") #conhecer o n�mero de ocorr�ncias
length(n.lines)
n.x<-max(count.fields("occurrence.txt", sep = "\t")) #conhecer o n�mero de atributos
length(n.x)

#Leitura dos dados de ocorr�ncia
df<-fread("occurrence.txt", na.strings=NULL, sep = '\t',stringsAsFactors=FALSE, dec = ".",  colClasses = NA_character_,header="auto", strip.white = TRUE, data.table = FALSE, integer64=getOption("datatable.integer64"),verbose = TRUE) 
#leitura do arquivo "occurence", que possui nosso conjunto de dados, por meio do pacote data.table, porque dessa maneira o processo � acelerado
beep(1) #aviso de finaliza��o do processo

View(df) #Vis�o geral do dataset (s� d� para visualizar at� o cent�simo atributo da tabela)

#Excluindo linhas com problemas
dfnovo <- df[-c(791843,709557,804795), ]
nrow(df) 
rm(df)
nrow(dfnovo)

#Removendo os espa�os em branco para a coluna Esp�cies 
dfnovo<-dfnovo[!dfnovo$species=="", ]
nrow(dfnovo) 

## c) Sele��o da lista de esp�cies para submiss�o ao TNRS

sp.gbif.for.tnrs<-data.frame(unique(dfnovo$species))
head(sp.gbif.for.tnrs)
View(sp.gbif.for.tnrs)
colnames(sp.gbif.for.tnrs)[1]<-"speciesgbif"

#Retirar esp�cies em branco 
sp.gbif.for.tnrs<-subset(sp.gbif.for.tnrs,!sp.gbif.for.tnrs$speciesgbif=="") 
View(sp.gbif.for.tnrs)
nrow(sp.gbif.for.tnrs) #17.522 esp�cies

write.table(sp.gbif.for.tnrs, file='spgbiffortnrs.csv', sep=';', dec=',', row.names=FALSE) #exporando lista de esp�cies

## d) Observa��o e Tratamento da planilha vinda do TNRS (remo��o de duplicados, sele��o de aceitos, exclus�o de campos em
## branco, remo��o de g�neros [presentes na coluna de esp�cies], inser��o de coluna identificadora)

species_gbif<-read.csv("GBIF_TNRS_novo.csv",h=T,sep=";")
View(species_gbif)

#Remo��o de duplicados 
tnrs.semdup.GBIF<-species_gbif[!duplicated(species_gbif[c("Accepted_name")]),]
View(tnrs.semdup.GBIF) 

#Sele��o dos nomes aceitos
tnrs.aceitos.GBIF<-subset(tnrs.semdup.GBIF,tnrs.semdup.GBIF$Taxonomic_status=="Accepted")
nrow(tnrs.aceitos.GBIF)

#Remo��o de "nomes inexistentes" (em branco)
tnrs.GBIF.final<-tnrs.aceitos.GBIF[!tnrs.aceitos.GBIF$Accepted_name=="", ]
nrow(tnrs.GBIF.final)

#Retirar os g�neros
str(tnrs.GBIF.final)
summary(tnrs.GBIF.final)
tnrs_GBIF_final<-subset(tnrs.GBIF.final,tnrs.GBIF.final$Name_matched_rank=="species")
nrow(tnrs_GBIF_final)

#Inserir ID para a lista
tnrs_GBIF_final$id_sp<- id(tnrs_GBIF_final[c("Accepted_name")], drop = FALSE)

View(tnrs_GBIF_final)

## e) Unir os dados registros de ocorr�ncia (dfnovo) com a lista de esp�cies p�s-TNRS (tnrs_GBIF_final)
names(tnrs_GBIF_final)
names(dfnovo)
versp<-data.frame(dfnovo$species)
View(versp)
colnames(tnrs_GBIF_final)[27]<-"Especie"

#Criar ID para o dataset de registros de ocorr�ncia (dfnovo)
dfnovo$id_sp<- id(dfnovo[c("species")], drop = FALSE)

#Unir tnrs_GBIF_final e dfnovo (usando sqldf)
juncao_sqldf<-sqldf('select dfnovo.*, tnrs_GBIF_final.* from dfnovo left outer join tnrs_GBIF_final on dfnovo.id_sp = tnrs_GBIF_final.id_sp') #Temos 732.998 registros de ocorr�ncia
View(juncao_sqldf) #confere!

#Verificar juncao_sqldf
verificar<-data.frame(cbind(juncao_sqldf$family,juncao_sqldf$species,juncao_sqldf$genus,juncao_sqldf$id_sp))
View(verificar)

sp1<-subset(verificar,verificar$X4==5351)
nrow(sp1)

df1<-subset(dfnovo,dfnovo$id_sp==5351)
nrow(df1) 

## f) Sele��o das lenhosas do dataset do GBIF (assim temos um conjunto de dados de registros de ocorr�ncia para esp�cies 
## lenhosas da caatinga)
lenhosas<-read.csv("lenhosas_revisadas.csv",header=T, sep=";")

#Interse��o das lenhosas com juncao_sqldf
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

#Exportar o conjunto de dados de registros de ocorr�ncia para esp�cies lenhosas da caatinga
write.table(juncao.lenhosas, file='juncao_lenhosas_GBIF.csv', sep=';', dec=',', row.names=FALSE)


## Resumo do Script 

# Arquivos exportados:
# Dados de ocorr�ncia baixados do GBIF: occurrence.txt
# Lista de esp�cies dos dados de ocorr�ncia para submiss�o ao TNRS: spgbiffortnrs.csv
# Conjunto de dados de registros de ocorr�ncia para esp�cies lenhosas da caatinga: juncao_lenhosas_GBIF.csv 

