library(sf)
library(dplyr)
library (FuzzyR)

options(scipen = 999) #disabilita not scientifica

  WDIR="Data"

shp1 <- st_read(file.path(WDIR,"Amérique_latine/SOTWIS_LACv1_t1s1d1.shp"))
shp2 <- st_read(file.path(WDIR,"Europe/SOVEUReast_SOTWISv1_t1s1d1.shp"))
shp3 <- st_read(file.path(WDIR,"Centre_Afrique/SOTWIS_CAFv1_t1s1d1.shp"))
shp4 <- st_read(file.path(WDIR,"Tunisie/TN_SOTWISv1_t1s1d1.shp"))
shp5 <- st_read(file.path(WDIR,"India/IGP-IN_SOTWISv1_t1s1d1.shp"))

names(shp1)
View(shp2)
class(shp1)
data1 <- st_drop_geometry(shp1)
data2 <- st_drop_geometry(shp2)
data3 <- st_drop_geometry(shp3)
data4 <- st_drop_geometry(shp4)
data5 <- st_drop_geometry(shp5)

names(data1)
names(data2)
class(data1)
View(data1)
View(data4)

data1 <- select(data1,SDTO,STPC,CLPC,CFRAG,PHAQ,BSAT,CECS,TOTC,TOTN)
data2 <- select(data2,SDTO,STPC,CLPC,CFRAG,PHAQ,BSAT,CECS,TOTC,TOTN)
data3 <- select(data3,SDTO,STPC,CLPC,CFRAG,PHAQ,BSAT,CECS,TOTC,TOTN)
data4 <- select(data4,SDTO,STPC,CLPC,CFRAG,PHAQ,BSAT,CECS,TOTC,TOTN)
data5 <- select(data5,SDTO,STPC,CLPC,CFRAG,PHAQ,BSAT,CECS,TOTC,TOTN)
data_is <- rbind(data1,data2,data3,data4,data5)

#Remove useless
rm(data1,data2,data3,data4,data5)
rm(shp1,shp2,shp3,shp4,shp5)


#cleaning
data_is <- filter(data_is,SDTO+CLPC+STPC==100)
data_is <- mutate(data_is,CNrt=TOTC/TOTN)
data_is <- filter(data_is,CFRAG>=0)
data_is <- na.omit(data_is) #probabile stesso errore ceh c'è con sis_data (H1_vs_all)


names(data_is)
View(data_is) # ma gli id dove sono?? sono soloelenco di variabili???


#FIS 1 : Physical

fis1 <- newfis("Physical")

#First input class : Sand (SDTO)
fis1 <- addvar(fis1,"input","sand",c(0,100))
fis1 <- addmf(fis1,"input",1,"Low","trapmf",c(0,0,25,30))
fis1 <- addmf(fis1,"input",1,"Medium","trapmf",c(25,30,50,55))
fis1 <- addmf(fis1,"input",1,"High","trapmf",c(50,55,100,100))
plotmf(fis1,"input",1)

#Second input class : Clay (CLPC)
fis1 <- addvar(fis1,"input","clay",c(0,100))
fis1 <- addmf(fis1,"input",2,"Low","trapmf",c(0,0,10,20))
fis1 <- addmf(fis1,"input",2,"Medium","trapmf",c(10,20,30,45))
fis1 <- addmf(fis1,"input",2,"High","trapmf",c(30,45,100,100))
plotmf(fis1,"input",2)

#Third input class : Coarse Fragments (CFRAG)
fis1 <- addvar(fis1,"input","coarse_frag",c(0,100))
fis1 <- addmf(fis1,"input",3,"Low","trapmf",c(0,0,10,20))
fis1 <- addmf(fis1,"input",3,"Medium","trapmf",c(10,20,30,35))
fis1 <- addmf(fis1,"input",3,"High","trapmf",c(30,35,100,100))
plotmf(fis1,"input",3)


#Output class : Physical score
fis1 <- addvar(fis1,"output","Physical_score",c(0,100))
fis1 <- addmf(fis1,"output",1,"Very Bad","trimf",c(0,0,10))
fis1 <- addmf(fis1,"output",1,"Bad","trapmf",c(5,15,30,45))
fis1 <- addmf(fis1,"output",1,"Medium","trapmf",c(30,45,55,70))
fis1 <- addmf(fis1,"output",1,"Good","trapmf",c(55,70,85,95))
fis1 <- addmf(fis1,"output",1,"Very Good","trimf",c(90,100,100))
plotmf(fis1,"output",1)

# Rules for physical properties
rules.matrix1<-matrix(c(
  1,1,1,1,1,1,
  1,1,2,1,1,1,
  1,1,3,1,1,1,
  1,2,1,2,1,1,
  1,2,2,2,1,1,
  1,2,3,1,1,1,
  1,3,1,1,1,1,
  1,3,2,1,1,1,
  1,3,3,1,1,1,
  2,1,1,3,1,1,
  2,1,2,3,1,1,
  2,1,3,1,1,1,
  2,2,1,5,1,1,
  2,2,2,4,1,1,
  2,2,3,1,1,1,
  2,3,1,3,1,1,
  2,3,2,3,1,1,
  2,3,3,1,1,1, 
  3,1,1,3,1,1,
  3,1,2,3,1,1,
  3,1,3,1,1,1,
  3,2,1,3,1,1,
  3,2,2,3,1,1,
  3,2,3,1,1,1,
  3,3,1,3,1,1,
  3,3,2,3,1,1,
  3,3,3,1,1,1),
  ncol=6,byrow = TRUE)

fis1 <- addrule(fis1,rules.matrix1)

#Rules firing 
input_matrix <- as.matrix(phy[, c("SDTO", "CLPC", "CFRAG")])
res <- rule_firing(input_matrix, fis1)

res$summary       # tabella riepilogo ordinata
res$never_fired   # regole mai attivate
View(res$firing)        # matrice completa delle firing strength


#Evaluation of the data with the FIS "Physical"
phy <- data_is |> 
  select(SDTO, CLPC,CFRAG)

phy <- mutate(phy,score=evalfis(as.matrix(phy),fis1))

write.csv2(phy,"phy.csv",row.names=FALSE)
phy <- read.csv2("phy.csv")


#Attribution of a letter to make the grade more visual

breaks <- c(-Inf, 7.5, 10, 20.5, 23, 25.5, 38.5, 41, 43.5, 56.5, 59, 61.5, 74.5, 77, 79.5, 90, 92.5, Inf)
grades <- c("E--", "E-", "E", "E+", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+", "A++")

phy <- mutate(
  phy,
  Grade = cut(score, breaks = breaks, labels = grades)
)

##FIS 2 : Chemical
fis2 <- newfis("Chemical")

#First input class : pH
fis2 <- addvar(fis2,"input","pH_H2O",c(0,14))
fis2 <- addmf(fis2,"input",1,"Acid","trapmf",c(0,0,5.8,6.3))
fis2 <- addmf(fis2,"input",1,"Neutral","trapmf",c(5.8,6.3,6.8,7.8))
fis2 <- addmf(fis2,"input",1,"Alcalin","trapmf",c(6.8,7.8,14,14))
plotmf(fis2,"input",1)


#Second input class : BSAT
fis2 <- addvar(fis2,"input","BSAT",c(0,100))
fis2 <- addmf(fis2,"input",2,"Low","trapmf",c(0,0,50,60))
fis2 <- addmf(fis2,"input",2,"Medium","trapmf",c(50,60,70,80))
fis2 <- addmf(fis2,"input",2,"High","trapmf",c(70,80,100,100))
plotmf(fis2,"input",2)

#Third input class : CEC
fis2 <- addvar(fis2,"input","CECS",c(0,150))
fis2 <- addmf(fis2,"input",3,"Low","trapmf",c(0,0,10,20))
fis2 <- addmf(fis2,"input",3,"Medium","trapmf",c(10,20,30,40))
fis2 <- addmf(fis2,"input",3,"High","trapmf",c(30,40,150,150))
plotmf(fis2,"input",3)

#Output class : Chemical score
fis2 <- addvar(fis2,"output","Chemical_score",c(0,100))
fis2 <- addmf(fis2,"output",1,"Very Bad","trimf",c(0,0,10))
fis2 <- addmf(fis2,"output",1,"Bad","trapmf",c(5,15,30,45))
fis2 <- addmf(fis2,"output",1,"Medium","trapmf",c(30,45,55,70))
fis2 <- addmf(fis2,"output",1,"Good","trapmf",c(55,70,85,95))
fis2 <- addmf(fis2,"output",1,"Very Good","trimf",c(90,100,100))
plotmf(fis2,"output",1)

#Rules for the chemical properties
rules.matrix2 <- matrix(c(
  1,1,1,1,1,1,
  1,1,2,2,1,1,
  1,1,3,1,1,1,
  1,2,1,2,1,1,
  1,2,2,3,1,1,
  1,2,3,2,1,1,
  1,3,1,2,1,1,
  1,3,2,3,1,1,
  1,3,3,2,1,1,
  2,1,1,2,1,1,
  2,1,2,3,1,1,
  2,1,3,2,1,1,
  2,2,1,3,1,1,
  2,2,2,4,1,1,
  2,2,3,3,1,1,
  2,3,1,3,1,1,
  2,3,2,5,1,1,
  2,3,3,3,1,1,
  3,1,1,1,1,1,
  3,1,2,2,1,1,
  3,1,3,1,1,1,
  3,2,1,2,1,1,
  3,2,2,3,1,1,
  3,2,3,2,1,1,
  3,3,1,2,1,1,
  3,3,2,3,1,1,
  3,3,3,2,1,1
),ncol=6,byrow = TRUE)

fis2 <- addrule(fis2,rules.matrix2)

#Evaluation of the data with the Fis "Chemical"
che <- data_is |> 
  select(PHAQ,BSAT,CECS)

che <- mutate(che,score=evalfis(as.matrix(che),fis2))

write.csv2(che,"che.csv",row.names=FALSE)
che <- read.csv2("che.csv")

#Attribution of a letter to make the grade more visual
breaks <- c(-Inf, 7.5, 10, 20.5, 23, 25.5, 38.5, 41, 43.5, 56.5, 59, 61.5, 74.5, 77, 79.5, 90, 92.5, Inf)
grades <- c("E--", "E-", "E", "E+", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+", "A++")

che <- mutate(
  che,
  Grade = cut(score, breaks = breaks, labels = grades)
)
##FIS 3 : SOC score
fis3 <- newfis("SOC")

#First input class : Total carbon
fis3 <- addvar(fis3,"input","TOTC",c(0,400))
fis3 <- addmf(fis3,"input",1,"Low","trapmf",c(0,0,20,25))
fis3 <- addmf(fis3,"input",1,"Medium","trapmf",c(20,25,50,55))
fis3 <- addmf(fis3,"input",1,"High","trapmf",c(50,55,400,400))
plotmf(fis3,"input",1)

#Second input class : Ratio C/N
fis3 <- addvar(fis3,"input","CNRrt",c(0.5,150))
fis3 <- addmf(fis3,"input",2,"Low","trapmf",c(0,0,5,8))
fis3 <- addmf(fis3,"input",2,"Medium","trapmf",c(5,8,15,18))
fis3 <- addmf(fis3,"input",2,"High","trapmf",c(15,18,150,150))
plotmf(fis3,"input",2)

#Output class : SOC score
fis3 <- addvar(fis3,"output","SOC_score",c(0,100))
fis3 <- addmf(fis3,"output",1,"Very Bad","trimf",c(0,0,10))
fis3 <- addmf(fis3,"output",1,"Bad","trapmf",c(5,15,30,45))
fis3 <- addmf(fis3,"output",1,"Medium","trapmf",c(30,45,55,70))
fis3 <- addmf(fis3,"output",1,"Good","trapmf",c(55,70,85,95))
fis3 <- addmf(fis3,"output",1,"Very Good","trimf",c(90,100,100))
plotmf(fis3,"output",1)

#Rules for the SOC properties
rules.matrix3 <- matrix(c(
  1,1,1,1,1,
  1,2,2,1,1,
  1,3,1,1,1,
  2,1,2,1,1,
  2,2,5,1,1,
  2,3,3,1,1,
  3,1,2,1,1,
  3,2,4,1,1,
  3,3,3,1,1
),ncol=5,byrow = TRUE)

fis3 <- addrule(fis3,rules.matrix3)

#Evaluation of the data with the FIS 3
soc <- data_is |> 
  select(TOTC,CNrt)

soc <- mutate(soc,score=evalfis(as.matrix(soc),fis3))

write.csv2(soc,"soc.csv",row.names=FALSE)
soc <- read.csv2("soc.csv")

#Attribution of a letter to make the grade more visual

breaks <- c(-Inf, 7.5, 10, 20.5, 23, 25.5, 38.5, 41, 43.5, 56.5, 59, 61.5, 74.5, 77, 79.5, 90, 92.5, Inf)
grades <- c("E--", "E-", "E", "E+", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+", "A++")

soc <- mutate(
  soc,
  Grade = cut(score, breaks = breaks, labels = grades)
)
#Final score
FIS <- newfis("Final")

#First input class : Physical score
FIS <- addvar(FIS,"input","Physical_score",c(0,100))
FIS <- addmf(FIS,"input",1,"Very Bad","trimf",c(0,0,10))
FIS <- addmf(FIS,"input",1,"Bad","trapmf",c(5,15,30,45))
FIS <- addmf(FIS,"input",1,"Medium","trapmf",c(30,45,55,70))
FIS <- addmf(FIS,"input",1,"Good","trapmf",c(55,70,85,95))
FIS <- addmf(FIS,"input",1,"Very Good","trimf",c(90,100,100))
plotmf(FIS,"input",1)

#Second input class : Chemical score
FIS <- addvar(FIS,"input","Chemical_score",c(0,100))
FIS <- addmf(FIS,"input",2,"Very Bad","trimf",c(0,0,10))
FIS <- addmf(FIS,"input",2,"Bad","trapmf",c(5,15,30,45))
FIS <- addmf(FIS,"input",2,"Medium","trapmf",c(30,45,55,70))
FIS <- addmf(FIS,"input",2,"Good","trapmf",c(55,70,85,95))
FIS <- addmf(FIS,"input",2,"Very Good","trimf",c(90,100,100))
plotmf(FIS,"input",2)

#Third input class : SOC score
FIS <- addvar(FIS,"input","SOC_score",c(0,100))
FIS <- addmf(FIS,"input",3,"Very Bad","trimf",c(0,0,10))
FIS <- addmf(FIS,"input",3,"Bad","trapmf",c(5,15,30,45))
FIS <- addmf(FIS,"input",3,"Medium","trapmf",c(30,45,55,70))
FIS <- addmf(FIS,"input",3,"Good","trapmf",c(55,70,85,95))
FIS <- addmf(FIS,"input",3,"Very Good","trimf",c(90,100,100))
plotmf(FIS,"input",3)

#Output class : Final score
FIS <- addvar(FIS,"output","Score",c(0,100))
FIS <- addmf(FIS,"output",1,"Very Very Bad","trimf",c(0,0,10))
FIS <- addmf(FIS,"output",1,"Very Bad","trapmf",c(5,10,20.5,25.5))
FIS <- addmf(FIS,"output",1,"Bad","trapmf",c(20.5,25.5,38.5,43.5))
FIS <- addmf(FIS,"output",1,"Medium","trapmf",c(38.5,43.5,56.5,61.5))
FIS <- addmf(FIS,"output",1,"Good","trapmf",c(56.5,61.5,74.5,79.5))
FIS <- addmf(FIS,"output",1," Very Good","trapmf",c(74.5,79.5,90,95))
FIS <- addmf(FIS,"output",1,"Very Very Good","trimf",c(90,100,100))
plotmf(FIS,"output",1)

#RULES
rules.matrix <- matrix(c(
  1,1,1,1,1,1,
  1,1,2,1,1,1,
  1,1,3,2,1,1,
  1,1,4,3,1,1,
  1,1,5,3,1,1,
  1,2,1,1,1,1,
  1,2,2,2,1,1,
  1,2,3,2,1,1,
  1,2,4,3,1,1,
  1,2,5,3,1,1,
  1,3,1,2,1,1,
  1,3,2,2,1,1,
  1,3,3,3,1,1,
  1,3,4,3,1,1,
  1,3,5,4,1,1,
  1,4,1,3,1,1,
  1,4,2,3,1,1,
  1,4,3,3,1,1,
  1,4,4,4,1,1,
  1,4,5,4,1,1,
  1,5,1,3,1,1,
  1,5,2,3,1,1,
  1,5,3,4,1,1,
  1,5,4,4,1,1,
  1,5,5,5,1,1,
  2,1,1,1,1,1,
  2,1,2,2,1,1,
  2,1,3,2,1,1,
  2,1,4,3,1,1,
  2,1,5,3,1,1,
  2,2,1,2,1,1,
  2,2,2,2,1,1,
  2,2,3,3,1,1,
  2,2,4,3,1,1,
  2,2,5,4,1,1,
  2,3,1,2,1,1,
  2,3,2,3,1,1,
  2,3,3,4,1,1,
  2,3,4,4,1,1,
  2,3,5,4,1,1,
  2,4,1,3,1,1,
  2,4,2,3,1,1,
  2,4,3,4,1,1,
  2,4,4,5,1,1,
  2,4,5,5,1,1,
  2,5,1,3,1,1,
  2,5,2,4,1,1,
  2,5,3,5,1,1,
  2,5,4,5,1,1,
  2,5,5,5,1,1,
  3,1,1,2,1,1,
  3,1,2,2,1,1,
  3,1,3,3,1,1,
  3,1,4,3,1,1,
  3,1,5,3,1,1,
  3,2,1,2,1,1,
  3,2,2,3,1,1,
  3,2,3,4,1,1,
  3,2,4,4,1,1,
  3,2,5,5,1,1,
  3,3,1,3,1,1,
  3,3,2,4,1,1,
  3,3,3,4,1,1,
  3,3,4,4,1,1,
  3,3,5,5,1,1,
  3,4,1,3,1,1,
  3,4,2,4,1,1,
  3,4,3,4,1,1,
  3,4,4,5,1,1,
  3,4,5,6,1,1,
  3,5,1,4,1,1,
  3,5,2,5,1,1,
  3,5,3,5,1,1,
  3,5,4,6,1,1,
  3,5,5,6,1,1,
  4,1,1,3,1,1,
  4,1,2,3,1,1,
  4,1,3,3,1,1,
  4,1,4,4,1,1,
  4,1,5,4,1,1,
  4,2,1,3,1,1,
  4,2,2,3,1,1,
  4,2,3,4,1,1,
  4,2,4,5,1,1,
  4,2,5,3,1,1,
  4,3,1,3,1,1,
  4,3,2,4,1,1,
  4,3,3,4,1,1,
  4,3,4,5,1,1,
  4,3,5,6,1,1,
  4,4,1,4,1,1,
  4,4,2,5,1,1,
  4,4,3,5,1,1,
  4,4,4,6,1,1,
  4,4,5,6,1,1,
  4,5,1,4,1,1,
  4,5,2,5,1,1,
  4,5,3,6,1,1,
  4,5,4,6,1,1,
  4,5,5,7,1,1,
  5,1,1,3,1,1,
  5,1,2,5,1,1,
  5,1,3,4,1,1,
  5,1,4,4,1,1,
  5,1,5,5,1,1,
  5,2,1,3,1,1,
  5,2,2,4,1,1,
  5,2,3,5,1,1,
  5,2,4,5,1,1,
  5,2,5,5,1,1,
  5,3,1,4,1,1,
  5,3,2,5,1,1,
  5,3,3,5,1,1,
  5,3,4,6,1,1,
  5,3,5,6,1,1,
  5,4,1,4,1,1,
  5,4,2,5,1,1,
  5,4,3,6,1,1,
  5,4,4,6,1,1,
  5,4,5,7,1,1,
  5,5,1,5,1,1,
  5,5,2,5,1,1,
  5,5,3,6,1,1,
  5,5,4,7,1,1,
  5,5,5,7,1,1
),ncol=6,byrow = TRUE)

FIS <- addrule(FIS,rules.matrix)

rule_FIS_pattern <- matrix(c(
  "VB","VB","VB","VVB",
  "VB","VB","B","VVB",
  "VB","VB","M","VB",
  "VB","VB","G","B",
  "VB","VB","VG","B",
  "B","B","VB","VB",
  "B","B","B","VB",
  "B","B","M","B",
  "B","B","G","B",
  "B","B","VG","M",
  "M","M","VB","B",
  "M","M","B","M",
  "M","M","M","M",
  "M","M","G","M",
  "M","M","VG","G",
  "G","G","VB","M",
  "G","G","B","G",
  "G","G","M","G",
  "G","G","G","VG",
  "G","G","VG","VG",
  "VG","VG","VB","G",
  "VG","VG","B","G",
  "VG","VG","M","VG",
  "VG","VG","G","VVG",
  "VG","VG","VG","VVG",
  "VB","B","M","VB",
  "VB","B","G","B",
  "VB","B","VG","M",
  "VB","M","G","B",
  "VB","M","VG","M",
  "VB","G","VG","G",
  "B","M","G","M",
  "B","G","VG","G",
  "B","M","VG","G",
  "M","G","VG","VG"
),ncol = 4,byrow = TRUE)
colnames(rule_FIS_pattern)<-c("Input 1","Input 2","Input 3","Output")
rule_FIS_pattern<-data.frame(rule_FIS_pattern)
library(knitr)
kable(rule_FIS_pattern,table.attr="border=1")

#Evaluation of the data with the final fis
fuzzy_data <- mutate(data_is,
                     Physical_score=phy$score,
                     Chemical_score=che$score,
                     SOC_score=soc$score)

data_for_eval <- as.matrix(fuzzy_data[,c("Physical_score","Chemical_score","SOC_score")])
X <- data_for_eval
results <- mutate(fuzzy_data,SCORE=evalfis(X,FIS))

write.csv2(results,"results.csv",row.names=FALSE)
results <- read.csv2("results.csv")


breaks <- c(-Inf, 7.5, 10, 20.5, 23, 25.5, 38.5, 41, 43.5, 56.5, 59, 61.5, 74.5, 77, 79.5, 90, 92.5, Inf)
grades <- c("E--", "E-", "E", "E+", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+", "A++")

results <- mutate(
  results,
  Grade = cut(SCORE, breaks = breaks, labels = grades)
)

#Statistics
par(mfrow=c(1,4))
boxplot(results$Physical_score,main="Physical_score",ylim=c(0,100))
boxplot(results$Chemical_score,main="Chemical_score",ylim=c(0,100))
boxplot(results$SOC_score,main="SOC_score",ylim=c(0,100))
boxplot(results$SCORE,main="Final_score",ylim=c(0,100))
table(phy$Grade)
table(che$Grade)
table(soc$Grade)
table(results$Grade)

