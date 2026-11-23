# AGGIUNTA LIBRERIE ----
library(sf)
library(dplyr)
library(FuzzyR)
library(soilhub)

options(scipen = 999) #disabilita not scientifica

# richiamo le funzioni
source("Functions.R")

# Caricamento dati scaricati dall'api
load("data_api.RData")

class(data_api)
names(data_api)

# variabili che ci servono, richieste
vars_30 <- c( "sand_content",  "silt_content", "clay_content",
              "cec_ph7",  "ph_h2o", "organic_carbon",  "total_nitrogen_kjeldahl" ) # è stato tolto "coarse_fragments_volumetric" 



# CONTROLLO completezza dati ----

# controllo sui primi 50 cm 
data_wos <- data_api %>%
  group_by(profile_id) %>%
  filter(any(upper_depth_cm < 50, na.rm = TRUE)) %>%
  ungroup()

data_wos2 <- check_completezza(data_wos, vars_30)

# # profili in cui tutti gli orizzonti sono completi, 7167 profili 
data_wos2 %>%
  group_by(profile_id) %>%
  summarise(profilo_completo = all(completezza == 1)) %>%
  count(profilo_completo)


# seleziono tutti i profili nei primi 30 cm in cui completezza == 1 ----

data_fis_30 <- data_wos2 |> 
  group_by(profile_id) |> 
  filter(any(upper_depth_cm < 30), # ha orizzonti nei primi 30 cm
         all(completezza[upper_depth_cm <30] == 1)) |>  # questi orizzonti sono completi
  ungroup()

n_distinct(data_fis_30$profile_id) # 16541 profili, 89545 orizzonti


# Assunzione  cfrag = 0 laddove manca, dove è presente viene utilizzata quella presente, 
# assunzione fatta perchè spesso se cfrag è trascurabile non
# viene analizzata/segnata

data_fis_30 <- data_fis_30 |> 
  mutate(cf_mancante = is.na(coarse_fragments_volumetric), # TRUE = non misurati
         cf_fis = ifelse(cf_mancante, 0, coarse_fragments_volumetric))

data_fis_30 <- mutate(data_fis_30, CNrt = organic_carbon/total_nitrogen_kjeldahl)

sum(data_fis_30$cf_mancante) 


# quanti profili hanno almeno un orizzonte nei primi 30 cm con cf_mancante
livelli <- data_fis_30 %>%
  group_by(profile_id) %>%
  summarise(livello = ifelse(any(cf_mancante[upper_depth_cm < 30]), "CF0", "CF1"))
# CF1 = scheletro misurato in tutto il topsoil
# CF0 = scheletro imputato a 0 in almeno un orizzonte del topsoil


count(livelli, livello)   # deve dare CF1 = 8734, CF0 = 7807


#calcolo CNrt
data_fis_30 <- mutate(data_fis_30, CNrt = organic_carbon/total_nitrogen_kjeldahl)














# FIS 
# FIS 1: Physical ----
fis1 <- newfis("Physical")

# First input class : Sand (sand_content)
fis1 <- addvar(fis1,"input","sand",c(0,100))
fis1 <- addmf(fis1,"input",1,"Low","trapmf",c(0,0,25,30))
fis1 <- addmf(fis1,"input",1,"Medium","trapmf",c(25,30,50,55))
fis1 <- addmf(fis1,"input",1,"High","trapmf",c(50,55,100,100))
plotmf(fis1,"input",1)

# Second input class : Clay (clay_content)
fis1 <- addvar(fis1,"input","clay",c(0,100))
fis1 <- addmf(fis1,"input",2,"Low","trapmf",c(0,0,10,20))
fis1 <- addmf(fis1,"input",2,"Medium","trapmf",c(10,20,30,45))
fis1 <- addmf(fis1,"input",2,"High","trapmf",c(30,45,100,100))
plotmf(fis1,"input",2)

# Third input class : Coarse Fragments (coarse_fragments_volumetric)
fis1 <- addvar(fis1,"input","coarse_frag",c(0,100))
fis1 <- addmf(fis1,"input",3,"Low","trapmf",c(0,0,10,20))
fis1 <- addmf(fis1,"input",3,"Medium","trapmf",c(10,20,30,35))
fis1 <- addmf(fis1,"input",3,"High","trapmf",c(30,35,100,100))
plotmf(fis1,"input",3)

# Output class : Physical score
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


# FIS 2: Chemical ----
fis2 <- newfis("Chemical")

#First input class : pH
fis2 <- addvar(fis2,"input","pH_H2O",c(0,14))
fis2 <- addmf(fis2,"input",1,"Acid","trapmf",c(0,0,5.8,6.3))
fis2 <- addmf(fis2,"input",1,"Neutral","trapmf",c(5.8,6.3,6.8,7.8))
fis2 <- addmf(fis2,"input",1,"Alcalin","trapmf",c(6.8,7.8,14,14))
plotmf(fis2,"input",1)

#Second input class : CEC
fis2 <- addvar(fis2,"input","CEC",c(0,150))
fis2 <- addmf(fis2,"input",2,"Low","trapmf",c(0,0,10,20))
fis2 <- addmf(fis2,"input",2,"Medium","trapmf",c(10,20,30,40))
fis2 <- addmf(fis2,"input",2,"High","trapmf",c(30,40,150,150))
plotmf(fis2,"input",2)

#Output class : Chemical score
fis2 <- addvar(fis2,"output","Chemical_score",c(0,100))
fis2 <- addmf(fis2,"output",1,"Very Bad","trimf",c(0,0,10))
fis2 <- addmf(fis2,"output",1,"Bad","trapmf",c(5,15,30,45))
fis2 <- addmf(fis2,"output",1,"Medium","trapmf",c(30,45,55,70))
fis2 <- addmf(fis2,"output",1,"Good","trapmf",c(55,70,85,95))
fis2 <- addmf(fis2,"output",1,"Very Good","trimf",c(90,100,100))
plotmf(fis2,"output",1)

# rules
rules.matrix2 <- matrix(c(
  1,1,1,1,1,   
  1,2,2,1,1,   
  1,3,1,1,1,   
  2,1,3,1,1,  
  2,2,5,1,1,   
  2,3,3,1,1,   
  3,1,2,1,1,  
  3,2,3,1,1,   
  3,3,2,1,1    
),ncol=5,byrow = TRUE)

fis2 <- addrule(fis2,rules.matrix2)







# FIS 3: SOC ----
fis3 <- newfis("SOC")

#First input class : Organic_carbon
fis3 <- addvar(fis3,"input","TOTC",c(0,400)) # tre arrivano sopra 400 (454; 453; 422)
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









# FIS final: SCORE HEALTH ----
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
  4,2,5,5,1,1,
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
  5,1,2,3,1,1,
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








#Attribution of a letter to make the grade more visual----

breaks <- c(-Inf, 7.5, 10, 20.5, 23, 25.5, 38.5, 41, 43.5, 56.5, 59, 61.5, 74.5, 77, 79.5, 90, 92.5, Inf)
grades <- c("E--", "E-", "E", "E+", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+", "A++")


# CALCOLO PHY ----
phy_0cfrag <- data_fis_30 |> 
  select(profile_id, upper_depth_cm, lower_depth_cm,
         sand_content, clay_content, cf_fis) |> 
  mutate(score = valuta_fis(cbind(sand_content, clay_content, cf_fis), fis1))


# aggiungo lettere e flag livello 

phy_0cfrag <- phy_0cfrag %>%
  mutate(Grade = cut(score, breaks = breaks, labels = grades)) %>%
  left_join(livelli, by = "profile_id")



# CALCOLO CHE ----
che_30 <- data_fis_30 |> 
  select(profile_id, upper_depth_cm, lower_depth_cm,
         ph_h2o, cec_ph7) |>  
  mutate(score = valuta_fis(cbind(ph_h2o, cec_ph7), fis2))

che_30 <- mutate(che_30,
                 Grade = cut(score, breaks = breaks, labels = grades))






# CALCOLO SOC ----
soc_30 <- data_fis_30 %>%
  select(profile_id, upper_depth_cm, lower_depth_cm, organic_carbon, CNrt) %>%
  mutate(score = valuta_fis(cbind(organic_carbon, CNrt), fis3))

soc_30 <- mutate(soc_30,
                 Grade = cut(score, breaks = breaks, labels = grades))



# CALCOLO FIS FINALE ----
results_30 <- data_fis_30 %>%
  select(profile_id, upper_depth_cm, lower_depth_cm) %>%
  mutate(Physical_score = phy_0cfrag$score,
         Chemical_score = che_30$score,
         SOC_score      = soc_30$score,
         SCORE = valuta_fis(cbind(Physical_score, Chemical_score, SOC_score), FIS))


results_30 <- mutate(results_30, 
                     Grade = cut(SCORE, breaks = breaks, labels = grades))








#Statistics ----
par(mfrow=c(1,4))
boxplot(results_30$Physical_score,main="Physical_score_30",ylim=c(0,100))
boxplot(results_30$Chemical_score,main="Chemical_score_30",ylim=c(0,100))
boxplot(results_30$SOC_score,main="SOC_score_30",ylim=c(0,100))
boxplot(results_30$SCORE,main="Final_score_30",ylim=c(0,100))
table(phy_0cfrag$Grade)
table(che_30$Grade)
table(soc_30$Grade)
table(results_30$Grade)

























# media ponderata solo sugli orizzonti nei primi 30 cm
p_score_weighted_30 <- results_30 %>%
  filter(upper_depth_cm < 30) %>%                                    # orizzonti nei primi 30 cm
  mutate(thickness = pmin(lower_depth_cm, 30) - upper_depth_cm) %>%  # spessore tagliato a 30 cm
  group_by(profile_id) %>%
  summarise(score_p_30 = weighted.mean(SCORE, thickness, na.rm = TRUE),
            n_horizons = n(),
            .groups = "drop") %>%
  left_join(livelli, by = "profile_id") %>%
  mutate(Grade_30 = cut(score_p_30, breaks = breaks, labels = grades))


# boxplot per i livelli CF1 e CF0
par(mfrow = c(1,1))

boxplot(score_p_30 ~ livello, data = p_score_weighted_30, ylim = c(0, 100),
        main = "Score ponderato 0-30 cm per livello")






















## APPLICATO IL MODELLO A TUTTO IL DATASET ----

data_fis_all <- as.data.frame(data_api)


# dividere per livelli con cfrag = 0
data_fis_all <- data_fis_all %>%  
  mutate(cf_mancante = is.na(coarse_fragments_volumetric), # TRUE = non misurati
         cf_fis = ifelse(cf_mancante, 0, coarse_fragments_volumetric))

data_fis_all <- data_fis_all %>%
  group_by(profile_id) %>%
  mutate(livello = ifelse(any(cf_mancante), "CF0", "CF1")) %>% # CF0 = scheletro 0, CF1 = scheletro presente
  ungroup()

#calcolo CNrt
data_fis_all <- mutate(data_fis_all, CNrt = organic_carbon/total_nitrogen_kjeldahl)

sum(data_fis_all$CNrt == Inf ) # 65
sum(is.na(data_fis_all$CNrt)) # 80% NA

# calcolo micor fis physical
phy_all <-  data_fis_all %>%
  select(profile_id, upper_depth_cm, lower_depth_cm,
         sand_content, clay_content, cf_fis) %>%
  mutate(score = valuta_fis(cbind(sand_content, clay_content, cf_fis), fis1))

# lettere
phy_all <- mutate(
  phy_all,
  Grade = cut(score, breaks = breaks, labels = grades)
)


# percentuale completezza totale su tutti gli orizzonti 
mean(!is.na(phy_all$score)) * 100 # 59.96% 

# calcolo micro fis Chemical
che_all <- data_fis_all %>% 
  select(profile_id, upper_depth_cm, lower_depth_cm,ph_h2o, cec_ph7) %>% 
  mutate(score = valuta_fis(cbind(ph_h2o, cec_ph7), fis2))

# lettere
che_all <- mutate(
  che_all,
  Grade = cut(score, breaks = breaks, labels = grades)
)

# percentuale completezza totale su tutti gli orizzonti
mean(!is.na(che_all$score))*100 # 33.24%

# calcolo micro fis SOC
soc_all <- data_fis_all %>% 
  select(profile_id, upper_depth_cm, lower_depth_cm, organic_carbon, CNrt) %>% 
  mutate(score = valuta_fis(cbind(organic_carbon, CNrt), fis3))


# letter
soc_all <- mutate(
  soc_all,
  Grade = cut(score, breaks = breaks, labels = grades)
)

# percentuale completezza totale su tutti gli orizzonti
mean(!is.na(soc_all$score))*100 # 19.63%



# final score
results_all <- data_fis_all %>% 
  select(profile_id, upper_depth_cm, lower_depth_cm) %>%
  mutate(Physical_score = phy_all$score,
         Chemical_score = che_all$score,
         SOC_score      = soc_all$score,
         SCORE = valuta_fis(cbind(Physical_score, Chemical_score, SOC_score), FIS))



# percentuale completezza totale su tutti gli orizzonti dello score finale
mean(!is.na(results_all$SCORE))*100 # 8.54%



phy_prof <- media_e_completezza(phy_all, "phy")
che_prof <- media_e_completezza(che_all, "che")
soc_prof <- media_e_completezza(soc_all, "soc")
results_prof <- media_e_completezza(rename(results_all, score = SCORE), "res_finale")


profili <- phy_prof %>%
  full_join(che_prof, by = "profile_id") %>%
  full_join(soc_prof, by = "profile_id") %>% 
  full_join(results_prof, by = "profile_id")



# grafici e statistica

library(ggplot2)

par(mfrow = c(1, 3))
hist(profili$completezza_phy, breaks = 5, main = "Physical", xlab = "Completezza (%)",
     col = "steelblue", border = "white")
hist(profili$completezza_che, breaks = 5, main = "Chemical", xlab = "Completezza (%)",
     col = "darkorange", border = "white")
hist(profili$completezza_soc, breaks = 5, main = "SOC", xlab = "Completezza (%)",
     col = "purple", border = "white")
par(mfrow = c(1, 1))



table(case_when(profili$completezza_phy == 0   ~ "0%",
                profili$completezza_phy == 100 ~ "100%",
                TRUE                           ~ "parziale")) #  0 < X < 100, maggiore di 0 ma minore di 100

table(case_when(profili$completezza_che == 0   ~ "0%",
                profili$completezza_che == 100 ~ "100%",
                TRUE                           ~ "parziale"))

table(case_when(profili$completezza_soc == 0   ~ "0%",
                profili$completezza_soc == 100 ~ "100%",
                TRUE                           ~ "parziale"))

table(case_when(profili$completezza_res_finale == 0 ~ "0%",
                profili$completezza_res_finale == 100 ~ "100%",
                TRUE                                  ~ "parziale"))



# grafici completezza dei profili

profili %>%
  tidyr::pivot_longer(starts_with("completezza_"), names_to = "microfis", values_to = "completezza",
                      names_prefix = "completezza_") %>%
  filter(completezza > 0, completezza < 100) %>%
  mutate(microfis = factor(microfis, levels = c("phy", "che", "soc", "res_finale"),
                           labels = c("Physical", "Chemical", "SOC", "Score finale"))) %>%
  ggplot(aes(x = completezza, fill = microfis)) +
  geom_histogram(binwidth = 5, boundary = 0, colour = "white") +
  facet_wrap(~ microfis) +
  scale_fill_manual(values = c("Physical" = "steelblue", "Chemical" = "darkorange",
                               "SOC" = "purple", "Score finale" = "forestgreen")) +
  labs(x = "Completezza (%)", y = "Numero di profili") +
  theme_classic() +
  theme(legend.position = "none")

# completezza dei profili 0 e 100%
# barre grigie: profili senza alcuno score per quel microFIS (completezza 0%);
# barre verdi: profili completamente valutati (completezza 100%).
profili %>%
  tidyr::pivot_longer(starts_with("completezza_"), names_to = "microfis", values_to = "completezza",
                      names_prefix = "completezza_") %>%
  filter(completezza %in% c(0, 100)) %>%
  mutate(microfis = factor(microfis, levels = c("phy", "che", "soc", "res_finale"),
                           labels = c("Physical", "Chemical", "SOC", "Score finale"))) %>%
  ggplot(aes(x = microfis, fill = factor(completezza))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "grey70", "100" = "forestgreen")) +
  labs(title = "Numero di profili senza alcuno score e profili completi", x = NULL, y = "Numero di profili", fill = "Completezza (%)") +
  theme_classic()


