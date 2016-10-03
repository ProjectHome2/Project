#Hvis run from R 
#dir <- dirname(parent.frame(2)$ofile)
#setwd(dir)
home = HOME2
#Hvis R studio
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load("home.RData")

head(home)
range(home[,3])
unique(home[,3])
range(home$EjdType)
names(home)

#Important: Run dplyr
library(dplyr)


#Vigtig: Se paa type af variable: Hvilke type, ex liggetid er difftime. Evt aendrer hvis unoedvendigt.
#Hvad indeholder de: Range, NA?, Hvordan tolkes?
str(home)
summary(home)
table(home$EjdType, useNA = "always")


#vi deler datasaet op i to dele
home_lejlighed = home %>% filter(EjdType == "Ejerlejlighed")
home_villa= home %>% filter(EjdType == "Villa, 1 fam.")

#vi finder information
sapply(home_lejlighed, table, useNA = "always")
sapply(home_villa, table, useNA = "always")

ggplot(home, aes(x=Postnr)) + geom_histogram(binwidth=5)
ggplot(home, aes(x=Kontantpris)) + geom_histogram(binwidth=1e5)
ggplot(home, aes(x=log(Kontantpris))) + geom_histogram(binwidth=0.05)

###########################################################################

l_nytk = ggplot(home_lejlighed, aes(x=NytKokken)) + geom_histogram()
v_nytk = ggplot(home_villa, aes(x=NytKokken)) + geom_histogram()

l_nytv = ggplot(home_lejlighed, aes(x=NyeVinduer)) + geom_histogram()
v_nytv = ggplot(home_villa, aes(x=NyeVinduer)) + geom_histogram()

l_taet = ggplot(home_lejlighed, aes(x=TaetVedVand)) + geom_histogram()
v_taet = ggplot(home_villa, aes(x=TaetVedVand)) + geom_histogram()

l_nytb = ggplot(home_lejlighed, aes(x=NytBadevaerelse)) + geom_histogram()
v_nytb = ggplot(home_villa, aes(x=NytBadevaerelse)) + geom_histogram()

l_antalt = ggplot(home_lejlighed, aes(x=AntalToiletter)) + geom_histogram()
v_antalt = ggplot(home_villa, aes(x=AntalToiletter)) + geom_histogram()

l_antals = ggplot(home_lejlighed, aes(x=AntalSovevaerelser)) + geom_histogram()
v_antals = ggplot(home_villa, aes(x=AntalSovevaerelser)) + geom_histogram()

l_manget = ggplot(home_lejlighed, aes(x=MangeToil)) + geom_histogram()
v_manget = ggplot(home_villa, aes(x=MangeToil)) + geom_histogram()

l_garage = ggplot(home_lejlighed, aes(x=GarageCarport)) + geom_histogram()
v_garage = ggplot(home_villa, aes(x=GarageCarport)) + geom_histogram()

l_ombyg = ggplot(home_lejlighed, aes(x=as.character(OmbygningSket))) + geom_histogram()
v_ombyg = ggplot(home_villa, aes(x=as.character(OmbygningSket))) + geom_histogram()

library(gridExtra)
grid.arrange(l_nytk, l_nytv, l_taet, l_nytb, l_antalt, l_antals, l_manget, l_garage, l_ombyg, 
          v_nytk, v_nytv, v_taet, v_nytb, v_antalt, v_antals, v_manget, v_garage, v_ombyg,
          ncol = 2)

grid.arrange(l_nytk, l_nytv, l_taet, l_nytb, l_antalt, l_antals, l_manget, l_garage, l_ombyg, 
             ncol = 3)
###########################################################################

#Aendring af variabeltype
home = home; home$Salgsdato = as.Date(home$Salgsdato, "%d-%m-%Y")
#Evt factorize en masse variable saasom, ellers vent til efter snak med Matt, Our man! Diskuter hvilke?!
for(col in c(3,"Storgrund")){
  #home[c(col)] <- factor(home[,c(col)])
  #If string
  try(home[col] <- factor(home[col]), silent = TRUE)
  #If numeric
  try(home[as.numeric(col)] <- factor(home[as.numeric(col)]), silent = TRUE)
}
#Faktoriser Boligtilstand og skift til engelsk navne paa levels
home$Boligtilstand <- factor(home$Boligtilstand, levels = c("Daarlig", "Middel", "God"))
levels(home$Boligtilstand) <- c("Bad", "Medium", "Good")

#Evt slaa kvartaler sammen til en raekke med faktor 1,2,3,4
home$Kvartal1[home$Kvartal1 == 1] = 1
home$Kvartal2[home$Kvartal2 == 1] = 2
home$Kvartal3[home$Kvartal3 == 1] = 3
home$Kvartal4[home$Kvartal4 == 1] = 4

home <- home %>% mutate(Kvartal = factor(Kvartal1 + Kvartal2 + Kvartal3 + Kvartal4))


#Bemaerk kun faa af typen 2 fam. Evt slet disse fra datasaet, og aendre 3 linjer laengere nede. And if so, maybe delete EjdType col afterwards
table(home$EjdType)

home <- home %>% filter(EjdType == "Villa, 1 fam." | EjdType == "Villa, 2 fam.") %>% 
  group_by(Postnr) %>%  
  arrange(Salgsdato)
#Kunne have valgt at arrange by kontantpris, salgsaar, opfoerelsesaar?

#Enten gem som R eller csv
save(home, file="home.R")
write.csv(home, file="home.csv")
load("home.R")

#Ide grupperinger af postnumre
table(home$Postnr)
hist(home$Kontantpris)

#Tjek hvor mange missing values vi har,- God til at tjekke hvorvidt vi kan bruge variabel eller ej, og hvordan NA fortolkes
table(home$NytKokken, useNA = "always")


#Opbygning af dplyr m. pipe
df.qty <- data.raw %>%
  filter(PURPOSE_CD == "OFF") %>%
  group_by(UNIT_REFERENCE_NO) %>%
  summarise(
    ZONE_CD = first(ZONE_CD),
    TOTAL_ACC_QTY = sum(AWARDED_QUANTITY_NO),
    AVG_DAILY_ACC_QTY = TOTAL_ACC_QTY / length(unique(BID_OFFER_DATE_DT)),
    AVG_HOURLY_ACC_QTY = AVG_DAILY_ACC_QTY / 24
  ) %>%
  arrange(desc(TOTAL_ACC_QTY)) %>%
  head(n = 10)

#Diverse info
str(home)
levels(home$Boligtilstand) #Har ingen levels
summary(home)
library(psych)
describe(home)

#Evt udregn correlation med cor

#Hvis traet af at skirve home$etelleandet, brug
attach(mtcars)
#Nu kan du blot skrive etellerandet
#Husk til sidst at bruge
detach()



#Graphs
#Evt proev at goere grafer like a beauty queen med xmax ymax osv 
#Ande theme: Proev + theme_dark()
library(ggplot2)
qplot(Boligareal, Kontantpris, data=home)

#Boligtilstand ~ Kontantpris, foerste virker ikke da ikke kan se hvor mange punkter
qplot(Boligtilstand, Kontantpris, data=home)
ggplot(home, aes(x=Boligtilstand, y=Kontantpris)) + geom_point(position = "jitter")

#Postnr ~ Kontantpris
ggplot(home, aes(x=as.factor(Postnr), y=Kontantpris, colour = Boligtilstand)) + geom_point(position = "jitter")

#Salgsdato ~ Kontantpris
ggplot(home, aes(x=Salgsdato, y=Kontantpris, colour = Boligtilstand)) + geom_point()

#Postnr ~ Liggetid
ggplot(home, aes(x=as.factor(Postnr), y=as.integer(Liggetid), colour = Boligtilstand)) + geom_point()


#Smart med wrap
ggplot(home, aes(x=as.integer(Liggetid), y=Kontantpris, colour = Boligtilstand)) + geom_point()
ggplot(home, aes(x=as.integer(Liggetid), y=Kontantpris, colour = Boligtilstand)) + geom_point() + facet_wrap(~Postnr)

#Boxplot
ggplot(home, aes(Boligtilstand, Kontantpris)) + geom_boxplot() 

#Barplot
ggplot(home, aes(x=Kontantpris, fill = as.factor(Postnr))) + xlim(c(0, 7*1e6)) + ylim(c(0,127))  + geom_histogram(bins = 100) + geom_vline(xintercept = mean(home$Kontantpris))

#Andet
qplot(Kontantpris, Boligareal, colour = Boligtilstand, shape = EjdType, data=home) #Shape virker kun ved diskrete, mens colour virker ved begge
qplot(Kontantpris, Boligareal, colour = Boligtilstand, shape = as.factor(AntalToiletter), data=home) #Kan bruge as.factor til at goere diskret, dog oftest bedre at kont faar col
ggplot(home, aes(x=Kontantpris, y=Boligareal)) + geom_point() #eller + geom_line()

#Histogram
ggplot(home, aes(x=Postnr)) + geom_histogram(binwidth=5)
ggplot(home, aes(x=Kontantpris)) + geom_histogram(binwidth=1e5)
ggplot(home, aes(x=log(Kontantpris))) + geom_histogram(binwidth=0.05)

pairs(home[,c(1,2,5)])


fit1=lm(Kontantpris~Salgsdato,data=home)
summary(fit1)
