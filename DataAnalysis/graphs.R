#Hvis run from R 
#dir <- dirname(parent.frame(2)$ofile)
#setwd(dir)

#Hvis R studio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("HOME2.RData")

head(HOME2)
range(HOME2[,3])
unique(HOME2[,3])
range(HOME2$EjdType)
names(HOME2)

#Important: Run dplyr
library(dplyr)


#Vigtig: Se paa type af variable: Hvilke type, ex liggetid er difftime. Evt aendrer hvis unoedvendigt.
#Hvad indeholder de: Range, NA?, Hvordan tolkes?
str(HOME)
summary(HOME)
table(HOME$AntalPlan, useNA = "always")

#Tell R that Salgsdato contains dates
HOME = HOME2; HOME$Salgsdato = as.Date(HOME$Salgsdato, "%d-%m-%Y")
#Evt factorize en masse variable saasom, ellers vent til efter snak med Matt, Our man! Diskuter hvilke?!
for(col in c(3,"Storgrund")){
  #HOME[c(col)] <- factor(HOME[,c(col)])
  #If string
  try(HOME[col] <- factor(HOME[col]), silent = TRUE)
  #If numeric
  try(HOME[as.numeric(col)] <- factor(HOME[as.numeric(col)]), silent = TRUE)
}
#Faktoriser Boligtilstand og skift til engelsk navne paa levels
HOME$Boligtilstand <- factor(HOME$Boligtilstand, levels = c("Daarlig", "Middel", "God"))
levels(HOME$Boligtilstand) <- c("Bad", "Medium", "Good")

#Evt slaa kvartaler sammen til en raekke med faktor 1,2,3,4
HOME$Kvartal1[HOME$Kvartal1 == 1] = 1
HOME$Kvartal2[HOME$Kvartal2 == 1] = 2
HOME$Kvartal3[HOME$Kvartal3 == 1] = 3
HOME$Kvartal4[HOME$Kvartal4 == 1] = 4

HOME <- HOME %>% mutate(Kvartal = factor(Kvartal1 + Kvartal2 + Kvartal3 + Kvartal4))


#Bemaerk kun faa af typen 2 fam. Evt slet disse fra datasaet, og aendre 3 linjer laengere nede. And if so, maybe delete EjdType col afterwards
table(HOME2$EjdType)

HOME <- HOME %>% filter(EjdType == "Villa, 1 fam." | EjdType == "Villa, 2 fam.") %>% 
  group_by(Postnr) %>%  
  arrange(Salgsdato)
#Kunne have valgt at arrange by kontantpris, salgsaar, opfoerelsesaar?

#Enten gem som R eller csv
save(HOME, file="HOME.R")
write.csv(HOME, file="HOME.csv")
load("HOME.R")

#Ide grupperinger af postnumre
table(HOME$Postnr)
hist(HOME$Kontantpris)

#Tjek hvor mange missing values vi har,- God til at tjekke hvorvidt vi kan bruge variabel eller ej, og hvordan NA fortolkes
table(HOME$NytKokken, useNA = "always")


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
str(HOME)
levels(HOME$Boligtilstand) #Har ingen levels
summary(HOME)
library(psych)
describe(HOME)

#Evt udregn correlation med cor

#Hvis traet af at skirve HOME$etelleandet, brug
attach(mtcars)
#Nu kan du blot skrive etellerandet
#Husk til sidst at bruge
detach()



#Graphs
#Evt proev at goere grafer like a beauty queen med xmax ymax osv 
#Ande theme: Proev + theme_dark()
library(ggplot2)
qplot(Boligareal, Kontantpris, data=HOME)

#Boligtilstand ~ Kontantpris, foerste virker ikke da ikke kan se hvor mange punkter
qplot(Boligtilstand, Kontantpris, data=HOME)
ggplot(HOME, aes(x=Boligtilstand, y=Kontantpris)) + geom_point(position = "jitter")

#Postnr ~ Kontantpris
ggplot(HOME, aes(x=as.factor(Postnr), y=Kontantpris, colour = Boligtilstand)) + geom_point(position = "jitter")

#Salgsdato ~ Kontantpris
ggplot(HOME, aes(x=Salgsdato, y=Kontantpris, colour = Boligtilstand)) + geom_point()

#Postnr ~ Liggetid
ggplot(HOME, aes(x=as.factor(Postnr), y=as.integer(Liggetid), colour = Boligtilstand)) + geom_point()


#Smart med wrap
ggplot(HOME, aes(x=as.integer(Liggetid), y=Kontantpris, colour = Boligtilstand)) + geom_point()
ggplot(HOME, aes(x=as.integer(Liggetid), y=Kontantpris, colour = Boligtilstand)) + geom_point() + facet_wrap(~Postnr)

#Boxplot
ggplot(HOME, aes(Boligtilstand, Kontantpris)) + geom_boxplot() 

#Barplot
ggplot(HOME, aes(x=Kontantpris, fill = as.factor(Postnr))) + xlim(c(0, 7*1e6)) + ylim(c(0,127))  + geom_histogram(bins = 100) + geom_vline(xintercept = mean(HOME$Kontantpris))

#Andet
qplot(Kontantpris, Boligareal, colour = Boligtilstand, shape = EjdType, data=HOME) #Shape virker kun ved diskrete, mens colour virker ved begge
qplot(Kontantpris, Boligareal, colour = Boligtilstand, shape = as.factor(AntalToiletter), data=HOME) #Kan bruge as.factor til at goere diskret, dog oftest bedre at kont faar col
ggplot(HOME, aes(x=Kontantpris, y=Boligareal)) + geom_point() #eller + geom_line()

#Histogram
ggplot(HOME, aes(x=Postnr)) + geom_histogram(binwidth=5)
ggplot(HOME, aes(x=Kontantpris)) + geom_histogram(binwidth=1e5)
ggplot(HOME, aes(x=log(Kontantpris))) + geom_histogram(binwidth=0.05)

pairs(HOME[,c(1,2,5)])


fit1=lm(Kontantpris~Salgsdato,data=HOME)
summary(fit1)
