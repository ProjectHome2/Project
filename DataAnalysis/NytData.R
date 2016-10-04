#Aendring af variabeltype
home = HOME2
library(dplyr)
home$Salgsdato = as.Date(home$Salgsdato, "%d-%m-%Y")

#Faktoriserer de relevante variable
for(col in c("Postnr", "Storgrund", "EjdType", "NytKokken", "NyeVinduer", "NytBadevaerelse", "Altan", "TaetVedVand", "GarageCarport", "MangeToil", "Kaelder", "Hoejhus", "OmbygningSket")){
  home[,col] = factor(home[,col])
}

#Laver nogle relevante variable om til heltal.
for(col in c("Alder", "AntalPlan", "Liggetid", "AntalSovevaerelser", "Salgsaar")){
  home[,col] = as.integer(home[,col])
}

#Faktoriser Boligtilstand og skift til engelsk navne paa levels
home$Boligtilstand = factor(home$Boligtilstand, levels = c("Daarlig", "Middel", "God"))
levels(home$Boligtilstand) = c("Bad", "Medium", "Good")

#Evt slaa kvartaler sammen til en raekke med faktor 1,2,3,4
home$Kvartal1[home$Kvartal1 == 1] = 1
home$Kvartal2[home$Kvartal2 == 1] = 2
home$Kvartal3[home$Kvartal3 == 1] = 3
home$Kvartal4[home$Kvartal4 == 1] = 4

home = home %>% mutate(Kvartal = factor(Kvartal1 + Kvartal2 + Kvartal3 + Kvartal4))

#slet nyt kvartal 1,2,3,4.
home = home[,-c(23,24,25,26)]

# home$Kvartal1 = NULL
# home$Kvartal2 = NULL
# home$Kvartal3 = NULL
# home$Kvartal4 = NULL


#Sorter i data om det skal vaere lejligh hhv. villa osv.

home = home %>% filter(EjdType == "Ejerlejlighed") %>% 
  group_by(Postnr) %>%  
  arrange(Salgsdato)
#Kunne have valgt at arrange by kontantpris, salgsaar, opfoerelsesaar?

#Renaming the variables 
name = c("PostalCode", "DateOfSale", "Type", "Condition", "Price", "LivingArea", "BasementArea", "GroundArea", "ConstructionYear", "Levels", "NewKitchen", "NumberOfToilets", "NewWindows", "NewBathroom", "NumberOfBedrooms", "Balcony", "CloseToWater", "Garage", "Adress", "SalesPeriod", "YearOfSale", "ManyToilets", "Basement", "LargeGround", "Age", "HighHouse", "Renovation", "Quarter")
for(i in 1:length(home)){
  names(home)[i] = name[i]
}

#Do we want to save as log(price) or not 
#home$Price = log(home$Price)
#names(home)[5] = "LogPrice" 


#Either save as R.data or csv
save(home, file="homeFull.Rdata")
write.csv(home, file="homeFull.csv")

