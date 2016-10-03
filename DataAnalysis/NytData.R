#Aendring af variabeltype
home = HOME2
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
home$Kvartal1 = NULL
home$Kvartal2 = NULL
home$Kvartal3 = NULL
home$Kvartal4 = NULL


#Sorter i data om det skal vaere lejligh hhv. villa osv.

home = home %>% filter(EjdType == "Ejerlejlighed") %>% 
  group_by(Postnr) %>%  
  arrange(Salgsdato)
#Kunne have valgt at arrange by kontantpris, salgsaar, opfoerelsesaar?

#Enten gem som R eller csv
save(home, file="home.Rdata")
write.csv(home, file="home.csv")