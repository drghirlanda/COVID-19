load.french.data <- function() {
    fr.raw <- fread("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv")
    fr <- rbind(
        fr.raw[ maille_code=="FRA" ],
        fr.raw[ grep("REG-",maille_code) ]
    )

    setnames( fr, c("date","maille_nom"), c("Day","Reg2") )
    fr[ Reg2=="France", Reg2 := "All" ]
    fr[, Reg1 := "France" ]

    ## fix some inconsistencies in names
    fr[ grep("Auvergne",Reg2), Reg2 := "Auvergne-Rhone-Alpes" ]
    fr[ grep("Grand",Reg2), Reg2 := "Grand Est" ]
    fr[ grep("Bourgogne",Reg2), Reg2 := "Bourgogne-Franche-Comte" ]
    fr[ grep("Provence",Reg2), Reg2 := "Provence-Alpes-Cote d'Azur" ]
    fr[ grep("Centre",Reg2), Reg2 := "Centre-Val de Loire" ]
    fr[ grep("La R",Reg2), Reg2 := "La Reunion" ]

    fr <- fr[, .(Reg2,Reg1,Day,cas_confirmes,deces) ]

    fr.c <- fr[, .(Reg2,Reg1,Day,cas_confirmes) ]
    setnames( fr.c, "cas_confirmes", "Count" )
    fr.c$What <- "Confirmed Cases"
    fr.d <- fr[, .(Reg2,Reg1,Day,deces) ]
    setnames( fr.d, "deces", "Count" )
    fr.d$What <- "Fatalities"
    
    fr <- rbind( fr.c, fr.d )
    
    fr <- fr[ complete.cases(fr) ]

    ## get rid of some inconsistencies and duplications
    fr <- fr[, mean(Count), by=.(Reg2,Reg1,Day,What) ]
    setnames( fr, "V1", "Count" )

    fr$Day <- as.Date( fr$Day )
    fr$Reg3 <- "All"
    fr
}
