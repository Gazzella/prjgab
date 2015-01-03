##leggo i dati
##pacchetto car
df.competitors <- read.csv("competitors.csv", sep=";", stringsAsFactors=FALSE, as.is=T)
class(df.competitors$Date)
df.competitors$datesf <- as.Date(as.character(df.competitors$Date), "%d-%m-%y")

##selez solo kakaotalk
KakaoTalk1 <- subset(df.competitors, grepl("KakaoTalk", df.competitors$Product))

##Tolgo gli Na
KakaoTalk <- KakaoTalk1[complete.cases(KakaoTalk1[,"Registered"]),]

##milioni
KakaoTalk$Registered.mil <- KakaoTalk$Registered/1000000



##plot dati originari e smooth
png(file =
            
            "plotKakaoTalk1.png", width = 800, height = 600
    
)

g <- ggplot(KakaoTalk, aes(datesf, Registered.mil))
g + geom_point(colour = "red", size = 3) + geom_smooth() +        
        labs(title = "Andamento Registrati KakaoTalk in milioni", y ="Numero (milioni)", x = "Data")
dev.off()



