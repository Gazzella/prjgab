##leggo i dati
df.competitors <- read.csv("./CYA competitors all.csv", sep=";", stringsAsFactors=FALSE, as.is=T)
str(df.competitors)

class(df.competitors$Date)

df.competitors$datesf <- as.Date(as.character(df.competitors$Date), "%d-%m-%y")

KakaoTalk <- subset(df.competitors, grepl("KakaoTalk", df.competitors$Product))

by_date <- group_by(KakaoTalk, datesf)
KakaoTalkMil <- summarize(by_date,                      
                              registmil = sum(Registered/1000000))

png(file =
            "plotKakaoTalk.png", width = 1000, height = 400 
) 
g <- ggplot(KakaoTalkMil, aes(Date, registmil)) 
g + geom_point(aes(color = "steelblue"),
        size = 4, alpha = 1/2) + geom_smooth() + 
        labs(title = "Andamento Registrati KakaoTalk", y ="Numero", x = "Data")

dev.off()
