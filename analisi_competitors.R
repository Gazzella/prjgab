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



###Funzione pacchetto car
y <- KakaoTalk$Registered.mil       
KakaoTalk$datesf_num <- as.numeric(KakaoTalk$datesf)
KakaoTalk$Registered.01 <- KakaoTalk$Registered.mil/180


##Starting point 165 milioni di registrati
a<-lm(logit(Registered.mil/165) ~ datesf_num, KakaoTalk)
summary(a)

##Funzione glm
glm.out = glm(Registered.mil/165 ~ datesf_num,
              family=binomial(logit), data=KakaoTalk)
summary(glm.out)





### plot dei valori e della funzione stimata

png(file =            
            "plotKakaoTalkLogfit.png", width = 800, height = 600    
)

plot(Registered.mil/165 ~ datesf, data=KakaoTalk)
lines(KakaoTalk$datesf, glm.out$fitted.values, type="l", col="red")
title(main="KakaoTalk with Fitted Logistic Regression Line")
dev.off()





##Stima parametri teta con start parametri funzione logit

kakao.mod <- nls(Registered.mil ~ theta1/(1 + exp(-(theta2 + theta3*datesf_num))),
                 
                 start=list(theta1 = 165, theta2 = -57, theta3 = 0.00362),
                 
                 data=KakaoTalk, trace=TRUE)



summary(kakao.mod)



##Errore standard

deltaMethod(kakao.mod, "-theta2/theta3")

r1 <- sum((KakaoTalk$datesf_num - mean(KakaoTalk$datesf_num))^2)
r2 <- sum(residuals(kakao.mod)^2)

r_sq <- (r1 - r2) / r1


nl1.boot <- nlsBoot(kakao.mod,niter=95)
a<- predict(kakao.mod, data.frame(datesf_num=seq(14686, 16800, by=1)), interval = "prediction",  level = 0.95)



##Valore max

previsione <- as.Date(as.character("31-12-15"), "%d-%m-%y")

previsionenum <- as.numeric(previsione)



##ultimo valore osservato



uvo <- as.Date(as.character("15/08/14"), "%d/%m/%y")

uvonum <- as.numeric(uvo)



png(file =
            
            "PlotKakaoTalkForecastNls.png", width = 800, height = 600
    
)

plot(Registered.mil ~ datesf, KakaoTalk, xlim=c(14686, 16800), ylim=c(1,250))

with(KakaoTalk, lines(seq(14686, 16800, by=1),
                      
                      + predict(kakao.mod, data.frame(datesf_num=seq(14686, 16800, by=1))), lwd=2))

points(16297, 152, pch="x", cex=1.3)

abline(h=0, lty=2)

abline(h=coef(kakao.mod)[1], lty=2)

abline(h=.5*coef(kakao.mod)[1], lty=2)

abline(v= -coef(kakao.mod)[2]/coef(kakao.mod)[3], lty=2)


dev.off()



###Self-Starting Models
kkss <- nls(Registered.mil ~ SSlogis(datesf_num, phi1, phi2, phi3), data=KakaoTalk)
summary(kkss)
str(kkss)


ccc<- predict(kkss, data.frame(datesf_num=seq(14686, 16800, by=1)), interval="prediction", )




registrati.pred.mil<- predict(kakao.mod, data.frame(datesf_num=seq(14686, 16800, by=1)))
datesf_num=seq(14686, 16800, by=1)
registrati.pred.df=as.data.frame(cbind(registrati.pred.mil,datesf_num))

class(registrati.pred.df$datesf_num)



d<-registrati.pred.df$datesf_num



##pacchetto zoo

registrati.pred.df$data <-  as.Date(d)

origin.df <- subset(KakaoTalk, select=c("datesf_num", "Registered"))







df.oss.pred<-join(registrati.pred.df, origin.df, by = "datesf_num", type = "left", match = "all")


class(registrati.pred.df)



library(xlsx)



write.xlsx(df.oss.pred, "osser_pred.xlsx", sheetName="Dati",
           
           col.names=TRUE, row.names=F, append=FALSE, showNA=TRUE)





###Analisi prima parte fino a

KakaoTalk.val <- df.oss.pred[complete.cases(df.oss.pred[,"Registered"]),]

KakaoTalk.new.es <- subset(KakaoTalk.val, KakaoTalk.val$datesf_num < 15800)

KakaoTalk.new.es$Registered.mil <- KakaoTalk.new.es$Registered/1000000



##lineare



reg <- lm(Registered.mil ~ datesf_num, KakaoTalk.new.es)



reg



summary(reg)



png(file =
            
            "PlotKakaoTalklm.png", width = 800, height = 600
    
)



plot(Registered.mil ~ data, KakaoTalk.new.es, pch = 16, cex = 1.3, col = "red",
     
     xlim=c(14686, 15800), ylim=c(0,80))

abline(-1.181e+03, 7.951e-02)



dev.off()







##Confronto i fit
png(file =
            
            "PlotKakaoTalknfit2.png", width = 800, height = 600
    
)

plot(Registered.mil ~ data, KakaoTalk.new.es, pch = 16, cex = 1.3, col = "red",
     
     xlim=c(14686, 15800), ylim=c(0,80))

fit1 <- lm( Registered.mil~offset(datesf_num) -1, KakaoTalk.new.es )
fit2 <- lm(Registered.mil ~ datesf_num, KakaoTalk.new.es)
fit3 <- lm( Registered.mil~poly(datesf_num,2), KakaoTalk.new.es )

library(splines)
fit5 <- lm( Registered.mil~ns(datesf_num, 3), KakaoTalk.new.es )





with(KakaoTalk.new.es, lines(seq(14686, 15800, by=1),
                             
                             + predict(fit1, data.frame(datesf_num=seq(14686, 15800, by=1))), lwd=2, col='blue'))



with(KakaoTalk.new.es, lines(seq(14686, 15800, by=1),
                             
                             + predict(fit2, data.frame(datesf_num=seq(14686, 15800, by=1))), lwd=2, col='red'))



with(KakaoTalk.new.es, lines(seq(14686, 15800, by=1),
                             
                             + predict(fit3, data.frame(datesf_num=seq(14686, 15800, by=1))), lwd=2, col='green'))



with(KakaoTalk.new.es, lines(seq(14686, 15800, by=1),
                             
                             + predict(fit5, data.frame(datesf_num=seq(14686, 15800, by=1))), lwd=2, col='purple'))





dev.off()



summary(fit3)

fit3

##ultimo valore osservato
uvo <- as.Date(as.character("15/08/14"), "%d/%m/%y")
uvonum <- as.numeric(uvo)

bb <- predict(fit3, data.frame(datesf_num=seq(14686, 15800, by=1)), interval = "prediction",
              level = 0.95)

aa <- predict(fit5, data.frame(datesf_num=seq(14686, 15800, by=1)))

aa

#lines(predict(fit1, data.frame(datesf_num=seq(14686, 15800, by=1))), col='blue')



#lines(predict(fit2, data.frame(datesf_num=seq(14686, 15800, by=1))), col='green')

#lines(predict(fit3, data.frame(datesf_num=seq(14686, 15800, by=1))), col='red')

#lines(predict(fit5, data.frame(datesf_num=seq(14686, 15800, by=1))), col='orange')

