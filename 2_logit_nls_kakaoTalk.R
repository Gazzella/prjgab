###Funzione pacchetto car
  
KakaoTalk$datesf_num <- as.numeric(KakaoTalk$datesf)
KakaoTalk$Registered.01 <- KakaoTalk$Registered.mil/180

library("car")


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


##Valore max

previsione <- as.Date(as.character("31-12-15"), "%d-%m-%y")
previsionenum <- as.numeric(previsione)

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

library(nls2)


registrati.pred.mil<- predict(kakao.mod, data.frame(datesf_num=seq(14686, 16800, by=1)))
datesf_num=seq(14686, 16800, by=1)
registrati.pred.df=as.data.frame(cbind(registrati.pred.mil,datesf_num))

class(registrati.pred.df$datesf_num)
d<-registrati.pred.df$datesf_num

library(zoo)

##pacchetto zoo per conversione date
registrati.pred.df$data <-  as.Date(d)
origin.df <- subset(KakaoTalk, select=c("datesf_num", "Registered"))

library(plyr)
df.oss.pred<-join(registrati.pred.df, origin.df, by = "datesf_num", type = "left", match = "all")



library(xlsx)
write.xlsx(df.oss.pred, "osser_pred.xlsx", sheetName="Dati",           
           col.names=TRUE, row.names=F, append=FALSE, showNA=TRUE)
