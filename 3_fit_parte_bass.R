###Analisi prima parte fino a

KakaoTalk.val <- df.oss.pred[complete.cases(df.oss.pred[,"Registered"]),]
KakaoTalk.new.es <- subset(KakaoTalk.val, KakaoTalk.val$datesf_num < 15800)
KakaoTalk.new.es$Registered.mil <- KakaoTalk.new.es$Registered/1000000
uvo <- as.Date(as.character("15/08/14"), "%d/%m/%y")
uvonum <- as.numeric(uvo)



##lineare
reg <- lm(Registered.mil ~ datesf_num, KakaoTalk.new.es)
reg
summary(reg)

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


legend('topleft', c("retta", "polinomiale", "spline"),  lwd=2,
       col=c('red', 'green',' purple'), bty='n', cex=1)
dev.off()

summary(fit3)
summary(fit5)


##ultimo valore osservato


df.osserv.fino.flesso_polin <- predict(fit3, data.frame(datesf_num=seq(14686, 15800, by=1)), interval = "prediction",
              level = 0.95)


data_num <-  seq(14686, 15800, by=1)

data <- as.Date(data_num)

class(data)
df.osserv.fino.flesso_polin=as.data.frame(df.osserv.fino.flesso_polin)
df.osserv.fino.flesso_polin$data <-  as.Date(data_num)

df.osserv.fino.flesso_spline <- predict(fit5, data.frame(datesf_num=seq(14686, 15800, by=1)), interval = "prediction",
                                       level = 0.95)

df.osserv.fino.flesso_spline=as.data.frame(df.osserv.fino.flesso_spline)
df.osserv.fino.flesso_spline$data <-  as.Date(data_num)

library(xlsx)
write.xlsx(df.osserv.fino.flesso_polin, "osser_pred_fino_a_polin.xlsx", sheetName="Dati",           
           col.names=TRUE, row.names=F, append=FALSE, showNA=TRUE)

write.xlsx(df.osserv.fino.flesso_spline, "osser_pred_fino_a_spline.xlsx", sheetName="Dati",           
           col.names=TRUE, row.names=F, append=FALSE, showNA=TRUE)



