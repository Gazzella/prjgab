foo <- ran.data(100, 25)
time <- foo$time
data <- foo$data
# fit dose response curves
d y
result <- gcFit(data, time, control=grofit.control(fit.opt="b"))
print(summary(result))
plot(result)

previsione <- as.Date(as.character("01-06-15"), "%d-%m-%y")


previsionenum <- as.numeric(previsione)
log.fit <- function(dep, ind, yourdata){
        #Self-starting...
        
        y <- KakaoTalkMil$registmil
        
        x <- as.numeric(KakaoTalkMil$datesf)
        class(x)
        d <- KakaoTalkMil$datesf
        
        log.ss <- nls(y ~ SSlogis(x, phi1, phi2, phi3))
        
        #C
        C <- summary(log.ss)$coef[1]
        #a
        A <- exp((summary(log.ss)$coef[2]) * (1/summary(log.ss)$coef[3]))
        #k
        K <- (1 / summary(log.ss)$coef[3])
        
        plot(y ~ x, main = "Logistic Function KakaoTalk", xlab="Data", ylab="Numero di registrati in milioni", xlim=c(14686, 17000), ylim=c(0, 200) )
        lines(14686:max(x), predict(log.ss, data.frame(x=14686:max(x))), col="red")
        
        x
        
        r1 <- sum((x - mean(x))^2)
        r2 <- sum(residuals(log.ss)^2)
        
        r_sq <- (r1 - r2) / r1
        
        out <- data.frame(cbind(c(C=C, a=A, k=K, R.value=sqrt(r_sq))))
        names(out)[1] <- "Logistic Curve"
        
        out
        
                datistime2<-predict(log.ss, data.frame(x=14686:max(previsionenum))) 
                datistime<-predict(log.ss, data.frame(x=14686:max(x))) 
        
        
        date_num <- seq(from=14686, to=previsionenum)
        
        prev_fino_15 <- cbind(date_num, datistime)
        
        prev_fino_15.df <- as.data.frame(prev_fino_15.df)
        
        z <- prev_fino_15.df$date_num
        q <- prev_fino_15.df$datistime
        prev_fino_15.df$dataeste <- as.Date(z)
        
        da
        
        png(file =
                    "plotKakaoTalk.png", width = 800, height = 400 
        ) 
        
        plot(q ~ da, main = "Logistic Function KakaoTalk previsione", xlab="Data", ylab="Numero di registrati in milioni", xlim=c(14686, 17000), ylim=c(0, 200) )
        
        
        dev.off()
        
        png(file =
                    "plotKakaoTalkForecast.png", width = 800, height = 600 
        ) 
        g <- ggplot(prev_fino_15.df, aes(dataeste, q)) 
        g + geom_point(aes(color = "steelblue"),
                       size = 4, alpha = 1/2) + geom_smooth() + 
                labs(title = "Logistic Function KakaoTalk previsione giugno 2015", y ="Numero di registrati", x = "Data") 
            #+ xlim=c(14686, 17000) + ylim=c(0, 200)
        
        dev.off()
        
        
        return(out)
}