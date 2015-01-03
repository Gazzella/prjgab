Ciao Gab,
ho stimato la parte inferiore del grafico (ho usato come riferimento la data del punto di flesso) 
sia con la polinomiale che con le spline tutte e due significative, di seguito i parametri:

Polimomiale
lm(formula = Registered.mil ~ poly(datesf_num, 2), data = KakaoTalk.new.es)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.5481 -1.3196  0.1384  1.1283  1.8531 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)           33.2727     0.5022   66.26 2.99e-12 ***
poly(datesf_num, 2)1  88.5064     1.6654   53.14 1.74e-11 ***
poly(datesf_num, 2)2  19.8145     1.6654   11.90 2.29e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.665 on 8 degrees of freedom
Multiple R-squared:  0.9973,	Adjusted R-squared:  0.9966 
F-statistic:  1483 on 2 and 8 DF,  p-value: 5.238e-11

Spline
lm(formula = Registered.mil ~ ns(datesf_num, 3), data = KakaoTalk.new.es)

Residuals:
   Min     1Q Median     3Q    Max 
-1.732 -1.057 -0.667  1.459  2.432 

Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)          -1.264      1.618  -0.781     0.46    
ns(datesf_num, 3)1   42.587      2.192  19.426 2.39e-07 ***
ns(datesf_num, 3)2   75.685      3.985  18.990 2.79e-07 ***
ns(datesf_num, 3)3   74.999      1.565  47.932 4.50e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.771 on 7 degrees of freedom
Multiple R-squared:  0.9973,	Adjusted R-squared:  0.9962 
F-statistic: 874.3 on 3 and 7 DF,  p-value: 2.263e-09

Ho stimato il valore teorico (fit) e l'intervallo di confidenza lwr e upr: ci sono i file in xls osser_pred_fino_a_spline.xlsx
osser_pred_fino_a_polin.xlsx.
I primi valori sono negativi perchè la polinomiale va sotto l'asse delle x.
Ti ho anche plottato le funzioni: PlotKakaoTalknfit2.png

