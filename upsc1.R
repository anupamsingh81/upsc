up$Rank=c(1:1099)
up$Total=up$Written+up$Interview

up$Muslim[is.na(up$Muslim)] <- 0

summary(up)

t.test(up$Interview~up$Muslim)

t.test(up$Written~up$Muslim)

x1 = lm(Interview~Written+Muslim+Comm,data=up)

summary(x1)

x2 = lm(Interview~Written+Muslim+as.factor(Comm),data=up)

summary(x2)

table(up$Muslim,up$Comm)

write.csv(up,file = "up.csv")

getwd()

plot(up$Rank~up$Interview)

cor.test(up$Rank,up$Interview)

x3 = lm(Interview~Written+Muslim+Written:Muslim,data=up)

summary(x3)
up$Muslim = as.factor(up$Muslim)
library(ggplot2)

str(up$Written)
qplot(up$Written,up$Interview)
ggplot(up, aes(x=Written,y=Interview , color= Muslim)) + 
  geom_point() + scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE) 
cor.test(up$Written,up$Interview)

library(dplyr)
a= filter(up, Muslim=="1") 

cor.test(a$Written,a$Interview)

