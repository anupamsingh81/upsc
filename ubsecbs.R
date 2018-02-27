

library(tidyverse)
ggplot(upm, aes(Interview, Muslim)) +
  geom_point() +
  geom_smooth(method = "glm", family="binomial",formula=y~x,se=FALSE , fullrange = TRUE) +
  xlim(130, 200)


logitloess <- function(x, y, s) {
  
  
  logit <- function(pr) {
    log(pr/(1-pr))
  }
  
  if (missing(s)) {
    locspan <- 0.7
  } else {
    locspan <- s
  }
  
  loessfit <- predict(loess(y~x,span=locspan))
  pi <- pmax(pmin(loessfit,0.9999),0.0001)
  logitfitted <- logit(pi)
  
  
  
  library(readxl)
  
  plot(x, logitfitted, ylab="logit",xlab=x)
  
}

logitloess(upm$Interview,upm$Gender)


############
library(tidyverse) 

library(ggrepel)

library(ggthemes)

pm = upm  %>% group_by(surname) %>% 
  summarise(n=n(),Rank=mean(Rank),total = mean(Total),Written=mean(Written),interview=mean(Interview)) %>% left_join(cbse) %>% 
  drop_na() %>% filter(n>5) %>% arrange(Rank) %>% 
  mutate(rank_upsc=rank(Rank),rank_cbse=rank(desc(`Average Aggregate`))) %>% arrange(rank_cbse) %>% 
  ggplot(aes(rank_upsc,rank_cbse))+geom_point(color="red")+
  stat_smooth(method="lm",se=FALSE)+
  geom_text(aes(rank_upsc, rank_cbse, label = surname))+
  geom_hline(yintercept = 30,linetype="dashed",color="blue")+
  geom_vline(xintercept = 30,linetype="dashed",color="blue")+
  xlab("Rank based on Performance in UPSC from 2013-16")+
  ylab("Rank based Performance in CBSE 2014")+
  theme_economist()+
  labs(
    title = "There is a correlation between UPSC and CBSE performance",
    subtitle = "Baniyas perform better in Both,Brahmins perform better in UPSC",
    caption = "Data from upsc website and learningpoint.com"
  )

upsccbse=upm  %>% group_by(surname) %>% 
  summarise(n=n(),Rank=mean(Rank),total = mean(Total),Written=mean(Written),interview=mean(Interview)) %>% left_join(cbse) %>% 
  drop_na() %>% filter(n>5) %>% arrange(Rank) %>% 
  mutate(rank_upsc=rank(Rank),rank_cbse=rank(desc(`Average Aggregate`))) %>% arrange(rank_cbse)
  

write.csv(upsccbse,'upsccbse.csv')


library(plotly)
ggplotly(pm)

cbse=read_csv('cbse.csv') %>% rename(surname=Surname) 