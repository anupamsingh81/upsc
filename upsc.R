UPSC = read.csv('up.csv',stringsAsFactors = FALSE)

library(tidyverse)

upm %>% ggplot(aes(x=Written))+geom_density()+ facet_wrap(~caste,ncol=1)


upm %>% ggplot(aes(x=Interview))+geom_density()+ facet_wrap(~caste,ncol=1)


upm %>% ggplot(aes(x=Total))+geom_density()+ facet_wrap(~caste,ncol=1)


upm %>% ggplot(aes(x=Interview))+geom_density(fill="red")+ facet_wrap(~Muslim,ncol=1)

upm %>% ggplot(aes(x=Interview))+geom_density(fill="red")+ facet_wrap(~Gender,ncol=1)

t.test(Written~Muslim,data=filter(upm,Rank<500))


t.test(Written~Muslim,data=upm)

upm %>% split(.$year) %>% map(~t.test(Interview~Muslim,data=.))


upm %>% split(.$year) %>% map(~t.test(Interview~Muslim,data=filter(.,Rank<500)))



upm %>% split(.$year) %>% map(~t.test(Interview~Gender,data=filter(.,Rank<500)))


upm %>% split(.$year) %>% map(~t.test(Interview~Gender,data=filter(.)))


summary(lm(Interview~Written+Muslim+Gender,data=upm))


summary(lm(scale(Interview)~scale(Written)+Muslim+Gender,data=upm))

summary(lm(scale(Interview,scale=FALSE)~scale(Written,scale=FALSE)+Muslim+Gender,data=upm))

center_scale <- function(x) {
  scale(x, scale = FALSE)
}

upm$Interview.c = center_scale(upm$Interview)
upm$Written.c = center_scale(upm$Written)

summary(lm(Interview.c~Written.c+Muslim+Gender,data=upm))

summary(lm(Interview~Written+Muslim+Gender,data=upm))


summary(lm(scale(Interview)~scale(Written)+Muslim+Gender,data=upm))

table(upm$Gender,upm$Muslim)

?facet

upm %>% group_by(caste,year) %>% summarise(n=n(),marks=mean(Total),Written=mean(Written),
                                       Interview=mean(Interview),Rank=mean(Rank),
                                       Muslims=mean(Muslim))


 UPSC %>%  ggplot(aes(y=Total,x=caste))+geom_boxplot()


 UPSC %>%  ggplot(aes(y=Written,x=caste))+geom_boxplot()
 
 UPSC %>%  ggplot(aes(y=Interview,x=caste))+geom_boxplot()
 
 
 
 write.csv(upm,'upm.csv')
 
 upm %>% ggplot(aes(x=Interview,y=Written,color=as.factor(Muslim),group=as.factor(Muslim)))+
                 geom_point()+  stat_smooth(se=FALSE)
 
 
 upm %>%  filter(Rank<500) %>% 
   ggplot(aes(x=Interview,y=Written,color=as.factor(Muslim),group=as.factor(Muslim)))+
  # geom_point()+ 
   stat_smooth(se=FALSE)
 
 
 
 upm %>%  #filter(Rank<500) %>% 
   ggplot(aes(x=Rank,y=Interview,color=as.factor(Muslim),group=as.factor(Muslim)))+
   # geom_point()+ 
   stat_smooth(se=FALSE)
 
 
 upsc13 %>% ggplot(aes(x=Interview,y=Written,color=as.factor(Muslim),group=as.factor(Muslim)))+
   stat_smooth(se=FALSE)
 
 upsc13 %>% count(as.character(Muslim))
 
 UPSC %>% count(Muslim)
 
 UPSC %>% ggplot(aes(x=Interview,y=Written,color=as.factor(caste),group=as.factor(caste)))+
   stat_smooth(method="lm")
 
 
 
 upm %>%#filter(!(caste=="ST")) %>% 
   ggplot(aes(x=Interview,y=Written,color=as.factor(caste),group=as.factor(caste)))+
   stat_smooth(se=FALSE)
 
 
 upm %>%#filter(!(caste=="ST")) %>% 
   ggplot(aes(x=caste,y=Interview,fill=as.factor(caste)))+
   geom_boxplot()
 
 
 
 upm %>%#filter(!(caste=="ST")) %>% 
   ggplot(aes(x=Muslim,y=Interview,fill=as.factor(Muslim)))+
   geom_boxplot()
 
 
 upm %>%
   filter(Rank<500) %>% #filter(!(caste=="ST")) %>% 
   ggplot(aes(x=Muslim,y=Interview,fill=as.factor(Muslim)))+
   geom_boxplot()
 
 
 
 
 UPSC %>% ggplot(aes(x=Interview,y=Total,color=as.factor(Muslim),group=as.factor(Muslim)))+
   stat_smooth(se=FALSE)
 
 
 upm %>% ggplot(aes(x=Interview,y=Written,color=as.factor(Muslim),group=as.factor(Muslim)))+
   stat_smooth(method="lm",se=FALSE)
 
 
 UPSC %>% ggplot(aes(x=Interview,y=Total,color=as.factor(Muslim),group=as.factor(Muslim)))+
   stat_smooth(se=FALSE)

##########upsc14








upm %>% #filter(Rank<500) %>% 
  filter(caste=="OBC") %>% 
  select(Name,caste) %>% mutate(surname= str_match(Name,patt)[,2]
) %>% group_by(surname) %>% summarise(n=n()) %>% arrange(desc(n)) %>% View()



upm %>% #filter(Rank<500) %>% 
  filter(caste=="ST") %>% 
  select(Name,caste) %>% mutate(surname= str_match(Name,patt)[,2]
  ) %>% group_by(surname) %>% summarise(n=n()) %>% arrange(desc(n)) %>% View()


upm %>% #filter(Rank<500) %>% 
  filter(Muslim==1) %>% 
  select(Name,caste) %>% mutate(surname= str_match(Name,patt)[,2]
  ) %>% group_by(surname) %>% summarise(n=n()) %>% arrange(desc(n)) %>% View()



upm %>% #filter(Rank<500) %>% 
  filter(Muslim==1,caste=="OBC") %>% 
  select(Name,caste) %>% mutate(surname= str_match(Name,patt)[,2]
  ) %>% group_by(surname) %>% summarise(n=n()) %>% arrange(desc(n)) %>% View()

 upsc15=
   upsc15 %>% select(-X) %>% mutate(Muslim=replace_na(Muslim,0))
 
 
 upm=full_join(upm,upsc15)
 
 upm=upm %>% select(-Roll.No.)
 
 
 upm %>% group_by(surname) %>% summarise(n=n()) %>% arrange(desc(n)) %>% View()
 
 summary(upm)
 
 upm %>% filter(caste=="General")%>% group_by(surname) %>% 
  summarise(n=n()) %>% 
   arrange(desc(n)) %>% 
   View()
   
   

 
 upm %>% filter(caste=="OBC")%>% group_by(surname) %>% 
   summarise(n=n()) %>% 
   arrange(desc(n)) %>% 
   View()
 
 
 
 upm %>% filter(caste=="SC")%>% group_by(surname) %>% 
   summarise(n=n()) %>% 
   arrange(desc(n)) %>% 
   View()
 
 
 
 upm %>% filter(caste=="ST")%>% group_by(surname) %>% 
   summarise(n=n()) %>% 
   arrange(desc(n)) %>% 
   View()
 
upm %>% 
  filter(surname=="SHARMA"|surname=="YADAV") %>% 
  ggplot(aes(x=as.factor(surname),y=Total,fill=as.factor(surname)))+
  geom_boxplot()


upm %>% filter(caste=="ST") %>% 
  mutate(MNM= case_when(surname=="MEENA"~"MEENA",
                        TRUE~"NON-MEENA")) %>% 
  ggplot(aes(x=as.factor(MNM),y=Total,fill=as.factor(MNM)))+
  geom_boxplot()


upm %>% filter(caste=="OBC") %>% 
  mutate(MNM= case_when(surname=="YADAV"~"YADAV",
                        TRUE~"NON-YADAV")) %>% 
  ggplot(aes(x=as.factor(MNM),y=Total,fill=as.factor(MNM)))+
  geom_boxplot()


upm %>% group_by(surname) %>% 
  summarize_at(vars(Total,Rank,Interview,Written),funs(mean,n=n())) %>% arrange(desc(Total_n)) %>% 
  rename(count=Total_n) %>% 
 select(-Rank_n,-Interview_n,-Written_n) %>% View()



upm %>% group_by(surname) %>% 
  summarize_at(vars(Total,Rank,Interview,Written),funs(mean,n=n())) %>% arrange(desc(Total_n)) %>% 
  rename(count=Total_n) %>% 
  select(-Rank_n,-Interview_n,-Written_n) %>% 
  filter(count>4) %>% 
  arrange(desc(Interview_mean)) %>% View()



upm %>% group_by(surname) %>% 
  summarize_at(vars(Total,Rank,Interview,Written),funs(mean,n=n())) %>% arrange(desc(Total_n)) %>% 
  rename(count=Total_n) %>% 
  select(-Rank_n,-Interview_n,-Written_n) %>% 
  filter(count>4) %>% 
  arrange(desc(Written_mean)) %>% View()



upm %>% group_by(surname) %>% 
  summarize_at(vars(Total,Rank,Interview,Written),funs(mean,n=n())) %>% arrange(desc(Total_n)) %>% 
  rename(count=Total_n) %>% 
  select(-Rank_n,-Interview_n,-Written_n) %>% 
  filter(count>4) %>% 
  arrange(Rank_mean) %>% View()


write.csv(upm,'upm.csv')

###POst Geneder
upm$Gender=replace_na(upm$Gender,0)

###Gender Gap



upm %>% group_by(surname) %>% 
  summarize_at(vars(Total,Rank,Interview,Written,Gender),funs(mean,n=n())) %>% arrange(desc(Total_n)) %>% 
  rename(count=Total_n) %>% 
  select(-Rank_n,-Interview_n,-Written_n,-Gender_n) %>% 
  filter(count>4) %>% 
  arrange(Rank_mean) %>% mutate(Female_percentage = round(100*Gender_mean,2)) %>% 
  mutate(Female_total = round(Female_percentage*count/100,0)) %>% 
   arrange(desc(Female_total)) %>% View()

# Percentage of Females

sum(upm$Gender>0)/length(upm$Name)

###




upm %>%  filter(Rank<500) %>% 
  ggplot(aes(x=Written,y=Interview,color=as.factor(Gender),group=as.factor(Gender)))+
  # geom_point()+ 
  stat_smooth(method="lm",se=FALSE)



upm %>%  filter(Rank<500) %>% 
  ggplot(aes(x=Written,y=Interview,color=as.factor(Muslim),group=as.factor(Muslim)))+
  # geom_point()+ 
  stat_smooth(method="lm",se=FALSE)

###


upm %>%  split(.$year) %>% 
  map(~ggplot(data=.,aes(x=Written,y=Interview,color=as.factor(Muslim),group=as.factor(Muslim)))+
  # geom_point()+ 
  stat_smooth(method="lm",se=FALSE))

######


upm %>%  split(.$year) %>% 
  map(~ggplot(data=.,aes(x=Written,y=Interview,color=as.factor(Gender),group=as.factor(Gender)))+
        # geom_point()+ 
        stat_smooth(method="lm",se=FALSE))


########

upm %>%  split(.$year) %>% 
  map(~ggplot(data=.,aes(x=Written,y=Interview,color=as.factor(caste),group=as.factor(caste)))+
        # geom_point()+ 
        stat_smooth(method="lm",se=FALSE))



#fixing 2014
upm %>% filter(year==2014) %>% tail(n=5)

upm$Muslim[2222:3457] = upsc140$Muslim

upm$Muslim = replace_na(upm$Muslim,0)

### ByCaste n Muslim



upm %>% group_by(caste) %>% 
  summarize_at(vars(Total,Rank,Interview,Written,Gender,Muslim),funs(mean,n=n())) %>% arrange(desc(Total_n)) %>% 
  rename(count=Total_n) %>% 
  select(-Rank_n,-Interview_n,-Written_n,-Gender_n) %>% 
  filter(count>4) %>% 
  arrange(Rank_mean) %>% mutate(Female_percentage = round(100*Gender_mean,2)) %>% 
  mutate(Female_total = round(Female_percentage*count/100,0)) %>% 
  arrange(desc(Female_total)) %>% 
  mutate(Muslim_percentage = round(100*Muslim_mean,2)) %>% 
  mutate(Muslim_total = round(Muslim_percentage*count/100,0)) %>% 
  arrange(desc(Muslim_total)) %>% 
View()

# correcting
library(tidyverse)

upm %>% filter(caste=="SC",Muslim==1) %>% View()

upm$Muslim[826]=0
upm$Muslim[674]=0
upm$Muslim[814]=0



###

upm %>% filter(caste=="ST",Muslim==1) %>% View()

upm$Muslim[703]=0

upm %>% filter(caste=="OBC",Muslim==1) %>% View()



upm %>%#filter(!(caste=="ST")) %>% 
  ggplot(aes(x=year,fill=as.factor(Muslim)))+
  geom_bar(position="dodge")


upm %>%#filter(!(caste=="ST")) %>% 
  ggplot(aes(x=year,y=100*Muslim)) +
  stat_summary(
    fun.y=mean
    )

str(upm$Muslim)

upm %>%#filter(!(caste=="ST")) %>% 
  ggplot(aes(x=year,fill=as.factor(Muslim)))+
  geom_bar(position="fill")

upm %>%#filter(!(caste=="ST")) %>% 
  ggplot(aes(x=year,fill=as.factor()))+
  geom_bar(position="fill")



upm %>%#filter(!(caste=="ST")) %>% 
  ggplot(aes(x=year,y=..prop..,fill=as.factor(Muslim)))+
  geom_bar()



?stat


upm %>%#filter(!(caste=="ST")) %>% 
  ggplot(aes(x=year,fill=as.factor(Gender)))+
  geom_bar(position="dodge")


########stats


?case_when
str(UPSC)



t.test(UPSC$Interview~UPSC$Muslim)

t.test(UPSC$Written~UPSC$Gender)

t.test(UPSC$Written~UPSC$Muslim)

t.test(UPSC$Rank~UPSC$Muslim)

x= lm(Interview~Written+Muslim+Rank ,data=UPSC)

summary(x)

y=lm(Interview~Written*Muslim ,data=UPSC)

summary(y)

UPSC$Written.c = UPSC$Written - mean(UPSC$Written)
UPSC$Gender.c = UPSC$Gender - mean(UPSC$Gender)

UPSC$Interview.c = UPSC$Interview - mean(UPSC$Interview)
UPSC$Muslim.c = UPSC$Muslim - mean(UPSC$Muslim)


x1= lm(Interview~Written.c+Muslim ,data=UPSC)

summary(x1)


x1.1= lm(Interview~Written*Muslim ,data=upm)

summary(x1.1)

x2= lm(Interview~Written.c+Muslim+Gender.c ,data=UPSC)

summary(x2)


table(UPSC$Muslim,UPSC$Gender)


plot(UPSC$Interview,UPSC$Rank)
cor.test(UPSC$Interview,UPSC$Rank)



f=aov(Total~caste,data=UPSC)


summary(f)

TukeyHSD(f)

upm %>% filter(surname=="MEENA") %>% 
  
  View()

summary()



upm %>% filter(caste=="SC") %>% summary()

upm %>% 
  
  ?geom_hline
round(100*mean(upm$Gender),2)

library(plo)
?annotate

####unique

length(!unique(upm$Name))/length(upm$Name)

upm %>% distinct(Name)

repeaters =
  upm %>% group_by(Name) %>% summarize(n=n()) %>% filter(n>1) %>%  pull(Name) %>% length()



upm %>% filter(Rank<100)

 upm %>% filter(Name %in% repeaters) %>% filter(Rank<100) %>% pull(Name) %>% length()

upm%>% filter(Name %in% repeaters) %>% arrange(Name,year) %>% View()

upm%>% filter(Name %in% repeaters) %>% filter(Rank<100) %>% arrange(Name,year) %>% View()




#####3prop

upm %>% filter(caste=="ST") %>% group_by(surname) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>% mutate(freq=round(100*freq,2)) %>% arrange(desc(freq))

upm %>% #filter(caste=="ST") %>% 
  group_by(surname) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>% mutate(freq=round(100*freq,2)) %>% arrange(desc(freq)) %>% 
  mutate(cumulative=cumsum(freq)) %>% 
  filter(cumulative<50)

upm %>% filter(caste=="ST") %>% 
  group_by(surname) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>% mutate(freq=round(100*freq,2)) %>% arrange(desc(freq)) %>% 
  mutate(cumulative=cumsum(freq)) %>% 
  filter(cumulative<50)

upm %>% filter(caste=="General") %>% 
  group_by(surname) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>% mutate(freq=round(100*freq,2)) %>% arrange(desc(freq)) %>% 
  mutate(cumulative=cumsum(freq)) %>% 
  filter(cumulative<50)



upm %>% filter(caste=="OBC") %>% 
  group_by(surname) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>% mutate(freq=round(100*freq,2)) %>% arrange(desc(freq)) %>% 
  mutate(cumulative=cumsum(freq)) %>% 
  filter(cumulative<50)


upm %>% filter(caste=="SC") %>% 
  group_by(surname) %>% 
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>% mutate(freq=round(100*freq,2)) %>% arrange(desc(freq)) %>% 
  mutate(cumulative=cumsum(freq)) %>% 
  filter(cumulative<50)


