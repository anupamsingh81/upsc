###### DATA EXTRACTION############

UPSC = read.csv('up.csv',stringsAsFactors = FALSE)


library(tidyverse)

UPSC=UPSC %>% mutate(caste=case_when(
  Comm =="3" ~ "OBC",
  Comm =="4" ~ "General",
  Comm =="2" ~ "ST",
  Comm =="1" ~ "SC"
))


UPSC$Muslim=UPSC$Muslim %>% replace_na(0)

UPSC = UPSC %>% select(-X)
names(UPSC)

UPSC=UPSC %>% mutate(year=rep(2016,length(Name)))

upsc13$Muslim = upsc13$Muslim %>% replace_na(0)


upsc13=upsc13 %>% select(Name,Community,Mains,Interview,Final,Muslim) %>% 
  rename(Comm=Community,Total=Final,Written=Mains) %>% 
  mutate(Rank=row_number()) %>% mutate(caste=case_when(
    Comm =="3" ~ "OBC",
    Comm =="4" ~ "General",
    Comm =="2" ~ "ST",
    Comm =="1" ~ "SC"
  ),year=rep(2013,length(Name)))


######upsc14


upsc14=upsc14 %>% mutate(Muslim=replace_na(Muslim,0)) %>% select(-PH) %>% rename(Roll.No=Roll,
                                                                                 Comm=Community) %>% 
  mutate(Rank=row_number()) %>% mutate(caste=case_when(
    Comm =="3" ~ "OBC",
    Comm =="4" ~ "General",
    Comm =="2" ~ "ST",
    Comm =="1" ~ "SC"
  ),year=rep(2014,length(Name)))

upm=full_join(UPSC,upsc13)


upm=full_join(upm,upsc14)


######upsc15


##upsc15

rm(MARKS_OF_RECOMMENDED_CANDIDATES_2015)



patter ="[A-Z]+"

texta= str_replace_all(marks$`ROLL_NO NAME COMM PH_CAT WT PT FT`,pattern=patter,replacement = " ")

texta=str_replace_all(texta,pattern = "\\s+"," ")

write.table(texta,"mark2.txt",quote = FALSE)
write.t
marks$`ROLL_NO NAME COMM PH_CAT WT PT FT`

namer=as_data_frame(str_extract_all(marks$`ROLL_NO NAME COMM PH_CAT WT PT FT`,pattern=patter,simplify = TRUE))

namy= namer %>% unite(V1:V5,sep=" ") %>%
  names(namy)="Name"

namy$Name = str_trim(namy$Name)

str_pad(namy$Name)

str_trim(namy$Name)

mark2$Name=namy$Name

mark2=mark2 %>% mutate(Rank=row_number()) %>% mutate(caste=case_when(
  Comm =="3" ~ "OBC",
  Comm =="4" ~ "General",
  Comm =="2" ~ "ST",
  Comm =="1" ~ "SC"
),year=rep(2015,length(Name)))

write.csv(mark2,"upsc15.csv")

upsc15= read.csv('upsc15.csv',stringsAsFactors = FALSE)



###surname better

patty = "\\b\\w+$" # starts with word boundary ends with word. implies last word i.e surname

struck = c(" John Tom","Ram singh Sharma")

str_match(struck,patty)

upm$surname=str_match(upm$Name,patty)

upm$surname =as.character(upm$surname)

#####surname extraction bad


##surname 
kk= "Balu Lodha"

patt = "\\w*\\s{1}(.+)"


library(stringr)

as.data.frame(unlist(str_match_all(upm$Name,patt))[[2]])



str_match(upm$Name,patt)[,2]


upm$Name %>% map_df(~str_match_all(.,patt))


str_replace_all(kk,patt,"/2")

?b
