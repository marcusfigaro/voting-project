## Data Vizualization
# Classification for data analysis?
# Hypothesis testing?
library(tidyverse)

# 2012 total vote data bar graph
VoteData_2012<-data.frame(VoteData$County_Names,VoteData$REPVotes_2012,
                          VoteData$DEMVotes_2012,VoteData$OtherVotes_2012)
master_votedata_2012<-VoteData_2012 %>%
  gather("Party","Votes",-VoteData.County_Names)
ElectionResults_2012<-ggplot(data=master_votedata_2012,aes(VoteData.County_Names,Votes))+
  geom_col(aes(fill=as.factor(Party)),position="dodge")+coord_flip()+
  scale_fill_manual(name="Candidate",values=c("blue3","yellow3","red3"),
                    labels=c("Barack Obama and Joe Biden","Other",
                             "Mitt Romney and Paul Ryan"))+
  labs(x="County",y="Total Votes",
       title="Votes by County: 2012 Presidential Election")

# 2016 total vote data bar graph

VoteData_2016<-data.frame(VoteData$County_Names,VoteData$REPVotes_2016,
                          VoteData$DEMVotes_2016,VoteData$OtherVotes_2016)
master_votedata_2016<-VoteData_2016 %>%
  gather("Party","Votes",-VoteData.County_Names)


ElectionResults_2016<-ggplot(data=master_votedata_2016,aes(x=VoteData.County_Names,y=Votes))+
  geom_col(aes(fill=as.factor(Party)),position="dodge")+coord_flip() +
  scale_fill_manual(name="Candidate",values=c("red3","yellow3","blue3"),
                    labels=c("Hillary Clinton and Tim Kaine",
                             "Other",
                             "Donald Trump and Mike Pence"))+
  labs(x="County",y="Total Votes",
       title="Votes by County: 2016 Presidential Election")


## Demographics data (Scatter plots)

# White
Pop_data_white <- Pop_data %>% filter(Population == 
                                        "White, not Hispanic or Latino %")%>%
  summarise(County,Population,"Percentage"=Percentage*100)
avg_white <- Pop_data_white[27,3]
Pop_data_white_county <- Pop_data_white[1:25,]

demog_white_plot<-ggplot(data=Pop_data_white_county,aes(x=County,y=Percentage))+
  geom_point()+geom_hline(yintercept=avg_white,color="red")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="Percentage of White People in Each County",
       y="Percentage of Population (%)")

# Black
Pop_data_black <- Pop_data %>% filter(Population == 
                                        "Black %")%>%
  summarise(County,Population,"Percentage"=Percentage*100)
avg_black <- Pop_data_black[27,3]
Pop_data_black_county <- Pop_data_black[1:25,]

demog_black_plot<-ggplot(data=Pop_data_black_county,aes(x=County,y=Percentage))+
  geom_point()+geom_hline(yintercept=avg_black,color="red")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Percentage of Population (%)",
       title="Percentage of African American People in Each County")

# Native American
Pop_data_na <- Pop_data %>% filter(Population == 
                                        "Native American %")%>%
  summarise(County,Population,"Percentage"=Percentage*100)
avg_na <- Pop_data_na[27,3]
Pop_data_na_county <- Pop_data_na[1:25,]

demog_na_plot<-ggplot(data=Pop_data_na_county,aes(x=County,y=Percentage))+
  geom_point()+geom_hline(yintercept=avg_na,color="red")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Percentage of Population (%)",
       title="Percentage of Native American People in Each County")

# Asian  
Pop_data_asian <- Pop_data %>% filter(Population == 
                                     "Asian %")%>%
  summarise(County,Population,"Percentage"=Percentage*100)
avg_asian <- Pop_data_asian[27,3]
Pop_data_asian_county <- Pop_data_asian[1:25,]

demog_asian_plot<-ggplot(data=Pop_data_asian_county,aes(x=County,y=Percentage))+
  geom_point()+geom_hline(yintercept=avg_asian,color="red")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Percentage of Population (%)",
       title="Percentage of Asian People in Each County")

# Pacific Islander
Pop_data_pi <- Pop_data %>% filter(Population == 
                                        "Pacific Islander alone %")%>%
  summarise(County,Population,"Percentage"=Percentage*100)
avg_pi <- Pop_data_pi[27,3]
Pop_data_pi_county <- Pop_data_pi[1:25,]

demog_pi_plot<-ggplot(data=Pop_data_pi_county,aes(x=County,y=Percentage))+
  geom_point()+geom_hline(yintercept=avg_pi,color="red")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Percentage of Population (%)",
       title="Percentage of Hawaiian/Pacific Islander People in Each County")

# Latino
Pop_data_latino <- Pop_data %>% filter(Population == 
                                        "Latino %")%>%
  summarise(County,Population,"Percentage"=Percentage*100)
avg_latino <- Pop_data_latino[27,3]
Pop_data_latino_county <- Pop_data_latino[1:25,]
demog_latina_plot<-ggplot(data=Pop_data_latino_county,aes(x=County,y=Percentage))+
  geom_point()+geom_hline(yintercept=avg_latino,color="red")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Percentage of Population (%)",
       title="Percentage of Latino People in Each County")

# Senior Citizens
Pop_data_sc <- Pop_data %>% filter(Population == 
                                        "Senior Citizens %")%>%
  summarise(County,Population,"Percentage"=Percentage*100)
avg_sc <- Pop_data_sc[27,3]
Pop_data_sc_county <- Pop_data_sc[1:25,]

demog_sc_plot<-ggplot(data=Pop_data_sc_county,aes(x=County,y=Percentage))+
  geom_point()+geom_hline(yintercept=avg_sc,color="red")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Percentage of Population (%)",
       title="Percentage of Senior Citizens in Each County")

## College grads
Pop_data_grad <- Pop_data %>% filter(Population == 
                                        "College Grad %")%>%
  summarise(County,Population,"Percentage"=Percentage*100)
avg_grad <- Pop_data_grad[27,3]
Pop_data_grad_county <- Pop_data_grad[1:25,]

demog_grad_plot<-ggplot(data=Pop_data_grad_county,aes(x=County,y=Percentage))+
  geom_point()+geom_hline(yintercept=avg_grad,color="red")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="Percentage of Population (%)",
       title="Percentage of College Grads in Each County")





