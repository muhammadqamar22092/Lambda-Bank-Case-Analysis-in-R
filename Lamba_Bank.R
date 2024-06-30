#Lambda Bank Case
data<-read.csv(file.choose(),header = TRUE)

str(data)
summary(data)
sum(is.na(data))
data$job <- as.factor(data$job)
data$marital<-as.factor(data$marital)
data$education<-as.factor(data$education)
data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan<- as.factor(data$loan)
data$contact<-as.factor(data$contact)
data$month<- as.factor(data$month)
data$day_of_week<-as.factor(data$day_of_week)
data$poutcome<-as.factor(data$poutcome)
data$y<-as.factor(data$y)
unique(data$marital)
str(data$marital)
library(dplyr)
data%>% group_by(marital)%>%filter(y=="yes",marital=="single"|marital=="married")%>%summarise(avg_duration=mean(duration),
                                                                                   avg_age=mean(age))
library(plyr)
data$education<-as.integer(data$education)
da
data$marital<-as.integer(data$marital)
data$default<-as.integer(data$default)
data$housing<-as.integer(data$housing)
data$loan <- as.integer(data$loan)
data$contact<- as.integer(data$contact)
data$month<-as.integer(data$month)
data$day_of_week<- as.integer(data$day_of_week)
data$poutcome<-as.integer(data$poutcome)

#Q2
data %>%filter(poutcome ==3,y=="no") %>%summarise(avg_age=mean(age),
                                      avg_edu=mean(education),
                                      avg_marital=mean(marital),
                                      avg_default=mean(default),
                                      avg_housing=mean(housing),
                                      avg_loan=mean(loan),
                                      avg_month=mean(month),
                                      avg_weekday=mean(day_of_week),
                                      avg_previous=mean(previous),
                                      avg_campaig=mean(campaign),
                                      avg_duration=mean(duration),
                                      avg_pd=mean(pdays),
                                      avg_cons_id=mean(cons.conf.idx))
str(data)
data %>% filter(y=="2",housing=="3",loan=="3") %>% dplyr::summarise(total=n(),
                                                                avg_age=mean(age))

#Q
data %>% filter(previous==0,default==3) %>% summarise(avg_cont=mean(campaign))
#Q

data$job<-as.integer(data$job)
data %>%filter(y=="yes") %>% group_by(education) %>% summarise(count_ed=n(),avg_job=mean(job)) %>% arrange(desc(job))


data %>% filter(housing==2,default==2,y=="yes") %>% group_by(y) %>% summarise(total=n(),avg_emp_variation=mean(emp.var.rate))
data %>% filter(y=="yes") %>% group_by(contact) %>% summarise(avg_du=mean(duration))

data %>% 
data %>%filter(housing == "1", y == "yes", loan == "1") %>%group_by(age) %>%
  summarise(Avg_Age =  mean(age),
            count = n())
data %>% 
  group_by(day_of_week) %>% 
  summarise(contact_density = sum(campaign)/unique()) %>% 
  arrange(desc(contact_density))
ali

str(data)
#. What is the overall subscription rate for clients who were contacted in the month of May
#and had a successful outcome in the previous marketing campaign? (4 Marks)
data %>% filter(month==7,poutcome==3) %>% dplyr::summarise(subscription_rate=sum(y==2)/n()*100)
library(dplyr)
#or
subscription_rate <- data %>%
  filter(month == 7, poutcome == 3) %>%
  summarise(subscription_rate = mean(y == 2)) # Calculate subscription rate as a proportion

print(subscription_rate)

#q5
data %>% group_by(y)%>% filter(y=="yes"|y=="no") %>% summarise(avg_inde=mean(cons.conf.idx))
data %>%select(y,cons.conf.idx) %>% filter(y==2) %>% summarise(avg=mean(cons.conf.idx))

library(dplyr)

confidence_comparison <- data %>%  group_by(y) %>%
  filter(y == "no", y == "yes") %>%
  summarise(avg_index = mean(cons.conf.idx))

print(confidence_comparison)
str(data$y)
