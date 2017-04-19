library(tidyverse)

# load data
# install.packages("SAScii") 
library(SAScii)
sas_instruction<-"http://www.nber.org/data/progs/cps/cpsjan2016.sas"
data_CPS<-"ProblemSets/PS1/data.dat"
dt<- read.SAScii(data_CPS, sas_instruction)
save(dt, file = "ProblemSets/PS1/dt.dat" )


head(df, n=10)
dim(df)
df<- dt %>%
  select (PESEX, PEEDUCA, PTDTRACE, PRMARSTA, PRTAGE, GEREG, HEFAMINC, PEHRUSL1, HRNUMHOU) %>%
  mutate (sex = ifelse (PESEX == 1, "male", "female")) %>%
  filter (PEEDUCA !=-1) %>%
  mutate (educ = ifelse (PEEDUCA %in% c(31:34), 1,
                         ifelse (PEEDUCA %in% c(35:38), 2,
                                 ifelse(PEEDUCA %in% c(39:40), 3,
                                        ifelse (PEEDUCA %in% c(41:42), 4,
                                                ifelse (PEEDUCA ==43, 5,
                                                        ifelse (PEEDUCA %in% c(44:45), 6, 7))))))) %>%
  filter (PTDTRACE != -1) %>%
  mutate (race = ifelse (PTDTRACE ==1, "white", 
                         ifelse (PTDTRACE ==2, "Black", 
                                 ifelse (PTDTRACE ==3, "Native American",
                                         ifelse (PTDTRACE ==4, "Asian", "Others")))) ) %>%
  filter (PRMARSTA != -1) %>%
  mutate (marital = ifelse (PRMARSTA %in% c(1:3), "married", "single")) %>%
  filter (GEREG != -1) %>%
  mutate (region = ifelse (GEREG == 1, "Northeast", 
                           ifelse (GEREG == 2, "Midewest",
                                   ifelse (GEREG ==3, "South", "West")))) %>%
  filter (PRTAGE !=-1) %>%
  mutate (age = PRTAGE) %>%
  mutate (HouseholdSize = HRNUMHOU) %>%
  mutate (HouseholdIncome = as.numeric(as.character(factor (HEFAMINC, levels = c(1:16), labels = c(0, 5000, 7500, 10000, 12500, 15000, 20000,25000, 30000, 35000,40000,50000,60000,75000,100000,150000))))) %>%
  filter (PEHRUSL1 !=-1) %>%
  mutate (WorkHour = PEHRUSL1) %>%
  mutate (income = HouseholdIncome/HouseholdSize) %>%
  select (sex, race, age, educ, marital, region, income, WorkHour) 
  
sum<-df %>%
  summarize(sample_number=n(), 
            age_mean=mean(age),age_sd=sd(age),
            educ_mean=mean(educ),educ_sd=sd(educ), 
            Male=100*(sum(sex=="male")/sample_number), 
            Female=100*(sum(sex=="female")/sample_number), 
            Percent_Married=100*(sum(marital=="married")/sample_number),
            income_mean=mean(income), income_sd=sd(income),
            workhour_mean = mean (WorkHour), workhour_sd=sd(WorkHour),
            Southern = 100*(sum(region=="South")/sample_number),
            Midwest = 100*(sum(region =="Midewest")/sample_number),
            Northeast = 100*(sum(region =="Northeast")/sample_number))
  

  
  
                        
                         
                        
                        
                   
