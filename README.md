# R4DS-Exercise_230704



## data prep code
getwd () 

setwd("C:/Users/wlghd/Desktop")

library(tidyverse) 

library(ggplot2)

pkpd <- read.csv("pkpd_dataset.csv")



## No.1 (cycle 1의 PK (CMT == 2의 LIDV)를 용량별로 시각화)
pkpd_cycle1 <- pkpd |> 
  filter(CYCLE == 1 & CMT == 2) |> 
  select(ID, TIME, NOMTIME, LIDV, CMT, DOSE, CYCLE) 

pkpd_cycle1 |> 
  ggplot(aes(x = TIME, y = LIDV, color = ID)) +
  geom_point()+
  geom_line()+
  labs(
    title = "pk_cycle1_by_dose",
    x =  "time (hr)", y = "Conc (ng/mL)"
  )+
  facet_wrap(~ DOSE)

![PK_cycle_by_dose](https://github.com/Jihong757/R4DS-Exercise_230704/assets/116873046/53b47704-26cc-45ed-9d4a-543d49698dd4)



## No.2 (cycle 1의 NOMTIME별 mean, SD ~)
pk_mean_by_dose <- pkpd_cycle1 |> 
  group_by(DOSE,NOMTIME) |> 
  summarise(mean = mean(LIDV), sd = sd(LIDV), .groups = "drop") 

pk_mean_by_dose |> 
  ggplot(aes(x = NOMTIME, y = mean))+
  geom_point()+
  geom_line()+
  facet_wrap(.~ DOSE)+ 
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd))+
  labs(
    title = "pk_mean_by_dose",
    x =  "NOMTIME (hr)", y = "Conc_mean (ng/mL)"
  )

![pk_mean_by_dose](https://github.com/Jihong757/R4DS-Exercise_230704/assets/116873046/84a17b93-8e40-4b09-94a7-5a479ed47370)




## No.3 (CMAX 기술통계)

library(NonCompart)

CMAX_summary <- tblNCA(pkpd_cycle1, key= c ("ID", "DOSE"), "TIME", "LIDV") |> 
  select(ID, DOSE, CMAX) |> 
  group_by(DOSE) |> 
  summarize(mean = mean(CMAX),
            median = median(CMAX),
            sd = sd(CMAX), 
            min = min(CMAX),
            max = max(CMAX)
  )  
  
![CMAX_SUMMARY](https://github.com/Jihong757/R4DS-Exercise_230704/assets/116873046/5e008ea5-5367-4c52-8d8c-35482018911f)


