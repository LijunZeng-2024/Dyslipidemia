rm(list = ls())
pacman::p_load(openxlsx, rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

dat_JQJX <- read.xlsx('../raw_data/CMEC/jqjx.xlsx')
dat_JQCFnew <- read.xlsx('../raw_data/CMEC/jqcf.xlsx')
data_reptransCF <- read.xlsx('../raw_data/CMEC/reptransCF.xlsx')

MetS_JX <- dat_JQJX
tiliJX <- dat_JQJX[,c(2,396:420)]
colnames(tiliJX)
tiliJX[,2:26] <- lapply(tiliJX[,2:26], as.numeric)
str(tiliJX)

tiliJX <- tiliJX %>%
  mutate(F1_1=case_when( 
    F1 == 2 | F1== 3  ~ 1,
    TRUE ~ 0))  
table(tiliJX$F1_1)

tiliJX$moderateShangban_fei <- 
  tiliJX$F1_1*tiliJX$F2*60

#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tiliJX <- tiliJX %>%
  mutate(F3_1=case_when( 
    F3 == 1| F3== 2 ~ 1,
    TRUE ~ 0))  
table(tiliJX$F3_1)

tiliJX$moderatelushang_feinong <- 
  (tiliJX$F3_1*tiliJX$F4)*5

#Manual work in the farming season——Vigorous
#Semi-mechanized work in the farming season——moderate
#Fully-mechanized work in the farming season——low
tiliJX <- tiliJX %>%
  mutate(F6b_1=case_when( 
    F6b == 2 ~ 1,
    TRUE ~ 0))  

tiliJX$moderate_nongmang <- 
  tiliJX$F6b_1*tiliJX$F6c*tiliJX$F6a*60*30/7*(tiliJX$F6a/12)#先算出每周的再进行加权

#Manual work in the non-farming season   moderate
tiliJX <- tiliJX %>%
  mutate(F6b_2=case_when( 
    F6b == 2 ~ 1,
    TRUE ~ 0))  

tiliJX$moderate_feinongmang <- 
  (tiliJX$F6b_2*tiliJX$F7)*60*(12-tiliJX$F6a)/12

#Sedentary work   Low
#Standing work   Moderate
#Manual work  Moderate
#Heavy manual work  Vigorous 
tiliJX <- tiliJX %>%
  mutate(F9_1=case_when( 
    F9 == 2 | F9== 3 ~ 1,
    TRUE ~ 0))  
table(tiliJX$F9_1)

tiliJX$moderate_qitagongzuo <- 
  (tiliJX$F9_1*tiliJX$F10)*60

#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tiliJX <- tiliJX %>%
  mutate(F11_1=case_when( 
    F11 == 1| F11== 2 ~ 1,
    TRUE ~ 0))  

tiliJX$moderatelushang_non <- 
  (tiliJX$F11_1*tiliJX$F12)*5

#Tai-Chi/qigong/leisure walking   Moderate
#brisk walking     Moderate
#jogging/aerobics     Vigorous
#Swimming         Vigorous
#Ball game        Moderate
#Exercise with fitness equipment  Moderate
#Other exercise, e.g. mountain walking, home exercise and rope jumping   Moderate

tiliJX <- tiliJX %>%
  mutate(F14_1=case_when( 
    F14 == 1 |F14 == 2 |F14 == 5|F14 == 6| F14 == 7 ~ 1,
    TRUE ~ 0))  
table(tiliJX$F14_1)

tiliJX$moderateduanlian <- 
  (tiliJX$F14_1*tiliJX$F15)*60

tiliJX[is.na(tiliJX)]=0

tiliJX$ModeratePA_JX <- 
  tiliJX$moderateShangban_fei+tiliJX$moderatelushang_feinong+
  tiliJX$moderate_nongmang+tiliJX$moderate_feinongmang+
  tiliJX$moderate_qitagongzuo+tiliJX$moderatelushang_non+
  tiliJX$moderateduanlian
summary(tiliJX$ModeratePA_JX)

tiliJX <- tiliJX %>%
  mutate(F1_zhong1=case_when( 
    F1 == 4  ~ 1,
    TRUE ~ 0))  
table(tiliJX$F1_zhong1)

tiliJX$vigorousShangban_fei <- 
  tiliJX$F1_zhong1*tiliJX$F2*60

#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tiliJX <- tiliJX %>%
  mutate(F3_zhong1=case_when( 
    F3 == 3 ~ 1,
    TRUE ~ 0))  
table(tiliJX$F3_zhong1)

tiliJX$vigorouslushang_feinong <- 
  (tiliJX$F3_zhong1*tiliJX$F4)*5

#Manual work in the farming season   Vigorous
#Semi-mechanized work in the farming season moderate
#Fully-mechanized work in the farming season low
tiliJX <- tiliJX %>%
  mutate(F6b_zhong1=case_when( 
    F6b == 1 ~ 1,
    TRUE ~ 0))  

table(tiliJX$F6b_zhong1)

tiliJX$vigorous_nongmang <- 
  tiliJX$F6b_zhong1*tiliJX$F6c*tiliJX$F6a*30/7*60*(tiliJX$F6a/12)#先算出每周的再进行加权

#Manual work in the non-farming season   moderate
tiliJX <- tiliJX %>%
  mutate(F6b_zhong2=case_when( 
    F6b == 1 ~ 1,
    TRUE ~ 0))  

table(tiliJX$F6b_zhong2)

tiliJX$vigorous_feinongmang <- 
  (tiliJX$F6b_zhong2*tiliJX$F7)*60*(12-tiliJX$F6a)/12

#Standing work   Moderate
#Sedentary work   Low
#Manual work  Moderate
#Heavy manual work    Vigorous
tiliJX <- tiliJX %>%
  mutate(F9_zhong1=case_when( 
    F9 == 4 ~ 1,
    TRUE ~ 0))  
table(tiliJX$F9_zhong1)

tiliJX$vigorous_qitagongzuo <- 
  (tiliJX$F9_zhong1*tiliJX$F10)*60

#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tiliJX <- tiliJX %>%
  mutate(F11_zhong1=case_when( 
    F11 == 3 ~ 1,
    TRUE ~ 0))  

tiliJX$vigorouslushang_non <- 
  (tiliJX$F11_zhong1*tiliJX$F12)*5

#Tai-Chi/qigong/leisure walking   Moderate
#jogging/aerobics     Vigorous
#brisk walking     Moderate
#Swimming         Vigorous
#Ball game        Moderate
#Exercise with fitness equipment  Moderate
#Other exercise, e.g. mountain walking, home exercise and rope jumping   Moderate

tiliJX <- tiliJX %>%
  mutate(F14_zhong1=case_when( 
    F14 == 3 |F14 == 4 ~ 1,
    TRUE ~ 0))  

tiliJX$vigorousduanlian <- 
  (tiliJX$F14_zhong1*tiliJX$F15)*60

tiliJX[is.na(tiliJX)]=0
tiliJX$VigorousPA_JX <-  tiliJX$vigorousShangban_fei + tiliJX$vigorouslushang_feinong+
  tiliJX$vigorous_nongmang + tiliJX$vigorous_feinongmang +
  tiliJX$vigorous_qitagongzuo + tiliJX$vigorouslushang_non + 
  tiliJX$vigorousduanlian

table(tiliJX$F19,useNA='ifany')
tiliJX$sedentary_leisure_time_JX <- tiliJX$F19/7
summary(tiliJX$sedentary_leisure_time_JX)

tiliJX1 <- tiliJX[,c(1,41,56,57)]
tiliJX1=tiliJX1%>%mutate(
  ModeratePA_goal_JX=case_when(
    ModeratePA_JX>=150 ~ 'Yes',
    TRUE ~ 'No'
  )
)
tiliJX1=tiliJX1%>%mutate(
  VigorousPA_goal_JX=case_when(
    VigorousPA_JX>=75 ~ 'Yes',
    TRUE ~ 'No'
  )
)
tiliJX1$Moderate_vigorous_JX <- tiliJX1$ModeratePA_JX/2+tiliJX1$VigorousPA_JX
tiliJX1=tiliJX1%>%mutate(
  Moderate_vigorous_goal_JX=case_when(
    Moderate_vigorous_JX>=75 ~ 'Yes',
    TRUE ~ 'No'
  )
)

colnames(MetS_JX)[1:100]
MetS_JX <- MetS_JX[,c(1,2)]
MetS_JX <- merge(MetS_JX,tiliJX1,by='粪便条码')

data <- merge(dat_JQCFnew,data_reptransCF,by='ID')

table(data$TG,useNA='ifany')
data <- data[complete.cases(data[,'TG']),]
table(data$HDL_CH,useNA='ifany')
table(data$CHOL,useNA='ifany')
table(data$LDL_CH,useNA='ifany')

data=data%>%mutate(
  TG_UP=case_when(
    TG>=2.3~1,
    TRUE~0
  ),
  HDL_Low=case_when(
    HDL_CH<1.0~1,
    TRUE~0
  ),
  TC_UP=case_when(
    CHOL>=6.2~1,
    TRUE~0
  ),
  LDL_UP=case_when(
    LDL_CH>=4.1~1,
    TRUE~0
  )
)
data$Dyslipidemia <- ifelse(data$TG>=2.3|data$HDL_CH<1|data$CHOL>=6.2|data$LDL_CH>=4.1,1,0)
data <- filter(data,data$冠心病==0)

data$Sex[data$A1=='男'] <- 'Male'
data$Sex[data$A1=='女'] <- 'Female'
table(data$A1,useNA='ifany')

data$Ethnicity[data$A2=='01-汉族'] <- 'Han'
data$Ethnicity[data$A2=='07-藏族'] <- 'Tibetan'

data$Education[data$A6=='01-未正规上过学'] <- 'Not attending school'
data$Education[data$A6=='02-小学' | data$A6=='03-初中' | data$A6=='04-高中(包括中专/技校)'] <- 'High School and below'
data$Education[data$A6=='05-大专' | data$A6=='06-大学(包括研究生)'] <- 'Tertiary and above'

data$Marriage[data$A5=='01-已婚/同居'] <- 'Married'
data$Marriage[data$A5=='02-分局/离异' | data$A5=='03-丧偶' | data$A5=='04-从未结婚'] <- 'Unmarried/Divorce'

data[,c('B1','C2')] <- lapply(data[,c('B1','C2')], as.numeric)
data = data %>% mutate(
  Smoke=case_when(
    B1==1 ~ 'Never-somkers',
    B1==2 | B1==3 ~ 'Current/previous smokers'
  ),
  Drink=case_when(
    C2==1 | C2==2 ~ 'Never-drinkers/rarely drinkers',
    C2==3 | C2==4 | C2==5 ~ 'Current drinkers'
  )
)

colnames(data)
tili <- data[,c(1,115:140)]
tili[,2:27] <- lapply(tili[,2:27], as.numeric)
str(tili)

tili <- tili %>%
  mutate(F1_1=case_when( 
    F1 == 2 | F1== 3  ~ 1,
    TRUE ~ 0))  
table(tili$F1_1)

tili$moderateShangban_fei <- 
  tili$F1_1*tili$F2*60

#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tili <- tili %>%
  mutate(F3_1=case_when( 
    F3 == 1| F3== 2 ~ 1,
    TRUE ~ 0))  
tili$moderatelushang_feinong <- 
  (tili$F3_1*tili$F4)*5

#Manual work in the farming season   Vigorous
#Semi-mechanized work in the farming season moderate
#Fully-mechanized work in the farming season low
tili <- tili %>%
  mutate(F6b_1=case_when( 
    F6b == 2 ~ 1,
    TRUE ~ 0))  

tili$moderate_nongmang <- 
  tili$F6b_1*tili$F6c*tili$F6a*60*30/7*(tili$F6a/12)

#Manual work in the non-farming season   moderate
tili <- tili %>%
  mutate(F6b_2=case_when( 
    F6b == 2 ~ 1,
    TRUE ~ 0))  

tili$moderate_feinongmang <- 
  (tili$F6b_2*tili$F7)*60*(12-tili$F6a)/12

#Sedentary work   Low
#Standing work   Moderate
#Manual work  Moderate
#Heavy manual work  Vigorous 
tili <- tili %>%
  mutate(F9_1=case_when( 
    F9 == 2 | F9== 3 ~ 1,
    TRUE ~ 0))  

tili$moderate_qitagongzuo <- 
  (tili$F9_1*tili$F10)*60

#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tili <- tili %>%
  mutate(F11_1=case_when( 
    F11 == 1| F11== 2 ~ 1,
    TRUE ~ 0))  

tili$moderatelushang_non <- 
  (tili$F11_1*tili$F12)*5

tili <- tili %>%
  mutate(F14_1=case_when( 
    F14 == 1 |F14 == 2 |F14 == 5|F14 == 6| F14 == 7 ~ 1,
    TRUE ~ 0))  

tili$moderateduanlian <- 
  (tili$F14_1*tili$F15)*60

tili[is.na(tili)]=0

tili$ModeratePA <- 
  tili$moderateShangban_fei+tili$moderatelushang_feinong+
  tili$moderate_nongmang+tili$moderate_feinongmang+
  tili$moderate_qitagongzuo+tili$moderatelushang_non+
  tili$moderateduanlian
summary(tili$ModeratePA)

tili <- tili %>%
  mutate(F1_zhong1=case_when( 
    F1 == 4  ~ 1,
    TRUE ~ 0))  

tili$vigorousShangban_fei <- 
  tili$F1_zhong1*tili$F2*60


#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tili <- tili %>%
  mutate(F3_zhong1=case_when( 
    F3 == 3 ~ 1,
    TRUE ~ 0))  

tili$vigorouslushang_feinong <- 
  (tili$F3_zhong1*tili$F4)*5

#Manual work in the farming season   Vigorous
#Semi-mechanized work in the farming season moderate
#Fully-mechanized work in the farming season low
tili <- tili %>%
  mutate(F6b_zhong1=case_when( 
    F6b == 1 ~ 1,
    TRUE ~ 0))  
tili$vigorous_nongmang <- 
  tili$F6b_zhong1*tili$F6c*tili$F6a*30/7*60*(tili$F6a/12)#先算出每周的再进行加权

#Manual work in the non-farming season   moderate
tili <- tili %>%
  mutate(F6b_zhong2=case_when( 
    F6b == 1 ~ 1,
    TRUE ~ 0))  

tili$vigorous_feinongmang <- 
  (tili$F6b_zhong2*tili$F7)*60*(12-tili$F6a)/12

#其他工作 
#Standing work   Moderate
#Sedentary work   Low
#Manual work  Moderate
#Heavy manual work    Vigorous
tili <- tili %>%
  mutate(F9_zhong1=case_when( 
    F9 == 4 ~ 1,
    TRUE ~ 0))  

tili$vigorous_qitagongzuo <- 
  (tili$F9_zhong1*tili$F10)*60


#农业路上
#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tili <- tili %>%
  mutate(F11_zhong1=case_when( 
    F11 == 3 ~ 1,
    TRUE ~ 0))  

tili$vigorouslushang_non <- 
  (tili$F11_zhong1*tili$F12)*5


#体力活动共用
#tili <- tili %>%
#  mutate(F13_1=case_when( 
#    F13 == 3 ~ '2',
#    F13 == 4 ~ '4',
#    F13 == 7 ~ '7',
#    TRUE ~ '0'))  

#体力活动类型
#Tai-Chi/qigong/leisure walking   Moderate
#jogging/aerobics     Vigorous
#快走     Moderate
#Swimming         Vigorous
#Ball game        Moderate
#Exercise with fitness equipment  Moderate
#Other exercise, e.g. mountain walking, home exercise and rope jumping   Moderate

tili <- tili %>%
  mutate(F14_zhong1=case_when( 
    F14 == 3 |F14 == 4 ~ 1,
    TRUE ~ 0))  

tili$vigorousduanlian <- 
  (tili$F14_zhong1*tili$F15)*60

#重度体力活动时间（总和）
tili[is.na(tili)]=0
tili$VigorousPA <-  tili$vigorousShangban_fei + tili$vigorouslushang_feinong+
  tili$vigorous_nongmang + tili$vigorous_feinongmang +
  tili$vigorous_qitagongzuo + tili$vigorouslushang_non + 
  tili$vigorousduanlian
summary(tili$VigorousPA)
write_xlsx(tili, 'tl.xlsx')

tili1 <- tili[,c(1,42,57)]
data <- merge(data,tili1,by='ID')
summary(data$VigorousPA)

data=data%>%mutate(
  ModeratePA_goal=case_when(
    ModeratePA>=150 ~ 'Yes',
    TRUE ~ 'No'
  )
)
data=data%>%mutate(
  VigorousPA_goal=case_when(
    VigorousPA>=75 ~ 'Yes',
    TRUE ~ 'No'
  )
)
data$Moderate_vigorous <- data$ModeratePA/2+data$VigorousPA
data=data%>%mutate(
  Moderate_vigorous_goal=case_when(
    Moderate_vigorous>=75 ~ 'Yes',
    TRUE ~ 'No'
  )
)
#

#膳食
SS <- dat_JQCFnew[,c(640,194:199,201:205,222:226,236:240,243:247)]
SS[,2:27] <- lapply(SS[,2:27],as.numeric)

SS <- filter(SS,!is.na(SS$H8))
SS$H8_B1[SS$H8_B1>=1] <- 7
SS <- SS %>%
  mutate(H8_B2_1=case_when(
    H8_B2<1 ~0.5,
    H8_B2>=1 & H8_B2<3.5 ~2.5,
    H8_B2>=3.5 & H8_B2<6.5 ~5,
    H8_B2>=6.5 ~7
  ))
table(SS$H8_B2_1,useNA='ifany')

SS$H8_B3 <- SS$H8_B3/4.29
SS <- SS %>%
  mutate(H8_B3_1=case_when(
    H8_B3<1 ~0.5,
    H8_B3>=1 & H8_B3<3.5 ~2.5,
    H8_B3>=3.5 & H8_B3<6.5 ~5,
    H8_B3>=6.5 ~7
  ))

SS$H8_B4 <- SS$H8_B4/52.14
SS$H8_B4[SS$H8_B4<1] <- 0.5

for(i in 1:nrow(SS)){
  if(SS$H8[i]==0){
    SS$红肉[i] <- 0
  }else{
    if(!is.na(SS$H8_B1[i])){
      SS$红肉[i] <- SS$H8_B1[i]
    }else{
      if(!is.na(SS$H8_B2_1[i])){
        SS$红肉[i] <- SS$H8_B2_1[i]
      }else{
        if(!is.na(SS$H8_B3[i])){
          SS$红肉[i] <- SS$H8_B3_1[i]
        }else{
          if(!is.na(SS$H8_B4[i])){
            SS$红肉[i] <- SS$H8_B4[i]
          }
        }
      }
    }
  }
}

SS <- filter(SS,!is.na(SS$H9))
SS$H9_B1[SS$H9_B1>=1] <- 7
SS$H9_B1[SS$H9_B1<1] <- 2.5

SS <- SS %>%
  mutate(H9_B2_1=case_when(
    H9_B2<1 ~0.5,
    H9_B2>=1 & H9_B2<3.5 ~2.5,
    H9_B2>=3.5 & H9_B2<6.5 ~5,
    H9_B2>=6.5 ~7
  ))

SS$H9_B3 <- SS$H9_B3/4.29
table(SS$H9_B3,useNA='ifany')
SS <- SS %>%
  mutate(H9_B3_1=case_when(
    H9_B3<1 ~0.5,
    H9_B3>=1 & H9_B3<3.5 ~2.5,
    H9_B3>=3.5 & H9_B3<6.5 ~5,
    H9_B3>=6.5 ~7
  ))

SS$H9_B4 <- SS$H9_B4/52.14
SS$H9_B4[SS$H9_B4<1] <- 0.5

for(i in 1:nrow(SS)){
  if(SS$H9[i]==0){
    SS$家禽[i] <- 0
  }else{
    if(!is.na(SS$H9_B1[i])){
      SS$家禽[i] <- SS$H9_B1[i]
    }else{
      if(!is.na(SS$H9_B2_1[i])){
        SS$家禽[i] <- SS$H9_B2_1[i]
      }else{
        if(!is.na(SS$H9_B3[i])){
          SS$家禽[i] <- SS$H9_B3_1[i]
        }else{
          if(!is.na(SS$H9_B4[i])){
            SS$家禽[i] <- SS$H9_B4[i]
          }
        }
      }
    }
  }
}

SS <- filter(SS,!is.na(SS$H12))
SS$H12_B1[SS$H12_B1>=1] <- 7
SS <- SS %>%
  mutate(H12_B2_1=case_when(
    H12_B2<1 ~0.5,
    H12_B2>=1 & H12_B2<3.5 ~2.5,
    H12_B2>=3.5 & H12_B2<6.5 ~5,
    H12_B2>=6.5 ~7
  ))

SS$H12_B3 <- SS$H12_B3/4.29
SS <- SS %>%
  mutate(H12_B3_1=case_when(
    H12_B3<1 ~0.5,
    H12_B3>=1 & H12_B3<3.5 ~2.5,
    H12_B3>=3.5 & H12_B3<6.5 ~5,
    H12_B3>=6.5 ~7
  ))

SS$H12_B4 <- SS$H12_B4/52.14
SS$H12_B4[SS$H12_B4<1] <- 0.5

for(i in 1:nrow(SS)){
  if(SS$H12[i]==0){
    SS$蔬菜[i] <- 0
  }else{
    if(!is.na(SS$H12_B1[i])){
      SS$蔬菜[i] <- SS$H12_B1[i]
    }else{
      if(!is.na(SS$H12_B2_1[i])){
        SS$蔬菜[i] <- SS$H12_B2_1[i]
      }else{
        if(!is.na(SS$H12_B3[i])){
          SS$蔬菜[i] <- SS$H12_B3_1[i]
        }else{
          if(!is.na(SS$H12_B4[i])){
            SS$蔬菜[i] <- SS$H12_B4[i]
          }
        }
      }
    }
  }
}

SS <- filter(SS,!is.na(SS$H14))
SS$H14_B1[SS$H14_B1>=1] <- 7
SS$H14_B1[SS$H14_B1<1] <- 2.5
SS <- SS %>%
  mutate(H14_B2_1=case_when(
    H14_B2<1 ~0.5,
    H14_B2>=1 & H14_B2<3.5 ~2.5,
    H14_B2>=3.5 & H14_B2<6.5 ~5,
    H14_B2>=6.5 ~7
  ))

SS$H14_B3 <- SS$H14_B3/4.29
SS <- SS %>%
  mutate(H14_B3_1=case_when(
    H14_B3<1 ~0.5,
    H14_B3>=1 & H14_B3<3.5 ~2.5,
    H14_B3>=3.5 & H14_B3<6.5 ~5,
    H14_B3>=6.5 ~7
  ))

SS$H14_B4 <- SS$H14_B4/52.14
SS$H14_B4[SS$H14_B4<1] <- 0.5

for(i in 1:nrow(SS)){
  if(SS$H14[i]==0){
    SS$腌菜[i] <- 0
  }else{
    if(!is.na(SS$H14_B1[i])){
      SS$腌菜[i] <- SS$H14_B1[i]
    }else{
      if(!is.na(SS$H14_B2_1[i])){
        SS$腌菜[i] <- SS$H14_B2_1[i]
      }else{
        if(!is.na(SS$H14_B3[i])){
          SS$腌菜[i] <- SS$H14_B3_1[i]
        }else{
          if(!is.na(SS$H14_B4[i])){
            SS$腌菜[i] <- SS$H14_B4[i]
          }
        }
      }
    }
  }
}

SS <- filter(SS,!is.na(SS$H15))
SS$H15_B1[SS$H15_B1>=1] <- 7
SS$H15_B1[SS$H15_B1<1] <- 2.5

SS <- SS %>%
  mutate(H15_B2_1=case_when(
    H15_B2<1 ~0.5,
    H15_B2>=1 & H15_B2<3.5 ~2.5,
    H15_B2>=3.5 & H15_B2<6.5 ~5,
    H15_B2>=6.5 ~7
  ))

SS$H15_B3 <- SS$H15_B3/4.29
SS <- SS %>%
  mutate(H15_B3_1=case_when(
    H15_B3<1 ~0.5,
    H15_B3>=1 & H15_B3<3.5 ~2.5,
    H15_B3>=3.5 & H15_B3<6.5 ~5,
    H15_B3>=6.5 ~7
  ))

SS$H15_B4 <- SS$H15_B4/52.14
SS$H15_B4[SS$H15_B4<1] <- 0.5

for(i in 1:nrow(SS)){
  if(SS$H15[i]==0){
    SS$水果[i] <- 0
  }else{
    if(!is.na(SS$H15_B1[i])){
      SS$水果[i] <- SS$H15_B1[i]
    }else{
      if(!is.na(SS$H15_B2_1[i])){
        SS$水果[i] <- SS$H15_B2_1[i]
      }else{
        if(!is.na(SS$H15_B3[i])){
          SS$水果[i] <- SS$H15_B3_1[i]
        }else{
          if(!is.na(SS$H15_B4[i])){
            SS$水果[i] <- SS$H15_B4[i]
          }
        }
      }
    }
  }
}

SS$红肉得分[SS$红肉<=median(SS$红肉)] <- 1
SS$红肉得分[SS$红肉>median(SS$红肉)] <- 0
SS$腌菜得分[SS$腌菜<=median(SS$腌菜)] <- 1
SS$腌菜得分[SS$腌菜>median(SS$腌菜)] <- 0
SS$家禽得分[SS$家禽>=median(SS$家禽)] <- 1
SS$家禽得分[SS$家禽<median(SS$家禽)] <- 0
SS$蔬菜得分[SS$蔬菜>=median(SS$蔬菜)] <- 1
SS$蔬菜得分[SS$蔬菜<median(SS$蔬菜)] <- 0
SS$水果得分[SS$水果>=median(SS$水果)] <- 1
SS$水果得分[SS$水果<median(SS$水果)] <- 0
for (i in 1:nrow(SS)) {
  SS$膳食得分[i] <- sum(SS$红肉得分[i],SS$腌菜得分[i],SS$家禽得分[i],SS$蔬菜得分[i],SS$水果得分[i])
}

SS$理想膳食模式[SS$膳食得分<=3] <- 0
SS$理想膳食模式[SS$膳食得分>3] <- 1

SS <- SS[,c(1,49)]

#BMI及腰围
BW <- dat_JQCFnew[,c(640,660:662,8)]
BW$身高 <- BW$身高/100
BW$BMI <- BW$体重/BW$身高^2

BW$BMI_正常 <- 0
BW$BMI_正常[BW$BMI>18.5 & BW$BMI<23.9] <- 1

BW=BW%>%mutate(
  腰围正常=case_when(
    BW$A1=='男' & BW$腰围<85 ~ 1,
    BW$A1=='女' & BW$腰围<80 ~ 1,
    TRUE ~ 0
  )
)
BW <- BW[,c(1,6:8)]

data <- merge(data,SS,by='粪便条码')
data <- merge(data,BW,by='粪便条码')
cols <- colnames(data)
var <- c("ID","粪便条码",'age','粪便性状','Dyslipidemia',
         'TG_UP','TC_UP','LDL_UP','HDL_Low',
         'DASH_score.y','Sex','Ethnicity','Education',
         'Marriage','Smoke','Drink','ModeratePA','VigorousPA','ModeratePA_goal',
         'VigorousPA_goal','Moderate_vigorous','Moderate_vigorous_goal','最近一个月内是否服用过抗生素',
         '理想膳食模式','BMI_正常','腰围正常','BMI','腰围')

num1 <- c()
for (i in 1:length(var)) {
  num1[i] <- c(which(colnames(data)==var[i]))
}
data <- data[,num1]

fvar <- c('Sex','Ethnicity','Education','Dyslipidemia',
          'Marriage','Smoke','Drink','ModeratePA_goal',
          'VigorousPA_goal','Moderate_vigorous_goal',
          '粪便性状')
num2 <- c()
for (i in 1:length(fvar)) {
  num2[i] <- c(which(colnames(data)==fvar[i]))
}

for (i in 1:length(num2)) {
  data[,num2[i]] <- as.factor(data[,num2[i]])
}

sum(is.na(data$DASH_score.y))

data <- filter(data,!is.na(data$最近一个月内是否服用过抗生素)&data$最近一个月内是否服用过抗生素!=1)
data$Antibiotics <- ifelse(data$最近一个月内是否服用过抗生素==2,'No','Yes')


data <- merge(data,MetS_JX,by='粪便条码')

data <- filter(data,data$ModeratePA<5880)
data <- filter(data,data$VigorousPA<5880)

#生活方式评分
data$吸烟[data$Smoke=='Never-somkers'] <- 1
data$吸烟[data$Smoke=='Current/previous smokers'] <- 0
data$饮酒[data$Drink=='Never-drinkers/rarely drinkers'] <- 1
data$饮酒[data$Drink=='Current drinkers'] <- 0
data$体力活动[data$Moderate_vigorous_goal=='No'] <- 0
for (i in 1:nrow(data)) {
  data$健康生活方式评分[i] <- sum(data$吸烟[i],data$饮酒[i],data$BMI_正常[i],
                          data$腰围正常[i],data$理想膳食模式[i],data$体力活动[i])
}
data_H <- filter(data,data$Dyslipidemia==0)
data_D <- data

data_TG <- rbind(filter(data,data$TG_UP==1),data_H)
data_TC <- rbind(filter(data,data$TC_UP==1),data_H)
data_LDL <- rbind(filter(data,data$LDL_UP==1),data_H)
data_HDL <- rbind(filter(data,data$HDL_Low==1),data_H)

write.xlsx(data_D, 'samples.xlsx')