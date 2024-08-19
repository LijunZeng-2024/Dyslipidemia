rm(list = ls())
pacman::p_load(tidyverse, openxlsx, rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

df_src <- read.xlsx('data/raw_metadata.xlsx', rowNames = T)
res <- df_src[,c('sex','age','BMI','waist')]
rownames(res) <- rownames(df_src)

# Antibiotics：LZ3 1=Y 2=N
tmp <- ifelse(!is.na(df_src$LZ3)&df_src$LZ3!=1, 0, df_src$LZ3) # 0未用抗生素
res['Antibiotics'] <- ifelse(!is.na(tmp)&tmp==1, 1, tmp) # 1用了抗生素

# 分数转换（要考虑空值，空值读取进来是NA）
# 吸烟:B1  1=不吸烟 2=吸烟 3=已戒烟；1分是不吸烟
res['smoke'] <- ifelse(!is.na(df_src$B1)&df_src$B1!=1, 0, df_src$B1) # na和1之外的结果定为0

# 饮酒:C2 1=从不或几乎不，2-6饮酒；1分是不或几乎不饮酒
res['drink'] <- ifelse(!is.na(df_src$C2)&df_src$C2!=1, 0, df_src$C2)

# BMI 18.5-23.9为1分，其余为0
res['BMI_score'] <- ifelse(df_src$BMI>18.5 & df_src$BMI<23.9, 1, 0)

# 腰围（sex 1男、2女）
tmp <- df_src %>% 
  select(sex, waist) %>%
  mutate(waist_score=case_when(
    df_src$sex==1 & df_src$waist<85 ~ 1, # 男腰围85以下
    df_src$sex==2 & df_src$waist<80 ~ 1, # 女腰围80以下
    TRUE ~ 0 # 相当于else
  )
)
res['waist_score'] <- tmp$waist_score

# 活动情况
# F

# 膳食情况
# H
# 吃红肉
df_meat <- filter(df_src, !is.na(df_src$H8_NUM))[,c('H8B1','H8B2','H8B3','H8B4','H8_NUM')]
meat_samples <- rownames(df_meat)
df_meat <- as.data.frame(lapply(df_meat, as.numeric))

# H8B1 每天吃几次
df_meat$H8B1[df_meat$H8B1>=1] <- 7

# H8B2 每周吃几次
df_meat <- df_meat %>%
  mutate(H8_B2_1=case_when(
    H8B2<1 ~0.5, # 不足1次
    H8B2>=1 & H8B2<3.5 ~2.5, # 1-3.5次
    H8B2>=3.5 & H8B2<6.5 ~5, # 3.5-6.5次
    H8B2>=6.5 ~7 # 7.5次以上
  ))


# H8B3 每月吃几次
df_meat$H8B3 <- df_meat$H8B3/4.29
df_meat <- df_meat %>%
  mutate(H8_B3_1=case_when(
    H8B3<1 ~0.5,
    H8B3>=1 & H8B3<3.5 ~2.5,
    H8B3>=3.5 & H8B3<6.5 ~5,
    H8B3>=6.5 ~7
  ))

# H8B4 每年吃几次
df_meat$H8B4 <- df_meat$H8B4/52.14
df_meat$H8B4[df_meat$H8B4<1] <- 0.5

# H8_NUM 每次吃0g的视为不吃红肉？
for(i in 1:nrow(df_meat)){
  if(df_meat$H8_NUM[i]==0){
    df_meat$meat[i] <- 0
  }else{
    if(!is.na(df_meat$H8B1[i])){
      df_meat$meat[i] <- df_meat$H8B1[i]
    }else{
      if(!is.na(df_meat$H8B2[i])){
        df_meat$meat[i] <- df_meat$H8_B2_1[i]
      }else{
        if(!is.na(df_meat$H8B3[i])){
          df_meat$meat[i] <- df_meat$H8_B3_1[i]
        }else{
          if(!is.na(df_meat$H8B4[i])){
            df_meat$meat[i] <- df_meat$H8B4[i]
          }
        }
      }
    }
  }
}
rownames(df_meat) <- meat_samples

table(df_meat$meat, useNA='ifany') # 统计频次，包括na（查看之用）
# 吃家禽
df_poultry <- filter(df_src, !is.na(df_src$H9_NUM))[,c('H9B1','H9B2','H9B3','H9B4','H9_NUM')] # 选非空（H8B1没有空）
poultry_samples <- rownames(df_poultry)
df_poultry <- as.data.frame(lapply(df_poultry, as.numeric))

# 注意空值
df_poultry$H9B1[df_poultry$H9B1>=1] <- 7
df_poultry$H9B1[df_poultry$H9B1<1] <- 2.5

df_poultry <- df_poultry %>%
  mutate(H9_B2_1=case_when(
    H9B2<1 ~0.5,
    H9B2>=1 & H9B2<3.5 ~2.5,
    H9B2>=3.5 & H9B2<6.5 ~5,
    H9B2>=6.5 ~7
  ))

df_poultry$H9B3 <- df_poultry$H9B3/4.29
df_poultry <- df_poultry %>%
  mutate(H9_B3_1=case_when(
    H9B3<1 ~0.5,
    H9B3>=1 & H9B3<3.5 ~2.5,
    H9B3>=3.5 & H9B3<6.5 ~5,
    H9B3>=6.5 ~7
  ))

df_poultry$H9B4 <- df_poultry$H9B4/52.14
df_poultry$H9B4[df_poultry$H9B4<1] <- 0.5

for(i in 1:nrow(df_poultry)){
  if(df_poultry$H9_NUM[i]==0){
    df_poultry$poultry[i] <- 0
  }else{
    if(!is.na(df_poultry$H9B1[i])){
      df_poultry$poultry[i] <- df_poultry$H9B1[i]
    }else{
      if(!is.na(df_poultry$H9B2[i])){
        df_poultry$poultry[i] <- df_poultry$H9_B2_1[i]
      }else{
        if(!is.na(df_poultry$H9B3[i])){
          df_poultry$poultry[i] <- df_poultry$H9_B3_1[i]
        }else{
          if(!is.na(df_poultry$H9B4[i])){
            df_poultry$poultry[i] <- df_poultry$H9B4[i]
          }
        }
      }
    }
  }
}
rownames(df_poultry) <- poultry_samples

# 吃蔬菜
df_veg <- filter(df_src, !is.na(df_src$H12_NUM))[,c('H12B1','H12B2','H12B3','H12B4','H12_NUM')]
veg_samples <- rownames(df_veg)
df_veg <- as.data.frame(lapply(df_veg, as.numeric))

df_veg$H12B1[df_veg$H12B1>=1] <- 7
df_veg <- df_veg %>%
  mutate(H12_B2_1=case_when(
    H12B2<1 ~0.5,
    H12B2>=1 & H12B2<3.5 ~2.5,
    H12B2>=3.5 & H12B2<6.5 ~5,
    H12B2>=6.5 ~7
  ))

df_veg$H12B3 <- df_veg$H12B3/4.29
df_veg <- df_veg %>%
  mutate(H12_B3_1=case_when(
    H12B3<1 ~0.5,
    H12B3>=1 & H12B3<3.5 ~2.5,
    H12B3>=3.5 & H12B3<6.5 ~5,
    H12B3>=6.5 ~7
  ))

df_veg$H12B4 <- df_veg$H12B4/52.14
df_veg$H12B4[df_veg$H12B4<1] <- 0.5

for(i in 1:nrow(df_veg)){
  if(df_veg$H12_NUM[i]==0){
    df_veg$veg[i] <- 0
  }else{
    if(!is.na(df_veg$H12B1[i])){
      df_veg$veg[i] <- df_veg$H12B1[i]
    }else{
      if(!is.na(df_veg$H12B2[i])){
        df_veg$veg[i] <- df_veg$H12_B2_1[i]
      }else{
        if(!is.na(df_veg$H12B3[i])){
          df_veg$veg[i] <- df_veg$H12_B3_1[i]
        }else{
          if(!is.na(df_veg$H12B4[i])){
            df_veg$veg[i] <- df_veg$H12B4[i]
          }
        }
      }
    }
  }
}
rownames(df_veg) <- veg_samples

# 吃腌菜
df_pickle <- filter(df_src, !is.na(df_src$H14_NUM))[,c('H14B1','H14B2','H14B3','H14B4','H14_NUM')]
pickle_samples <- rownames(df_pickle)
df_pickle <- as.data.frame(lapply(df_pickle, as.numeric))

df_pickle$H14B1[df_pickle$H14B1>=1] <- 7
df_pickle$H14B1[df_pickle$H14B1<1] <- 2.5
df_pickle <- df_pickle %>%
  mutate(H14_B2_1=case_when(
    H14B2<1 ~0.5,
    H14B2>=1 & H14B2<3.5 ~2.5,
    H14B2>=3.5 & H14B2<6.5 ~5,
    H14B2>=6.5 ~7
  ))

df_pickle$H14B3 <- df_pickle$H14B3/4.29
df_pickle <- df_pickle %>%
  mutate(H14_B3_1=case_when(
    H14B3<1 ~0.5,
    H14B3>=1 & H14B3<3.5 ~2.5,
    H14B3>=3.5 & H14B3<6.5 ~5,
    H14B3>=6.5 ~7
  ))

df_pickle$H14B4 <- df_pickle$H14B4/52.14
df_pickle$H14B4[df_pickle$H14B4<1] <- 0.5

for(i in 1:nrow(df_pickle)){
  if(df_pickle$H14_NUM[i]==0){
    df_pickle$pickle[i] <- 0
  }else{
    if(!is.na(df_pickle$H14B1[i])){
      df_pickle$pickle[i] <- df_pickle$H14B1[i]
    }else{
      if(!is.na(df_pickle$H14B2[i])){
        df_pickle$pickle[i] <- df_pickle$H14_B2_1[i]
      }else{
        if(!is.na(df_pickle$H14B3[i])){
          df_pickle$pickle[i] <- df_pickle$H14_B3_1[i]
        }else{
          if(!is.na(df_pickle$H14B4[i])){
            df_pickle$pickle[i] <- df_pickle$H14B4[i]
          }
        }
      }
    }
  }
}
rownames(df_pickle) <- pickle_samples

# 吃水果
df_fruit <- filter(df_src, !is.na(df_src$H15_NUM))[,c('H15B1','H15B2','H15B3','H15B4','H15_NUM')]
fruit_samples <- rownames(df_fruit)
df_fruit <- as.data.frame(lapply(df_fruit, as.numeric))

df_fruit$H15B1[df_fruit$H15B1>=1] <- 7
df_fruit$H15B1[df_fruit$H15B1<1] <- 2.5

df_fruit <- df_fruit %>%
  mutate(H15_B2_1=case_when(
    H15B2<1 ~0.5,
    H15B2>=1 & H15B2<3.5 ~2.5,
    H15B2>=3.5 & H15B2<6.5 ~5,
    H15B2>=6.5 ~7
  ))

df_fruit$H15B3 <- df_fruit$H15B3/4.29
df_fruit <- df_fruit %>%
  mutate(H15_B3_1=case_when(
    H15B3<1 ~0.5,
    H15B3>=1 & H15B3<3.5 ~2.5,
    H15B3>=3.5 & H15B3<6.5 ~5,
    H15B3>=6.5 ~7
  ))

df_fruit$H15B4 <- df_fruit$H15B4/52.14
df_fruit$H15B4[df_fruit$H15B4<1] <- 0.5

for(i in 1:nrow(df_fruit)){
  if(df_fruit$H15_NUM[i]==0){
    df_fruit$fruit[i] <- 0
  }else{
    if(!is.na(df_fruit$H15B1[i])){
      df_fruit$fruit[i] <- df_fruit$H15B1[i]
    }else{
      if(!is.na(df_fruit$H15B2[i])){
        df_fruit$fruit[i] <- df_fruit$H15_B2_1[i]
      }else{
        if(!is.na(df_fruit$H15B3[i])){
          df_fruit$fruit[i] <- df_fruit$H15_B3_1[i]
        }else{
          if(!is.na(df_fruit$H15B4[i])){
            df_fruit$fruit[i] <- df_fruit$H15B4[i]
          }
        }
      }
    }
  }
}
rownames(df_fruit) <- fruit_samples

df_meat$score[df_meat$meat<=median(df_meat$meat)] <- 1
df_meat$score[df_meat$meat>median(df_meat$meat)] <- 0
df_pickle$score[df_pickle$pickle<=median(df_pickle$pickle)] <- 1
df_pickle$score[df_pickle$pickle>median(df_pickle$pickle)] <- 0

df_veg$score[df_veg$veg>=median(df_veg$veg)] <- 1
df_veg$score[df_veg$veg<median(df_veg$veg)] <- 0
df_fruit$score[df_fruit$fruit>=median(df_fruit$fruit)] <- 1
df_fruit$score[df_fruit$fruit<median(df_fruit$fruit)] <- 0
df_poultry$score[df_poultry$poultry>=median(df_poultry$poultry)] <- 1
df_poultry$score[df_poultry$poultry<median(df_poultry$poultry)] <- 0

res <- merge(res, df_meat[, c('meat','score')], all.x = TRUE, by = 'row.names')
colnames(res)[ncol(res)] <- 'meat_score'
rownames(res) <- res[,1]
res <- res[,-1]

res <- merge(res, df_pickle[, c('pickle', 'score')], all.x = TRUE, by = 'row.names')
colnames(res)[ncol(res)] <- 'pickle_score'
rownames(res) <- res[,1]
res <- res[,-1]

res <- merge(res, df_veg[, c('veg', 'score')], all.x = TRUE, by = 'row.names')
colnames(res)[ncol(res)] <- 'veg_score'
rownames(res) <- res[,1]
res <- res[,-1]

res <- merge(res, df_fruit[, c('fruit', 'score')], all.x = TRUE, by = 'row.names')
colnames(res)[ncol(res)] <- 'fruit_score'
rownames(res) <- res[,1]
res <- res[,-1]

res <- merge(res, df_poultry[, c('poultry', 'score')], all.x = TRUE, by = 'row.names')
colnames(res)[ncol(res)] <- 'poultry_score'
rownames(res) <- res[,1]
res <- res[,-1]

res['meal_score'] <- res$meat_score + res$pickle_score + res$veg_score + res$fruit_score + res$poultry_score
res$meal_score <- ifelse(res$meal_score<=3, 0, 1)

##### 体力活动评分 #####
tiliJX <- df_src[,c(22:55)]
colnames(tiliJX)
tiliJX <- lapply(tiliJX, as.numeric)
tiliJX <- as.data.frame(tiliJX)
rownames(tiliJX) <- rownames(df_src)

##### 中等强度 #####
##### 上班——方式活动量 #####
# f1 上班方式
# f2a 周均上班天数
# f2b 日均上班时长
tiliJX <- tiliJX %>%
  mutate(F1_1=case_when(  # 上班活动状态？静坐、站立、体力活
    F1 == 2 | F1== 3  ~ 1,
    TRUE ~ 0))
tiliJX$moderateShangban_fei <-
  tiliJX$F1_1 * (tiliJX$F2a*tiliJX$F2b*60) # 工作方式*周均工作分钟数

##### 非农——通勤活动量 #####
# f3 出行方式
# f4 日均通勤时长
#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tiliJX <- tiliJX %>%
  mutate(F3_1=case_when( 
    F3 == 1| F3== 2 ~ 1,
    TRUE ~ 0))  
tiliJX$moderatelushang_feinong <- 
  tiliJX$F3_1 * (tiliJX$F4*5) # 出行方式*工作日通勤时长（周均）

##### 农忙季-农活活动量 #####
# f6a农忙月数
# f6c农忙时日均农活时长
# f6b农忙工作方式

#Manual work in the farming season   Vigorous
#Semi-mechanized work in the farming season moderate
#Fully-mechanized work in the farming season low
tiliJX <- tiliJX %>%
  mutate(F6b_1=case_when( # f6b农忙工作方式 
    F6b == 2 ~ 1,
    TRUE ~ 0))  
tiliJX$moderate_nongmang <- 
  tiliJX$F6b_1 * (tiliJX$F6c * tiliJX$F6a*30*60/7) * (tiliJX$F6a/12) #先算出每周的再进行加权

##### 非农忙季-农活活动量 #####
# f6b 农忙工作方式
# f7a 周均农活天数
# f7b 日均农活时长
#Manual work in the non-farming season moderate
tiliJX <- tiliJX %>%
  mutate(F6b_2=case_when(
    F6b == 2 ~ 1,
    TRUE ~ 0))

tiliJX$moderate_feinongmang <- 
  tiliJX$F6b_2 * (tiliJX$F7a*tiliJX$F7b*60) *(12-tiliJX$F6a)/12 # 周均农活分钟数 * 非农忙月数全年占比

##### 其他工作——活动量 #####
#Sedentary work——Low
#Standing work——Moderate
#Manual work——Moderate
#Heavy manual work——Vigorous
# f9 其他工作方式
# f10a 其他工作周均天数
# f10b 其他工作日均时长
tiliJX <- tiliJX %>%
  mutate(F9_1=case_when( 
    F9 == 2 | F9== 3 ~ 1,
    TRUE ~ 0))  
table(tiliJX$F9_1)

tiliJX$moderate_qitagongzuo <- 
  tiliJX$F9_1 * (tiliJX$F10a * tiliJX$F10b * 60) # 方式*周均分钟数

##### 农业——通勤活动量 #####
#Walking——Moderate
#Motorbike——Moderate
#Bicycle——Vigorous
#Private or public transportation（such as bus, car, underground and ferry） ——Low
tiliJX <- tiliJX %>%
  mutate(F11_1=case_when( 
    F11 == 1| F11== 2 ~ 1,#赋值已更改
    TRUE ~ 0))
tiliJX$moderatelushang_non <- 
  tiliJX$F11_1 * (tiliJX$F12 * 5) # 通勤方式*工作日通勤分钟数（周均）


# 锻炼类型 f14
#Tai-Chi/qigong/leisure walking   Moderate
#快走                             Moderate
#jogging/aerobics                 Vigorous
#Swimming                         Vigorous
#Ball game                        Moderate
#Exercise with fitness equipment  Moderate
#Other exercise, e.g. mountain walking, home exercise and rope jumping   Moderate

##### 锻炼方式 #####
# f14 锻炼方式
# f15 日均锻炼分钟数
tiliJX <- tiliJX %>%
  mutate(F14_1=case_when( 
    F14 == 1 |F14 == 2 |F14 == 5|F14 == 6| F14 == 7 ~ 1,
    TRUE ~ 0))  
table(tiliJX$F14_1)

tiliJX$moderateduanlian <- 
  tiliJX$F14_1 * (tiliJX$F15 * 60) # 锻炼方式*日均锻炼分钟数

#中度体力活动时间（总和）
tiliJX[is.na(tiliJX)]=0

tiliJX$ModeratePA_JX <- 
  tiliJX$moderateShangban_fei + tiliJX$moderatelushang_feinong+
  tiliJX$moderate_nongmang + tiliJX$moderate_feinongmang+
  tiliJX$moderate_qitagongzuo + tiliJX$moderatelushang_non+
  tiliJX$moderateduanlian
summary(tiliJX$ModeratePA_JX)


##### 高强度 #####
##### 上班——方式活动量 #####
tiliJX <- tiliJX %>%
  mutate(F1_zhong1=case_when( 
    F1 == 4  ~ 1,
    TRUE ~ 0))
tiliJX$vigorousShangban_fei <- 
  tiliJX$F1_zhong1 * (tiliJX$F2a*tiliJX$F2b*60)

##### 非农——通勤
#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tiliJX <- tiliJX %>%
  mutate(F3_zhong1=case_when( 
    F3 == 3 ~ 1,
    TRUE ~ 0))
tiliJX$vigorouslushang_feinong <- 
  tiliJX$F3_zhong1* (tiliJX$F4 * 5)

#农忙农活
#Manual work in the farming season   Vigorous
#Semi-mechanized work in the farming season moderate
#Fully-mechanized work in the farming season low
tiliJX <- tiliJX %>%
  mutate(F6b_zhong1=case_when( 
    F6b == 1 ~ 1,
    TRUE ~ 0))
tiliJX$vigorous_nongmang <- 
  tiliJX$F6b_zhong1 * (tiliJX$F6c * tiliJX$F6a*30*60/7) * (tiliJX$F6a/12)

#非农忙农活
#Manual work in the non-farming season   moderate
tiliJX <- tiliJX %>%
  mutate(F6b_zhong2=case_when( 
    F6b == 1 ~ 1,
    TRUE ~ 0))
tiliJX$vigorous_feinongmang <- 
  tiliJX$F6b_zhong2 * (tiliJX$F7a*tiliJX$F7b*60) *(12-tiliJX$F6a)/12

#其他工作 
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
  tiliJX$F9_zhong1 * (tiliJX$F10a * tiliJX$F10b * 60)

#农业路上
#Walking-Moderate
#Motorbike-Moderate
#Bicycle-Vigorous
#Private or public transportation（such as bus, car, underground and ferry -Low
tiliJX <- tiliJX %>%
  mutate(F11_zhong1=case_when( 
    F11 == 3 ~ 1,
    TRUE ~ 0))
tiliJX$vigorouslushang_non <- 
  tiliJX$F11_zhong1 * (tiliJX$F12*5)


#体力活动共用
#tiliJX <- tiliJX %>%
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

tiliJX <- tiliJX %>%
  mutate(F14_zhong1=case_when( 
    F14 == 3 |F14 == 4 ~ 1,
    TRUE ~ 0))  

tiliJX$vigorousduanlian <- 
  tiliJX$F14_zhong1 * (tiliJX$F15 * 60)

#重度体力活动时间（总和）
tiliJX[is.na(tiliJX)]=0
tiliJX$VigorousPA_JX <-  tiliJX$vigorousShangban_fei + tiliJX$vigorouslushang_feinong+
  tiliJX$vigorous_nongmang + tiliJX$vigorous_feinongmang +
  tiliJX$vigorous_qitagongzuo + tiliJX$vigorouslushang_non + 
  tiliJX$vigorousduanlian

##### 静坐时长 #####
# f19 日均坐卧时间
tiliJX$sedentary_leisure_time_JX <- tiliJX$F19
summary(tiliJX$sedentary_leisure_time_JX)

##### 结果统计 #####
# 中等活动达标-150
tiliJX1 <- tiliJX[,c('ModeratePA_JX','VigorousPA_JX')]
tiliJX1 = tiliJX1 %>% mutate(
  ModeratePA_goal_JX=case_when(
    ModeratePA_JX>=150 ~ 1, # 1为达标，0为未达标
    TRUE ~ 0
  )
)
# 高强度活动达标-75
tiliJX1 = tiliJX1 %>% mutate(
  VigorousPA_goal_JX=case_when(
    VigorousPA_JX>=75 ~ 1,
    TRUE ~ 0
  )
)
# 中高强度活动达标-75
tiliJX1$Moderate_vigorous_JX <- tiliJX1$ModeratePA_JX/2 + tiliJX1$VigorousPA_JX # 基线中等取一半
tiliJX1 = tiliJX1 %>% mutate(
  Moderate_vigorous_goal_JX=case_when(
    Moderate_vigorous_JX>=75 ~ 1,
    TRUE ~ 0
  )
)

res <- merge(res, tiliJX1, all.x = TRUE, by = 'row.names')
rownames(res) <- res[,1]
res <- res[,-1]

# 排除日均活动时长14小时以上的样本
res <- filter(res, res$ModeratePA_JX<5880)
res <- filter(res, res$VigorousPA_JX<5880)
samples <- rownames(res)
res <- as.data.frame(lapply(res, as.numeric))
rownames(res) <- samples

res$lifestyle_score <- res$smoke + res$drink + res$BMI_score + res$waist_score + res$meal_score + res$Moderate_vigorous_goal_JX

##### 血脂 #####
#血脂异常
table(df_src$TG, useNA='ifany') # 看是否有空值
table(df_src$HDL, useNA='ifany')
table(df_src$TC, useNA='ifany')
table(df_src$LDL, useNA='ifany')

df_blood <- df_src[complete.cases(df_src[,'TG']),c('TG', 'HDL', 'TC', 'LDL')] # 去掉有空值的行
samples <- rownames(df_blood)
df_blood <- as.data.frame(lapply(df_blood, as.numeric))
rownames(df_blood) <- samples

df_blood <- df_blood %>% mutate(
  TG_UP=case_when(
    TG>=2.3 ~ 1,
    TRUE ~ 0
  ),
  HDL_Low=case_when(
    HDL<1.0 ~ 1,
    TRUE ~ 0
  ),
  TC_UP=case_when(
    TC>=6.2 ~ 1,
    TRUE ~ 0
  ),
  LDL_UP=case_when(
    LDL>=4.1 ~ 1,
    TRUE ~ 0
  )
)
df_blood$Dyslipidemia <- ifelse(df_blood$TG>=2.3 | df_blood$HDL<1 | df_blood$TC>=6.2 | df_blood$LDL>=4.1, 1, 0)
res <- merge(res, df_blood[,c('TG_UP', 'HDL_Low', 'TC_UP', 'LDL_UP', 'Dyslipidemia')], all.x = TRUE, by='row.names')
rownames(res) <- res[,1]
res <- res[,-1]

write.csv(res, 'metadata.csv')

