# this script investigates deeper the scores to the ST persepctive test

library(rio)
library(plyr)
library(car)
library(dbplyr)
library(dplyr)
library(dtplyr)
library(effects)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(grid)
library(lattice)
library(magrittr)
library(multcomp)
library(PairedData)
library(plyr)
library(rio)
library(rstatix)
library(tibble)
library(tidyverse)
library(ggstatsplot)
library(lme4)
library(car)
library(MuMIn)
library(multimode)
library(nlme)

getwd()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/CHUV_datasets")
data <- read.delim('CHUV_study4.txt', header = T)

data_fem <- subset(data, Gender == 'F')
summary(data_fem)
sd(data_fem$Age)
data_males <- subset(data, Gender == 'M')
summary(data_males)
sd(data_males$Age)

data$cubes_acc<-as.numeric(data$cubes_acc)
data$objects_acc<-as.numeric(data$objects_acc)
data$bodies_acc<-as.numeric(data$bodies_acc)
data$Gender <- as.factor(data$Gender)
data$ST_score<-as.numeric(data$ST_score)
data$ST_rt<-as.numeric(data$ST_rt)
data$OSIQV_style <- as.factor(data$OSIQV_style)
data$object_scale_group <- as.factor(data$object_scale_group)
data$spatial_scale_group <- as.factor(data$spatial_scale_group)


data2 <- data%>% gather(key = "condition", value = "accuracy" , "cubes_acc", "bodies_acc", "objects_acc")

mod0 <- lm(accuracy ~  Gender + condition + OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data2)
anova(mod0)

mod01 <- lm(accuracy ~  Gender + condition + OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data2)
anova(mod01)

mod02 <- lm(accuracy ~ condition + OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data2)
anova(mod02)


mod03 <- lm(accuracy ~  condition + OSIQV_style + CFS_group + st_score_group + st_speed_group, data2)
anova(mod03)

mod03 <- lm(accuracy ~  condition + OSIQV_style + spatial_scale_group + object_scale_group + CFS_group + st_score_group + st_speed_group, data2)
Anova(mod03)

mod03 <- lm(accuracy ~  condition + OSIQV_style + spatial_scale_group + object_scale_group + CFS_group + st_score_group, data2)
Anova(mod03)

mod04 <- lm(accuracy ~  condition*OSIQV_style*CFS_group*st_score_group*st_speed_group, data2)
anova(mod04)



mod05<- lm(accuracy ~  condition + OSIQV_style + CFS_group + st_score_group + st_speed_group, data2)
Anova(mod05)
mod05 <- car::Anova(mod05)
mod05



mod06 <- lm(accuracy ~  condition + OSIQV_style + CFS_group + st_score_group + st_speed_group + condition:OSIQV_style + condition:CFS_group + condition:st_score_group + condition:st_speed_group, data2)
anova(mod06)

# there is an effect of gender and condition, but no interaction
#dataMen <- subset(data, Gender =="M")
#dataWomen <- subset(data, Gender =="F")
#t.test(dataMen$ST_score,dataWomen$ST_score)

data2$condition <- gsub("cubes_acc", "cubes", data2$condition)
data2$condition <- gsub("bodies_acc", "bodies", data2$condition)
data2$condition <- gsub("objects_acc", "objects", data2$condition)

cond <- ggplot(data2,aes(condition, accuracy, fill=condition))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
                                      panel.grid.minor = element_blank())+
  ggtitle("Acc according to Condition")

cond2 <- cond + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))

cond2 + scale_fill_manual(values=c("#EC7063", "#42A5F5", "#4CAF50"))

data_bodies <- subset(data2, condition == 'bodies')
data_cubes <- subset(data2, condition == 'cubes')
data_objects <- subset(data2, condition == 'objects')

t.test(data_bodies$accuracy, data_cubes$accuracy)
t.test(data_bodies$accuracy, data_objects$accuracy)
t.test(data_objects$accuracy, data_cubes$accuracy)

ggplot(data=data2, aes(x=OSIQV_style, y=accuracy)) + geom_boxplot() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
#ggplot(data=data2, aes(x=OSIQV_style, y=accuracy, color=Gender)) + geom_boxplot() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


osiqv <- ggplot(data2,aes(OSIQV_style, accuracy, fill=OSIQV_style))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(),
                                      panel.grid.minor = element_blank())+
  ggtitle("Total MR score according to OSIQV_style")

osiqv + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))


osiqv + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.text.x=element_text(size=11),axis.text.y=element_text(size=11)) + scale_fill_manual(breaks = c("object", "spatial"), 
                               values=c("red", "blue")) + xlab('OSIQV style')


######################################### PLOTS FOR SPATIAL SCALE GROUP ################



ssg <- ggplot(data2,aes(spatial_scale_group, accuracy, fill=spatial_scale_group))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(),
                                      panel.grid.minor = element_blank())+
  ggtitle("Total MR score according to spatial_scale group")

ssg + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))


ssg + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.text.x=element_text(size=11),axis.text.y=element_text(size=11)) + scale_fill_manual(breaks = c("high", "low"), 
                                                                                                                    values=c("red", "blue")) +
  xlab('spatial scale group')

######################################### PLOTS FOR OBJECT SCALE GROUP ################



osg <- ggplot(data2,aes(object_scale_group, accuracy, fill=object_scale_group))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(),
                                      panel.grid.minor = element_blank())+
  ggtitle("Total MR score according to object_scale group")

osg + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))


osg + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                           axis.text.x=element_text(size=11),axis.text.y=element_text(size=11)) + scale_fill_manual(breaks = c("high", "low"), 
                                                                                                                    values=c("red", "blue")) +
  xlab('object scale group')



#########################################  PLOT FOR PERSPECTIVE TAKING GROUP 

data2$st_score_group <- gsub("high", "bad", data2$st_score_group)
data2$st_score_group <- gsub("low", "good", data2$st_score_group)
data2$st_score_group <- gsub("bad", "low", data2$st_score_group)
data2$st_score_group <- gsub("good", "high", data2$st_score_group)

SOT <- ggplot(data2,aes(st_score_group, accuracy, fill=st_score_group))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(),
                                      panel.grid.minor = element_blank())+
  ggtitle("Total MR score according to ST score group")

SOT + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                         axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))


SOT + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                         axis.text.x=element_text(size=11),axis.text.y=element_text(size=11)) + scale_fill_manual(breaks = c("low", "high"), 
                                                                                                                  values=c("blue", "red")) +
  xlab('Perspective-taking group')





mod1 <- lm(cubes_acc ~ OSIQV_style + CFS_group + spatial_scale_group + object_scale_group + st_speed_group + st_score_group, data)
Anova(mod1)

ggplot(data,aes(OSIQV_style, cubes_acc, fill=OSIQV_style))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(),
                                      panel.grid.minor = element_blank())+
  ggtitle("Accuracy according to OSIQ style")

ggplot(data=data, aes(x=object_scale, y=cubes_acc)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=spatial_scale, y=cubes_acc)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


mod2 <- lm(bodies_acc ~ OSIQV_style + CFS_group + spatial_scale_group + object_scale_group + st_speed_group + st_score_group, data)
Anova(mod2)

ggplot(data,aes(OSIQV_style, bodies_acc))+
  geom_boxplot() + theme_bw() +
  ggtitle("Acc according to Condition and Gender")

mod3 <- lm(objects_acc ~ OSIQV_style + CFS_group + spatial_scale_group + object_scale_group + st_speed_group + st_score_group, data)
Anova(mod3)

ggplot(data,aes(OSIQV_style, objects_acc))+
  geom_boxplot() + theme_bw() +
  ggtitle("Acc according to Condition and Gender")


mod4 <- lm(spatial_scale ~  OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data)
anova(mod4)

library("ggpubr")

cor.test(spatial_scale, ST_score,  method = "pearson")

ggscatter(data, x = "spatial_scale", y = "ST_score", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Score on spatial scale", ylab = "Score on ST task")


ggplot(data=data, aes(x=OSIQV_style, y=ST_score)) + geom_boxplot() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

ggplot(data,aes(OSIQV_style, ST_score))+
  geom_boxplot() + theme_bw() +
  ggtitle("")

ggplot(data,aes(spatial_scale, ST_score, color=OSIQV_style))+
  geom_point() + theme_bw() +
  ggtitle("")



ggplot(data=data, aes(x=OSIQV_style, y=cubes_acc, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)



ggplot(data=data, aes(x=ST_score, y=accuracy)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=ST_score, y=accuracy, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)



ggplot(data=data, aes(x=ST_rt, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=ST_rt, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

########## PIE CHARTS ########## 
# for OSIQ
data_obj <- subset(data, OSIQV_style =="object")

data_spatial <- subset(data, OSIQV_style =="spatial")

ggplot(data, aes(x="", y=OSIQV_style, fill=OSIQV_style)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() 

ggplot(data_obj, aes(x="", y=OSIQV_style, fill=Gender)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() 

ggplot(data_spatial, aes(x="", y=OSIQV_style, fill=Gender)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() 

# Count the occurrences of each gender within OSIQV_style and calculate percentages
# Count the occurrences of each OSIQV_style
osiqv_counts <- data %>% 
  group_by(OSIQV_style) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for OSIQV_style
ggplot(osiqv_counts, aes(x="", y=Percent, fill=OSIQV_style)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Overall Proportion of OSIQV Styles") +
  
  # Add percentage labels for OSIQV_style
  geom_text(aes(label = paste0(OSIQV_style, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("blue", "red")) # Assign distinct colors for object and spatial


# Count the occurrences of each gender in the object group
gender_obj_counts <- data_obj %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for Gender distribution in the Object group
ggplot(gender_obj_counts, aes(x="", y=Percent, fill=Gender)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Proportion of Males and Females in Object Group") +
  
  # Add percentage labels for Gender
  geom_text(aes(label = paste0(Gender, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("red", "blue")) # Assign colors for male and female

# Count the occurrences of each gender in the spatial group
gender_spatial_counts <- data_spatial %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for Gender distribution in the Spatial group
ggplot(gender_spatial_counts, aes(x="", y=Percent, fill=Gender)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Proportion of Males and Females in Spatial Group") +
  
  # Add percentage labels for Gender
  geom_text(aes(label = paste0(Gender, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("red", "blue")) # Assign colors for male and female

# Subset data for the high and low object scale groups
object_high <- subset(data, object_scale_group == "high")
object_low <- subset(data, object_scale_group == "low")

# Subset data for the high and low spatial scale groups
spatial_high <- subset(data, spatial_scale_group == "high")
spatial_low <- subset(data, spatial_scale_group == "low")

# Count the occurrences of each gender in the high object scale group
gender_object_high_counts <- object_high %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for Gender distribution in the high object scale group
ggplot(gender_object_high_counts, aes(x="", y=Percent, fill=Gender)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Proportion of Males and Females in High Object Scale Group") +
  
  # Add percentage labels for Gender
  geom_text(aes(label = paste0(Gender, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("red", "blue")) # Assign colors for male and female

# Count the occurrences of each gender in the low object scale group
gender_object_low_counts <- object_low %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for Gender distribution in the low object scale group
ggplot(gender_object_low_counts, aes(x="", y=Percent, fill=Gender)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Proportion of Males and Females in Low Object Scale Group") +
  
  # Add percentage labels for Gender
  geom_text(aes(label = paste0(Gender, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("red", "blue")) # Assign colors for male and female

# Count the occurrences of each gender in the high spatial scale group
gender_spatial_high_counts <- spatial_high %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for Gender distribution in the high spatial scale group
ggplot(gender_spatial_high_counts, aes(x="", y=Percent, fill=Gender)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Proportion of Males and Females in High Spatial Scale Group") +
  
  # Add percentage labels for Gender
  geom_text(aes(label = paste0(Gender, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("red", "blue")) # Assign colors for male and female

# Count the occurrences of each gender in the low spatial scale group
gender_spatial_low_counts <- spatial_low %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for Gender distribution in the low spatial scale group
ggplot(gender_spatial_low_counts, aes(x="", y=Percent, fill=Gender)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Proportion of Males and Females in Low Spatial Scale Group") +
  
  # Add percentage labels for Gender
  geom_text(aes(label = paste0(Gender, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("red", "blue")) # Assign colors for male and female

# Subset data for the high and low ST groups
st_high <- subset(data, st_score_group == "high")
st_low <- subset(data, st_score_group == "low")

# Subset data for the high and low CFS groups
cfs_high <- subset(data, CFS_group == "high")
cfs_low <- subset(data, CFS_group == "low")

# Count the occurrences of each gender in the high ST group
gender_st_high_counts <- st_high %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for Gender distribution in the high ST group
ggplot(gender_st_high_counts, aes(x="", y=Percent, fill=Gender)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Proportion of Males and Females in High ST Group") +
  
  # Add percentage labels for Gender
  geom_text(aes(label = paste0(Gender, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("red", "blue")) # Assign colors for male and female

# Count the occurrences of each gender in the low ST group
gender_st_low_counts <- st_low %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for Gender distribution in the low ST group
ggplot(gender_st_low_counts, aes(x="", y=Percent, fill=Gender)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Proportion of Males and Females in Low ST Group") +
  
  # Add percentage labels for Gender
  geom_text(aes(label = paste0(Gender, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("red", "blue")) # Assign colors for male and female

# Count the occurrences of each gender in the high CFS group
gender_cfs_high_counts <- cfs_high %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for Gender distribution in the high CFS group
ggplot(gender_cfs_high_counts, aes(x="", y=Percent, fill=Gender)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Proportion of Males and Females in High CFS Group") +
  
  # Add percentage labels for Gender
  geom_text(aes(label = paste0(Gender, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("red", "blue")) # Assign colors for male and female

# Count the occurrences of each gender in the low CFS group
gender_cfs_low_counts <- cfs_low %>% 
  group_by(Gender) %>% 
  summarise(Count = n()) %>%
  mutate(Percent = Count / sum(Count) * 100)

# Create the pie chart for Gender distribution in the low CFS group
ggplot(gender_cfs_low_counts, aes(x="", y=Percent, fill=Gender)) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + 
  theme_void() +
  ggtitle("Proportion of Males and Females in Low CFS Group") +
  
  # Add percentage labels for Gender
  geom_text(aes(label = paste0(Gender, "\n", round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5), colour = "white") +
  
  # Customize the color fill
  scale_fill_manual(values=c("red", "blue")) # Assign colors for male and female



########## NOW DOING THE SAME FOR RT #########################


data3 <- data%>% gather(key = "condition", value = "RT" , "cubes_rt", "bodies_rt", "objects_rt")

mod0 <- lm(RT ~  Gender + condition + OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data3)
anova(mod0)

mod01 <- lm(RT ~  Gender + condition + OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data3)
anova(mod01)

mod02 <- lm(RT ~ condition + OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data3)
anova(mod02)


mod03 <- lm(RT ~  condition + OSIQV_style + CFS_group + st_score_group + st_speed_group, data3)
anova(mod03)

mod031 <- lm(RT ~  condition + OSIQV_style + spatial_scale_group + object_scale_group + CFS_group + st_score_group + st_speed_group, data3)
Anova(mod03)

mod031 <- lm(RT ~  condition + OSIQV_style + spatial_scale_group + object_scale_group + CFS_group + st_score_group, data3)
Anova(mod031)

mod032 <- lm(RT ~  condition + OSIQV_style + spatial_scale_group + object_scale_group + CFS_group + st_score_group + st_speed_group + condition:OSIQV_style + condition:spatial_scale_group + condition:object_scale_group + condition:ST_rt + condition:ST_score + condition:CFS_group + condition:st_score_group + condition:st_speed_group, data3)
Anova(mod032)

mod033 <- lm(RT ~  condition*OSIQV_style*spatial_scale_group*object_scale_group*CFS_group*st_score_group*st_speed_group, data3)
Anova(mod033)

mod04 <- lm(RT ~  condition*OSIQV_style*CFS_group*st_score_group*st_speed_group, data3)
anova(mod04)

mod05<- lm(RT ~  condition + OSIQV_style + CFS_group + st_score_group + st_speed_group, data3)
Anova(mod05)
mod05 <- car::Anova(mod05)
mod05



mod06 <- lm(RT ~  condition + OSIQV_style + CFS_group + st_score_group + st_speed_group + condition:OSIQV_style + condition:CFS_group + condition:st_score_group + condition:st_speed_group, data3)
anova(mod06)

# there is an effect of gender and condition, but no interaction
#dataMen <- subset(data, Gender =="M")
#dataWomen <- subset(data, Gender =="F")
#t.test(dataMen$ST_score,dataWomen$ST_score)

ggplot(data=data3, aes(x=OSIQV_style, y=RT)) + geom_boxplot() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
#ggplot(data=data3, aes(x=OSIQV_style, y=RT, color=Gender)) + geom_boxplot() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

data3$condition <- gsub("cubes_rt", "cubes", data3$condition)
data3$condition <- gsub("bodies_rt", "bodies", data3$condition)
data3$condition <- gsub("objects_rt", "objects", data3$condition)

cond <- ggplot(data3,aes(condition, RT, fill=condition))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
                                      panel.grid.minor = element_blank())+
  ggtitle("RT according to Condition")

cond2 <- cond + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))

cond2 + scale_fill_manual(values=c("#EC7063", "#42A5F5", "#4CAF50"))

data_bodies <- subset(data3, condition == 'bodies')
data_cubes <- subset(data3, condition == 'cubes')
data_objects <- subset(data3, condition == 'objects')

t.test(data_bodies$RT, data_cubes$RT)
t.test(data_bodies$RT, data_objects$RT)
t.test(data_objects$RT, data_cubes$RT)

osqv_rt <- ggplot(data3,aes(OSIQV_style, RT, fill=OSIQV_style))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(),
                                      panel.grid.minor = element_blank())+
  ggtitle("Total MR RT according to OSIQV_style")

osqv_rt + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                         axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))


osqv_rt + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                         axis.text.x=element_text(size=11),axis.text.y=element_text(size=11)) + scale_fill_manual(breaks = c("object", "spatial"), 
                                                                                                                  values=c("red", "blue")) +
  xlab('OSIQV style')


ggplot(data3,aes(OSIQV_style, RT, fill=OSIQV_style))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(),
                                      panel.grid.minor = element_blank())+
  ggtitle("Mean MR RT according to OSIQV_style")


ggplot(data=data3, aes(x=object_scale_group, y=RT, fill=object_scale_group)) + geom_boxplot() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

ggplot(data=data3, aes(x=spatial_scale_group, y=RT, fill=spatial_scale_group)) + geom_boxplot() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


ggplot(data=data3, aes(x=st_score_group, y=RT, fill=st_score_group)) + geom_boxplot() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)



mod1 <- lm(cubes_acc ~ OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data)
anova(mod1)

ggplot(data,aes(OSIQV_style, cubes_acc, fill=OSIQV_style))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(),
                                      panel.grid.minor = element_blank())+
  ggtitle("RT according to OSIQ style")

ggplot(data=data, aes(x=object_scale, y=cubes_acc)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data, aes(x=spatial_scale, y=cubes_acc)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


mod2 <- lm(bodies_acc ~  OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data)
anova(mod2)

ggplot(data,aes(OSIQV_style, bodies_acc))+
  geom_boxplot() + theme_bw() +
  ggtitle("Acc according to Condition and Gender")

mod3 <- lm(objects_acc ~  OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data)
anova(mod3)

ggplot(data,aes(OSIQV_style, objects_acc))+
  geom_boxplot() + theme_bw() +
  ggtitle("Acc according to Condition and Gender")


mod4 <- lm(spatial_scale ~  OSIQV_style + CFS_score + spatial_scale + object_scale + ST_rt + ST_score, data)
anova(mod4)

library("ggpubr")

cor.test(spatial_scale, ST_score,  method = "pearson")

ggscatter(data, x = "spatial_scale", y = "ST_score", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Score on spatial scale", ylab = "Score on ST task")


ggplot(data=data, aes(x=OSIQV_style, y=ST_score)) + geom_boxplot() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

ggplot(data,aes(OSIQV_style, ST_score))+
  geom_boxplot() + theme_bw() +
  ggtitle("")

ggplot(data,aes(spatial_scale, ST_score, color=OSIQV_style))+
  geom_point() + theme_bw() +
  ggtitle("")


