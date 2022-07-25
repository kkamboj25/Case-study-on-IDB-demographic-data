library(forcats)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)



census_2022_2002 <- read.csv('C:/Users/karti/Downloads/census_2022_2002.csv')
census_2022 <- census_2022_2002[census_2022_2002$year == "2022",]
census_2002 <- census_2022_2002[census_2022_2002$year == "2002",]

head(census_2022_2002)
summary(census_2022_2002)
head(census_2022)
head(census_2002)
summary(census_2022)
summary(census_2002)
unique(census_2022_2002$country)
summary(census_2022$total.fertility.rate)

#Question 1

hist(census_2022$total.fertility.rate,freq = FALSE,ylim =c(0,1.0),col ='cornflowerblue',xlab = "Total fertility rate",main =" ",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5 )


hist(census_2022$life.expectancy.both.sexes,freq = FALSE,ylim =c(0,0.08),col ='cornflowerblue',xlab = "Life Expectancy (Both Sexes)",main =" ",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5 )


hist(census_2022$life.expectancy.males,freq = FALSE,ylim =c(0,0.08),col ='cornflowerblue',xlab = "Life Expectancy (Males)",main =" ",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5 )


hist(census_2022$life.expectancy.females,freq = FALSE,ylim =c(0,0.08),col ='cornflowerblue',xlab = "Life Expectancy (Females)",main =" ",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5 )


#Difference between both sexes

hist(census_2022$life.expectancy.females,freq = FALSE, col=rgb(0,0,1,0.2), ylim=c(0, 0.08),
     xlab="Life Expectancy", ylab='Frequency', main="",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
hist(census_2022$life.expectancy.males,freq = FALSE, col=rgb(1,0,0,0.2),add=TRUE)
legend("topright",c('Male','Female'),fill=c(rgb(1,0,0,0.2),rgb(0,0,1,0.2)),cex=1.1,inset=.02)

#Question 2
## Pearson's correlation
P1_2 <- cor.test(census_2022$life.expectancy.both.sexes,census_2022$total.fertility.rate, method = "pearson", 
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95)
vale <-  ggplot() + annotate("text",x = 1,y = 1,label = "-0.79",size=7) + theme_void()
P1_3 <- cor.test(census_2022$life.expectancy.males,census_2022$total.fertility.rate, method = "pearson",
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95)
vale1 <-  ggplot() + annotate("text",x = 1,y = 1,label = "-0.76",size=7) + theme_void()
P1_4 <- cor.test(census_2022$life.expectancy.females,census_2022$total.fertility.rate, method = "pearson",
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95) 
vale2 <-  ggplot() + annotate("text",x = 1,y = 1,label = "-0.81",size=7) + theme_void()
P2_3 <- cor.test(census_2022$life.expectancy.both.sexes,census_2022$life.expectancy.males, method = "pearson",
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95) 
vale3 <-  ggplot() + annotate("text",x = 1,y = 1,label = "0.99",size=7) + theme_void()
P2_4 <- cor.test(census_2022$life.expectancy.both.sexes,census_2022$life.expectancy.females, method = "pearson",
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95) 
vale4 <-  ggplot() + annotate("text",x = 1,y = 1,label = "0.99",size=7) + theme_void()
P3_4 <- cor.test(census_2022$life.expectancy.males,census_2022$life.expectancy.females, method = "pearson",
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95) 
vale5 <-  ggplot() + annotate("text",x = 1,y = 1,label = "0.97",size=7) + theme_void()

p1_1 <- ggplot() + annotate("text",x = 1,y = 1,label = "Total Fertility Rate") + theme_void() 


p1_2 <- qplot(census_2022$total.fertility.rate,census_2022$life.expectancy.both.sexes,xlab = "Total fertility Rate",ylab = "LE of Both sexes",colour = I("cornflowerblue"))

p1_3 <- qplot(census_2022$total.fertility.rate,census_2022$life.expectancy.males,xlab = "Total fertility Rate",ylab = "LE of Males",colour = I("cornflowerblue")) 

p1_4 <- qplot(census_2022$total.fertility.rate,census_2022$life.expectancy.females,xlab = "Total fertility Rate",ylab = "LE of Females",colour = I("cornflowerblue")) 

p2_1 <- p1_2
p2_2 <- ggplot() + annotate("text",x = 2,y = 2,label = "LE of Both sexes") + theme_void()

p2_3 <- qplot(census_2022$life.expectancy.both.sexes,census_2022$life.expectancy.males,xlab = "LE of Both sexes",ylab = "LE of Males",colour = I("cornflowerblue")) 

p2_4 <- qplot(census_2022$life.expectancy.both.sexes,census_2022$life.expectancy.females,xlab = "LE of Both sexes",ylab = "LE of Females",colour = I("cornflowerblue")) 

p3_1 <- p1_3
p3_2 <- p2_3
p3_3 <- ggplot() + annotate("text",x = 1,y = 1,label = "LE of Males") + theme_void()
p3_4 <- qplot(census_2022$life.expectancy.males,census_2022$life.expectancy.females,xlab = "LE of Males",ylab = "LE of Females",colour = I("cornflowerblue")) 

p4_1 <- p1_4
p4_2 <- p2_4
p4_3 <- p3_4
p4_4 <- ggplot() + annotate("text",x = 1,y = 1,label = "LE of Females") + theme_void()

grid.arrange(p1_1,vale,vale1,vale2,
             p2_1,p2_2,vale3,vale4,
             p3_1,p3_2,p3_3,vale5,
             p4_1,p4_2,p4_3,p4_4,
             nrow = 4)



#####################################
##Spearman's Rank correlation
S1_2 <- cor.test(census_2022$life.expectancy.both.sexes,census_2022$total.fertility.rate, method = "spearman", 
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95)
sale <-  ggplot() + annotate("text",x = 1,y = 1,label = "-0.75",size=7) + theme_void()
S1_3 <- cor.test(census_2022$life.expectancy.males,census_2022$total.fertility.rate, method = "spearman",
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95)
sale1 <-  ggplot() + annotate("text",x = 1,y = 1,label = "-0.72",size=7) + theme_void()
S1_4 <- cor.test(census_2022$life.expectancy.females,census_2022$total.fertility.rate, method = "spearman",
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95) 
sale2 <-  ggplot() + annotate("text",x = 1,y = 1,label = "-0.78",size=7) + theme_void()
S2_3 <- cor.test(census_2022$life.expectancy.both.sexes,census_2022$life.expectancy.males, method = "spearman",
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95) 
sale3 <-  ggplot() + annotate("text",x = 1,y = 1,label = "0.99",size=7) + theme_void()
S2_4 <- cor.test(census_2022$life.expectancy.both.sexes,census_2022$life.expectancy.females, method = "spearman",
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95) 
sale4 <-  ggplot() + annotate("text",x = 1,y = 1,label = "0.99",size=7) + theme_void()
S3_4 <- cor.test(census_2022$life.expectancy.males,census_2022$life.expectancy.females, method = "spearman",
                 alternative = c("two.sided", "less", "greater"), exact = NULL, conf.level = 0.95) 
sale5 <-  ggplot() + annotate("text",x = 1,y = 1,label = "0.96",size=7) + theme_void()

s1_1 <- ggplot() + annotate("text",x = 1,y = 1,label = "Total Fertility Rate") + theme_void() 


s1_2 <- qplot(census_2022$total.fertility.rate,census_2022$life.expectancy.both.sexes,xlab = "Total fertility Rate",ylab = "LE of Both sexes",colour = I("cornflowerblue"))

s1_3 <- qplot(census_2022$total.fertility.rate,census_2022$life.expectancy.males,xlab = "Total fertility Rate",ylab = "LE of Males",colour = I("cornflowerblue")) 

s1_4 <- qplot(census_2022$total.fertility.rate,census_2022$life.expectancy.females,xlab = "Total fertility Rate",ylab = "LE of Females",colour = I("cornflowerblue")) 

s2_1 <- s1_2
s2_2 <- ggplot() + annotate("text",x = 2,y = 2,label = "LE of Both sexes") + theme_void()

s2_3 <- qplot(census_2022$life.expectancy.both.sexes,census_2022$life.expectancy.males,xlab = "LE of Both sexes",ylab = "LE of Males",colour = I("cornflowerblue")) 

s2_4 <- qplot(census_2022$life.expectancy.both.sexes,census_2022$life.expectancy.females,xlab = "LE of Both sexes",ylab = "LE of Females",colour = I("cornflowerblue")) 

s3_1 <- s1_3
s3_2 <- s2_3
s3_3 <- ggplot() + annotate("text",x = 1,y = 1,label = "LE of Males") + theme_void()
s3_4 <- qplot(census_2022$life.expectancy.males,census_2022$life.expectancy.females,xlab = "LE of Males",ylab = "LE of Females",colour = I("cornflowerblue")) 

s4_1 <- s1_4
s4_2 <- s2_4
s4_3 <- s3_4
s4_4 <- ggplot() + annotate("text",x = 1,y = 1,label = "LE of Females") + theme_void()
grid.arrange(s1_1,sale,sale1,sale2,
             s2_1,s2_2,sale3,sale4,
             s3_1,s3_2,s3_3,sale5,
             s4_1,s4_2,s4_3,s4_4,
             nrow = 4)

#######################################
# Question 3

census_2022 %>% mutate(subregion = fct_reorder(subregion, region,.fun = max )) %>%
  ggplot( aes(x=subregion, y=total.fertility.rate, fill=region)) + 
  geom_boxplot() +
  ylab("TFR") +
  coord_flip()


census_2022  %>% mutate(subregion = fct_reorder(subregion, region,.fun = max )) %>%
  ggplot( aes(x=subregion, y=life.expectancy.both.sexes, fill=region))+ 
  geom_boxplot() +
  ylab("LE Both sexes") +
  coord_flip()

census_2022  %>% mutate(subregion = fct_reorder(subregion, region,.fun = max ))%>%
  ggplot( aes(x=subregion, y=life.expectancy.males, fill=region)) + 
  geom_boxplot() +
  ylab("LE Males") + 
  coord_flip()


census_2022 %>% mutate(subregion = fct_reorder(subregion, region,.fun = max ))%>%
  ggplot( aes(x=subregion, y=life.expectancy.females, fill=region)) + 
  geom_boxplot() +  ylab("LE Females") +coord_flip()

#Question 4

#TFR
p <- ggplot(census_2002, aes(x=total.fertility.rate, y=census_2022$total.fertility.rate,xlab="TFR2002",colour=region,label = region)) + geom_point(size=2)+theme(axis.title.x = element_text(vjust = 0, size = 13),
                                                                                                                                                                 axis.title.y = element_text(vjust = 2, size = 13), )

p + xlab("TFR (2002)") + ylab("TFR (2022)") +annotate(geom="text", y=6.7, x=7.8, label="Niger",color="black",na.rm = TRUE,size = 4) + 
  annotate(geom="text", y=1.4, x=0.8, label="Macau",color="black",na.rm = TRUE,size = 4)

#LE of Both sexes
p <- ggplot(census_2002, aes(x=life.expectancy.both.sexes, y=census_2022$life.expectancy.both.sexes,colour=region,label = region)) + geom_point(size=3)+theme(axis.title.x = element_text(size = 14),
                                                                                                                                                              axis.title.y = element_text(size = 14), )
p + xlab("LE both sexes (2002)") + ylab("LE both sexes (2022)")+xlim(40,100)+
  theme(text = element_text(size = 12))+geom_text(aes(label=ifelse(life.expectancy.both.sexes < 55 | life.expectancy.both.sexes > 85 ,as.character(country),'')),hjust=0,vjust=1,check_overlap=T)


#LE males
p <- ggplot(census_2002, aes(x=life.expectancy.males, y=census_2022$life.expectancy.males,colour=region,label = region)) + geom_point(size=3)+theme(axis.title.x = element_text(size = 14),
                                                                                                                                                    axis.title.y = element_text(size = 14), )
p + xlab("LE males (2002)") + ylab("LE males (2022)") +xlim(40,100)+ 
  theme(text = element_text(size = 12))+geom_text(aes(label=ifelse(life.expectancy.males>82 |life.expectancy.males < 45 ,as.character(country),'')),hjust=0,vjust=1)


#LF females

p <- ggplot(census_2002, aes(x=life.expectancy.females, y=census_2022$life.expectancy.females,colour=region,label = region)) + geom_point(size=3)+theme(axis.title.x = element_text(size = 14),
                                                                                                                                                        axis.title.y = element_text(size = 14), )
p + xlab("LE females (2002)") + ylab("LE females (2022)")+xlim(40,100)+
  theme(text = element_text(size = 12))+geom_text(aes(label=ifelse(life.expectancy.females>89 |life.expectancy.females < 46 ,as.character(country),'')),hjust=0,vjust=0)



