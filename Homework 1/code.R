library(data.table)
setwd("C:/Users/User/Desktop/R_data")
top.1000.sites <- fread("top_1000_sites.csv", header=T, sep =",")

attach(top.1000.sites)
library(ggplot2)
ggplot(top.1000.sites, aes(x = PageViews, y = UniqueVisitors, color = "chocolate1")) +
  geom_point()+
  theme_light()
ggplot(top.1000.sites, aes(x = UniqueVisitors)) +
  geom_density()+
  theme_light()
ggplot(top.1000.sites, aes(x = log(UniqueVisitors))) +
  geom_density()+
  theme_light()
ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors),color = "chocolate1")) +
  geom_point()+
  theme_light()+
  geom_smooth(method = 'lm', se = FALSE,color = "brown")
lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors),
             data = top.1000.sites)
summary(lm.fit)
lm.fit2 <- lm(log(PageViews) ~ HasAdvertising + log(UniqueVisitors) + InEnglish,
             data = top.1000.sites)
summary(lm.fit2)
barplot(Category)
barplot(table(Category))
sort(table(Category))
nrow(as.matrix(table(Category)))

library(ggplot2)
top.100.sites = top.1000.sites[1:100,]
others <- c("Web Portals","Social Networks","News","Newspaper","Online Games",
            "Online Video","Search Engines","File Sharing & Hosting","Blogging Resources & Services","")
is.others <- top.100.sites$Category %in% others
top.100.sites$Category[!is.others] = 'Others'
top.100.sites[Category==""]$Category = 'NA'
#Barplot by Category
#先把低於20的變數改為Others
sort(table(Category))
others <- c("Web Portals","Social Networks","News","Newspaper","Online Games",
            "Online Video","Search Engines","File Sharing & Hosting","Blogging Resources & Services","")
is.others <- top.1000.sites$Category %in% others
top.1000.sites$Category[!is.others] = 'Others'
top.1000.sites[Category==""]$Category = 'NA'
ggplot(top.1000.sites, aes(x = reorder(Category,Category,function(x)-length(x))))+
  geom_bar(fill="lightblue")+
  theme_light()+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5, color=I("dodgerblue3"), size = 3.5)+
  xlab("Category (Merge Categories below 20)")

#Barplot by LTD
ggplot(top.100.sites, aes(x = reorder(TLD,TLD,function(x)-length(x))))+
  geom_bar(fill="lightblue")+
  theme_light()+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5, color=I("dodgerblue3"), size = 3.5)+
  xlab("LTD")
#Barplot by LTD, stack with HasAdvertising
library(RColorBrewer)
install.packages("RColorBrewer")
ggplot(top.100.sites, aes(x = reorder(TLD,TLD,function(x)-length(x))))+
  geom_bar(aes(fill=HasAdvertising))+
  scale_fill_brewer(palette = "Blues")+
  theme_light()+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5, color=I("dodgerblue3"), size = 3.5)+
  xlab("LTD")
#Barplot by LTD, stack with InEnglish
library(RColorBrewer)
install.packages("RColorBrewer")
ggplot(top.100.sites, aes(x = reorder(TLD,TLD,function(x)-length(x))))+
  geom_bar(aes(fill=InEnglish))+
  scale_fill_brewer(palette = "Blues")+
  theme_light()+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5, color=I("dodgerblue3"), size = 3.5)+
  xlab("LTD")
#Piechart by InEnglish/HasAdvertising
IE=data.frame(group=c("Yes","No"),value=c(58,42))
ggplot(IE, aes(x = "",y=value,fill=group))+
  geom_bar(stat = "identity")+
  coord_polar("y", start=0)+
  scale_fill_brewer(palette = "Blues")+
  theme_light()+
  xlab("")+
  ylab("InEnglish")+
  theme(panel.grid=element_blank(),
        panel.border=element_blank())+
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = value, color=I("dodgerblue3")), size=5)
HA=data.frame(group=c("Yes","No"),value=c(838,162))
ggplot(HA, aes(x = "",y=value,fill=group))+
  geom_bar(stat = "identity")+
  coord_polar("y", start=0)+
  scale_fill_brewer(palette = "Blues")+
  theme_light()+
  xlab("")+
  ylab("HasAdvertising")+
  theme(panel.grid=element_blank(),
        panel.border=element_blank())+
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = value, color=I("dodgerblue3")), size=5)
#補齊LTD之資料
tld = gsub("([^\\.]+\\.)(.*)", "\\2", top.1000.sites$Site, perl = T)
(w = which(tld[1:100] != top.1000.sites$TLD[1:100])) #驗證是否正確
top.1000.sites[w, ] #觀察不正確之資料，反而是原始資料錯誤
top.1000.sites$TLD = tld
top.1000.sites[101:105, ]
#InEnglish忽略NA值
top.1000.sites$InEnglish = factor(top.1000.sites$InEnglish, levels = c("Yes","No"))
lm.fit3 <- lm(log(PageViews) ~ HasAdvertising + log(UniqueVisitors) + InEnglish,
              data = top.1000.sites)
summary(lm.fit3)
lm.fit4 <- lm(log(PageViews) ~ log(UniqueVisitors) + InEnglish,
              data = top.1000.sites)
summary(lm.fit4)
#Reach
ggplot(top.1000.sites, aes(x = PageViews, y = Reach, color = "chocolate1")) +
  geom_point()+
  theme_light()
ggplot(top.1000.sites, aes(x = Reach)) +
  geom_density()+
  theme_light()
ggplot(top.1000.sites, aes(x = log(Reach))) +
  geom_density()+
  theme_light()
ggplot(top.1000.sites, aes(x = log(PageViews), y = log(Reach),color = "chocolate1")) +
  geom_point()+
  theme_light()+
  geom_smooth(method = 'lm', se = FALSE,color = "brown")
#Stepwise
spfit= lm(log(PageViews) ~ log(UniqueVisitors) + log(Reach)+
            factor(InEnglish)+factor(TLD)+factor(HasAdvertising)+factor(Category),
          data = top.1000.sites)
summary(spfit)
library(MASS) 
step <- stepAIC(spfit, direction="both")
step$anova
final.fit=lm(log(PageViews) ~ log(UniqueVisitors) + factor(InEnglish) + factor(TLD) + 
               factor(Category),data = top.1000.sites)
summary(final.fit)
final.fit2=lm(log(PageViews) ~ log(UniqueVisitors) + factor(TLD) + 
               factor(Category),data = top.1000.sites)
summary(final.fit2)
#補InEnglish之缺失值
library(mice)
init = mice(top.1000.sites, maxit=0)
meth = init$method
predM = init$predictorMatrix
predM[, c("Rank","Site")]=0
meth[c("InEnglish")]="logreg"
imputed = mice(top.1000.sites, method=meth, predictorMatrix=predM, m=5)
summary(imputed)
imputed <- complete(imputed)
top.1000.sites.mice <- imputed
micefit= lm(log(PageViews) ~ log(UniqueVisitors) + log(Reach)+
          factor(InEnglish)+factor(TLD)+factor(HasAdvertising)+factor(Category),
        data = top.1000.sites.mice)
library(MASS) 
micestep <- stepAIC(micefit, direction="both")
micestep$anova
final.micefit=lm(log(PageViews) ~ log(UniqueVisitors) + factor(TLD) + factor(HasAdvertising) + 
                   factor(Category),data=top.1000.sites.mice)
summary(final.micefit)
top.1000.sites.mice[101:105,]
others <- c("Web Portals","Social Networks","News","Newspaper","Online Games",
            "Online Video","Search Engines","File Sharing & Hosting","Blogging Resources & Services","")
is.others <- top.1000.sites.mice$Category %in% others
top.1000.sites.mice$Category[!is.others] = 'Others'
top.1000.sites.mice[Category==""]$Category = 'NA'
