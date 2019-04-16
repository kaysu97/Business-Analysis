#How would you do to know the data? 
laptop.df <- read.csv('C:\\Users\\USER\\Desktop\\kay\\BusinessAnalysis\\LaptopSalesJanuary2008Sub.csv')
str(laptop.df)
str(laptop.df)
dim(laptop.df)
head(laptop.df)
tail(laptop.df)
install.packages("psych")
library(psych)
describe(laptop.df[,c(2,5:9,11,13)])

#At what price are the laptops actually selling? Hint: define “actually selling”
#我們定義的actually selling是平均下來一般人大概在某規格下的某個價格
#平均離店家這個距離的顧客會消費在上述各種平均規格條件下的電腦價格
ConQuan = table(laptop.df$Configuration)
str(ConQuan)
as.numeric(ConQuan)
CountCon <- as.data.frame(ConQuan)
Config<-as.numeric(CountCon$Var1)
maxConfigQuan<-which(CountCon$Freq==max(CountCon$Freq))
maxConfig<-CountCon$Var1[maxConfigQuan]
maxConfigPrice<-which(laptop.df$Configuration==maxConfig)
table(laptop.df$Retail.Price[maxConfigPrice])

#Create a bar chart, showing the average retail price by store. Which store has the highest
average? Which has the lowest?
price.mean <- aggregate(Retail.Price~Store.Postcode,data=laptop.df, mean)
library(lattice)
barchart(Retail.Price~Store.Postcode, data=price.mean,ylab='Retail Price Mean', xlab='Store PostCode',col="lightblue")

# Are average prices consistent across retail outlets in general?
sd(price.mean$Retail.Price)
IQR(price.mean$Retail.Price)


#To better compare retail prices across stores, create boxplots of retail price by store. Now compare the prices in the two stores from (3). Does there seem to be a difference between their price distributions?
boxplot(Retail.Price~Store.Postcode, data=laptop.df, yaxt='n', ylab='價格 ($k)')
a<-which(laptop.df$Store.Postcode=='W4 3PH'| laptop.df$Store.Postcode=='N17 6QA') 
#選出價格平均最大和最小的兩家店位置
b<-laptop.df$Retail.Price[a]#找出最大最小的兩店價格分布
c<-laptop.df$Store.Postcode[a]
two <- factor(c)#打斷結構，不要出現剩下14個沒有用到的店家
minmax<-data.frame( "Store"=two, "Price"=b)#只比較最大和最小兩家店
boxplot(Price~Store, data=minmax, col=c(5,6),xlab='Store Postcode', ylab='價格 ($k)')#最大和最小的box plot

#Which stores are selling the most in terms of quantities? Provide a visual presentation, and a description about which store it is. Do store quantities vary in general?
plot(sort(table(laptop.df$Store.Postcode),decreasing = FALSE))
table(laptop.df$Store.Postcode)

#In general, are stores’ sales quantities depend on computer configurations?
ConQuan = table(laptop.df$Configuration)
str(ConQuan)
as.numeric(ConQuan)
CountCon <- as.data.frame(ConQuan)
Config<-as.numeric(CountCon$Var1)
cor(x=Config,y=CountCon$Freq)
aggregate(Retail.Price~Configuration, data=laptop.df, mean)

#What are revenues of stores? 
sum(laptop.df$Retail.Price)
Storesum = aggregate(laptop.df$Retail.Price,by=list(Country=laptop.df$Store.Postcode), sum)
library(lattice)
barchart(x~Country, data=Storesum, col="lightblue")
seg.aov.own <- aov(Retail.Price ~ Store.Postcode, data=laptop.df) 
anova(seg.aov.own)

