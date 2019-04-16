#第一題:用以下所有資料盡可能分析此筆資料的概況
laptop.df <- read.csv('C:\\Users\\USER\\Desktop\\kay\\BusinessAnalysis\\LaptopSalesJanuary2008Sub.csv')
str(laptop.df)
str(laptop.df)
dim(laptop.df)
head(laptop.df)
tail(laptop.df)
install.packages("psych")
library(psych)
describe(laptop.df[,c(2,5:9,11,13)])

#第二題:
boxplot(Retail.Price~Configuration, data=laptop.df, ylab='價格 ($k)',xlab='Configurtion')#查看型號和價格分佈的關係，有型號越大，價格越高的趨勢
mean(laptop.df$Retail.Price)#算出售出電腦的價格平均
#方法一
ConQuan = table(laptop.df$Configuration)#先把各個configuration進行分組，看每組configuration的數量各是多少
CountCon <- as.data.frame(ConQuan)#將分組好的電腦型號製成資料表
plot(x=CountCon$Var1,y=CountCon$Freq)#觀察型號跟數量的關係
maxConfigQuan<-which(CountCon$Freq==max(CountCon$Freq))#找出最多人買的電腦型號的數量
maxConfig<-CountCon$Var1[maxConfigQuan]#找出最多人買的電腦型號
maxConfigPrice<-which(laptop.df$Configuration==maxConfig)#找出在甚麼價格下最多人買的電腦型號
laptop.df$Retail.Price[maxConfigPrice]#在最多人買的電腦型號下的價格是多少
laptop.df[maxConfigPrice[1],c(2,5:12)]#最多人買的電腦型號規格
#方法二
PriceMAx<-data.frame(table(laptop.df$Retail.Price))#各價格下的購買筆數
MaxPrice<-PriceMAx$Var1[PriceMAx$Freq==max(PriceMAx$Freq)]#最大購買筆數的價格
plot(PriceMAx)#各價格的數量分佈
ConfigmaxPrice<-laptop.df$Configuration[laptop.df$Retail.Price==MaxPrice]#符合最大購買價格筆數的configuraiton
MaxPriceconfig<-data.frame(table(ConfigmaxPrice))#計算最大購買價格筆數的configuraiton分別購買筆數為多少
CC<-which(laptop.df$Configuration=='7'|
        laptop.df$Configuration=='56'|
        laptop.df$Configuration=='159'|
        laptop.df$Configuration=='208'|
        laptop.df$Configuration=='299'|
        laptop.df$Configuration=='348')
laptop.df[CC[c(1:4,9:10)],c(2,5:12)]#查看最大購買價格筆數的configuraiton的規格


#第三題
price.mean <- aggregate(Retail.Price~Store.Postcode,data=laptop.df, mean)#計算不同店家的平均銷售金額
library(lattice)
barchart(Retail.Price~Store.Postcode, data=price.mean,ylab='Retail Price Mean', xlab='Store PostCode',col="lightblue")
#不同店家對平均銷售金額圖形

#第四題
seg.aov.own <- aov(Retail.Price ~ Store.Postcode, data=laptop.df) 
anova(seg.aov.own)
#用ANOVA檢定看各店家母體的銷售金額是否一致

#第五題
boxplot(Retail.Price~Store.Postcode, data=laptop.df, xlab='Store', ylab='價格 ($k)')
#各店家銷售金額盒狀分佈圖
a<-which(laptop.df$Store.Postcode=='W4 3PH'| laptop.df$Store.Postcode=='N17 6QA') 
#選出價格平均最大和最小的兩家店位置
b<-laptop.df$Retail.Price[a]#取出平均最大最小的兩店價格
c<-laptop.df$Store.Postcode[a]#將平均最大和最小的店家取出
two <- factor(c)#打斷結構，不要出現剩下14個沒有用到的店家
minmax<-data.frame( "Store"=two, "Price"=b)#只比較平均最大和最小兩家店
boxplot(Price~Store, data=minmax, col=c(5,6),xlab='Store Postcode', ylab='價格 ($k)')#平均最大和最小的價格分佈盒狀圖
t.test(Price ~ Store, data=minmax) #t檢定平均最大和最小的店家母體銷售額平均是否相同 
var.test(Price ~ Store, data=minmax)#f檢定平均最大和最小的店家母體銷售額變異數是否相同

#第六題
plot(sort(table(laptop.df$Store.Postcode),decreasing = TRUE))#將各店家銷售數量由小到大排序
CustomerDis.mean <- aggregate(CustomerStoreDistance~Store.Postcode,data=laptop.df, mean)#將各店家的顧客離店家距離算平均
library(lattice)
barchart(CustomerStoreDistance~Store.Postcode, data=CustomerDis.mean,ylab='顧客跟店家的平均距離', xlab='Store PostCode',col="lightblue")
#各店家的顧客離店家距離圖
chisq.test(table(laptop.df$Store.Postcode))
#用Chi square test檢定各店家母體銷售數量佔母體總銷售數量比例是否不同

#第七題
chisq.test(table(laptop.df$Configuration))
#用chi square test檢定各型號母體銷售數量佔母體總銷售量比例是否相同，是否有依賴相關性


#第八題
sum(laptop.df$Retail.Price)#計算一月總銷售額
Storesum = aggregate(laptop.df$Retail.Price,
                        by=list(Postcode=laptop.df$Store.Postcode), sum)#算出各店家的銷售額
library(lattice)
barchart(x~Postcode, xlab='Store',ylab='金額',data=Storesum, col="lightblue")
#各店家銷售額圖
















