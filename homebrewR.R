# require(stringr)
# require(RCurl)
# require('rvest')
# require(plyr)  
# require(ggplot2)


ext_num <- function(x,txt){
  t <- gsub(paste(".*",x,sep=""),'',txt)
  i <- gregexpr("[0-9|\\.]+",t)[[1]]
  substr(t,start = i[1],i[1]+attr(i,'match.length')[1]-1)}


parseBeer <- function(i){  
  #i <- 11
  
  #i <- 10236#6768#1944
  print(i)
  
  url <- paste('http://beerrecipes.org/showrecipe.php?recipeid=',i,sep='')
  beer <- html(url)
  
  name <- html_nodes(beer,"div h1") %>% html_text()
  name
  if(name=="Find a Beer Recipe")
    return(NULL)#c(i,rep(NA,12)))
  
  desc <- html_nodes(beer,"div p") %>% 
    html_text() %>% str_trim() 
  
  bsi <- grep('Beer Style:',desc)
  srci <- grep('Source:',desc)
  prdi <- srci-1
  dcrpi <- bsi+1
  
  type <- desc[bsi]
  type <- gsub('Beer Style:|Recipe Type:|Yield:',';',type)
  
  type <- str_trim(strsplit(type,';')[[1]])
  
  style <- str_trim(gsub("\\(.+",'',type[2]))
  
  class <- gsub(".+\\(|)",'',type[2])
  
  rtype <- type[3]
  yld <- type[4]
  dcrp <- desc[dcrpi]
  proc <- ifelse(length(prdi)==0,NA,desc[prdi])
  src <- ifelse(length(srci)==0,NA,str_trim(gsub('Source:','',desc[srci])))
  
  ing <- html_nodes(beer,"li span") %>% 
    html_text() %>%
    paste(collapse = '||')
  
  txt <- html_nodes(beer,".columns") %>% 
    html_text()
  
  ii <- grep('Gravity:',txt)
  
  ii <- grep(' Original Gravity:',txt)[1]
  og=ext_num(x = 'Original Gravity:',txt[ii])
  fg=ext_num(x = 'Final Gravity:',txt[ii])
  abv=ext_num(x = 'Alcohol by Vol:',txt[ii])
  srm=ext_num(x = 'Color SRM:',txt[ii])
  ibu=ext_num(x = 'Bitterness IBU:',txt[ii])
  
  data.frame(stringsAsFactors = F,id=i,
             name=name,
             style=style,
             class=class,
             type=rtype,
             yield=yld,
             OG=og,
             FG=fg,
             ABV=abv,
             SRM=srm,
             IBU=ibu,
             description=dcrp,
             ingredients=ing,
             procedure=proc,
             src=src)
}


df1<-ldply(0001:1000,parseBeer)
save(df1,file='data/df1.rdata')
df2<-ldply(1001:2000,parseBeer)
save(df2,file='data/df2.rdata')
df3<-ldply(2001:3000,parseBeer)
save(df3,file='data/df3.rdata')
df4<-ldply(3001:4000,parseBeer)
save(df4,file='data/df4.rdata')
df5<-ldply(4001:5000,parseBeer)
save(df5,file='data/df5.rdata')
df6<-ldply(5001:6000,parseBeer)
save(df6,file='data/df6.rdata')
# df7<-ldply(6001:7000,parseBeer)
# save(df7,file='data/df7.rdata')
# df8<-ldply(7001:8000,parseBeer)
# save(df8,file='data/df8.rdata')
# df9<-ldply(8001:9000,parseBeer)
# save(df9,file='data/df9.rdata')
# df10<-ldply(9001:10000,parseBeer)
# save(df10,file='data/df10.rdata')
# df11<-ldply(10001:11000,parseBeer)
# save(df11,file='data/df11.rdata')
# df12<-ldply(11001:12000,parseBeer)
# save(df12,file='data/df12.rdata')

load('df1.rdata')
load('df2.rdata')
load('df3.rdata')
load('df4.rdata')
load('df5.rdata')
load('df6.rdata')
load('df7.rdata')
load('df8.rdata')
load('df9.rdata')
load('df10.rdata')
load('df11.rdata')

df <- rbind(df1,df2,df3,df4,df5,df6,
            df7,df8,df9,df10,df11 )

df$OG<-as.numeric(df$OG)
df$FG<-as.numeric(df$FG)
df$ABV<-as.numeric(df$ABV)
df$SRM<-as.numeric(df$SRM)
df$IBU<-as.numeric(df$IBU)

df$ABV[df$ABV>20&!is.na(df$ABV)]<-NA
df$ABV[df$ABV<1&!is.na(df$ABV)]<-NA
df$OG[df$OG>1.2&!is.na(df$OG)]<-NA
df$OG[df$OG<=1&!is.na(df$OG)]<-NA
df$FG[df$FG>1.2&!is.na(df$FG)]<-NA
df$FG[df$FG<0.9&!is.na(df$FG)]<-NA
df$IBU[df$IBU==1&!is.na(df$IBU)]<-NA
df$IBU[df$IBU==0&!is.na(df$IBU)]<-NA
df$SRM[df$SRM==0&!is.na(df$SRM)]<-NA


save(df,file='df.rdata')

load('df.rdata')
# save(df,file='C:/Users/ds10/Dropbox/R/homebrewR/R/df.rdata')
# 
styles <- sort(unique(df$style)) 
save(styles,file='styles.rdata')

i <- 6768#10236#1944#
print(i)

url <- paste('http://beerrecipes.org/showrecipe.php?recipeid=',i,sep='')
beer <- html(url)

txt <- html_nodes(beer,".columns") %>% html_text()

txt



"Beer Style:"
"Recipe Type:"
"Yield:"
"Original Gravity:"
"Final Gravity:"
"Alcohol by Vol:"
"Color SRM:"
"Bitterness IBU:"
"Recipe Type:"
"Yield:"
"Source:"
grep('Yield:',txt)[1]






##################
load('df.rdata')


hops.sum <- sapply(df$ingredients,FUN = function(x){
  sapply(hops,FUN = function(y) length(grep(y,x)))/length(x)
}) %>% rbind.data.frame()

malts.sum <- sapply(df$ingredients,FUN = function(x){
  sapply(malts,FUN = function(y) length(grep(y,x)))/length(x)
}) %>% rbind.data.frame()


hops.sum <- t(hops.sum)
malts.sum <- t(malts.sum)

row.names(hops.sum)<-NULL
row.names(malts.sum)<-NULL

head(malts.sum)
head(hops.sum)

styledf<-cbind(malts.sum,hops.sum)

X <- styledf
Y <- factor(df$style)

testi <- sample(1:nrow(X),size = 20,replace = F)
traini <- (1:nrow(X))[-testi]

# require(epicalc)
# require(randomForest)
summary(Y)
rf <- randomForest(x = X,y=Y,ntree=2500)
rf

Xt <- X[1,]
Xt[] <- rep(0,43)
Xt[c(34)]<-1

hops
Xh <- rep(0,length(hops))
names(Xh) <- hops
Xm <- rep(0,length(malts))
names(Xm) <- malts

Xp <- c(Xh,Xm)

pdf <- predict(rf,Xp,type='prob') %>% data.frame()%>%melt()
tti<-order(pdf$value,decreasing=T)[1:10]
ggplot(pdf[tti,],aes(x=variable,y=value))+geom_point(stat='identity')+coord_flip()
Y[testi[i]]
require(reshape2)



########'##########################


require(plyr)
coldf <- ldply(cols,.fun = function(x) c(col2rgb(x)))
coldf$SRM <- 1:40
save(coldf,file='SRMcols.rdata')

sapply(cols,col2rgb)


str(coldf)


SRMcol <- function(x){
  i<-!is.na(x)
  x[x>40]<-40
  x[x<1]<-1
  x[i]<-rgb(
  approx(coldf$SRM,coldf$V1,x[i])$y/255,
  approx(coldf$SRM,coldf$V2,x[i])$y/255,
  approx(coldf$SRM,coldf$V3,x[i])$y/255
  )
  x
}

SRMcol(c(1,NA,0.1,35.6))

ggplot(df,aes(x=style,y=SRM,col=SRMcol(SRM)))+
  scale_y_log10()+scale_color_identity()+
  coord_flip()+
  geom_point(size=5)#+scale_color_identity()

df$SRM










