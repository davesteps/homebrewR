#require(plyr)
require(gridExtra)
require(dplyr)
require(shiny)
require(ggplot2)
require(reshape2)
require(wesanderson)



pal<-function(n)wes_palette(n = n,name =  "Darjeeling",type = 'continuous')

load('data/df.rdata')

# 
# str(df)
df <- as.tbl(df)
# set.seed(1)
# df <- df[sample(1:nrow(df),nrow(df)/2),]

load('SRMcols.rdata')

SRMcol <- function(x){
  #x <- c(1,2,3)
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

df$SRMcol <- SRMcol(df$SRM)

df$SRMcol
p1 <- ggplot(df,aes(x=ABV))+
  ggtitle('%Alcohol')+
  geom_histogram(fill='red',alpha=0.6)
p2 <- ggplot(df,aes(x=SRM))+
  ggtitle('Colour')+
  scale_x_log10()
  # geom_histogram(fill=SRMcol(29))
  # geom_histogram(aes(fill=SRMcol))
  # scale_fill_identity()
p3 <- ggplot(df,aes(x=IBU))+
  ggtitle('Bitterness')+
  scale_x_log10()+
  geom_histogram(fill='green',alpha=0.6)





shinyServer(function(input, output,session) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

  
  builddf <- reactive({
    
    
    itype <- df$type%in%input$chk_type
    i <- rep(T,nrow(df))
    
    if(input$rdb_search == 'Ingredients'){
      ci <- c(input$slt_malts,input$slt_hops)    
      
      print(ci)
      if(!is.null(ci)){
        exp <- paste('(?=.*',ci,')',sep='') %>%
          paste(collapse = '')
        i <- grepl(exp,df$ingredients,perl = T)}      
      
      df <- df%>%filter(i,itype)
      } 
    
    if(input$rdb_search == 'Style'){
      ci <- input$slt_styles
      
        # print(ci)
        
        # if(is.null(ci)){
          # ci <- df$style%in%ci}
        
        df <- df%>%filter(style%in%ci,itype)
  }
  
  
  if(input$rdb_search == 'Character'){
        
    #SRMi <- df$SRM>=input$sld_SRM[1]&df$SRM<=input$sld_SRM[2]&!is.na(df$SRM)
    #ABVi <- df$ABV>=input$sld_ABV[1]&df$ABV<=input$sld_ABV[2]&!is.na(df$ABV)
    #IBUi <- df$IBU>=input$sld_IBU[1]&df$IBU<=input$sld_IBU[2]&!is.na(df$IBU)
    df <- df%>%filter(!is.na(df$IBU),!is.na(df$ABV),!is.na(df$SRM),itype)
    
    SRM <- input$sld_SRM#17    
    IBU <- input$sld_IBU
    ABV <- input$sld_ABV
    
    ABVs <- scale(df$ABV)
    ABVi <- (ABV-attr(ABVs,'scaled:center'))/attr(ABVs,'scaled:scale')
    SRMs <- scale(df$SRM)
    SRMi <- (SRM-attr(SRMs,'scaled:center'))/attr(SRMs,'scaled:scale')
    IBUs <- scale(df$IBU)
    IBUi <- (IBU-attr(IBUs,'scaled:center'))/attr(IBUs,'scaled:scale')

    i <- sqrt(((ABVs-ABVi)^2)+((IBUs-IBUi)^2)+((SRMs-SRMi)^2))[,1]

    
    df<-df[order(i),]
    df
    
  }
  

    df#[order(df$style),]
    
    
  })
  
  output$df1 <- renderDataTable({
    
    builddf()#[,-c(10:12)]
    
  })
  
  output$df2 <- renderDataTable({
    
    builddf()#[,-c(10:12)]
    
  })
  
  output$df3 <- renderDataTable({
    
    builddf()#[,-c(10:12)]
    
  })
  
  ###Style############
  
  output$ABVplot <- renderPlot({
    
    
    dfsub <- builddf()
    
  
    p <- ggplot(dfsub,aes(y=ABV,x=style,fill=style))+
      geom_boxplot(alpha=0.5) +
      scale_y_continuous(breaks=seq(0,20,1))+
      scale_fill_manual(values = pal(length(unique(dfsub$style))))+
      coord_flip()#+theme(legend.position='none')

    print(p)
    
  })

  output$SRMIBUplot <- renderPlot({
    
    
    dfsub <- builddf()
    
  
    p <- ggplot(dfsub,aes(y=SRM,x=IBU))+
      facet_wrap(~style)+
      #geom_boxplot(alpha=0.5) +
      stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)+
      geom_point()+
      scale_y_log10()+#continuous(breaks=seq(0,20,1))+
      scale_x_log10()+
      coord_equal()

    print(p)
    
  })

  
  output$SRMplot <- renderPlot({
    
    
    dfsub <- builddf()
    

    
    p <- ggplot(dfsub,aes(y=SRM,x=style,color=SRMcol))+
      #geom_boxplot(aes())+
      geom_jitter(size=3,position = position_jitter(width = .1))+
      scale_y_log10(breaks=c((1:10),seq(15,100,5)))+
      scale_color_identity()+#fill_manual(values = pal(length(unique(dfsub$style))))+
      coord_flip()#+theme(legend.position='none')
    
    print(p)
    
  })
  
  output$IBUplot <- renderPlot({
    
    
    dfsub <- builddf()
    
    p <- ggplot(dfsub,aes(y=IBU,x=style,fill=style))+
      scale_fill_manual(values = pal(length(unique(dfsub$style))))+
      geom_boxplot(alpha=0.5) +
      scale_y_log10(breaks=(1:50)*10)+
      coord_flip()#+theme(legend.position='none')
    
    print(p)
    
  })
  
  output$IBUSRMplot <- renderPlot({
    
    
    dfsub <- builddf()
    
    # p <- ggplot(dfsub,aes(y=SRM,x=IBU,color=style))+
#       scale_color_manual(values = pal(length(unique(dfsub$style))))+
#       geom_point(size=3) 
#     # +
#       # scale_y_log10(breaks=(1:50)*10)

      p <- ggplot(dfsub,aes(y=SRM,x=IBU,color=style))+
        # facet_wrap(~style)+
        geom_point(size=2) +
        geom_density2d() + 
        scale_y_log10(breaks=(1:50)*10)+
        scale_x_log10(breaks=(1:50)*10)+
        scale_color_manual(values = pal(length(unique(dfsub$style))))
      # stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)
    
    
        print(p)
    
  })
  
  
  output$OGplot <- renderPlot({
    
    
    dfsub <- builddf()
    

    p <- ggplot(dfsub,aes(y=ABV,x=OG,col=style))+
      scale_color_manual(values = pal(length(unique(dfsub$style))))+
      geom_point(size=4,alpha=0.75)#+
      #stat_smooth(method='lm',se=F)#+
      #theme(legend.position='none')
    
    print(p)
    
  })
  
  output$summaryTble <- renderTable({
    
    df <- builddf() %>% group_by(style) %>% 
      summarise(n=n(),
                mean.OG=mean(OG,na.rm=T),
                mean.FG=mean(FG,na.rm=T),
                mean.ABV=mean(ABV,na.rm=T),
                mean.SRM=mean(SRM,na.rm=T),
                mean.IBU=mean(IBU,na.rm=T))
    
  data.frame(df)
    
  })
  
  output$maltPlot <- renderPlot({
    
    
    df <- builddf()
    
    malt.sum <- tapply(tolower(df$ingredients),df$style,FUN = function(x){
      (sapply(tolower(malts),FUN = function(y) length(grep(y,x)))/length(x))
    }) %>% rbind.data.frame()
    

    malt.sum$malt <- malts
    malt.sum <- melt(malt.sum)
    
    p <- ggplot(malt.sum,aes(x=malt,y=value,fill=malt))+
      geom_bar(stat="identity")+
      coord_flip()+
      scale_fill_manual(values = pal(length(malts)))+
      facet_wrap(~variable)

    print(p)
  
  })
  
  
  output$hopsPlot <- renderPlot({
    
    df <- builddf()
    
    hops.sum <- tapply(tolower(df$ingredients),df$style,FUN = function(x){
      sapply(tolower(hops),FUN = function(y) length(grep(y,x)))/length(x)
    }) %>% rbind.data.frame()
    

    hops.sum$hops <- hops
    hops.sum <- melt(hops.sum)
    
    p <- ggplot(hops.sum,aes(x=hops,y=value,fill=hops))+
      geom_bar(stat="identity")+
      coord_flip()+
      scale_fill_manual(values = pal(length(hops)))+
      facet_wrap(~variable)
    
    print(p)
    
  })
  
  output$GravityPlot <- renderPlot({
    
    df <- builddf()[,c('style','OG','FG')]
    

    df <- melt(df)

    p <- ggplot(df,aes(x=style,y=value,fill=variable))+
      geom_boxplot()+
      scale_fill_manual(values = pal(2))+
      coord_flip()
    
    print(p)
    
  })  
  
  ###Ingredients###############
  
  output$ingPlot <- renderPlot({
    
    df <- builddf()
  
    
    sumdf <- df %>%
      group_by(style) %>%
      summarise(n=n())

    i <- order(sumdf$n,decreasing=T)[1:20]
    p <- ggplot(sumdf[i,],aes(x=style,y=n))+
      geom_bar(stat="identity")+
      coord_flip()
    
  
    print(p)
    
  })
  
  ###Character#################
  

  
  output$summaryTble2 <- renderTable({
    
    df <- builddf() 
    df<-df[,c('name','style','OG','FG','ABV','SRM','IBU')]    

    head(df,10)
    
    
  })  
  output$ABVdensplot <- renderPlot({
    
    #dfsub <- builddf() 

    
    p1 <-p1+geom_vline(xintercept=c(input$sld_ABV))
    p2 <-p2+  geom_histogram(fill=SRMcol(input$sld_SRM))+geom_vline(xintercept=c(input$sld_SRM))
    p3 <-p3+geom_vline(xintercept=c(input$sld_IBU))
       
    print(grid.arrange(p1,p2,p3,nrow=1))
  })  



})