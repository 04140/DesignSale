server = (function(input, output,session) {
  
  observe({

    for (i in 1:nrow(new_old_customer_2016)){
      new_old_customer_2016$new_target_acc[i]<-sum(new_old_customer_2016$new_target[1:i])
      new_old_customer_2016$old_target_acc[i]<-sum(new_old_customer_2016$old_target[1:i])
      revenue_2016$target_acc[i]<-sum(revenue_2016$target[1:i])
    }
    
    customer_rev<-customer_rev_2017
    customer_rev$total_price_after_currency<-as.numeric(customer_rev$total_price_after_currency)
    customer_rev<-dcast(customer_rev, order_created_at ~ new, value.var="total_price_after_currency", fun.aggregate=sum)
    customer_rev$time<-c(1:nrow(customer_rev))
    customer_rev_plot <- melt(customer_rev[,c(2:4)], id=c("time"))
    
    i<-switch(input$month,
               sep = 1,         
               oct = 2,       
               nov = 3,
               dec = 4)
    
    output$new_customer <- renderPlot({
      
      par(mfrow=c(2,1),bg = 'gray19',cex.main=1.3)
      plot.progress <- function(num) {
        if (length(new_old_customer_2016$new_target_acc[i-1])>0){
          begin<-new_old_customer_2016$new_target_acc[i-1]
          end<-new_old_customer_2016$new_target_acc[i]
        }else{
          begin<-0
          end<-new_old_customer_2016$new_target_acc[i]
        }
        if ((num-begin)>=0){
          if ((num-begin)>=(end-begin)){
            num2<-(end-begin)
          }else{
            num2<-(num-begin)
          }
        }else{
          num2<-0
        }
        plot(c(begin,end+100),c(0,5), type='n', xlab='', ylab='', yaxt='n',axes=FALSE)
        axis(1, at=c(0,new_old_customer_2016$new_target_acc), labels=c(0,new_old_customer_2016$new_target_acc),col.axis="white")
        rect(0, 1, num, 5, col='orange',border='White')
        title(paste('Progress: ', round((num2/(end-begin))*100,2), 
                    '%',',',num2,'/',toString((end-begin)), sep=''),col.main = "White")
        text(par("usr")[1] - 1, 5.5, adj = 1, labels = "Monthly", xpd = TRUE,col='White',cex=1.3) 
      }
      new<-c(data.frame(table(customer$new[which(customer$new=='new')]))[1,2])
      plot.progress(new)
      
      plot.progress <- function(num) {
        plot(c(0,new_old_customer_2016$new_target_acc[4]+100),c(0,5), type='n', xlab='', ylab='', yaxt='n',axes=FALSE)
        axis(1, at=c(0,new_old_customer_2016$new_target_acc), labels=c(0,paste(new_old_customer_2016$new_target_acc[1],'\n Sep'),paste(new_old_customer_2016$new_target_acc[2],'\n Oct'),
                                                                       paste(new_old_customer_2016$new_target_acc[3],'\n Nov'),paste(new_old_customer_2016$new_target_acc[4],'\n Dec')),col.axis="white")
        rect(0, 1, num, 5, col='orange',border='White')
        title(paste('Progress: ', round((num/new_old_customer_2016$new_target_acc[4])*100,2), 
                    '%',',',num,'/',toString(new_old_customer_2016$new_target_acc[4]), sep=''),col.main = "White")
        text(par("usr")[1] - 1, 5.5, adj = 1, labels = "Total", xpd = TRUE,col='White',cex=1.5) 
      }
      new<-c(data.frame(table(customer$new[which(customer$new=='new')]))[1,2])
      plot.progress(new)
    })
    
    
    output$old_customer <- renderPlot({
      
      par(mfrow=c(2,1),bg = 'gray19',cex.main=1.3)
      plot.progress <- function(num) {
        if (length(new_old_customer_2016$old_target_acc[i-1])>0){
          begin<-new_old_customer_2016$old_target_acc[i-1]
          end<-new_old_customer_2016$old_target_acc[i]
        }else{
          begin<-0
          end<-new_old_customer_2016$old_target_acc[i]
        }
        if ((num-begin)>=0){
          if ((num-begin)>=(end-begin)){
            num2<-(end-begin)
          }else{
            num2<-(num-begin)
          }
        }else{
          num2<-0
        }
        plot(c(begin,end+100),c(0,5), type='n', xlab='', ylab='', yaxt='n',axes=FALSE)
        axis(1, at=c(0,new_old_customer_2016$old_target_acc), labels=c(0,new_old_customer_2016$old_target_acc),col.axis="white")
        rect(0, 1, num, 5, col='orange',border='White')
        title(paste('Progress: ', round((num2/(end-begin))*100,2), 
                    '%',',',num2,'/',toString((end-begin)), sep=''),col.main = "White")
        text(par("usr")[1] - 1, 5.5, adj = 1, labels = "Monthly", xpd = TRUE,col='White',cex=1.3) 
      }
      old<-c(data.frame(table(customer$new[which(customer$new=='old')]))[1,2])
      plot.progress(old)
      
      plot.progress <- function(num) {
        plot(c(0,new_old_customer_2016$old_target_acc[4]+100),c(0,5), type='n', xlab='', ylab='', yaxt='n',axes=FALSE)
        axis(1, at=c(0,new_old_customer_2016$old_target_acc), labels=c(0,paste(new_old_customer_2016$old_target_acc[1],'\n Sep'),paste(new_old_customer_2016$old_target_acc[2],'\n Oct'),
                                                                       paste(new_old_customer_2016$old_target_acc[3],'\n Nov'),paste(new_old_customer_2016$old_target_acc[4],'\n Dec')),col.axis="white")
        rect(0, 1, num, 5, col='orange',border='White')
        title(paste('Progress: ', round((num/new_old_customer_2016$old_target_acc[4])*100,2), 
                    '%',',',num,'/',toString(new_old_customer_2016$old_target_acc[4]), sep=''),col.main = "White")
        text(par("usr")[1] - 1, 5.5, adj = 1, labels = "Total", xpd = TRUE,col='White',cex=1.5) 
      }
      old<-c(data.frame(table(customer$new[which(customer$new=='old')]))[1,2])
      plot.progress(old)
    })
    
    output$revenue <- renderPlot({
      
      par(mfrow=c(2,1),bg = 'gray19',cex.main=1.3)
      
      plot.progress <- function(num) {
        if (length(revenue_2016$target_acc[i-1])>0){
          begin<-revenue_2016$target_acc[i-1]
          end<-revenue_2016$target_acc[i]
        }else{
          begin<-0
          end<-revenue_2016$target_acc[i]
        }
        if ((num-begin)>=0){
          if ((num-begin)>=(end-begin)){
            num2<-(end-begin)
          }else{
            num2<-(num-begin)
          }
        }else{
          num2<-0
        }
        plot(c(begin,end+100),c(0,5), type='n', xlab='', ylab='', yaxt='n',axes=FALSE)
        axis(1, at=c(0,revenue_2016$target_acc), labels=c(0,revenue_2016$target_acc),col.axis="white")
        rect(0, 1, num, 5, col='orange',border='White')
        title(paste(round((num2/(end-begin))*100,2), 
                    '%',',',toString(format(num2, big.mark=",", scientific=FALSE)),'/',toString(format((end-begin), big.mark=",", scientific=FALSE)), sep=''),col.main = "White")
        text(par("usr")[1] - 1, 5.5, adj = 1, labels = "Monthly", xpd = TRUE,col='White',cex=1.3) 
      }
      old<-revenue[1,2]
      plot.progress(old)
      
      plot.progress <- function(num) {

        plot(c(0,revenue_2016$target_acc[4]+1000),c(0,5), type='n', xlab='', ylab='', yaxt='n',axes=FALSE)
        axis(1, at=c(0,revenue_2016$target_acc), labels=c(0,paste(revenue_2016$target_acc[1],'\n Sep'),paste(revenue_2016$target_acc[2],'\n Oct'),
                                                          paste(revenue_2016$target_acc[3],'\n Nov'),paste(revenue_2016$target_acc[4],'\n Dec')),col.axis="white")
        rect(0, 1, num, 5, col='orange',border='White')
        title(paste(round((num/revenue_2016$target_acc[4])*100,2),
                    '%',',',toString(format(num, big.mark=",", scientific=FALSE)),'/',toString(format(revenue_2016$target_acc[4], big.mark=",", scientific=FALSE)), sep=''),col.main = "White")
        text(par("usr")[1] - 1, 5.5, adj = 1, labels = "Total", xpd = TRUE,col='White',cex=1.5)
      }
      old<-revenue[1,2]
      plot.progress(old)
    })
    output$new_old_customer <- renderPlot({
      par(cex.axis=3,cex.lab=3)
      gg <- ggplot(customer_rev_plot, aes(x=as.numeric(as.character(time)), y=value))
      gg <- gg + geom_area(aes(colour=variable, fill=variable,size=2))
      gg <- gg +　labs(x = NULL, y = NULL)
      gg <- gg + scale_x_discrete(limits=c(1:nrow(customer_rev_plot)),labels=customer_rev$order_created_at)
      gg <- gg + theme(panel.background = element_rect(fill = 'grey', colour = 'grey'),
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.text.x = element_text(colour = "white",size=14),axis.text.y = element_text(colour = "white",size=14),
                       legend.text=element_text(size=14))
      gg <- gg + theme(plot.background = element_rect(fill = 'gray18', colour = 'gray18'))
      gg
    })
    output$text<-renderText({
      paste('$NTD/order number: ',round(sum(customer_rev_plot$value)/length(table(customer$order_id))))
    })
    })
  })