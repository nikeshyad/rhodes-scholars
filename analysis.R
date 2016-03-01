library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)

scholars <- (read.csv('Allison.csv',header = T, stringsAsFactors = FALSE))
scholars[which(scholars$Institution=='Massachusetts Institute of Technology'),] [1] <- 'MIT'
scholars[which(scholars$Institution=='University of North Carolina - Chapel Hill'),] [1] <- 'UNC'
scholars[which(scholars$Institution=='Harvard University'),] [1] <- 'Harvard'
scholars[which(scholars$Institution=='Yale University'),] [1] <- 'Yale'
scholars[which(scholars$Institution=='Stanford University'),] [1] <- 'Stanford'
scholars[which(scholars$Institution=='Princeton University'),] [1] <- 'Princeton'
scholars[which(scholars$Institution=='University of Chicago'),] [1] <- 'UChicago'
scholars[which(scholars$Institution=='Brown University'),] [1] <- 'Brown'
scholars[which(scholars$Institution=='U.S. Naval Academy'),] [1] <- 'USNA'
scholars[which(scholars$Institution=='U.S. Military Academy'),] [1] <- 'USMA'
scholars[which(scholars$Institution=='University of North Carolina'),] [1] <- 'UNC'
scholars[which(scholars$Institution=='Duke University'),] [1] <- 'Duke'
scholars[which(scholars$Institution=='Georgetown University'),] [1] <- 'Georgetown'
scholars[which(scholars$Institution=='University of Pennsylvania'),] [1] <- 'UPenn'
scholars[which(scholars$Institution=='John Hopkins University'),] [1] <- 'John Hopkins'
scholars[which(scholars$Institution=='Northwestern University'),] [1] <- 'Northwestern'
scholars[which(scholars$Institution=='University of Virginia'),] [1] <- 'UVA'

rhodes <- scholars[scholars$Scholarship.Awarded == 'Rhodes',]
marshalls <- scholars[scholars$Scholarship.Awarded == 'Marshalls',]
mitchell <- scholars[scholars$Scholarship.Awarded == 'Mitchell',]

rhodes_institutions <- rhodes$Institution
marshalls_institutions <- marshalls$Institution
mitchell_institutions <- mitchell$Institution

rhodes_institutions <- transform(rhodes_institutions, count = table(rhodes_institutions)[rhodes_institutions])
colnames(rhodes_institutions)[1] <- 'institutions'
rhodes_institutions <- rhodes_institutions[!duplicated(rhodes_institutions),]

marshalls_institutions <- transform(marshalls_institutions, count = table(marshalls_institutions)[marshalls_institutions])
colnames(marshalls_institutions)[1] <- 'institutions'
marshalls_institutions <- marshalls_institutions[!duplicated(marshalls_institutions),]

mitchell_institutions <- transform(mitchell_institutions, count = table(mitchell_institutions)[mitchell_institutions])
colnames(mitchell_institutions)[1] <- 'institutions'
mitchell_institutions <- mitchell_institutions[!duplicated(mitchell_institutions),]

rhodes_institutions <- rhodes_institutions[order(rhodes_institutions$count, decreasing = TRUE),]
marshalls_institutions <- marshalls_institutions[order(marshalls_institutions$count, decreasing = TRUE),]
mitchell_institutions <- mitchell_institutions[order(mitchell_institutions$count, decreasing = TRUE),]

rhodes_top10 <- rhodes_institutions[1:10,]
marshalls_top10 <- marshalls_institutions[1:10,]
mitchell_top10 <- mitchell_institutions[1:10,]

rhodes_top10$institutions <- factor(rhodes_top10$institutions, levels = rhodes_institutions$institutions)
marshalls_top10$institutions <- factor(marshalls_top10$institutions, levels = marshalls_top10$institutions)
mitchell_top10$institutions <- factor(mitchell_top10$institutions, levels = mitchell_top10$institutions)

rhodes_top10$ivy <- c('Ivy','Ivy','Non-Ivy','Ivy','Non-Ivy','Non-Ivy','Ivy','Non-Ivy','Non-Ivy','Non-Ivy')
marshalls_top10$ivy <- c('Ivy','Non-Ivy','Ivy','Non-Ivy','Ivy','Non-Ivy','Non-Ivy','Non-Ivy','Non-Ivy','Ivy')
mitchell_top10$ivy <- c('Non-Ivy','Non-Ivy','Ivy','Non-Ivy','Non-Ivy','Ivy','Non-Ivy','Ivy','Non-Ivy','Non-Ivy')

rhodes_plot <- ggplot(data=rhodes_top10, aes(x=institutions,y=count,fill=ivy))+geom_bar(width=0.8,stat='identity') + ggtitle('Top Ten Rhodes Scholars Winning Institutions: 2005-2015') + xlab('Institutions') + ylab('Frequency') + theme(legend.title = element_blank(), axis.text= element_text(size=20), axis.title = element_text(size=25,face='bold',color='black'),plot.title = element_text(size=25,face='bold',color='black'))
marshalls_plot <- ggplot(data=marshalls_top10, aes(x=institutions,y=count,fill=ivy))+geom_bar(width=0.8,stat='identity') + ggtitle('Top Ten Marshalls Scholars Winning Institutions: 2005-2015') + xlab('Institutions') + ylab('Frequency') + theme(legend.title = element_blank()) + theme(legend.title = element_blank(), axis.text= element_text(size=20), axis.title = element_text(size=25,face='bold',color='black'),plot.title = element_text(size=25,face='bold',color='black'))
mitchell_plot <- ggplot(data=mitchell_top10, aes(x=institutions,y=count,fill=ivy))+geom_bar(width=0.8,stat='identity') + ggtitle('Top Ten Mitchell Scholars Winning Institutions: 2005-2015') + xlab('Institutions') + ylab('Frequency') + theme(legend.title = element_blank()) + theme(legend.title = element_blank(), axis.text= element_text(size=20), axis.title = element_text(size=25,face='bold',color='black'),plot.title = element_text(size=25,face='bold',color='black'))
#grid.arrange(rhodes_plot, marshalls_plot, mitchell_plot, ncol=2)

total_winners_df <- data.frame(type = c('Marshalls','Mitchell','Rhodes'),Ivy = c(85,16,109),IvyLACN= c(2,4,4),LACN = c(11,5,6),Other=c(312,99,226))
total_winners_df.m <- melt(total_winners_df)
stacked_barchart <- ggplot(data=total_winners_df.m, aes(x=type,y=value,fill=variable)) + geom_bar(width = 0.8, stat='identity') + ggtitle('Scholarship Winners Broken Out By Institution Type: 2005-2015') + xlab('Scholarship Type') + ylab('Frequency') + theme(legend.title = element_blank(), axis.text = element_text(size=20), axis.title = element_text(size=25, face='bold',color='black'), plot.title= element_text(size=25,face='bold',color='black')) + scale_fill_manual(values = c("red","yellow",'blue','forestgreen'))
stacked_barchart

