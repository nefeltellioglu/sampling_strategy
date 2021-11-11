#clear all
.rs.restartR()
rm(list = ls())

#libraries
library(fitdistrplus)
library(dplyr)
library(plyr)
library(purrr)
library(ggplot2)

#######
#SET DIRECTORY
#setwd()

##############################
#IMPORTANT
##############################
#please reach the authors about the age-hh dist data (ABC_household_minimal.csv)
#the data is not provided here due to data privacy.
#you won't be able to run all the analysis without the data.
##############################



#################
#READ DATA
#################
{
  age.dist<-read.csv("Data/Age_OneDisease.csv")
  hh.dist<-read.csv("Data/HH_OneDisease.csv")
  age.hh.dist<-read.csv("Data/ABC_household_minimal.csv")
  #adjust last row of hh.dist table
  hh.dist[7:(6+length(sort(unique(age.hh.dist$overall_no[age.hh.dist$overall_no>6])))),]<-data.frame(NoPeople=sort(unique(age.hh.dist$overall_no[age.hh.dist$overall_no>6])), NoHHs=NA)
  all.sizes<-hh.dist$NoHHs[6]
  all.sizes2<-hh.dist$NoPeople
  for (i in 6:nrow(hh.dist)){
    hh.size.this<-hh.dist$NoPeople[i]
    hh.dist$NoHHs[i]<- round(all.sizes*sum(age.hh.dist$overall_no==hh.size.this)/sum(age.hh.dist$overall_no>=6))
  }
  hh.dist$perc<-hh.dist$NoHHs/sum(hh.dist$NoHHs)
  #hist(age.hh.dist$overall_no, breaks = seq(0,max(age.hh.dist$overall_no)))
  #hist(rep(hh.dist$NoPeople,hh.dist$NoHHs),  breaks = seq(0,max(hh.dist$NoPeople)))
  
}

#################
#FIGURES - age- hh size distribution
#################
{ 
  #upload new table 
  {
    #setwd("/Users/ntellioglu/Dropbox/OneDiseaseDocuments")
    #results<-read.csv("Data/OneDiseaseResultsTableFeb16.csv")
    results <- data.frame()
    for (i in c(1:4)){
      cur.results <- read.csv(paste0("Data/ResultsTable_",i,".csv"))
      results<- rbind.fill(results,cur.results )
    }
    #remove rows where school size children size are less than the expected sampling percentage
    #results <- results[!is.na(results$PrevalenceinSample ),]
    results <- results[!duplicated(results[,c('PopulationIndex')]),]
    allhhsizes<-hh.dist$NoPeople
    allhhsizes<-all.sizes2
    rounding.list=c(0.01,0.03,0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
    rounded.perc<-unlist(sapply(1:nrow(results), 
                                function(x) rounding.list[which.min(abs(rounding.list - results$ActualSamplingPercentage[x] ))]
    ))
    #head(rounded.perc,10)
    results$Sampling.Method[results$Sampling.Method=="hh1.s"]<-"Household Sampling"
    results$Sampling.Method[results$Sampling.Method=="school.s"]<-"School Sampling"
    results$Sampling.Method[results$Sampling.Method=="random.s"]<-"Random Sampling"
    
    results$roundedSamplingPercentage<-rounded.perc
    results$PrevalenceRatio<-results$PrevalenceinSample/results$ScabiesPrevalence
    
  }
  p <- c(0.025, 0.5, 0.975)  #95% quantile
  p_names <- map_chr(p, ~paste0(.x*100))
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  hhcolnames<-paste0("NoHHsinHHSize",all.sizes2)
  
  
  
  
  #Compare hh and age histograms
  {
    #hh size histograms
    {
      results1<-results
      hh.dist<-read.csv("Data/HH_OneDisease.csv")
      hh.dist$perc<-hh.dist$NoHHs/sum(hh.dist$NoHHs)
      hh.dist$type<-"ABS"
      hh.dist$perc025<-hh.dist$perc
      hh.dist$perc975<-hh.dist$perc
      hh.dist$NoPeople[6]<-"6+"
      mytable<-hh.dist
      
      #all.sizes2<-hh.dist$NoPeople
      
      #standardization function
      
      hhcolnames<-paste0("NoHHsinHHSize",all.sizes2)
      cols<-c(which(colnames(results1)%in% hhcolnames))
      cols<-cols[order(match(colnames(results1[cols]),hhcolnames))]
      sumsinrows<-rowSums(results1[cols],na.rm =TRUE)
      for (i in 1:length(cols)){
        results1[,cols[i]]<-results1[,cols[i]]/sumsinrows
      }
      colnames<-factor(paste0(all.sizes2), 
                       levels=paste0(all.sizes2))
      results1[cols[6]]<-rowSums(results1[(cols[1]+5):cols[length(cols)]],na.rm=TRUE)
      results1[cols]<-as.matrix(results1[cols])
      mydf1 <- data.frame(group=colnames,
                          mean=colMeans(results1[cols],na.rm =TRUE ), 
                          sd=apply(results1[cols], 2, na.rm =TRUE,sd))
      
      p <- c(0.025, 0.5, 0.975)  #95% quantile
      p_names <- map_chr(p, ~paste0(.x*100))
      p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
        set_names(nm = p_names)
      df <- results1[cols] %>% 
        #group_by(colnames(results1[cols])) %>% 
        summarize_at(vars(colnames(results1[cols])), funs(!!!p_funs))
      mytable2<-data.frame(NoPeople=c(1:6), perc=t(df[,grepl("_50$", colnames(df))])[1:6], 
                           perc025=t(df[,grepl("_2.5$", colnames(df))])[1:6], 
                           perc975=t(df[,grepl("_97.5$", colnames(df))])[1:6], 
                           type="Simulation")
      mytable2$NoPeople[6]<-"6+"
      mytable<-rbind.fill(mytable,mytable2)
      
      p<-ggplot(mytable, aes(x=NoPeople,y=perc*100,fill=type)) + 
        geom_bar(stat="identity", position=position_dodge()) +
        geom_errorbar(aes(ymin=perc025*100, ymax=perc975*100), width=.3,
                      position=position_dodge(.9))+ theme_minimal() + 
        xlab("Household Size") +
        ylab("%") +ylim(c(0,40))+
        #ggtitle(paste0(""))+
        theme(plot.title = element_text(hjust = 0.5),
              axis.title=element_text(size=17),
              axis.text=element_text(size=15),
              legend.text=element_text(size=13),
              legend.title=element_text(size=15))+ 
        guides(fill=guide_legend(title="Dataset"))
      
      p2<-ggplot(mytable, aes(x=NoPeople,y=perc*100, color = as.factor(type))) + 
        geom_point(stat="identity", position=position_dodge(0.7), size = 2) +
        geom_errorbar(aes(ymin=perc025*100, ymax=perc975*100), width=.3, size = 0.7,
                      position=position_dodge(0.7))+ theme_bw() + 
        xlab("Household Size") +
        ylab("%") +ylim(c(0,40))+
        #ggtitle(paste0(""))+
        theme(plot.title = element_text(hjust = 0.5))+ 
        guides(fill=guide_legend(title="Datasets"))+
        labs(colour="Datasets")
      
      ggsave(filename=paste0("Outputs/HHSizeDistribution.pdf"),
             plot=p, width=8, height=4,dpi =300,  units="in", device='pdf')
      ggsave(filename=paste0("Outputs/HHSizeDistribution2.pdf"),
             plot=p2, width=8, height=4,dpi =300,  units="in", device='pdf')
    }
    #age histograms
    {
      sampled.pop<-4000
      age.dist<-read.csv("Data/Age_OneDisease.csv")
      age.dist$AgeGroup[nrow(age.dist)]<-"75–90"
      age.dist2<-data.frame(Age=c(seq(2,72,5), 83), Freq= age.dist$NoPeople)
      #hist(age.dist2$Age, freq = age.dist2$Freq, breaks=c(seq(0,85,5)))
      df.freq= as.vector(rep(age.dist2$Age,age.dist2$Freq))
      #hist(df.freq, main = "Age Distribution in NT Communities",probability =TRUE)
      #assign an age density function using the binned freq table age.dist2
      #with(age.dist2, Freq / sum(Freq))
      dat2 <- unlist(apply(age.dist2, 1, function(x) rep(x[1], x[2])))
      dens <- density(dat2,adjust = 4, from=0, to=95);# bw=5)
      kern.samp = round(sample(dens$x, sampled.pop, replace=TRUE, prob=dens$y))
      #hist(kern.samp, breaks =c(seq(0,95,5)), main = "Age Distribution in a Sample",probability =TRUE)
      #max(kern.samp)
      #assign Adult ages
      mytable<-data.frame(groups=c("Pre-school","School","Adult"),
                          perc=c(sum(kern.samp<5)/length(kern.samp),
                                 1-(sum(kern.samp<5)/length(kern.samp) +sum(kern.samp>=16)/length(kern.samp)),
                                 sum(kern.samp>=16)/length(kern.samp)),
                          perc025=c(sum(kern.samp<5)/length(kern.samp),
                                    1-(sum(kern.samp<5)/length(kern.samp) +sum(kern.samp>=16)/length(kern.samp)),
                                    sum(kern.samp>=16)/length(kern.samp)),
                          perc975 = c(sum(kern.samp<5)/length(kern.samp),
                                      1-(sum(kern.samp<5)/length(kern.samp) +sum(kern.samp>=16)/length(kern.samp)),
                                      sum(kern.samp>=16)/length(kern.samp)),
                          type="Published Data")
      
      
      #age.dist$perc<-age.dist$NoPeople/sum(age.dist$NoPeople)
      
      results1<-results
      agecolnames<-c("PreschoolSize","SchoolSize","AdultSize")
      cols<-c(which(colnames(results1)%in% agecolnames))
      cols<-cols[order(match(colnames(results1[cols]),agecolnames))]
      sumsinrows<-rowSums(results1[cols],na.rm =TRUE)
      for (i in 1:length(cols)){
        results1[,cols[i]]<-results1[,cols[i]]/sumsinrows
      }
      
      mydf1 <- data.frame(groups=c("Pre-school","School","Adult"),
                          mean=colMeans(results1[cols],na.rm =TRUE ), 
                          sd=apply(results1[cols], 2, na.rm =TRUE,sd))
      mydf1
      mytable2<-data.frame(groups=c("Pre-school","School","Adult"), perc=mydf1$mean, sd=mydf1$sd, type="Simulation")
      p <- c(0.025, 0.5, 0.975)  #95% quantile
      p_names <- map_chr(p, ~paste0(.x*100))
      p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
        set_names(nm = p_names)
      df <- results1[cols] %>% 
        #group_by(colnames(results1[cols])) %>% 
        summarize_at(vars(colnames(results1[cols])), funs(!!!p_funs))
      mytable2<-data.frame(groups=c("Pre-school","School","Adult"),
                           perc=t(df[,grepl("_50$", colnames(df))]), 
                           perc025=t(df[,grepl("_2.5$", colnames(df))]), 
                           perc975=t(df[,grepl("_97.5$", colnames(df))]), 
                           type="Simulation")
      
      
      
      mytable<-rbind.fill(mytable,mytable2)
      mytable$groups<-ordered(mytable$groups,levels=c("Pre-school","School","Adult"))
      
      p<- ggplot(mytable, aes(x=groups,y=perc*100,fill=type)) + 
        geom_bar(stat="identity", position=position_dodge()) +
        geom_errorbar(aes(ymin=perc025*100, ymax=perc975*100), width=.3,
                      position=position_dodge(.9))+ theme_minimal() + 
        xlab("Age Group") +
        ylab("%") +ylim(c(0,100))+
        #ggtitle(paste0("Age Distribution"))+
        theme(plot.title = element_text(hjust = 0.5),
              axis.title=element_text(size=17),
              axis.text=element_text(size=15),
              legend.text=element_text(size=13),
              legend.title=element_text(size=15))+ 
        guides(fill=guide_legend(title="Dataset"))
      
      p2<-ggplot(mytable, aes(x=groups,y=perc*100, color = as.factor(type))) + 
        geom_point(stat="identity", position=position_dodge(0.7), size = 1.5) +
        geom_errorbar(aes(ymin=perc025*100, ymax=perc975*100), width=.2, size = 0.6,
                      position=position_dodge(0.7))+ theme_bw() + 
        xlab("Age Groups") +
        ylab("%") +ylim(c(0,100))+
        #ggtitle(paste0("Age Distribution"))+
        theme(plot.title = element_text(hjust = 0.5))+ 
        guides(fill=guide_legend(title="Datasets"))+
        labs(colour="Datasets")
      
      ggsave(filename=paste0("Outputs/AgeDistribution.pdf"),
             plot=p, width=8, height=4,dpi =300,  units="in", device='pdf')
      ggsave(filename=paste0("Outputs/AgeDistribution2.pdf"),
             plot=p2, width=8, height=4,dpi =300,  units="in", device='pdf')
      
    }
    
  }
  
}

#################
#FIGURES - scabies prevalence
#################
{
  #upload new table 
  {
    #setwd("/Users/ntellioglu/Dropbox/OneDiseaseDocuments")
    #results<-read.csv("Data/OneDiseaseResultsTableFeb16.csv")
    results <- data.frame()
    for (i in c(1:4)){
      cur.results <- read.csv(paste0("Data/ResultsTable_",i,".csv"))
      results<- rbind.fill(results,cur.results )
    }
    #remove rows where school size children size are less than the expected sampling percentage
    #results <- results[!is.na(results$PrevalenceinSample ),]
    
    allhhsizes<-hh.dist$NoPeople
    allhhsizes<-all.sizes2
    rounding.list=c(0.01,0.03,0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
    rounded.perc<-unlist(sapply(1:nrow(results), 
                                function(x) rounding.list[which.min(abs(rounding.list - results$ActualSamplingPercentage[x] ))]
    ))
    #head(rounded.perc,10)
    results$Sampling.Method[results$Sampling.Method=="hh1.s"]<-"Household Sampling"
    results$Sampling.Method[results$Sampling.Method=="school.s"]<-"School Sampling"
    results$Sampling.Method[results$Sampling.Method=="random.s"]<-"Random Sampling"
    
    results$roundedSamplingPercentage<-rounded.perc
    results$PrevalenceRatio<-results$PrevalenceinSample/results$ScabiesPrevalence
    
  }
  #age groups
  {
    p <- c(0.025, 0.5, 0.975)  #95% quantile
    p_names <- map_chr(p, ~paste0(.x*100))
    p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
      set_names(nm = p_names)
    
    #overall plot where prevalence is between 20-30%
    prev.method<-unique(results$PrevalenceBasedOn)
    prev.method.names<-c("Random", "High HH-Specific",
                         "Mild HH-Specific", "Age-Specific",
                         "HH- & Age-Specific")
    sum.results<-data.frame()
    for (i in c(1:length(prev.method))){
      results11<-results[results$PrevalenceBasedOn==prev.method[i],]
      
      results1<-results11[results11$ScabiesPrevalence>=0.2 & results11$ScabiesPrevalence<=0.3,]
      
      cols<-c(which(colnames(results1)=="ScabiesPrevalenceinPreschool"),
              which(colnames(results1)=="ScabiesPrevalenceinSchool" ),
              which(colnames(results1)=="ScabiesPrevalenceinAdults"),
              which(colnames(results1)=="ScabiesPrevalence"))
      #cols<-c(which(colnames(results1)%in% mycolnames))
      mycolnames<-c("ScabiesPrevalenceinPreschool","ScabiesPrevalenceinSchool",
                    "ScabiesPrevalenceinAdults","ScabiesPrevalence")
      #cols<-cols[order(match(colnames(results1[cols]),mycolnames))]
      colnames<-factor(c("Pre-School", "School","Adult","Overall"), 
                       levels=c("Pre-School", "School","Adult","Overall"))
      
      results1[cols]<-as.matrix(results1[cols])*100
      #mydf1  <- data.frame(group=colnames,
      #                     mean=colMeans(results1[cols],na.rm =TRUE ), 
      #                     sd=apply(results1[cols], 2, na.rm =TRUE,sd),
      #                     basedon=prev.method.names[i])
      #sum.results<-rbind(sum.results,mydf1)
      df <- results1[cols] %>% 
        #group_by(colnames(results1[cols])) %>% 
        summarize_at(vars(colnames(results1[cols])), funs(!!!p_funs))
      mytable2<-data.frame(group=colnames, perc=t(df[,grepl("_50$", colnames(df))]), 
                           perc025=t(df[,grepl("_2.5$", colnames(df))]), 
                           perc975=t(df[,grepl("_97.5$", colnames(df))]), 
                           basedon=prev.method.names[i])
      sum.results<-rbind(sum.results,mytable2)
      #pdf(paste0("Outputs/AgeGroups_Prevalence_in_Method_",prev.method[i],".pdf"),
      #         width=5, height=2)
      #p <- ggplot(mydf1, aes(x=group,y=mean,fill=group)) + 
      #  geom_bar(stat="identity", position=position_dodge()) +
      #  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
      #                position=position_dodge(.9))+ theme_minimal() + 
      #  xlab("") +
      #  ylab("%") +ylim(c(0,100))+
      #  ggtitle(paste0("Scabies Prevalence 20-30% - Method: ", prev.method.names[i]))+
      #  theme(plot.title = element_text(hjust = 0.5))+ 
      #  guides(fill=guide_legend(title=""))
      #print(p)
      #dev.off()
      #ggsave(filename=paste0("Outputs/AgeGroups_Prevalence_in_Method_",prev.method[i],".pdf"),
      #       plot=p, width=5, height=2.5,dpi =300,  units="in", device='pdf')
    }
    sum.results
    p <- ggplot(sum.results, aes(x=group,y=perc,fill=basedon)) + 
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=perc025, ymax=perc975), width=.3,
                    position=position_dodge(.9))+ theme_minimal() + 
      xlab("") +
      ylab("Prevalence (%)") +ylim(c(0,100))+
      #ggtitle(paste0(""))+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title=element_text(size=17),
            axis.text=element_text(size=15),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15))+ 
      guides(fill=guide_legend(title="Method"))
    #print(p)
    #dev.off()
    ggsave(filename=paste0("Outputs/AgeGroups_Prevalence_All_Methods.pdf"),
           plot=p, width=8, height=4,dpi =300,  units="in", device='pdf')
    ggsave(filename=paste0("Outputs/AgeGroups_Prevalence_All_Methods.png"),
           plot=p, width=8, height=4,dpi =300,  units="in", device='png')
  }
  
  #hh size groups
  {
    #overall plot where prevalence is between 20-30%
    prev.method<-unique(results$PrevalenceBasedOn)
    prev.method.names<-c("Random", "High HH-Specific",
                         "Mild HH-Specific", "Age-Specific",
                         "HH- & Age-Specific")
    sum.results<-data.frame()
    for (i in c(1:length(prev.method))){
      results11<-results[results$PrevalenceBasedOn==prev.method[i],]
      
      results1<-results11[results11$ScabiesPrevalence>=0.2 & results11$ScabiesPrevalence<=0.3,]
      hhcolnames<-paste0("PrevinHHSize",allhhsizes)
      hhsizecolnames<-paste0("NoHHsinHHSize",allhhsizes)
      cols<-c(which(colnames(results1)%in% hhcolnames))
      cols<-cols[order(match(colnames(results1[cols]),hhcolnames))]
      hhsizecols<-c(which(colnames(results1)%in% hhsizecolnames))
      hhsizecols<-hhsizecols[order(match(colnames(results1[hhsizecols]),hhsizecolnames))]
      
      colnames<-factor(paste0(allhhsizes), 
                       levels=paste0(allhhsizes))
      results1[cols]<-as.matrix(results1[cols])*100
      results1["1-2"]<-as.matrix((results1[cols[1]]*results1[hhsizecols[1]]+
                                    results1[cols[2]]*results1[hhsizecols[2]])/
                                   rowSums(results1[hhsizecols[1:2]]))
      results1["3-4"]<-as.matrix((results1[cols[3]]*results1[hhsizecols[3]]+
                                    results1[cols[4]]*results1[hhsizecols[4]])/
                                   rowSums(results1[hhsizecols[3:4]]))
      results1["5-6"]<-as.matrix((results1[cols[5]]*results1[hhsizecols[5]]+
                                    results1[cols[6]]*results1[hhsizecols[6]])/
                                   rowSums(results1[hhsizecols[5:6]]))
      results1["7-8"]<-as.matrix((results1[cols[7]]*results1[hhsizecols[7]]+
                                    results1[cols[8]]*results1[hhsizecols[8]])/
                                   rowSums(results1[hhsizecols[7:8]]))
      results1["9-10"]<-as.matrix((results1[cols[9]]*results1[hhsizecols[9]]+
                                     results1[cols[10]]*results1[hhsizecols[10]])/
                                    rowSums(results1[hhsizecols[9:10]]))
      results1["11+"]<-as.matrix((results1[cols[11]]*results1[hhsizecols[11]]+
                                    results1[cols[12]]*results1[hhsizecols[12]]+
                                    results1[cols[13]]*results1[hhsizecols[13]]+
                                    results1[cols[14]]*results1[hhsizecols[14]]+
                                    results1[cols[15]]*results1[hhsizecols[15]]+
                                    results1[cols[16]]*results1[hhsizecols[16]]+
                                    results1[cols[17]]*results1[hhsizecols[17]]+
                                    results1[cols[18]]*results1[hhsizecols[18]] )/
                                   rowSums(results1[hhsizecols[11:18]]))
      newcolnames<-factor(c("1-2","3-4","5-6","7-8","9-10","11+"),
                          levels=c("1-2","3-4","5-6","7-8","9-10","11+"))
      colnames<-newcolnames
      cols<-c((ncol(results1)-length(newcolnames)+1):ncol(results1))
      #mydf1  <- data.frame(group=colnames,
      #                     mean=colMeans(results1[cols],na.rm =TRUE ), 
      #                     sd=apply(results1[cols], 2, na.rm =TRUE,sd),
      #                     basedon=prev.method.names[i])
      #sum.results<-rbind(sum.results,mydf1)
      df <- results1[cols] %>% 
        #group_by(colnames(results1[cols])) %>% 
        summarize_at(vars(colnames(results1[cols])), funs(!!!p_funs))
      mytable2<-data.frame(group=colnames, perc=t(df[,grepl("_50$", colnames(df))]), 
                           perc025=t(df[,grepl("_2.5$", colnames(df))]), 
                           perc975=t(df[,grepl("_97.5$", colnames(df))]), 
                           basedon=prev.method.names[i])
      sum.results<-rbind(sum.results,mytable2)
      #pdf(paste0("Outputs/AgeGroups_Prevalence_in_Method_",prev.method[i],".pdf"),
      #         width=5, height=2)
      #p <- ggplot(mydf1, aes(x=group,y=mean,fill=group)) + 
      #  geom_bar(stat="identity", position=position_dodge()) +
      #  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
      #                position=position_dodge(.9))+ theme_minimal() + 
      #  xlab("") +
      #  ylab("%") +ylim(c(0,75))+
      #  ggtitle(paste0("Scabies Prevalence 20-30% - Method: ", prev.method.names[i]))+
      #  theme(plot.title = element_text(hjust = 0.5))+ 
      #  guides(fill=FALSE)
      #print(p)
      #dev.off()
      #ggsave(filename=paste0("Outputs/HHSizeGroups_Prevalence_in_Method_",prev.method[i],".pdf"),
      #       plot=p, width=8, height=5, units="in")
    }
    sum.results
    p <- ggplot(sum.results, aes(x=group,y=perc,fill=basedon)) + 
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=perc025, ymax=perc975), width=.3,
                    position=position_dodge(.9))+ theme_minimal() + 
      xlab("Household Size") +
      ylab("Prevalence (%)") +ylim(c(0,75))+
      #ggtitle(paste0("Scabies Prevalence"))+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title=element_text(size=17),
            axis.text=element_text(size=15),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15)) + 
      guides(fill=guide_legend(title="Method"))
    #print(p)
    #dev.off()
    ggsave(filename=paste0("Outputs/HHsizeGroups_Prevalence_All_Methods.pdf"),
           plot=p, width=8, height=4,dpi =300,  units="in", device='pdf')
    
    ggsave(filename=paste0("Outputs/HHsizeGroups_Prevalence_All_Methods.png"),
           plot=p, width=8, height=4,dpi =300,  units="in",  device='png')
    
  }
  
  
  #hh with different no of cases
  {
    #overall plot where prevalence is between 20-30%
    prev.method<-unique(results$PrevalenceBasedOn)
    prev.method.names<-c("Random", "High HH-Specific",
                         "Mild HH-Specific", "Age-Specific",
                         "HH- & Age-Specific")
    
    sum.results<-data.frame()
    plot.list<- list()
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    colors1 <- gg_color_hue(5)
    colors <- c(colors1[5],colors1[3],colors1[4],colors1[1],colors1[2] )
    for (i in c(1:length(prev.method))){
      results11<-results[results$PrevalenceBasedOn==prev.method[i],]
      
      results1<-results11[results11$ScabiesPrevalence>=0.2 & results11$ScabiesPrevalence<=0.3,]
      results1$PercHHswithonecase <- results1$PercHHswith.atleastonecase - results1$PercHHswith.atleasttwocase
      hhcolnames<-c("PercHHswithzerocase", 
                    "PercHHswithonecase",
                    "PercHHswithtwocase",
                    "PercHHswiththreecase",
                    "PercHHswithfourcase",
                    "PercHHswithatleastfivecase")
      cols<-c(which(colnames(results1)%in% hhcolnames))
      cols<-cols[order(match(colnames(results1[cols]),hhcolnames))]
      colnames<-factor(c("0",
                         "1",
                         "2",
                         "3",
                         "4",
                         "5+"), 
                       levels=c("0",
                                "1",
                                "2",
                                "3",
                                "4",
                                "5+"))
      results1[cols]<-as.matrix(results1[cols])*100
      df <- results1[cols] %>% 
        #group_by(colnames(results1[cols])) %>% 
        summarize_at(vars(colnames(results1[cols])), funs(!!!p_funs))
      mytable2 <- data.frame(group=(colnames), 
                             perc=t(df[,grepl("_50$", colnames(df))]), 
                             perc025=t(df[,grepl("_2.5$", colnames(df))]), 
                             perc975=t(df[,grepl("_97.5$", colnames(df))]), 
                             basedon=prev.method.names[i])
      sum.results <- rbind(sum.results,mytable2)
      
      plot.list[[i]] <- ggplot(mytable2, aes(x=group,y=perc)) + 
        geom_bar(color="black",position=position_dodge(0.9), width=1, #inwidth=0,
          stat="identity", fill = colors[i]) +
        geom_errorbar(aes(ymin=perc025, ymax = perc975), width=.3,
                      position=position_dodge(.9))+ theme_minimal() + 
        xlab("Number of Cases") +
        ylab("% of Households") +ylim(c(0,100))+
        ggtitle(paste0(prev.method.names[i],collapse = "")) +
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold"),
              axis.title=element_text(size=17),
              axis.text=element_text(size=15),
              legend.text=element_text(size=13),
              legend.title=element_text(size=15)) +  
        guides(fill=guide_legend(title="Method"))
      #mydf1  <- data.frame(group=colnames,
      #                     mean=colMeans(results1[cols],na.rm =TRUE ), 
      #                     sd=apply(results1[cols], 2, na.rm =TRUE,sd),
      #                     basedon=prev.method.names[i])
      #sum.results<-rbind(sum.results,mydf1)
      #pdf(paste0("Outputs/AgeGroups_Prevalence_in_Method_",prev.method[i],".pdf"),
      #         width=5, height=2)
      #p <- ggplot(mydf1, aes(x=group,y=mean,fill=group)) + 
      #  geom_bar(stat="identity", position=position_dodge()) +
      #  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
      #                position=position_dodge(.9))+ theme_minimal() + 
      #  xlab("Households") +
      #  ylab("%") +ylim(c(0,100))+
      #  ggtitle(paste0("Scabies Prevalence - Method: ", prev.method.names[i]))+
      #  theme(plot.title = element_text(hjust = 0.5))+ 
      #  guides(fill=FALSE)
      #print(p)
      #dev.off()
      #ggsave(filename=paste0("Outputs/HHGroups_Prevalence_in_Method_",prev.method[i],".pdf"),
      #       plot=p, width=8, height=5, units="in")
    }
    sum.results
    
    p2 <- ggplot(sum.results, aes(x=group,y=perc,color=basedon, group=basedon)) + 
      geom_line() +
      geom_errorbar(aes(ymin=perc025, ymax = perc975,color=basedon), width=.3,
                    position=position_dodge(0))+ theme_minimal() + 
      xlab("Number of Cases") +
      ylab("% of Households") +ylim(c(0,100))+
      #ggtitle(paste0("Scabies Prevalence"))+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title=element_text(size=17),
            axis.text=element_text(size=15),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15)) +  
      guides(color=guide_legend(title="Method"))
    
    library(ggplot2)
    #library(gridExtra)
    library(ggpubr)
    
    p <- ggplot(sum.results, aes(x=group,y=perc,fill=basedon)) + 
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=perc025, ymax = perc975), width=.3,
                    position=position_dodge(.9))+ theme_minimal() + 
      xlab("Number of Cases") +
      ylab("% of Households") +ylim(c(0,100))+
      #ggtitle(paste0("Scabies Prevalence"))+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title=element_text(size=17),
            axis.text=element_text(size=15),
            legend.text=element_text(size=20),
            legend.title=element_text(size=20),
            #legend.spacing.x = unit(2.0, 'cm'),
            legend.key.size = unit(1.5, "cm")) +  
      guides(fill=guide_legend(title="Method"))
    
    
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}
    mylegend<-g_legend(p)
    
    p3<- ggarrange(plotlist = list(plot.list[[1]] + theme(legend.position="none"),
                                   plot.list[[2]] + theme(legend.position="none"),
                                   plot.list[[3]] + theme(legend.position="none"),
                                   plot.list[[4]] + theme(legend.position="none"),
                                   plot.list[[5]] + theme(legend.position="none"),
                                   mylegend),#heights=c(10, 1)
                   nrow=3,ncol=2,labels=c("a", "b","c","d","e",""),
                   font.label = list(size = 25, color = "black", face = "bold", family = NULL),
                   vjust = 1)
    
    #print(p)
    #dev.off()
    p <- ggplot(sum.results, aes(x=group,y=perc,fill=basedon)) + 
      geom_bar(stat="identity", position=position_dodge()) +
      geom_errorbar(aes(ymin=perc025, ymax = perc975), width=.3,
                    position=position_dodge(.9))+ theme_minimal() + 
      xlab("Number of Cases") +
      ylab("% of Households") +ylim(c(0,100))+
      #ggtitle(paste0("Scabies Prevalence"))+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title=element_text(size=17),
            axis.text=element_text(size=15),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15)) +  
      guides(fill=guide_legend(title="Method"))
    p
    ggsave(filename=paste0("Outputs/HHGroups_Prevalence_All_Methodsv3.pdf"),
           plot=p, width=8, height=4,dpi =300,  units="in", device='pdf')
    ggsave(filename=paste0("Outputs/HHGroups_Prevalence_All_Methodsv4.pdf"),
           plot=p2, width=8, height=4,dpi =300,  units="in", device='pdf')
    ggsave(filename=paste0("Outputs/HHGroups_Prevalence_All_Methodsv5.pdf"),
           plot=p3, width=14, height=15,dpi =300,  units="in", device='pdf')
    ggsave(filename=paste0("Outputs/HHGroups_Prevalence_All_Methodsv2.png"),
           plot=p, width=8, height=4,dpi =300,  units="in", device='png')
    
    
    
  }
  
  
  #Different sampling percentages: sampling prevalence with different strategies
  `%notin%` <- Negate(`%in%`)
  {
    bp.vals <- function(x, probs=c(0.025, 0.025,0.5, 0.975, 0.975)) {
      r <- quantile(x, probs=probs , na.rm=TRUE)
      #r = c(r[1:2], exp(mean(log(x))), r[3:4])
      r = r
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      r
    }
    #overall plot where prevalence is between 20-30%
    prev.method<- ordered(unique(results$PrevalenceBasedOn), 
                          levels = c("random", "hh1",    "hh2",    "age" ,   "hh-age"))
     
    prev.method.names<-c("Random", "High HH-Specific",
                         "Mild HH-Specific", "Age-Specific",
                         "HH- & Age-Specific")
    sum.results<-data.frame()
    plot.list<-list()
    for (i in c(1:length(prev.method))){
      results11<-results[results$PrevalenceBasedOn==prev.method[i],]
      
      results1<-results11[results11$ScabiesPrevalence>=0.2 & results11$ScabiesPrevalence<=0.3,]
      mycolnames<-c("PrevalenceinpreschoolSample",
                    "PrevalenceinSchoolSample", 
                    "PrevalenceinAdultSample",
                    "PrevalenceinSample")
      results1$PrevalenceinSample<-results1$PrevalenceinSample*100
      results1<-results1[results1$roundedSamplingPercentage %notin% c(0.01,0.03,1),]
      #results1<-results1[results1$roundedSamplingPercentage %notin% c(0.01,0.03,0.6,0.7,0.8,0.9,1),]
      plot.list[[i]]<-ggplot(results1, 
                aes(x=as.factor(roundedSamplingPercentage*100), 
                    y=PrevalenceinSample, color=Sampling.Method)) + 
        #geom_boxplot(outlier.size =0)+ theme_minimal() + 
        #stat_summary( aes(fill=Sampling.Method), position=position_dodge(.9), fun.data=bp.vals, geom="point")+
        stat_summary(fun=median,#base::mean, 
                     position=position_dodge(.7),
                     geom="point", size=2) + 
        stat_summary( aes(color=Sampling.Method), position=position_dodge(.7), 
                      fun.data=bp.vals, geom="errorbar",size=1, width=0.7)+ theme_minimal() + 
        xlab("\nSampling % of the Population") +
        ylab("Observed Prevalence (%)") +
        ggtitle(paste0(prev.method.names[i],collapse = "")) +
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold"),
              axis.title=element_text(size=17),
              axis.text=element_text(size=15),
              legend.text=element_text(size=20),
              legend.title=element_text(size=20),
              #legend.spacing.x = unit(2.0, 'cm'),
              legend.key.size = unit(1.5, "cm")) + ylim(0,100) +
        geom_hline(yintercept=20,linetype="dashed",  color = "red")+
        geom_hline(yintercept=30,linetype="dashed",  color = "red")+
        
        guides(color=guide_legend(title="Sampling Method"))
      
      #ggsave(filename=paste0("Outputs/Actual_Prevalence_in_Samplings_in_Method_",prev.method[i],".pdf"),
      #       plot=p, width=8, height=5, units="in")
      
      #ggsave(filename=paste0("Outputs/Actual_Prevalence_in_Samplings_in_Method_",prev.method[i],".png"),
      #       plot=p, width=5, height=2.5,dpi =130,  units="in", device='png')
      
    }
    results1<-results[results$ScabiesPrevalence>=0.2 & results$ScabiesPrevalence<=0.3,]
    results1$PrevalenceinSample<-results1$PrevalenceinSample*100
    results1<-results1[results1$roundedSamplingPercentage %notin% c(0.01,0.03,1),]
    
    library(ggplot2)
    library(gridExtra)
    library(ggpubr)
    
    #extract legend
    #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}
    mylegend<-g_legend(plot.list[[1]])
     
    p3<- ggarrange(plotlist = list(plot.list[[1]] + theme(legend.position="none"),
                             plot.list[[2]] + theme(legend.position="none"),
                             plot.list[[3]] + theme(legend.position="none"),
                             plot.list[[4]] + theme(legend.position="none"),
                             plot.list[[5]] + theme(legend.position="none"),
                             mylegend),#heights=c(10, 1)
              nrow=3,ncol=2,labels=c("a", "b","c","d","e",""),
              font.label = list(size = 25, color = "black", face = "bold", family = NULL),
              vjust = 1
      )
    #ncol=1,
    #heights=c(10, 1))
    
    ggsave(filename=paste0("Outputs/Actual_Prevalence_inSamplings_separate_Methods.pdf"),
           plot=p3, width=14, height=15,dpi =300,  units="in", device='pdf')
    
    
    p<-ggplot(results1, 
              aes(x=as.factor(roundedSamplingPercentage*100), 
                  y=PrevalenceinSample, color=Sampling.Method)) + 
      #geom_boxplot(outlier.size =0)+ theme_minimal() + 
      #stat_summary( aes(fill=Sampling.Method), position=position_dodge(.9), fun.data=bp.vals, geom="point")+
      stat_summary(fun=median, 
                   position=position_dodge(.7),
                   geom="point", size=2) + 
      stat_summary( aes(color=Sampling.Method), position=position_dodge(.7), 
                    fun.data=bp.vals, geom="errorbar",size=1, width=0.7)+ theme_minimal() + 
      xlab("\nSampling % of the Population") +
      ylab("Observed Prevalence (%)") +
      #ggtitle(paste0("Actual Scabies Prevalence 20-30% ",collapse = "")) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title=element_text(size=17),
            axis.text=element_text(size=15),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15)) + ylim(0,100) +
      geom_hline(yintercept=20,linetype="dashed",  color = "red")+
      geom_hline(yintercept=30,linetype="dashed",  color = "red")+
      guides(color=guide_legend(title="Sampling Method"))
    #print(p)
    #dev.off()
    ggsave(filename=paste0("Outputs/Actual_Prevalence_inSamplings_All_Methods.pdf"),
           plot=p, width=8, height=5,dpi =300,  units="in", device='pdf')
    
    ggsave(filename=paste0("Outputs/Actual_Prevalence_inSamplings_All_Methods.png"),
           plot=p, width=8, height=5,dpi =300,  units="in", device='png')
    
  }
  
  #Diffrent assignment: sampling prevalence with different strategies
  {
    bp.vals <- function(x, probs=c(0.025, 0.025,0.5, 0.975, 0.975)) {
      r <- quantile(x, probs=probs , na.rm=TRUE)
      #r = c(r[1:2], exp(mean(log(x))), r[3:4])
      r = r
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      r
    }
    #overall plot where prevalence is between 20-30%
    prev.method<-unique(results$PrevalenceBasedOn)
    prev.method.names<-c("Random", "High HH-Specific",
                         "Mild HH-Specific", "Age-Specific",
                         "HH- & Age-Specific")
    sum.results<-data.frame()
    for (i in c(1:length(prev.method))){
      results11<-results[results$PrevalenceBasedOn==prev.method[i],]
      
      results1<-results11[results11$ScabiesPrevalence>=0.2 & results11$ScabiesPrevalence<=0.3,]
      results1<-results1[results1$roundedSamplingPercentage>=0.2 & results1$roundedSamplingPercentage<=0.3,]
      mycolnames<-c("PrevalenceinpreschoolSample",
                    "PrevalenceinSchoolSample", 
                    "PrevalenceinAdultSample",
                    "PrevalenceinSample")
      results1$PrevalenceinSample<-results1$PrevalenceinSample*100
      p<-ggplot(results1, 
                aes(x=Sampling.Method, 
                    y=PrevalenceinSample, color=Sampling.Method)) + 
        #geom_boxplot(outlier.size =0)+ theme_minimal() + 
        #stat_summary( aes(fill=Sampling.Method), position=position_dodge(.9), fun.data=bp.vals, geom="point")+
        stat_summary(fun=base::mean, 
                     position=position_dodge(.9),
                     geom="point", size=2) + 
        stat_summary( aes(fill=Sampling.Method), position=position_dodge(.9), 
                      fun.data=bp.vals, geom="errorbar",size=1)+ theme_minimal() + 
        xlab("Sampling Method") +
        ylab("Observed % Prevalence") +
        ggtitle(paste0("Actual Scabies Prevalence 20-30% - Method: ", prev.method.names[i],collapse = "")) +
        theme(plot.title = element_text(hjust = 0.5))+#ylim(0,6) +
        geom_hline(yintercept=20,linetype="dashed",  color = "red")+
        geom_hline(yintercept=30,linetype="dashed",  color = "red")
      
      #ggsave(filename=paste0("Outputs/Actual_Prevalence_givenSamplingSize020_in_Samplings_in_Method_",prev.method[i],".pdf"),
      #       plot=p, width=8, height=5, units="in")
    }
    results1<-results[results$ScabiesPrevalence>=0.2 & results$ScabiesPrevalence<=0.3,]
    results1$PrevalenceinSample<-results1$PrevalenceinSample*100
    results1<-results1[results1$roundedSamplingPercentage>=0.2 & results1$roundedSamplingPercentage<=0.3,]
    
    p<-ggplot(results1, 
              aes(x=PrevalenceBasedOn, 
                  y=PrevalenceinSample, color=Sampling.Method)) + 
      #geom_boxplot(outlier.size =0)+ theme_minimal() + 
      #stat_summary( aes(fill=Sampling.Method), position=position_dodge(.9), fun.data=bp.vals, geom="point")+
      stat_summary(fun=median, 
                   position=position_dodge(.5),
                   geom="point", size=2) + 
      stat_summary( aes(color=Sampling.Method), position=position_dodge(.5), 
                    fun.data=bp.vals, geom="errorbar",size=1, width= 0.5)+ theme_minimal() + 
      xlab("\nScabies Assignment Method") +
      ylab("\nObserved Prevalence %") +
      #ggtitle(paste0("Actual Scabies Prevalence 20-30% ",collapse = "")) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title=element_text(size=17),
            axis.text=element_text(size=15),
            legend.text=element_text(size=13),
            legend.title=element_text(size=15)) +  
      geom_hline(yintercept=20,linetype="dashed",  color = "red")+
      geom_hline(yintercept=30,linetype="dashed",  color = "red")+
      scale_x_discrete(labels = c('Age','HH & Age','High HH', "Mild HH",
                                  "Random"))+#,expand=c(0, 2))+
      guides(color=guide_legend(title="Sampling Method"))
    
    
    #print(p)
    #dev.off()
    ggsave(filename=paste0("Outputs/Actual_Prevalence_inSamplings_GivenSamplingSizes025__All_Methods.pdf"),
           plot=p, width=9, height=4,dpi =300,  units="in", device='pdf')
    ggsave(filename=paste0("Outputs/Actual_Prevalence_inSamplings_GivenSamplingSizes025__All_Methods.png"),
           plot=p, width=8, height=4,dpi =300,  units="in",device='png')
    
  } 
  
  #4 plots with different assignment methods: 
  `%notin%` <- Negate(`%in%`)
  {
    bp.vals <- function(x, probs=c(0.025, 0.025,0.5, 0.975, 0.975)) {
      r <- quantile(x, probs=probs , na.rm=TRUE)
      #r = c(r[1:2], exp(mean(log(x))), r[3:4])
      r = r
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      r
    }
    #overall plot where prevalence is between 20-30%
    prev.method<- ordered(unique(results$PrevalenceBasedOn), 
                          levels = c("random", "hh1",    "hh2",    "age" ,   "hh-age"))
    
    prev.method.names<-c("Random", "High HH-Specific",
                         "Mild HH-Specific", "Age-Specific",
                         "HH- & Age-Specific")
    sum.results<-data.frame()
    plot.list<-list()
    selected.prev.list <- c(0.05,0.1,0.2,0.4)
    for (i in c(1:length(selected.prev.list))){
      #results11<-results[results$PrevalenceBasedOn==prev.method[i],]
      results1 <- results[results$ScabiesPrevalence==selected.prev.list[i],]
      results1 <- results1[results1$roundedSamplingPercentage == 0.2,]
      results1$PrevalenceinSample<-results1$PrevalenceinSample*100
      #results1<-results1[results1$roundedSamplingPercentage %notin% c(0.01,0.03,0.6,0.7,0.8,0.9,1),]
      plot.list[[i]] <- ggplot(results1, 
                             aes(x=PrevalenceBasedOn, 
                                 y=PrevalenceinSample, color=Sampling.Method)) + 
        #geom_boxplot(outlier.size =0)+ theme_minimal() + 
        #stat_summary( aes(fill=Sampling.Method), position=position_dodge(.9), fun.data=bp.vals, geom="point")+
        stat_summary(fun=base::mean, 
                     position=position_dodge(.5),
                     geom="point", size=2) + 
        stat_summary( aes(color=Sampling.Method), position=position_dodge(.5), 
                      fun.data=bp.vals, geom="errorbar",size=1, width= 0.5)+ theme_minimal() + 
        xlab("\nScabies Assignment Method") +
        ylab("Observed Prevalence (%)") + #ylim(0,100)+
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold"),
              axis.title.y=element_text(size=17),
              axis.title.x=element_text(size=19),
              axis.text=element_text(size=18),
              legend.text=element_text(size=20),
              legend.title=element_text(size=20),
              #legend.spacing.x = unit(2.0, 'cm'),
              legend.key.size = unit(1.2, "cm")) +
        geom_hline(yintercept=selected.prev.list[i]*100,linetype="dashed",  color = "red") +
        scale_x_discrete(labels = c('Age','HH & Age','High HH', "Mild HH", "Random")) +
        guides(color=guide_legend(title="Sampling Method"))
      
      #ggsave(filename=paste0("Outputs/Actual_Prevalence_in_Samplings_in_Method_",prev.method[i],".pdf"),
      #       plot=p, width=8, height=5, units="in")
      
      #ggsave(filename=paste0("Outputs/Actual_Prevalence_in_Samplings_in_Method_",prev.method[i],".png"),
      #       plot=p, width=5, height=2.5,dpi =130,  units="in", device='png')
      
    }
    
    library(ggplot2)
    library(gridExtra)
    library(ggpubr)
    
    
    p3<- ggarrange(plotlist = list(plot.list[[1]],
                                   plot.list[[2]],
                                   plot.list[[3]],
                                   plot.list[[4]]),
                   nrow=2,ncol=2,labels=c("a", "b","c","d",""),
                   font.label = list(size = 25, color = "black", face = "bold", family = NULL),
                   vjust = 1,hjust = -0.2,widths = c(0.8,0.8,0.8,0.8),heights = c(0.8,0.8,0.8,0.8),
                   common.legend = TRUE, legend = "bottom" )
    #ncol=1,
    #heights=c(10, 1))
    
    ggsave(filename=paste0("Outputs/Four_subplots_actual_20.pdf"),
           plot=p3, width=15, height=10,dpi =300,  units="in", device='pdf')
    
  }
  
  #4 plots with different sampling percentages: 
  `%notin%` <- Negate(`%in%`)
  {
    bp.vals <- function(x, probs=c(0.025, 0.025,0.5, 0.975, 0.975)) {
      r <- quantile(x, probs=probs , na.rm=TRUE)
      #r = c(r[1:2], exp(mean(log(x))), r[3:4])
      r = r
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      r
    }
    #overall plot where prevalence is between 20-30%
    prev.method<- ordered(unique(results$PrevalenceBasedOn), 
                          levels = c("random", "hh1",    "hh2",    "age" ,   "hh-age"))
    
    prev.method.names<-c("Random", "High HH-Specific",
                         "Mild HH-Specific", "Age-Specific",
                         "HH- & Age-Specific")
    sum.results<-data.frame()
    plot.list<-list()
    selected.prev.list <- c(0.05,0.1,0.2,0.4)
    for (i in c(1:length(selected.prev.list))){
      #results11<-results[results$PrevalenceBasedOn==prev.method[i],]
      results1 <- results[results$ScabiesPrevalence==selected.prev.list[i],]
      #results1 <- results1[results1$roundedSamplingPercentage == 0.2,]
      results1$PrevalenceinSample<-results1$PrevalenceinSample*100
      results1<-results1[results1$roundedSamplingPercentage %notin% c(0.01,0.03,1),]
      plot.list[[i]] <- ggplot(results1,
                               aes(x=as.factor(roundedSamplingPercentage*100), 
                                   y=PrevalenceinSample, color=Sampling.Method)) +
                               #aes(x=roundedSamplingPercentage, 
                              #     y=PrevalenceinSample, color=Sampling.Method)) + 
        #geom_boxplot(outlier.size =0)+ theme_minimal() + 
        #stat_summary( aes(fill=Sampling.Method), position=position_dodge(.9), fun.data=bp.vals, geom="point")+
        stat_summary(fun=base::mean, 
                     position=position_dodge(.7),
                     geom="point", size=2) + 
        stat_summary( aes(color=Sampling.Method), position=position_dodge(.7), 
                      fun.data=bp.vals, geom="errorbar",size=1,width=0.7)+ theme_minimal() + 
        xlab("\nSampling % of the Population") +
        ylab("Observed Prevalence (%)") + #ylim(0,100)+
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold"),
              axis.title.y=element_text(size=17),
              axis.title.x=element_text(size=19),
              axis.text=element_text(size=18),
              legend.text=element_text(size=20),
              legend.title=element_text(size=20),
              #legend.spacing.x = unit(2.0, 'cm'),
              legend.key.size = unit(1.2, "cm")) +
        geom_hline(yintercept=selected.prev.list[i]*100,linetype="dashed",  color = "red") +
        guides(color=guide_legend(title="Sampling Method"))
      
      #ggsave(filename=paste0("Outputs/Actual_Prevalence_in_Samplings_in_Method_",prev.method[i],".pdf"),
      #       plot=p, width=8, height=5, units="in")
      
      #ggsave(filename=paste0("Outputs/Actual_Prevalence_in_Samplings_in_Method_",prev.method[i],".png"),
      #       plot=p, width=5, height=2.5,dpi =130,  units="in", device='png')
      
    }
    
    library(ggplot2)
    library(gridExtra)
    library(ggpubr)
    
    
    p3<- ggarrange(plotlist = list(plot.list[[1]],
                                   plot.list[[2]],
                                   plot.list[[3]],
                                   plot.list[[4]]),
                   nrow=2,ncol=2,labels=c("a", "b","c","d",""),
                   font.label = list(size = 25, color = "black", face = "bold", family = NULL),
                   vjust = 1,hjust = -0.2,#widths = c(0.8,0.8,0.8,0.8),heights = c(0.8,0.8,0.8,0.8),
                   common.legend = TRUE, legend = "bottom" )
    #ncol=1,
    #heights=c(10, 1))
    
    ggsave(filename=paste0("Outputs/Four_subplots_actual_20_sampling_prev.pdf"),
           plot=p3, width=14, height=10,dpi =300,  units="in", device='pdf')
    
  }
  
  #3 plots with different pop sizes sampling percentages: 
  `%notin%` <- Negate(`%in%`)
  {
    bp.vals <- function(x, probs=c(0.025, 0.025,0.5, 0.975, 0.975)) {
      r <- quantile(x, probs=probs , na.rm=TRUE)
      #r = c(r[1:2], exp(mean(log(x))), r[3:4])
      r = r
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      r
    }
    #overall plot where prevalence is between 20-30%
    prev.method<- ordered(unique(results$PrevalenceBasedOn), 
                          levels = c("random", "hh1",    "hh2",    "age" ,   "hh-age"))
    
    prev.method.names<-c("Random", "High HH-Specific",
                         "Mild HH-Specific", "Age-Specific",
                         "HH- & Age-Specific")
    sum.results<-data.frame()
    plot.list<-list()
    given.popsizes.list<-c()#as.list(unique(results$PopulationSize))
    given.popsizes.list[[1]]<-c(500,1500)
    given.popsizes.list[[2]]<-c(1501,2500)
    given.popsizes.list[[3]]<-c(2501,4000)
    plot.titles <- c("Small", "Medium", "Large")
    selected.prev <- c(0.2,0.3)
    for (i in c(1:length(given.popsizes.list))){
      pop.limits <- given.popsizes.list[[i]]
      results1 <- results[results$ScabiesPrevalence>= selected.prev[1] &
                            results$ScabiesPrevalence<= selected.prev[2] &
                            results$PopulationSize >=pop.limits[1] &
                            results$PopulationSize <=pop.limits[2],]
      #results1 <- results1[results1$roundedSamplingPercentage == 0.2,]
      results1$PrevalenceinSample<-results1$PrevalenceinSample*100
      results1<-results1[results1$roundedSamplingPercentage %notin% c(0.01,0.03,1),]
      plot.list[[i]] <- ggplot(results1,
                               aes(x=as.factor(roundedSamplingPercentage*100), 
                                   y=PrevalenceinSample, color=Sampling.Method)) +
        #aes(x=roundedSamplingPercentage, 
        #     y=PrevalenceinSample, color=Sampling.Method)) + 
        #geom_boxplot(outlier.size =0)+ theme_minimal() + 
        #stat_summary( aes(fill=Sampling.Method), position=position_dodge(.9), fun.data=bp.vals, geom="point")+
        stat_summary(fun=median, 
                     position=position_dodge(.9),
                     geom="point", size=2) + 
        stat_summary( aes(color=Sampling.Method), position=position_dodge(.9), 
                      fun.data=bp.vals, geom="errorbar",size=1)+ theme_minimal() + 
        coord_cartesian(ylim = c(0, 65)) +
        ggtitle(plot.titles[i]) +
        xlab("\nSampling % of the Population") +
        ylab("Observed Prevalence (%)") + #ylim(0,100)+
        theme(plot.title = element_text(size=20,hjust = 0.5,face = "bold"),
              axis.title.y=element_text(size=17),
              axis.title.x=element_text(size=19),
              axis.text=element_text(size=18),
              legend.text=element_text(size=20),
              legend.title=element_text(size=20),
              #legend.spacing.x = unit(2.0, 'cm'),
              legend.key.size = unit(1.2, "cm")) +
        geom_hline(yintercept=selected.prev * 100,linetype="dashed",  color = "red") +
        guides(color=guide_legend(title="Sampling Method"))
      
      #ggsave(filename=paste0("Outputs/Actual_Prevalence_in_Samplings_in_Method_",prev.method[i],".pdf"),
      #       plot=p, width=8, height=5, units="in")
      
      #ggsave(filename=paste0("Outputs/Actual_Prevalence_in_Samplings_in_Method_",prev.method[i],".png"),
      #       plot=p, width=5, height=2.5,dpi =130,  units="in", device='png')
      
    }
    
    library(ggplot2)
    library(gridExtra)
    library(ggpubr)
    
    
    p3<- ggarrange(plotlist = list(plot.list[[1]],
                                plot.list[[2]],
                                plot.list[[3]]),
                nrow=1,ncol=3,labels=c("a", "b","c"),
                font.label = list(size = 25, color = "black", face = "bold", family = NULL),
                vjust = 1,hjust = -0.2,
                common.legend = TRUE, legend = "bottom" )
    
  
    #ncol=1,
    #heights=c(10, 1))
    
    ggsave(filename=paste0("Outputs/Three_subplots_actual_20_sampling_prev.pdf"),
           plot=p3, width=18, height=5,dpi =300,  units="in", device='pdf')
    
  }
  
  
  
}

#################
#TABLES
#################
{
  #all results tables
  {
    #upload new table 
    {  results <- data.frame()
    for (m in c(1:4)){
      cur.results <- read.csv(paste0("Data/ResultsTable_",m,".csv"))
      results<- rbind.fill(results,cur.results )
    }
    #remove rows where school size children size are less than the expected sampling percentage
    results <- results[!is.na(results$PrevalenceinSample ),]
    
    
    rounding.list=c(0.01,0.03,0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
    rounded.perc<-unlist(sapply(1:nrow(results), 
                                function(x) rounding.list[which.min(abs(rounding.list - results$ActualSamplingPercentage[x] ))]
    ))
    #head(rounded.perc,10)
    results$Sampling.Method[results$Sampling.Method=="hh1.s"]<-"Household Sampling"
    results$Sampling.Method[results$Sampling.Method=="school.s"]<-"School Sampling"
    results$Sampling.Method[results$Sampling.Method=="random.s"]<-"Random Sampling"
    
    results$roundedSamplingPercentage<-rounded.perc
    results$PrevalenceRatio<-results$PrevalenceinSample/results$ScabiesPrevalence
    colnames(results)
    }
    #results<-results[results$PrevalenceBasedOn==prev.method[i],]
    {
      
      # WHAT I DID:
      #set desired precision levels: 0.01, 0.02,0.05,0.1. 
      #For ex, 0.02 precision correspond to 18-22% range around 20% actual scabies prevalence.
      #Then,
      #create a 95% CI of observed prevalences given a pop size, sampling method, and actual scabies prevalence range.
      #Then,
      #check for all the sampling percentages if the observed prevalence CI (of all the actual prevalence values in the actual prevalence range)
      #is in the given precision range. Report the smallest sampling percentage as 
      #the min sampling necessary to obtain a prevalence in the given range with 95% CI
      p <- c(0.025, 0.5, 0.975)
      p_names <- map_chr(p, ~paste0(.x*100))
      
      p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
        set_names(nm = p_names)
      
      given.method.list<- unique(results$Sampling.Method)#c("Random Sampling","Household Sampling")  #unique(results$Sampling.Method)
      given.popsizes.list<-c()#as.list(unique(results$PopulationSize))
      given.popsizes.list[[1]]<-c(500,1500)
      given.popsizes.list[[2]]<-c(1501,2500)
      given.popsizes.list[[3]]<-c(2501,4000)
      
      given.actual.prevalence.list<-as.list(unique(results$ScabiesPrevalence))#at least estimated/expected prevalence
      given.actual.prevalence.list[[9]]<-c(0.2,0.3)
      given.actual.prevalence.list[[10]]<-c(0.15,0.35)  
      given.actual.prevalence.list[[11]]<-c(0.1,0.4) 
      given.actual.prevalence.list[[12]]<-c(0.05,0.1)
      given.actual.prevalence.list[[13]]<-c(0.15,0.2)  
      given.actual.prevalence.list[[14]]<-c(0.25,0.3) 
      given.actual.prevalence.list[[15]]<-c(0.35,0.4)
      
      precision.levels<-c(0.02,0.05,0.1)
      library(gridExtra)
      results$diffPrevalence<-results$PrevalenceinSample-results$ScabiesPrevalence
      df <- expand.grid(given.actual.prevalence.list[12:15], given.popsizes.list)
      coldf <- expand.grid(precision.levels, given.method.list)
      mytable<-data.frame(matrix(ncol = 9, nrow = 12))
      rownames(mytable)<-unlist(lapply(1:nrow(mytable), function(x) paste0(df$Var2[x]," ", df$Var1[x])))
      colnames(mytable)<-unlist(lapply(1:ncol(mytable), function(x) paste0(coldf$Var2[x]," ", coldf$Var1[x])))
      
      for (c in c(1:nrow(coldf))){
        for(r in c(1:nrow(df))){
          ci <- coldf$Var1[[c]]
          given.method <- coldf$Var2[[c]]
          given.actual.prevalence <- df$Var1[[r]]
          given.popsizes<-df$Var2[[r]]
          
          prev.limits<-c(min(given.actual.prevalence),max(given.actual.prevalence) )
          if (is.na(prev.limits[2])){prev.limits[2]<-prev.limits[1]}
          pop.limits<-c(min(given.popsizes),max(given.popsizes) )
          if (is.na(pop.limits[2])){pop.limits[2]<-pop.limits[1]}
          results2<-results[results$ScabiesPrevalence>=prev.limits[1] &
                              results$ScabiesPrevalence<=prev.limits[2] &
                              #results$PopulationSize %in% given.popsizes
                              results$PopulationSize >=pop.limits[1] &
                              results$PopulationSize <=pop.limits[2], ]
          results2<-results2[results2$roundedSamplingPercentage!=1.0,]
          df3<-results2 %>%
            group_by(Sampling.Method, roundedSamplingPercentage) %>%
            summarize_at(vars(diffPrevalence), funs(!!!p_funs))
          df4<-results2 %>%
            group_by(Sampling.Method, roundedSamplingPercentage) %>%
            dplyr::summarize(n=dplyr::n())
          df3$n<-df4$n
          my.df<-df3[df3$Sampling.Method==given.method,]
          colnames(my.df)<-c("Sampling.Method", "Sampling.Percentage",
                             paste0("DifferenceInPrevalence_Quantile",p[1]),
                             paste0("DifferenceInPrevalence_Quantile",p[2]),
                             paste0("DifferenceInPrevalence_Quantile",p[3]), "Number of Runs")
          
          #check the limits of observe CI range and expected range of (-ci,ci)
          upper.diff<-unlist(unname(my.df[,5]))-ci
          lower.diff<--(unlist(unname(my.df[,3]))+ci)
          
          if (sum(upper.diff<0)==0 | sum(lower.diff<0)==0){
            #none of the observed CI is in the range of (-ci,ci)
            cur.sampling.percentage<-min(max(my.df$Sampling.Percentage)*100,90)
            mytable[r,c]<-paste0(">",cur.sampling.percentage,"%", collapse="")
            if (mytable[r,c] == ">20%"){
              mytable[r,c]<-"X"
            }
          }else{
            #there is at least one CI in the range of (-ci,ci)
            #return the CI correspond to the smaller sampling percentage
            my.index<-max(min(which(upper.diff<=0)),min(which(lower.diff<=0)))
            cur.sampling.percentage<-my.df$Sampling.Percentage[my.index]
            mytable[r,c]<-paste0(cur.sampling.percentage*100,"%", collapse="")
          }
        }
          
        
        #filename<-paste0("Outputs/Tables/",(p[3]-p[1])*100,"%CI_SummaryTables","_","Allprevassignments","_",given.method,"_Accuracy_",ci, ".csv")
        #write.csv(mytable,filename)
        }
       print(mytable)
       filename<-paste0("Outputs/Tables/",(p[3]-p[1])*100,"%CI_SummaryTables","_","Allprevassignments", ".csv")
       write.csv(mytable,filename)
         
      
      
      
    }
  }
  #prev method based tables
  {
    #upload new table 
    {  results <- data.frame()
    for (m in c(1:4)){
      cur.results <- read.csv(paste0("Data/ResultsTable_",m,".csv"))
      results<- rbind.fill(results,cur.results )
    }
    #remove rows where school size children size are less than the expected sampling percentage
    results <- results[!is.na(results$PrevalenceinSample ),]
    
    
    rounding.list=c(0.01,0.03,0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
    rounded.perc<-unlist(sapply(1:nrow(results), 
                                function(x) rounding.list[which.min(abs(rounding.list - results$ActualSamplingPercentage[x] ))]
    ))
    #head(rounded.perc,10)
    results$Sampling.Method[results$Sampling.Method=="hh1.s"]<-"Household Sampling"
    results$Sampling.Method[results$Sampling.Method=="school.s"]<-"School Sampling"
    results$Sampling.Method[results$Sampling.Method=="random.s"]<-"Random Sampling"
    
    results$roundedSamplingPercentage<-rounded.perc
    results$PrevalenceRatio<-results$PrevalenceinSample/results$ScabiesPrevalence
    colnames(results)
    }
    prev.method<-unique(results$PrevalenceBasedOn)
    prev.method.names<-c("Random", "High HH-Specific",
                         "Mild HH-Specific", "Age-Specific",
                         "HH- & Age-Specific")
    for (i in c(1:length(prev.method))){
      #upload new table 
      {  results <- data.frame()
      for (m in c(1:4)){
        cur.results <- read.csv(paste0("Data/ResultsTable_",m,".csv"))
        results<- rbind.fill(results,cur.results )
      }
      #remove rows where school size children size are less than the expected sampling percentage
      results <- results[!is.na(results$PrevalenceinSample ),]
      
      
      rounding.list=c(0.01,0.03,0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
      rounded.perc<-unlist(sapply(1:nrow(results), 
                                  function(x) rounding.list[which.min(abs(rounding.list - results$ActualSamplingPercentage[x] ))]
      ))
      #head(rounded.perc,10)
      results$Sampling.Method[results$Sampling.Method=="hh1.s"]<-"Household Sampling"
      results$Sampling.Method[results$Sampling.Method=="school.s"]<-"School Sampling"
      results$Sampling.Method[results$Sampling.Method=="random.s"]<-"Random Sampling"
      
      results$roundedSamplingPercentage<-rounded.perc
      results$PrevalenceRatio<-results$PrevalenceinSample/results$ScabiesPrevalence
      colnames(results)
      }
      results<-results[results$PrevalenceBasedOn==prev.method[i],]
      {
        
        # WHAT I DID:
        #set desired precision levels: 0.01, 0.02,0.05,0.1. 
        #For ex, 0.02 precision correspond to 18-22% range around 20% actual scabies prevalence.
        #Then,
        #create a 95% CI of observed prevalences given a pop size, sampling method, and actual scabies prevalence range.
        #Then,
        #check for all the sampling percentages if the observed prevalence CI (of all the actual prevalence values in the actual prevalence range)
        #is in the given precision range. Report the smallest sampling percentage as 
        #the min sampling necessary to obtain a prevalence in the given range with 95% CI
        p <- c(0.025, 0.5, 0.975)
        p_names <- map_chr(p, ~paste0(.x*100))
        
        p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
          set_names(nm = p_names)
        
        given.method.list<- unique(results$Sampling.Method)#c("Random Sampling","Household Sampling")  #unique(results$Sampling.Method)
        given.popsizes.list<-c()#as.list(unique(results$PopulationSize))
        given.popsizes.list[[1]]<-c(500,1500)
        given.popsizes.list[[2]]<-c(1501,2500)
        given.popsizes.list[[3]]<-c(2501,4000)
        
        given.actual.prevalence.list<-as.list(unique(results$ScabiesPrevalence))#at least estimated/expected prevalence
        given.actual.prevalence.list[[9]]<-c(0.2,0.3)
        given.actual.prevalence.list[[10]]<-c(0.15,0.35)  
        given.actual.prevalence.list[[11]]<-c(0.1,0.4) 
        given.actual.prevalence.list[[12]]<-c(0.05,0.1)
        given.actual.prevalence.list[[13]]<-c(0.15,0.2)  
        given.actual.prevalence.list[[14]]<-c(0.25,0.3) 
        given.actual.prevalence.list[[15]]<-c(0.35,0.4)
        
        precision.levels<-c(0.02,0.05,0.1)
        library(gridExtra)
        results$diffPrevalence<-results$PrevalenceinSample-results$ScabiesPrevalence
        df <- expand.grid(given.actual.prevalence.list[12:15], given.popsizes.list)
        
        coldf <- expand.grid(precision.levels, given.method.list)
        mytable<-data.frame(matrix(ncol = 9, nrow = 12))
        rownames(mytable)<-unlist(lapply(1:nrow(mytable), function(x) paste0(df$Var2[x]," ", df$Var1[x])))
        colnames(mytable)<-unlist(lapply(1:ncol(mytable), function(x) paste0(coldf$Var2[x]," ", coldf$Var1[x])))
        
        for (c in c(1:nrow(coldf))){
          for(r in c(1:nrow(df))){
            ci <- coldf$Var1[[c]]
            given.method <- coldf$Var2[[c]]
            given.actual.prevalence <- df$Var1[[r]]
            given.popsizes<-df$Var2[[r]]
            
            prev.limits<-c(min(given.actual.prevalence),max(given.actual.prevalence) )
            if (is.na(prev.limits[2])){prev.limits[2]<-prev.limits[1]}
            pop.limits<-c(min(given.popsizes),max(given.popsizes) )
            if (is.na(pop.limits[2])){pop.limits[2]<-pop.limits[1]}
            results2<-results[results$ScabiesPrevalence>=prev.limits[1] &
                                results$ScabiesPrevalence<=prev.limits[2] &
                                #results$PopulationSize %in% given.popsizes
                                results$PopulationSize >=pop.limits[1] &
                                results$PopulationSize <=pop.limits[2], ]
            results2<-results2[results2$roundedSamplingPercentage!=1.0,]
            df3<-results2 %>%
              group_by(Sampling.Method, roundedSamplingPercentage) %>%
              summarize_at(vars(diffPrevalence), funs(!!!p_funs))
            df4<-results2 %>%
              group_by(Sampling.Method, roundedSamplingPercentage) %>%
              dplyr::summarize(n=dplyr::n())
            df3$n<-df4$n
            my.df<-df3[df3$Sampling.Method==given.method,]
            colnames(my.df)<-c("Sampling.Method", "Sampling.Percentage",
                               paste0("DifferenceInPrevalence_Quantile",p[1]),
                               paste0("DifferenceInPrevalence_Quantile",p[2]),
                               paste0("DifferenceInPrevalence_Quantile",p[3]), "Number of Runs")
            
            #check the limits of observe CI range and expected range of (-ci,ci)
            upper.diff<-unlist(unname(my.df[,5]))-ci
            lower.diff<--(unlist(unname(my.df[,3]))+ci)
            
            if (sum(upper.diff<0)==0 | sum(lower.diff<0)==0){
              #none of the observed CI is in the range of (-ci,ci)
              cur.sampling.percentage<-min(max(my.df$Sampling.Percentage)*100,90)
              mytable[r,c]<-paste0(">",cur.sampling.percentage,"%", collapse="")
              if (mytable[r,c] == ">20%"){
                mytable[r,c]<-"X"
              }
            }else{
              #there is at least one CI in the range of (-ci,ci)
              #return the CI correspond to the smaller sampling percentage
              my.index<-max(min(which(upper.diff<=0)),min(which(lower.diff<=0)))
              cur.sampling.percentage<-my.df$Sampling.Percentage[my.index]
              mytable[r,c]<-paste0(cur.sampling.percentage*100,"%", collapse="")
            }
          }
          
          
          #filename<-paste0("Outputs/Tables/",(p[3]-p[1])*100,"%CI_SummaryTables","_","Allprevassignments","_",given.method,"_Accuracy_",ci, ".csv")
          #write.csv(mytable,filename)
        }
        print(paste0(prev.method[i]))
        print(mytable)
        filename<-paste0("Outputs/Tables/",prev.method[i],"_",(p[3]-p[1])*100,"%CI_SummaryTables", ".csv")
        write.csv(mytable,filename)}
      
        
      }
    
    
  }
}




