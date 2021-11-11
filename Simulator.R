#clear all
.rs.restartR()
rm(list = ls())

#libraries
library(fitdistrplus)
library(dplyr)
library(purrr)

#######
#SET DIRECTORY
#setwd()
source("SimulatorFunctions.R")

##############################
#IMPORTANT
##############################
#please reach the authors about the age-hh dist data (ABC_household_minimal.csv)
#the data is not provided here due to data privicy.
#you won't be able to run the simulator without the data
##############################

#READ DATA
{
  age.dist<-read.csv("Data/Age_OneDisease.csv")
  hh.dist<-read.csv("Data/HH_OneDisease.csv")
  age.hh.dist<-read.csv("Data/ABC_household_minimal.csv")
   #adjust last row of hh.dist table
  hh.dist[7:(6+length(sort(unique(age.hh.dist$overall_no[age.hh.dist$overall_no>6])))),]<-data.frame(NoPeople=sort(unique(age.hh.dist$overall_no[age.hh.dist$overall_no>6])), NoHHs=NA)
  all.sizes<-hh.dist$NoHHs[6]
  for (i in 6:nrow(hh.dist)){
    hh.size.this<-hh.dist$NoPeople[i]
    hh.dist$NoHHs[i]<- round(all.sizes*sum(age.hh.dist$overall_no==hh.size.this)/sum(age.hh.dist$overall_no>=6))
  }
  hh.dist$perc<-hh.dist$NoHHs/sum(hh.dist$NoHHs)
  #hist(age.hh.dist$overall_no, breaks = seq(0,max(age.hh.dist$overall_no)))
  #hist(rep(hh.dist$NoPeople,hh.dist$NoHHs),  breaks = seq(0,max(hh.dist$NoPeople)))
  
}

#ASSIGN PARAMETERS
{
  cur.seed<-32 #current seed
  set.seed(cur.seed)
  #population.size.list<-seq(500,4000,500)
  no.repeats<-50 * 10
  popsize.limits <- c(500,4000)
  population.size.list<- sort(sample(popsize.limits[1]:popsize.limits[2],no.repeats,replace=FALSE ))
  #plot(sample(popsize.limits[1]:popsize.limits[2],no.repeats,replace=FALSE ))
  prevalence.list=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4)
  sampling.percentage.list<-c(0.01,0.03,0.05,0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  #pop.size<-600
  #assign scabies
  method.list<-c("random", "hh1", "hh2","age","hh-age")
  sampling.method.list<-c("random.s", "hh1.s","school.s")
  #method<-method.list[1]
  #prevalence<-0.2
  tolerance<-0.01 #used in hh1/hh2 methods a value between 0-1 added to prevalence percentage as the acceptable range
  #ex for 0.2 prevalence, tolerance 0.01, prevalence can be between 0.19-0.21
  prevalence.in.hh2<-0.5 #prevalence percentage observed in the households
  size.tolerance<-0.05 #used in create population function to assign an acceptable range for pop size
  #ex pop size 500 and size.tolerance 0.05, a pop size between 475 and 525 is in the acceptable range
  # parameters<-expand.grid(population.size = population.size.list, 
  #                         prevalence = prevalence.list,
  #                         sampling.percentage=sampling.percentage.list,
  #                         method=method.list,
  #                         no.repeats=c(1:no.repeats))
  # parameters$pop.id<-c(1:nrow(parameters))
  
  }

#RUN
{
  pop.id<-0
  next.save.index <- 1
  set.seed(cur.seed)
  results<-data.frame()
  
  for (pop.size in population.size.list){ #assign pop size 
    #create population with full hh/age/scabies information
    pop.id<-pop.id+1
    population.null<-create.population(pop.size,my.seed=NULL,age.dist,hh.dist,age.hh.dist,size.tolerance)
    for(method in method.list){ #assign method of scabies prevalence
      for (prevalence in prevalence.list){ #assign prev percentage
        
        #test:
        # pop.id<-0
        # sampling.percentage<-0.6
        # prevalence<-0.1
        # pop.size<-500
        # method<-"random"
        # s.method<-"random.s"
        population<-assign.scabies(population.null,method,prevalence,tolerance,prevalence.in.hh2,my.seed=NULL)
        prevalenceinadults<-sum(population$age.group=="Adult" & population$scabies ==1)/sum(population$age.group=="Adult")
        prevalenceinschool<-sum(population$age.group=="School" & population$scabies ==1)/sum(population$age.group=="School")
        prevalenceinpreschool<-sum(population$age.group=="Pre-school" & population$scabies ==1)/sum(population$age.group=="Pre-school")
        #original data hh
        hh.size<- population %>%
          dplyr::group_by(hh.id) %>% #hh id:hh_a_03
          dplyr::summarise(hhsize = dplyr::n(),
                           scabies = sum(scabies)) %>% 
          dplyr::group_by(hhsize) %>%
          dplyr::summarise(nohhs = dplyr::n(),
                           scabies.prevalence = mean(scabies/hhsize))
        hhs<- population %>%
          dplyr::group_by(hh.id) %>% #hh id:hh_a_03
          dplyr::summarise(hhsize = dplyr::n(),
                           scabies = sum(scabies)) 
        hhswithzerocases<-sum(hhs$scabies==0)/nrow(hhs)
        hhswithonecase<-sum(hhs$scabies==1)/nrow(hhs)
        hhswithtwocase<-sum(hhs$scabies==2)/nrow(hhs)
        hhswiththreecase<-sum(hhs$scabies==3)/nrow(hhs)
        hhswithfourcase<-sum(hhs$scabies==4)/nrow(hhs)
        hhswithatleastfivecase<-sum(hhs$scabies>=5)/nrow(hhs)
        hhswith.atleastonecases<-sum(hhs$scabies>=1)/nrow(hhs)
        hhswith.atleasttwocases<-sum(hhs$scabies>=2)/nrow(hhs)
        #at least two household member necessary for calculation of hhswithhalfcases
        hhswithhalfcases<-sum(hhs$hhsize >= 2 & 
                                hhs$scabies == ceiling(hhs$hhsize/2) | hhs$scabies == floor(hhs$hhsize/2))/nrow(hhs)
        
        hhswith.atleasthalfcases<-sum(hhs$hhsize >= 2 & hhs$scabies >= ceiling(hhs$hhsize/2))/nrow(hhs)
        hhswithfullcases<-sum(hhs$scabies==hhs$hhsize)/nrow(hhs)
        allhhsizes<-hh.dist$NoPeople
        prevalenceinhhsizes<-unlist(lapply(1:length(allhhsizes), function(x) ifelse(allhhsizes[x]%in%hh.size$hhsize, 
                                                                                    hh.size$scabies.prevalence[hh.size$hhsize==allhhsizes[x]],NA)))
        nohhsinhhsizes<-unlist(lapply(1:length(allhhsizes), function(x) ifelse(allhhsizes[x]%in%hh.size$hhsize, 
                                                                               hh.size$nohhs[hh.size$hhsize==allhhsizes[x]],NA)))
        
        for (s.method in sampling.method.list){
          for (sampling.percentage in sampling.percentage.list){
            #apply a sampling strategy
            sampling.results<-sampling.population(population, s.method,sampling.percentage, my.seed=NULL)
            newresults<-data.frame(PopulationIndex=pop.id,
                                   PopulationSize=pop.size,AdultSize=sum(population$age.group=="Adult"), 
                                   SchoolSize=sum(population$age.group=="School"),
                                   PreschoolSize=sum(population$age.group=="Pre-school"), ScabiesPrevalence=prevalence,
                                   ScabiesPrevalenceinAdults=prevalenceinadults,
                                   ScabiesPrevalenceinSchool=prevalenceinschool,
                                   ScabiesPrevalenceinPreschool=prevalenceinpreschool,
                                   PrevalenceBasedOn=method, SamplingPercentage=sampling.percentage, 
                                   Sampling.Method=s.method,
                                   ActualSamplingPercentage=sampling.results$sampling.size/nrow(population), 
                                   SamplingSize=sampling.results$sampling.size,
                                   AdultSamplingSize=sampling.results$samp.size.adults,
                                   SchoolSamplingSize=sampling.results$samp.size.school,
                                   PreschoolSamplingSize=sampling.results$samp.size.preschool,
                                   PrevalenceinSample=sampling.results$sampling.prevalence,
                                   PrevalenceinAdultSample=sampling.results$samp.prevalence.adults,
                                   PrevalenceinSchoolSample=sampling.results$samp.prevalence.school,
                                   PrevalenceinpreschoolSample=sampling.results$samp.prevalence.preschool,
                                   PercHHswithzerocase=hhswithzerocases,
                                   PercHHswith.atleastonecase=hhswith.atleastonecases,
                                   PercHHswith.atleasttwocase=hhswith.atleasttwocases,
                                   PercHHswithhalfcases=hhswithhalfcases,
                                   PercHHswith.atleasthalfcases=hhswith.atleasthalfcases,
                                   PercHHswithfullcases=hhswithfullcases,
                                   PercHHswithtwocase=hhswithtwocase,
                                   PercHHswiththreecase=hhswiththreecase,
                                   PercHHswithfourcase=hhswithfourcase,
                                   PercHHswithatleastfivecase=hhswithatleastfivecase)
            newresults[,(ncol(newresults)+1):(ncol(newresults)+length(allhhsizes))]<-prevalenceinhhsizes
            colnames(newresults)[(ncol(newresults)-length(allhhsizes)+1):ncol(newresults)]<-c(paste0("PrevinHHSize",allhhsizes))
            newresults[,(ncol(newresults)+1):(ncol(newresults)+length(allhhsizes))]<-nohhsinhhsizes
            colnames(newresults)[(ncol(newresults)-length(allhhsizes)+1):ncol(newresults)]<-c(paste0("NoHHsinHHSize",allhhsizes))
            results<-rbind(results,newresults)
          }}
        # }
        #}
      }
    }
    if(pop.id == next.save.index * length(population.size.list)/4){
      write.csv(results,paste0("Data/ResultsTable2_",next.save.index,".csv"))
      next.save.index <- next.save.index + 1
      set.seed(cur.seed)
      results<-data.frame()
    }
  }
  
}






