#library(triangle)
library(fitdistrplus)

'%notin%' <- Negate('%in%')
#functions
create.population<-function(pop.size,my.seed=NULL,age.dist,hh.dist,age.hh.dist,size.tolerance){
  #size.tolerance<-0.05 #used in create population function to assign an acceptable range for pop size
  #for ex if pop size == 500 & size.tolerance==0.05 then pop size can be between 475 - 525
  if (!is.null(my.seed)){set.seed(my.seed)}
  #ASSIGN HOUSEHOLDS 
  #based on hh structure data from Vino et al, 2017
  hh.freq<- as.vector(rep(hh.dist$NoPeople,hh.dist$NoHHs))
  avg.hh.per.person<-sum(hh.dist$NoHHs)/sum(hh.dist$NoPeople*hh.dist$NoHHs)
  while(TRUE){ 
    #sample hh sizes until sum of hh sizes is equal to population size
    hh.sizes<-sample(hh.dist$NoPeople, size=round(pop.size*avg.hh.per.person), prob =hh.dist$perc, replace = TRUE )
    if (abs(sum(hh.sizes)-pop.size) < size.tolerance*pop.size & sum(hh.sizes==0)==0){
      #all hh sizes greater than 0 and sum of hh members is in the acceptable range of the population size
      break}
  }
  #create emtpy population data frame
  population<-data.frame(id=c(1:sum(hh.sizes)), hh.id=NA, age=NA, age.group=NA,scabies=NA )
  
  #assign hh ids
  hh.sizes<-sort(hh.sizes)
  population$hh.id<-NA
  for (h in c(1:length(hh.sizes))){
    cur.size<-hh.sizes[h]
    population$hh.id[is.na(population$hh.id)][1:cur.size]<-h
  }
  #hist(hh.sizes, breaks = seq(1,max(hh.sizes)))
  #ASSIGN AGE GROUPS 
  #based on hh age structure data from Vino et al, 2017
  age.list<-c("Adult","School","Pre-school")
  
  population$age.group<-NA
  for (h in c(1:length(hh.sizes))){
    #print(h)
    cur.hh.size<-hh.sizes[h] #current hh size
    cur.hh.index<-h
    given.hhs<-age.hh.dist[age.hh.dist$overall_no==cur.hh.size,] #age dist in this hh size
    if(nrow(given.hhs)==0){
      #if there is no hh size in the data, use the age distribution in the closest hh size
      closest.hh.size<-unique(age.hh.dist$overall_no)[which.min(abs(unique(age.hh.dist$overall_no)-cur.hh.size))]
      given.hhs<-age.hh.dist[age.hh.dist$overall_no==closest.hh.size,] #age dist in this hh size
    }
    adult.perc<-sum(given.hhs$all_adult)/sum(given.hhs$overall_no) #adult percentage in this hh size
    school.perc<-sum(given.hhs$all_child)/sum(given.hhs$overall_no) #school aged percentage in this hh size
    
    #sample age groups for given hh size
    while(TRUE){
      age.groups<-sample(c("Adult","School","Pre-school"),size=cur.hh.size, replace=TRUE, 
                         prob =  c("Adult" = adult.perc, 
                                   "School" = school.perc,
                                   "Pre-school"=1-adult.perc-school.perc))
      sizes.in.groups<-c(sum(age.groups=="Adult"),sum(age.groups=="School"),sum(age.groups=="Pre-school"))
      if(sizes.in.groups[1]>0){
        #there is at least one adult in the hh
        break
      }}
    
    population$age.group[population$hh.id==cur.hh.index]<-age.groups
    
  }
  
  
  #assign ages
  {
    sampled.pop<-pop.size*10
    #set max age as 90
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
    adult.ages<-sample(kern.samp[kern.samp>=16], size=sum(population$age.group =="Adult"), replace=TRUE)
    school.ages<-sample(kern.samp[kern.samp<16 & kern.samp>=5], size=sum(population$age.group =="School"), replace=TRUE)
    preschool.ages<-sample(kern.samp[kern.samp<5], size=sum(population$age.group =="Pre-school"), replace=TRUE)
    population$age[population$age.group=="Adult"]<-adult.ages
    population$age[population$age.group=="School"]<-school.ages
    population$age[population$age.group=="Pre-school"]<-preschool.ages
    
    # hist(population$age, breaks =c(seq(0,95,5)), main = "Age Distribution in a Sample",probability =TRUE)
    
  }
  
  return(population)
}


assign.scabies<-function(population,method,prevalence,tolerance,prevalence.in.hh2,my.seed=NULL){
  # method.list<-c("random", "hh1", "hh2","age","hh-age")
  # sampling.method.list<-c("random.s", "hh1.s","school.s")
  # method<-method.list[1]
  # prevalence<-0.2
  # tolerance<-0 #used in hh1/hh2/hh-age a value between 0-1 added to prevalence percentage as the acceptable range
  # ex: prevalence==0.2 tolerance==0.01 then acceptable prevalence range is [0.19 - 0.21]
  # #ex for 0.2 prevalence, tolerance 0.05, prevalence can be between 0.15-0.25
  # prevalence.in.hh2<-0.5 #prevalence percentage observed in the households
  
  if (!is.null(my.seed)){set.seed(my.seed)}
  sum.pop<-population %>% group_by(hh.id)%>% dplyr::count()
  hh.sizes<-sum.pop$n
  if (method== "random"){
    population$scabies<-0
    #randomly select indexes to set scabies status as 1
    scab.indexes<-sample(1:nrow(population), size=round(nrow(population)*prevalence), replace = FALSE )
    population$scabies[scab.indexes]<-1
  }else if (method =="hh1"){
    population$scabies<-0
    #decide on an average number of households with all household members having scabies
    #which would corresponds to the prevalence observed in population
    no.hhs<-round(nrow(population)*prevalence/(nrow(population)/length(unique(population$hh.id))))
    while(TRUE){
      #randomly select that amound of households to assign scabies to all the members
      hh.indexes<-sort(sample(unique(population$hh.id), size=no.hhs, replace = FALSE))
      #continue if total selected people having scabies is 
      #close to the expected prevalence in the population
      if(abs(sum(hh.sizes[hh.indexes])-nrow(population)*prevalence)<=tolerance*pop.size){
        break
      }
    }
    #assign scabies
    population$scabies[population$hh.id %in% hh.indexes]<-1
  }else if (method =="hh3"){
    population$scabies<-0
    #decide on an average number of households with at least household members having scabies
    #which would corresponds to the prevalence observed in population
    no.hhs<-round(length(unique(population$hh.id)) * comm.transmission.perc)
      
      #round(nrow(population)*prevalence/(nrow(population)/length(unique(population$hh.id))))
    while(TRUE){
      #randomly select that amound of households to assign scabies to all the members
      hh.indexes<-sort(sample(unique(population$hh.id), size=no.hhs, replace = FALSE))
      #continue if total selected people having scabies is 
      #close to the expected prevalence in the population
      if(abs(sum(hh.sizes[hh.indexes])-nrow(population)*prevalence)<=tolerance*pop.size){
        break
      }
    }
    #find at least one hh member from each hh
    atleast_onemember <- c()
    for (i in 1:length(hh.indexes)){
      atleast_onemember[i]<-sample(which(population$hh.id %in% hh.indexes[i]),1)
    }
    
    indexes.in.scab.hhs<-which(population$hh.id %in% hh.indexes)
    rest.hhs <- which(indexes.in.scab.hhs %notin% atleast_onemember)
    hh.trans.no <- round(nrow(population)*prevalence) - length(atleast_onemember)
    hh.trans.indexes <- sample(rest.hhs,hh.trans.no)
    scab.indexes <- c(atleast_onemember,hh.trans.indexes )
    
    population$scabies[scab.indexes]<-1
  }else if (method =="hh2"){
    population$scabies<-0
    #decide on an average number of households with half household members having scabies
    #which would corresponds to the prevalence observed in population
    no.hhs<-round(nrow(population)*prevalence/(prevalence.in.hh2* nrow(population)/length(unique(population$hh.id))))
    while(TRUE){
      #randomly select that amound of households to assign scabies to all the members
      hh.indexes<-sort(sample(unique(population$hh.id), size=no.hhs, replace = FALSE))
      #continue if total selected people having scabies is 
      #close to the expected prevalence in the population
      if(abs(sum(hh.sizes[hh.indexes])*prevalence.in.hh2-nrow(population)*prevalence)<=tolerance*pop.size){
        break
      }
    }
    
    indexes.in.scab.hhs<-which(population$hh.id %in% hh.indexes)
    #try to assign scabies until expected population-level prevalence is achieved
    while(TRUE){
      population$scabies<-0
      for (i in indexes.in.scab.hhs){
        if(runif(1)<prevalence.in.hh2){
          population$scabies[i]<-1
        }}
      #continue if total selected people having scabies is 
      #close to the expected prevalence in the population
      if(abs(sum(population$scabies)-nrow(population)*prevalence)<=tolerance*pop.size){
        break
      }
    }
  }else if (method =="age"){ 
    #Split population into children (< 15 years) and adults (15 years and older), 
    #with children 3 times more likely to be assigned a scabies positive status than adults
    population$scabies<-0
    scab.sizes<-table(sample(c("A", "C"), size=pop.size*prevalence,replace = TRUE, prob = c("1","3")))
    adult.indexes<-sort(sample(which(population$age.group=="Adult"),
               size=min(scab.sizes[names(scab.sizes)=="A"],sum(population$age.group=="Adult") ), replace = FALSE))
    #population$age.group[adult.indexes]
    young.indexes<-sort(sample(which(population$age.group!="Adult"),
               size=min(scab.sizes[names(scab.sizes)=="C"], sum(population$age.group!="Adult")), replace = FALSE))
    #population$age.group[young.indexes]
    population$scabies[c(adult.indexes,young.indexes)]<-1
  }else if (method == "hh-age"){
    population$scabies<-0
    no.hhs<-round(nrow(population)*prevalence/(prevalence.in.hh2* nrow(population)/length(unique(population$hh.id))))
    while(TRUE){
      hh.indexes<-sort(sample(unique(population$hh.id), size=no.hhs, replace = FALSE))
      if(abs(sum(hh.sizes[hh.indexes])*prevalence.in.hh2-nrow(population)*prevalence)<=tolerance*pop.size){
        break
      }
    }
    
    sizes<-table(population$age.group[population$hh.id %in% hh.indexes])
    while(TRUE){
      scab.pop<-c(rep("A", sizes[names(sizes)=="Adult"]), rep("C", sum(sizes[names(sizes) !="Adult"])))
      scab.sizes<-table(sample(scab.pop, size=sum(population$hh.id %in% hh.indexes)*prevalence.in.hh2,replace = TRUE, 
                               prob = c(rep(.25, sizes[names(sizes)=="Adult"]), rep(.75, sum(sizes[names(sizes) !="Adult"])))))
      if((scab.sizes[names(scab.sizes)=="A"]<= sizes[names(sizes)=="Adult"]) &
         (scab.sizes[names(scab.sizes)=="C"]<= sum(sizes[names(sizes)!="Adult"]))){
        #found a sample in hh with scabies having necessary number of adults/children
        break
      }
    }
    adult.indexes<-sort(sample(which(population$age.group=="Adult" & population$hh.id %in% hh.indexes),
                               size=min(scab.sizes[names(scab.sizes)=="A"],sum(population$age.group=="Adult") ), replace = FALSE))
    #population$age.group[adult.indexes]
    young.indexes<-sort(sample(which(population$age.group!="Adult"  & population$hh.id %in% hh.indexes),
                               size=min(scab.sizes[names(scab.sizes)=="C"], sum(population$age.group!="Adult")), replace = FALSE))
    #population$age.group[young.indexes]
    population$scabies[c(adult.indexes,young.indexes)]<-1
  }else {
    stop("Error: there is no defined method")
  }
  return(population)
}

sampling.population<-function(population, s.method,sampling.percentage, my.seed=NULL){
  # sampling.method.list<-c("random.s", "hh1.s","school.s")
  # s.method<-"school.s"
  # sampling.percentage<-0.1 #sampling percentage of the population (not children for ex.)
  if (!is.null(my.seed)){set.seed(my.seed)}
  sampling.size <- round(nrow(population)*sampling.percentage) #estimated sampling size
  if (s.method=="random.s"){
    samp.indexes<-sample(1:nrow(population), size=sampling.size, replace = FALSE)
  }else if (s.method=="hh1.s"){
    no.hhs<-round(sampling.size/(nrow(population)/length(unique(population$hh.id))))
    hh.indexes<-sort(sample(unique(population$hh.id), size=no.hhs, replace = FALSE))
    samp.indexes<-which(population$hh.id %in% hh.indexes)
  }else if (s.method=="school.s"){
    if (sum(population$age.group == "School")>=sampling.size){
      #if no of school aged children is higher than the sampling size
      samp.indexes<-sample(which(population$age.group == "School"), size=sampling.size, replace = FALSE)
    }else{
      #new version:
      samp.indexes<- c()
      #old version:
      #if no of school aged children is less than the sampling size, sample all the children
      #samp.indexes<-which(population$age.group == "School")
    }
  }
  if(length(samp.indexes) > 0){ #sample is being done for the first time given the parameter sets
    sampled.pop<-population[samp.indexes,]
    samp.prevalence<-mean(sampled.pop$scabies)
    samp.prevalence.adults<-mean(sampled.pop$scabies[sampled.pop$age.group=="Adult"])
    samp.prevalence.school<-mean(sampled.pop$scabies[sampled.pop$age.group=="School"])
    samp.prevalence.preschool<-mean(sampled.pop$scabies[sampled.pop$age.group=="Pre-school"])
    samp.size.adults<-sum(sampled.pop$age.group=="Adult")
    samp.size.school<-sum(sampled.pop$age.group=="School")
    samp.size.preschool<-sum(sampled.pop$age.group=="Pre-school")
    samp.size<-length(samp.indexes) #real sampling size
    
  }else{ #sample is being doubled given the parameter sets therefore, we do not sample
    #this condition is only satisfied with school sampling
    #when necessary sampling size is greater than number of school-aged children
    
    sampled.pop<-NA #population[samp.indexes,]
    samp.prevalence<-NA #mean(sampled.pop$scabies)
    samp.prevalence.adults<-NA #mean(sampled.pop$scabies[sampled.pop$age.group=="Adult"])
    samp.prevalence.school<- NA #mean(sampled.pop$scabies[sampled.pop$age.group=="School"])
    samp.prevalence.preschool<- NA #mean(sampled.pop$scabies[sampled.pop$age.group=="Pre-school"])
    samp.size.adults<-NA #sum(sampled.pop$age.group=="Adult")
    samp.size.school<-NA #sum(sampled.pop$age.group=="School")
    samp.size.preschool<-NA #sum(sampled.pop$age.group=="Pre-school")
    samp.size<-length(samp.indexes) #real sampling size which is zero
  }
  
  return(list(sampling.prevalence=samp.prevalence, 
              sampling.size=samp.size,
              samp.prevalence.adults=samp.prevalence.adults,
              samp.prevalence.school=samp.prevalence.school,
              samp.prevalence.preschool=samp.prevalence.preschool,
              samp.size.adults=samp.size.adults,
              samp.size.school=samp.size.school,
              samp.size.preschool=samp.size.preschool))
}



