
buildInfectNet <- function(interval.list, statuslist,neighbors=TRUE,
keep.infected=FALSE,seed.select=NULL,color.mode="concur",shape.mode="sex",
size.mode="seed"){
 #status list should have
 #[1] "id"                 "sex"                "age"                "hsv"               
 #[5] "circumcised"        "serostatus"         "time.infect"        "infector.ID"       
 #[9] "infector.recency"   "ego.concurrent"     "partner.concurrent" "heterosexual"      
 #[13] "concurrencytype"    "concurrencychain" 
 
#get a list of all the ids with positive serostatus
 infectids <-statuslist$id[statuslist$serostatus==TRUE];
 cat(length(infectids),"infected nodes","\n");
 if (!is.null(seed.select)){
  #shorten the list of infected nodes to include only those starting from specified seeds
  infectids <- statuslist$id[
    sapply(statuslist$seed[infectids],
    function(x){x%in%seed.select})];
 }
 if (neighbors){
 # if we are going to include their neighbors, also go through and find all
 #the nodes they are tied to
 neighborList <- c();
   for (r in 1:nrow(interval.list)){
    from <- remap.id(interval.list[r,1],infectids);
    to <- remap.id(interval.list[r,2],infectids);
    if ((from == 0) & (to > 0)) {
      neighborList <- c(neighborList,interval.list[r,1]);
    } 
    if ((to == 0) & (from > 0)){
      neighborList <- c(neighborList, interval.list[r,2]);
    }
    
  }
  infectids <- unique(c(infectids,neighborList));
  cat(length(infectids),"infected nodes plus neighbors","\n");
  }
 # create a dynamic network of that many elements
 infectdyn <- as.dynamic.intervals(interval.list,subsample=infectids,directed=FALSE);
 # give the old ids as labels so we can debug
 infectdyn <- set.vertex.attribute(infectdyn,"vertex.names",infectids);
 if (color.mode != "chainletter"){
  #set default values for node and edge color
  infectdyn <- set.dynamic.vertex.attribute(infectdyn, "color","gray",valid.time=0);
 }
  #loop over edges to set default color at start time
       #for edges that didn't get color
      for(e in 1:length(infectdyn$etl)){
      #  if (is.null(infectdyn$mel[[e]]$atl$"concur")){
         infectdyn <- set.dynamic.edge.attribute(infectdyn,"color","black",
          valid.time=infectdyn$etl[[e]][1],e=e);
       # }  else if (infectdyn$mel[[e]]$atl$"concur"[1,2] > 
        #    infectdyn$etl[[e]][1]){
         #    infectdyn <- set.dynamic.edge.attribute(infectdyn,"concur","black",
        #      valid.time=infectdyn$etl[[e]][1],e=e);
        #}
       }
 maxTime <- get.time.bounds(infectdyn)[2];
    if (color.mode=="seed"){
        seedColors =  colorize(statuslist$seed[infectids]);
    } else if (color.mode=="race" | color.mode=="chainletter") {
        raceColors = colorize(statuslist$race[infectids],colors=c("white","gray"))
    } else if (color.mode=="age"){
       #make a vector of chaceter lengths of chains
       seedGen = sapply(statuslist$concurrencychain[infectids],
         function(x){nchar(x)});
        #convert to age
        seedGen <- max(seedGen) - seedGen;
       #colorise
       seedGen <- colorize(seedGen, values = sort(unique(seedGen)),colors=heat.colors(              length(unique(seedGen))));
    }

 # loop over the nodes, adding infected status at the appropriate times
 for (n in 1:network.size(infectdyn)){
  if (statuslist$serostatus[infectids[n]] > 0){
     inftime <- statuslist$time.infect[infectids[n]];

     inftype <- statuslist$concurrencytype[infectids[n]];
     infector <- remap.id(statuslist$infector.ID[infectids[n]],infectids);
     chain <-  statuslist$concurrencychain[infectids[n]];
     
     #tweak the size to show the seeds better
    if (size.mode=="seed") {
       size <- 10;
       if (inftype == "D") {
        size <- 15;
       }
       infectdyn <- set.vertex.attribute(infectdyn,"size",size,v=n);
    }
    if (color.mode=="concur") {
       if (inftype == "M"){      #monagomy
         color <- "brown";
        } else if (inftype == "U"){     #unknown
         color <- "red";
        } else if (inftype == "A"){
           color <- "orange";
       } else if (inftype == "C"){    #not used   , now S
          color <-"yellow";
        } else if (inftype == "D"){    #seed node
          color <-"black";
        } else if (inftype == "S"){     #strict concurrency
          color <-"yellow";
       } else if (inftype == "R"){     #residual concurrency
          color <-"tan";
       }
    }  else if (color.mode=="chainletter"){
       #use the fraction of each kind to make an rgb color
       color="yellow";  #so I can find bugs
       #if (inftype == "D"){
       # color="black";
      # } else 
       if (chain != ""){
           chainLetter <- unlist(strsplit(as.character(chain),""));
            ms <- sum(chainLetter=="M");
            cs <-  sum(chainLetter=="S");
           cs <-  sum(chainLetter=="A")+cs;
           cs <-  sum(chainLetter=="R")+cs;
           cs <-  sum(chainLetter=="U")+cs;
           denom <- length(chainLetter);
           color <- rgb(cs/denom,0,ms/denom);
      }
      #temporary hack, set the node color
      infectdyn <- set.vertex.attribute(infectdyn, "color",raceColors[n],v=n);
      
    } else if (color.mode=="seed"){
       color <- seedColors[n];
    } else if (color.mode=="age"){
       color <- seedGen[n]; 
    } else if (color.mode=="race"){
      color <- raceColors[n];
      infectdyn <- set.dynamic.vertex.attribute(infectdyn, "color",color,0,v=n);
      color<-"black"; #for active edeges
    }
      #race colors are not dynamic  so we do later
       if ((color.mode != "race" )& (color.mode !="chainletter")){
       infectdyn <- set.dynamic.vertex.attribute(infectdyn, "color",color,inftime,v=n);
      }
      #if the node is infected, mark the infecting edge
        if (!is.na(infector)){
          eid <- get.edgeIDs.at(infectdyn,time.point=inftime,v=n,alter=infector);
        #  cat("eid",eid,"v",n,"alter",infector,"\n");
          infectdyn <- set.dynamic.edge.attribute(infectdyn,"color",
            valid.time=inftime,color,e=eid);
          #if we are makeing infected edges stick around...
          if (keep.infected){
             #...modify the end time of the edge
             old <- get.edge.end(infectdyn, eid);
             infectdyn <- set.edge.end(infectdyn,eid,maxTime);
             if (!color.mode%in%c("age","chainletter")){
             # and change the color
             infectdyn <- set.dynamic.edge.attribute(infectdyn,"color",
              valid.time=old,"gray",e=eid);
              }
          }
          
        }
        
      }
      
    if (shape.mode == "race"){
       #give nodes a shape based on race
       shape <- "circle";
      if (statuslist$race[infectids[n]] == 2){
         shape <- "square";
       }
       infectdyn <- set.vertex.attribute(infectdyn,"shape",shape,v=n); 
    }  else  if (shape.mode == "sex"){
       #give nodes a shape based on sex
       shape <- "square";
      if (statuslist$sex[infectids[n]] == "F"){
         shape <- "circle";
       }
       infectdyn <- set.vertex.attribute(infectdyn,"shape",shape,v=n); 
    }
    
    
      
 }
 

      
      
 
  #loop over edges to set default color at start time
       #for edges that didn't get color
     # for(e in 1:length(infectdyn$etl)){
      #  if (is.null(infectdyn$mel[[e]]$atl$"concur")){
       #  infectdyn <- set.dynamic.edge.attribute(infectdyn,"concur","black",
        #  valid.time=infectdyn$etl[[e]][1],e=e);
       # }  else if (infectdyn$mel[[e]]$atl$"concur"[1,2] > 
        #    infectdyn$etl[[e]][1]){
         #    infectdyn <- set.dynamic.edge.attribute(infectdyn,"concur","black",
        #      valid.time=infectdyn$etl[[e]][1],e=e);
        #}
      # }
 
 
    return(infectdyn);
}


#extract the network of edges across which transmissions occur
getInfectPaths <- function(nodedata){
  infectids <-nodedata$id[nodedata$serostatus==TRUE];
  edgelist <- cbind(nodedata$id,nodedata$infector.ID)[!is.na(nodedata$infector.ID),];

  #remap ids on edgelist
  for (e in 1:nrow(edgelist)){
    edgelist[e,1] <- remap.id(edgelist[e,1],infectids);
    edgelist[e,2] <- remap.id(edgelist[e,2],infectids);
  }
  return(as.network(edgelist));
}
