## function to propagate values through a time based network
percolate.dynamic <- function(dynet,
  item,   #name of variable containing infection status
  infected.state="i", #status indicating infectivity
  tprob=1.0,        #transmission probability for each event
  time.range=get.time.bounds(dynet),
  time.increment=1.0,
  transmit.all = TRUE)    #should all ties active at a timestep be picked for transmission?
  {
#check that network is dynamic

#start at the beginning of the time.range
  now <- time.range[1];
  #at each time point
  while (now < time.range[2]){
   #find all the infected nodes (nodes with open infection intervals)
   infected <- get.verts.with(dynet,now,item,infected.state);
    #for each infected node, get the edges active at the time point
    for (v in infected){
     eids <- get.edgeIDs.at(dynet,time.point=now,v=v);
    #pick one of the active edges uniformly at random  (or according to edge weight...)
    # or if transmit.all = TRUE, pick all edges
    if (transmit.all){
      for (eid in eids){
        #decide if transmission occurs by picking a probability to compare with tprob
        #if transmission occurs
        if (runif(1) <= tprob){
           #determine if transmission was concurrent
            #mark edge, and node at other end as infected  by copy
            dynet <- set.dynamic.vertex.attribute(dynet,item,infected.state,
            valid.time = now);
        }
      }
       } else {
     stop("random edge picking not yet supported");
    }
    }
#increment the time  and repeat
  now <- now + time.increment;
  }



}
