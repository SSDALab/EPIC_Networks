# when all tests pass, warnings should be changed to stop

require(networkDynamic)
# -------------- activate.vertex.attribute----------------------

# gives error if argument not a network?
nd <-list()
result <-try(activate.vertex.attribute(nd,"letters","a",onset=0,terminus=1),T)
if (!"try-error"%in%class(result)){
  stop("activate.vertex.attribute did not throw error when initial argument was not a network")
}

nd <- network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=1)
activate.vertex.attribute(nd,"letters","b",onset=1,terminus=2)
activate.vertex.attribute(nd,"letters","c",onset=2,terminus=3)
# was the expected structure created?
if(!all(nd$val[[1]]$letters.active[[1]][[1]] == "a",
    nd$val[[1]]$letters.active[[1]][[2]] == "b",
    nd$val[[1]]$letters.active[[1]][[3]] == "c",
    all(nd$val[[1]]$letters.active[[2]] == matrix(c(0,1, 1,2, 2,3),ncol=2,byrow=TRUE)))){
  stop("activate.vertex.attribute did not create activity attribute with expected structure")
}
if(!all(nd$val[[2]]$letters.active[[1]][[1]] == "a",
        nd$val[[2]]$letters.active[[1]][[2]] == "b",
        nd$val[[2]]$letters.active[[1]][[3]] == "c",
        all(nd$val[[2]]$letters.active[[2]] == matrix(c(0,1, 1,2, 2,3),ncol=2,byrow=TRUE)))){
  stop("activate.vertex.attribute did not create activity attribute with expected structure")
}

# sets nD class on argument
if(!is.networkDynamic(nd)){
  stop("networkDynamic class was not set on network argument of activate.vertex.attribute")
}

# store and retreive attribute at specific times
if(!all(get.vertex.attribute.active(nd,"letters",onset=0,terminus=1) == c("a","a","a","a","a"),
    get.vertex.attribute.active(nd,"letters",onset=2,terminus=5) == c("c","c","c","c","c"))){
  stop("unexpected values returned for time ranges by get.vertex.attribute.active")
}

# invisably returns argument
nd2 <- activate.vertex.attribute(nd,"letters","z",onset=-1,terminus=0)
if(!all(deparse(nd2)==deparse(nd))){
  stop("activate.vertex.attribute did not return modified network argument or it does not match argument modified in place")
}

# apply to subset of vertices
activate.vertex.attribute(nd,"letters","d",onset=3,terminus=4,v=1:2)
if(!all(get.vertex.attribute.active(nd,"letters",onset=3,terminus=4)[1:2] == c("d","d"),
    is.na(get.vertex.attribute.active(nd,"letters",onset=3,terminus=4)[3:5]))){
  stop("activate.vertex.attribute did not correctly activate the assigned subset of vertex attributes")
}

# maintain spell order when added out of order?
nd <-network.initialize(5)
activate.vertex.attribute(nd,"letters","b",onset=1,terminus=2)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=1)
activate.vertex.attribute(nd,"letters","c",onset=2,terminus=3)
if (!all(nd$val[[1]]$letters.active[[2]] == matrix(c(0,1, 1,2, 2,3),ncol=2,byrow=TRUE))){
  stop("activate.vertex.attribute did not correctly maintain attribute spell ordering when added in non-temporal order")
}

# merge spells when same attribute added in adjoining spell
nd <-network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=1)
activate.vertex.attribute(nd,"letters","a",onset=1,terminus=2)
if (!all(nd$val[[1]]$letters.active[[2]] == matrix(c(0,2),ncol=2,byrow=TRUE))){
  stop("activate.vertex.attribute did not correctly merge adjoining attribute spells")
}

# overwrite existing spell
nd <-network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=1,terminus=2)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=3)
if (!all(nd$val[[1]]$letters.active[[2]] == matrix(c(0,3),ncol=2,byrow=TRUE))){
  stop("activate.vertex.attribute did not correctly overwrite attribute spells")
}


# store multiple object types
nd <-network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=1)
activate.vertex.attribute(nd,"letters",5,onset=1,terminus=2)
activate.vertex.attribute(nd,"letters",NULL,onset=2,terminus=3)
activate.vertex.attribute(nd,"letters",list(list(hello="world")),onset=3,terminus=4)
if (!all(nd$val[[1]]$letters.active[[1]][[1]] == "a",
         nd$val[[1]]$letters.active[[1]][[2]] == 5,
         nd$val[[1]]$letters.active[[1]][[3]] == NULL,
         nd$val[[1]]$letters.active[[1]][[4]]$hello == "world")){
  stop("activate.vertex.attribute did not correctly store attributes of different types")
}

# replace existing attribute with TEA
nd <-network.initialize(5)
set.vertex.attribute(nd,"letters","a")
activate.vertex.attribute(nd,"letters","b",onset=0,terminus=2)
if("letters"%in%list.vertex.attributes(nd) | !"letters.active"%in%list.vertex.attributes(nd)){
  stop("activate.vertex.attribute did not replace non-active attribute with same name")
}

# don't replace existing attribute with TEA if dynamic.only=TRUE
nd <-network.initialize(5)
set.vertex.attribute(nd,"letters","a")
activate.vertex.attribute(nd,"letters","b",onset=0,terminus=2,dynamic.only=TRUE)
if(!"letters"%in%list.vertex.attributes(nd) | !"letters.active"%in%list.vertex.attributes(nd)){
  stop("activate.vertex.attribute should not have removed non-active attribute with same name")
}

# test at syntax
nd <-network.initialize(5)
activate.vertex.attribute(nd,"letters","a",at=1)
if(!all(nd$val[[2]]$letters.active[[1]][[1]] == "a",
        all(nd$val[[2]]$letters.active[[2]] == matrix(c(1,1),ncol=2,byrow=TRUE)))){
  stop("activate.vertex.attribute did not create activity attribute with expected structure using 'at' parameter")
}

# test onset and length syntax
nd <-network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=1,length=5.5)
if(!all(nd$val[[2]]$letters.active[[1]][[1]] == "a",
        all(nd$val[[2]]$letters.active[[2]] == matrix(c(1,6.5),ncol=2,byrow=TRUE)))){
  stop("activate.vertex.attribute did not create activity attribute with expected structure using 'onset' nad 'length' parameters")
}

# test terminus and length syntax
nd <-network.initialize(5)
activate.vertex.attribute(nd,"letters","a",terminus=7,length=5.5)
if(!all(nd$val[[2]]$letters.active[[1]][[1]] == "a",
        all(nd$val[[2]]$letters.active[[2]] == matrix(c(1.5,7),ncol=2,byrow=TRUE)))){
  stop("activate.vertex.attribute did not create activity attribute with expected structure using 'onset' nad 'length' parameters")
}


# test no time params
nd <-network.initialize(5)
activate.vertex.attribute(nd,"letters","a")
if(!all(nd$val[[2]]$letters.active[[1]][[1]] == "a",
        all(nd$val[[2]]$letters.active[[2]] == matrix(c(-Inf,Inf),ncol=2,byrow=TRUE)))){
  stop("activate.vertex.attribute did not create activity attribute with expected structure using no time prams")
}


# test assigning multiple values
nd <-network.initialize(5)
activate.vertex.attribute(nd,"letters",c("a","b","c","d","e"))
if(!all(get.vertex.attribute.active(nd,"letters",onset=1,terminus=1)==c("a","b","c","d","e"))){
  stop("activate.vertex.attribute did not correctly assign multiple values to multiple vertices")
}


# test assigning multiple times
nd <-network.initialize(5)
activate.vertex.attribute(nd,"letters","a",at=c(1,2,3,4,5))
if(!all(all(nd$val[[1]]$letters.active[[2]] == matrix(c(1,1),ncol=2,byrow=TRUE)),
        all(nd$val[[2]]$letters.active[[2]] == matrix(c(2,2),ncol=2,byrow=TRUE)),
        all(nd$val[[3]]$letters.active[[2]] == matrix(c(3,3),ncol=2,byrow=TRUE)),
        all(nd$val[[4]]$letters.active[[2]] == matrix(c(4,4),ncol=2,byrow=TRUE)),
        all(nd$val[[5]]$letters.active[[2]] == matrix(c(5,5),ncol=2,byrow=TRUE)))){
  stop("activate.vertex.attribute did not create activity attribute with expected values using multiple input times")
}

# test assigning complex objects (lists)
nd <-network.initialize(5)
activate.vertex.attribute(nd,'letters',list(list("a","b")),onset=0,terminus=1)
if(!all(nd$val[[2]]$letters.active[[1]][[1]][[1]]=="a",nd$val[[2]]$letters.active[[1]][[1]][[2]]=="b")){
  stop("activate.vertex.attribute did not store list object in expected form")
}

activate.vertex.attribute(nd,'letters',list(list("c","d")),onset=1,terminus=2)
if(!all(nd$val[[2]]$letters.active[[1]][[2]][[1]]=="c",nd$val[[2]]$letters.active[[1]][[2]][[2]]=="d")){
  stop("activate.vertex.attribute did not store list object in expected form")
}


# test passing in zero-length list of vertices returns net unmodified
nd <-network.initialize(5)
nd2 <-activate.vertex.attribute(nd,'letters',list(list("a","b")),onset=0,terminus=1,v=numeric(0))
if (!all(deparse(nd)==deparse(nd2))){
  stop("activate.vertex.attribute did not return network argument unchanged when v is length 0")
}

# ----- get.vertex.attribute.active ----------------

# return appropriate value for structure (allready tested above)
nd <-network.initialize(5)
activate.vertex.attribute(nd,'letters',"a",onset=0,terminus=1)
activate.vertex.attribute(nd,'letters',"b",onset=1,terminus=2)

if (!all(get.vertex.attribute.active(nd,'letters',onset=0,terminus=1)==rep("a",5),
get.vertex.attribute.active(nd,'letters',onset=1,terminus=2)==rep("b",5))){
  stop("get.vertex.attribute.active did not return expected values for simple spell queries")
}

# test query that will return multiple values (should throw warning)
if (!all(try(get.vertex.attribute.active(nd,'letters',onset=0,terminus=3),silent=TRUE)=="a")){
  stop("get.vertex.attribute.active did not return expected values for query encompasing multiple values")
}
#cant figure how to check if warning message generated

         
# test unlist and structure
nd <-network.initialize(5)
activate.vertex.attribute(nd,'letters',"a",onset=0,terminus=1) 
result<-get.vertex.attribute.active(nd,"letters",onset=0,terminus=1,unlist=FALSE)         
if (class(result[[1]])=="list"){
  stop("get.vertex.attribute.active returned values with unexpected list nesting when unlist=FALSE")
}




# test at param
if (!all(get.vertex.attribute.active(nd,'letters',at=0)==rep("a",5))){
     stop("get.vertex.attribute.active did not return expected values for at query")
}
         
# test length param
if (!all(get.vertex.attribute.active(nd,'letters',onset=0,length=1)==rep("a",5))){
 stop("get.vertex.attribute.active did not return expected values for length query")
}


# test storing a list object
nd <-network.initialize(5)
activate.vertex.attribute(nd,'letters',list(list("a","b")),onset=0,terminus=1) 
if (!all(get.vertex.attribute.active(nd,"letters",onset=0,terminus=1,unlist=FALSE)[[3]][[1]]=="a",
         get.vertex.attribute.active(nd,"letters",onset=0,terminus=1,unlist=FALSE)[[3]][[2]]=="b")){
  stop("get.vertex.attribute.active did not return stored list object correctly")
}

# find non-active version if active not found
nd <-network.initialize(5)
set.vertex.attribute(nd,"letters","a")
if(!all(get.vertex.attribute.active(nd,"letters",onset=1,terminus=2) == rep("a",5))){
  stop("get.vertex.attribute did not correctly return non-TEA attribute when TEA attribute not found")
}
         
 # don't find non-active version if dynamic.only = TRUE
if (!all(is.na(get.vertex.attribute.active(nd,"letters",onset=1,terminus=2,dynamic.only=TRUE)))){
  stop("get.vertex.attribute failed to return NA instead of matching non-TEA attribute when dynamic.only=TRUE")
}

# return NAs when no attributes match
nd <-network.initialize(5)
if(!all(is.na(get.vertex.attribute.active(nd,"letters",onset=1,terminus=2)))){
  stop("get.vertex.attribute did not return NAs when no attribute found matching name")
}

# test return teas should return truncated spell lists
nd <-network.initialize(5)
activate.vertex.attribute(nd,'letters',"a",onset=0,terminus=1)
activate.vertex.attribute(nd,'letters',"b",onset=1,terminus=2)
activate.vertex.attribute(nd,'letters',"c",onset=2,terminus=3)
result <-get.vertex.attribute.active(nd,'letters',onset=1,terminus=2,return.tea=TRUE)
if(!all(result[[1]][[1]]=="b",result[[1]][[2]]==c(1,2),result[[5]][[1]]=="b",result[[5]][[2]]==c(1,2))){
  stop("get.vertex.attribute.active did not return expected list structures when return.tea=TRUE")
}

# test if some vertices have activity and not others
nd <-network.initialize(5)
activate.vertex.attribute(nd,'letters',"a",onset=0,terminus=1,v=c(1,2,4,5))
if(!all(get.vertex.attribute.active(nd,"letters",onset=0,terminus=1)[c(1,2,4,5)]=="a",is.na(get.vertex.attribute.active(nd,"letters",onset=0,terminus=1)[3]))){
  stop("get.vertex.attribute.active did not return NA for vertices missing the active attribute")
}

# test require.active argument
nd <-network.initialize(5)
activate.vertex.attribute(nd,'letters',"a",onset=0,terminus=1)
deactivate.vertices(nd,v=2:3)
result <-get.vertex.attribute.active(nd,'letters',onset=0,terminus=1,require.active=TRUE)  
if(!all(result[c(1,4,5)]=="a",is.na(result[2:3]))){
  stop("get.vertex.attribute.active did not return NA for attributes of inactive vertices when require.active=TRUE")
}

# this was crashing at one point
get.vertex.attribute.active(network.initialize(3),"test",at=1,require.active=TRUE)
         
# test active.default argument
nd <-network.initialize(5)
activate.vertex.attribute(nd,'letters',"a",onset=0,terminus=1)
activate.vertices(nd,v=2:3)
result <-get.vertex.attribute.active(nd,'letters',onset=0,terminus=1,require.active=TRUE,active.default=FALSE)  
if(!all(result[2:3]=="a",is.na(result[c(1,4,5)]))){
   stop("get.vertex.attribute.active did not return NA for attributes of vertices considered inactive when require.active=TRUE and active.default=FALSE")
}         
         
# test na.omit argument
nd <-network.initialize(5)
nd$val[[3]]$na <- TRUE
activate.vertex.attribute(nd,"letters",c("a","b","c","d","e"),onset=0,terminus=1)
if(!all(get.vertex.attribute.active(nd,"letters",onset=0,terminus=1,na.omit=TRUE)==c("a","b","d","e"))){
  warning("get.vertex.attribute.active did not correctly omit values for missing vertex when na.omit=TRUE")
}
#NOTE: it does the na.omit ok, but is returning attributes in wrong index positions think it is a feature issue in network

# test null.na argument
nd <-network.initialize(5)
if(!all(is.na(get.vertex.attribute.active(nd,"letters",onset=0,terminus=1,null.na=TRUE)))){
  stop("get.vertex.attribute.active did not return NA instead of NULL when null.na=TRUE")
}

# test rule (any vs all) argument
nd <-network.initialize(5)
activate.vertex.attribute(nd,'letters',"a",onset=0,terminus=1)
if (!all(is.na(get.vertex.attribute.active(nd,'letters',onset=-1,terminus=2,rule="all")))){
  stop("get.vertex.attribute.active did not return expected values when rule='all'")
}
     
         
# test multiple onsets


# -------------------- activate.edge.attribute ---------------------
# gives error if argument not a network?
nd <-list()
result <-try(activate.edge.attribute(nd,"letters","a",onset=0,terminus=1),T)
if (!"try-error"%in%class(result)){
  stop("activate.edge.attribute did not throw error when initial argument was not a network")
}

# activate attribute on non existant edge should throw error
nd <- network.initialize(5)
add.edge(nd,1,2)
result <-try(activate.edge.attribute(nd,"letters","a",onset=0,terminus=1,e=2),T)
if (!"try-error"%in%class(result)){
  stop("activate.edge.attribute did not throw error when bad edge specified")
}

nd <- network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters","a",onset=0,terminus=1)
activate.edge.attribute(nd,"letters","b",onset=1,terminus=2)
activate.edge.attribute(nd,"letters","c",onset=2,terminus=3)
# was the expected structure created?
if(!all(nd$mel[[1]]$atl$letters.active[[1]][[1]] == "a",
        nd$mel[[1]]$atl$letters.active[[1]][[2]] == "b",
        nd$mel[[1]]$atl$letters.active[[1]][[3]] == "c",
        all(nd$mel[[1]]$atl$letters.active[[2]] == matrix(c(0,1, 1,2, 2,3),ncol=2,byrow=TRUE)))){
  stop("activate.edge.attribute did not create activity attribute with expected structure")
}

# sets nD class on argument
if(!is.networkDynamic(nd)){
  stop("networkDynamic class was not set on network argument of activate.edge.attribute")
}

# store and retreive attribute at specific times
if(!all(get.edge.value.active(nd,"letters",onset=0,terminus=1) == c("a","a","a"),
        get.edge.value.active(nd,"letters",onset=2,terminus=5) == c("c","c","c"))){
  stop("unexpected values returned for time ranges by get.edge.value.active")
}

if(length(get.edge.value.active(nd,"letters",onset=0,terminus=1)) != network.edgecount(nd)){
  warning("number of values returned by get.edge.value.active does not match number of edges")
}

# invisably returns argument
nd2 <- activate.edge.attribute(nd,"letters","z",onset=-1,terminus=0)
if(!all(deparse(nd2)==deparse(nd))){
  stop("activate.edge.attribute did not return modified network argument or it does not match argument modified in place")
}

# what if an edge is null because it was deleted

# apply to subset of edge
nd <-network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters","d",onset=3,terminus=4,e=1:2)
if(!all(get.edge.value.active(nd,"letters",onset=3,terminus=4)[1:2] == c("d","d"), is.na(get.edge.value.active(nd,"letters",onset=3,terminus=4)[3]))){
  stop("activate.edge.attribute did not correctly activate the assigned subset of vertex attributes")
}

# maintain spell order when added out of order?
nd <-network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters","b",onset=1,terminus=2)
activate.edge.attribute(nd,"letters","a",onset=0,terminus=1)
activate.edge.attribute(nd,"letters","c",onset=2,terminus=3)
if (!all(nd$mel[[1]]$atl$letters.active[[2]] == matrix(c(0,1, 1,2, 2,3),ncol=2,byrow=TRUE))){
  stop("activate.vertex.attribute did not correctly maintain attribute spell ordering when added in non-temporal order")
}


# error if onset > terminus?
result <- try(activate.vertex.attribute(nd,"letters","z",onset=6,terminus=5),T)
if (!"try-error"%in%class(result)){
  warning("activate.edge.attribute did not throw error when onset > terminus")
}

# convert existing attribute to TEA

# test at syntax

# test length syntax

# ----------------- get.edge.value.active ---------------

# return values when only subset of edges have activated attributes
nd <-network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters","d",onset=3,terminus=4,e=1:2)
vals <-get.edge.value.active(nd,'letters',onset=3,terminus=4,unlist=FALSE)
if (!all(vals[[1]]=="d",vals[[2]]=="d",is.na(vals[[3]]))){
  stop("get.edge.value.active returned unexpected values when some edges did not have activity attribute set")
}

# test return.tea = TRUE
nd <-network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters","a",onset=0,terminus=1)
activate.edge.attribute(nd,"letters","b",onset=1,terminus=2)
activate.edge.attribute(nd,"letters","c",onset=2,terminus=3)
if(!all(get.edge.value.active(nd,"letters",onset=1,terminus=2,return.tea=TRUE)[[3]][[1]][[1]]=="b",get.edge.value.active(nd,"letters",onset=1,terminus=2,return.tea=TRUE)[[3]][[2]]==c(1,2))){
  stop("get.edge.value.active did not return expected structure when return.tea=TRUE")
}
# test query spell includes multiple values
if(!all(get.edge.value.active(nd,"letters",onset=0,terminus=3)=="a")){
  stop("get.edge.value.active did not return earliest values when query spell matched multiple")
}

if(!all(get.edge.value.active(nd,"letters",onset=0,terminus=3,return.tea=TRUE)[[3]][[2]]== matrix(c(0,1, 1,2, 2,3), ncol=2,byrow=TRUE),
        get.edge.value.active(nd,"letters",onset=0,terminus=3,return.tea=TRUE)[[3]][[1]][[1]]=="a",
        get.edge.value.active(nd,"letters",onset=0,terminus=3,return.tea=TRUE)[[3]][[1]][[2]]=="b",
        get.edge.value.active(nd,"letters",onset=0,terminus=3,return.tea=TRUE)[[3]][[1]][[3]]=="c")){
  stop("get.edge.value.active did not return multiple values when query spell matched multiple and return.tea=TRUE")
}


# test unlist

# test not return value if edge not active and require.active = TRUE
nd <-network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
activate.edges(nd,e=1,onset=0,terminus=2)
deactivate.edges(nd,e=2,onset=0,terminus=2)
activate.edge.attribute(nd,"letters","a",onset=0,terminus=1)
if(!all(get.edge.value.active(nd,"letters",onset=0,terminus=1,require.active=TRUE)[1]=="a",is.na(get.edge.value.active(nd,"letters",onset=0,terminus=1,require.active=TRUE)[2]))){
  stop("get.edge.value.active did not return expected values for inactive edge when require.active=TRUE")
}

# test deleted edge
nd <-network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters",list("a","b","c"),onset=0,terminus=1)
delete.edges(nd,eid=2)
if (!all(get.edge.value.active(nd,"letters",onset=0,terminus=1)[c(1,3)]==c("a","c"),
         is.na(get.edge.value.active(nd,"letters",onset=0,terminus=1)[2]))){
  stop('get.edge.value.active did not return expected values when edge was deleted')
}
         
# find non-active version if active not found

nd <-network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
set.edge.attribute(nd,"letters",list("a","b","c"))
if(!all(get.edge.value.active(nd,"letters",onset=1,terminus=2) == c("a","b","c"))){
  stop("get.vertex.attribute did not correctly return non-TEA attribute")
}

# don't find non-active version if dynamic.only = TRUE
if (!all(is.na(get.edge.value.active(nd,"letters",onset=1,terminus=2,dynamic.only=TRUE)))){
  stop("get.vertex.attribute failed to return NA instead of matching non-TEA attribute when dynamic.only=TRUE")
}

# return NAs if no matching attribute
nd <-network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
get.edge.value.active(nd,"letters",onset=1,terminus=2,unlist=FALSE)

# what if network has no edges?
nd <-network.initialize(5)
activate.edge.attribute(nd,"letters","a",onset=0,terminus=1)

# --------- activate.network.attribute ------------------------
nd <-network.initialize(5)
activate.network.attribute(nd,"test","a",onset=1,terminus=2)

# expected structure?
if(!all(nd$gal$test.active[[1]][[1]]=="a",nd$gal$test.active[[2]]==c(1,2))){
  stop("activate.network.attribute did not store attribute and activity spell in expected structure")
}

activate.network.attribute(nd,"test","b",onset=2,terminus=3)
if(!all(nd$gal$test.active[[1]][[2]]=="b",nd$gal$test.active[[2]][2,]==c(2,3))){
  stop("activate.network.attribute did not store attribute and activity spell in expected structure")
}

# got nd class?
if(!is.networkDynamic(nd)){
  stop("activate.network.attribute did not set networkDynamic class")
}

#store list
activate.network.attribute(nd,"test",list("c","d"),onset=3,terminus=4)
if(!all(unlist(nd$gal$test.active[[1]][[3]])==c("c","d"),nd$gal$test.active[[2]][3,]==c(3,4))){
  stop("activate.network.attribute did not store attribute and activity spell in expected structure")
}

# test length
nd <-network.initialize(5)
activate.network.attribute(nd,"test","a",onset=1,length=2)
if(!all(nd$gal$test.active[[1]][[1]]=="a",nd$gal$test.active[[2]]==c(1,3))){
  stop("activate.network.attribute did not store attribute and activity spell in expected structure when using 'length' param")
}


# test at
nd <-network.initialize(5)
activate.network.attribute(nd,"test","a",at=2)
if(!all(nd$gal$test.active[[1]][[1]]=="a",nd$gal$test.active[[2]]==c(2,2))){
  stop("activate.network.attribute did not store attribute and activity spell in expected structure when using 'at' param")
}

# test dynamic only 
nd <-network.initialize(5)
set.network.attribute(nd,"test","a")
activate.network.attribute(nd,"test","b",onset=1,terminus=2)
if ("test"%in%list.network.attributes(nd)){
  stop("same-named non-active attribute was not replaced by activate.network.attribute ")
}

# test dynamic.only = TRUE
nd <-network.initialize(5)
set.network.attribute(nd,"test","a")
activate.network.attribute(nd,"test","b",onset=1,terminus=2,dynamic.only=TRUE)
if (!"test"%in%list.network.attributes(nd)){
  stop("same-named non-active attribute was wrongly replaced by activate.network.attribute when dynamic.only=TRUE")
}


# ---- get.network.attribute.active
nd <-network.initialize(5)
activate.network.attribute(nd,"test","a",onset=1,terminus=2)



# expected structure?
if(!get.network.attribute.active(nd,"test",onset=1,terminus=1)=="a"){
  stop("get.network.attribute.active did not return expected attribute value")
}

activate.network.attribute(nd,"test","b",onset=2,terminus=3)
if(!get.network.attribute.active(nd,"test",onset=2,terminus=3)=="b"){
  stop("get.network.attribute.active did not return expected attribute value")
}


#store list
activate.network.attribute(nd,"test",list("c","d"),onset=3,terminus=4)
if(!all(unlist(get.network.attribute.active(nd,"test",onset=3,terminus=4))==c("c","d"))){
  stop("get.network.attribute.active did not return expected attribute value")
}

# return multiple values with warning
nd <-network.initialize(5)
activate.network.attribute(nd,"test","a",onset=1,terminus=2)
activate.network.attribute(nd,"test","b",onset=2,terminus=3)
activate.network.attribute(nd,"test","c",onset=3,terminus=4)
if(!get.network.attribute.active(nd,"test",onset=2,terminus=4)=="b"){
  stop("get.network.attribute.active did not return appropriate value when multiple attributes matched query spell")
}


# return appropriate value for return.tea = T
returned <-get.network.attribute.active(nd,"test",onset=2,terminus=4,return.tea=TRUE)
if(!all(returned[[1]][[1]]=="b",returned[[1]][[2]]=="c",returned[[2]][1,]==c(2,3),returned[[2]][2,]==c(3,4))){
  stop("get.network.attribute.active did not return appropriate value when multiple attributes matched query spell")
}


# test dynamic only fall through
nd <-network.initialize(5)
set.network.attribute(nd,"test","a")
if (!get.network.attribute.active(nd,"test",onset=2,terminus=4)=="a"){
  stop("get.network.attribute.active did not return non-dynamic attribute when TEA not found")
}

# test dynamic only not fall through
if (!is.null(get.network.attribute.active(nd,"test",onset=2,terminus=4,dynamic.only=TRUE))){
  stop("get.network.attribute.active did not return NULL instead non-dynamic attribute when TEA not found and dynamic.only=TRUE")
}

# test unlist
nd <-network.initialize(5)
activate.network.attribute(nd,"test",list("a","b"),onset=1,terminus=2)
if(!all(get.network.attribute.active(nd,"test",onset=1,terminus=2)==c("a","b"))){
  stop("get.network.attribute.active did not unlist returned attribute correctly")
}
                           
if(!all(is.list(get.network.attribute.active(nd,"test",onset=1,terminus=2,unlist=FALSE)),get.network.attribute.active(nd,"test",onset=1,terminus=2,unlist=FALSE)[[1]]=="a",get.network.attribute.active(nd,"test",onset=1,terminus=2,unlist=FALSE)[[2]]=="b")){
  stop("get.network.attribute.active did not return attribute correctly in list when unlist=FALSE")
}      

# test at
nd <-network.initialize(5)
activate.network.attribute(nd,"test","a",onset=1,terminus=2)
activate.network.attribute(nd,"test","b",onset=2,terminus=3)
activate.network.attribute(nd,"test","c",onset=5,terminus=6)

if(get.network.attribute.active(nd,"test",at=2)!="b"){
  stop("get.network.attribute.active did not return expected value when using 'at' parameter")
}

# test length
if(get.network.attribute.active(nd,"test",onset=0,length=1.5)!="a"){
  stop("get.network.attribute.active did not return expected value when using 'length' parameter")
}

# test rule

if(!is.na(get.network.attribute.active(nd,"test",onset=2,terminus=4,rule="all"))){
  stop("get.network.attribute.active did not return expected value when using rule='all' parameter")
}
if(get.network.attribute.active(nd,"test",onset=2,terminus=4,rule="any")!="b"){
  stop("get.network.attribute.active did not return expected value when using rule='any' parameter")
}

# test out of range
nd <-network.initialize(5)
activate.network.attribute(nd,"test","a",onset=1,terminus=2)
if(!is.na(get.network.attribute.active(nd,"test",onset=3,terminus=4))){
  stop("get.network.attribute.active did not return NA when query spell did not match any attributes")
}

##########################################################################
# ------list.vertex.attributes.active ----
######################################################################################
nd <- network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=3)
activate.vertex.attribute(nd,"letters","b",onset=4,terminus=5)
activate.vertex.attribute(nd,"letters","a",onset=5,terminus=7)
activate.vertex.attribute(nd,"numbers","1",onset=2,terminus=3)
activate.vertex.attribute(nd,"numbers","2",onset=4,terminus=8)
activate.vertex.attribute(nd,"numbers","3",onset=8,terminus=10)
if(!all(list.vertex.attributes.active(nd,onset=-Inf,terminus=Inf)==c("na","vertex.names","letters.active","numbers.active"))){
  stop("list.vertex.attributes.active did not return expected values")
}

if(!all(list.vertex.attributes.active(nd,onset=-Inf,terminus=Inf,dynamic.only=TRUE)==c("letters.active","numbers.active"))){
  stop("list.vertex.attributes.active did not return expected values when dynamic.only=TRUE")
}


##########################################################################
##########################################################################
#----- list.edge.values.active----
######################################################################################



nd <- network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters","a",onset=0,terminus=3)
activate.edge.attribute(nd,"letters","b",onset=4,terminus=5)
activate.edge.attribute(nd,"letters","a",onset=5,terminus=7)
activate.edge.attribute(nd,"numbers","1",onset=2,terminus=3)
activate.edge.attribute(nd,"numbers","2",onset=4,terminus=8)
activate.edge.attribute(nd,"numbers","3",onset=8,terminus=10)

if(!all(list.edge.values.active(nd,onset=-Inf,terminus=Inf)==c('na','letters.active','numbers.active'))){
  stop("list.edge.values.active returned unexpected values")
}

# test dynamic only
if(!all(list.edge.values.active(nd,onset=-Inf,terminus=Inf,dynamic.only=TRUE)==c('letters.active','numbers.active'))){
  stop("list.edge.values.active returned unexpected values when dynamic.only=TRUE")
}

# test time range
if(!all(list.edge.values.active(nd,onset=8,terminus=10)==c('na','numbers.active'))){
  stop("list.edge.values.active returned unexpected values")
}


##########################################################################
##########################################################################
# ------list.network.attributes.active-------
######################################################################################

nd <-network.initialize(5)
activate.network.attribute(nd,"test","a",onset=1,terminus=4)
if(!all(list.network.attributes.active(nd,onset=-Inf,terminus=Inf) ==c("bipartite","directed","hyper"     ,"loops","mnext","multiple","n","test.active"))){
  stop("list.network.attributes.active did not return expected results")
}

# outside of range
if(!all(list.network.attributes.active(nd,onset=-Inf,terminus=0) ==c("bipartite","directed","hyper"     ,"loops","mnext","multiple","n"))){
  stop("list.network.attributes.active did not return expected results for query that should miss attributes")
}

# dynamic only
if(!all(list.network.attributes.active(nd,onset=-Inf,terminus=Inf,dynamic.only=TRUE) =="test.active")){
  stop("list.network.attributes.active did not return expected results for dynamic.only=TRUE")
}

# also add a more complex attribute
nd <-network.initialize(5)
activate.network.attribute(nd,"test",c("a","b"),onset=1,terminus=4)
if(list.network.attributes.active(nd,onset=-Inf,terminus=Inf,dynamic.only=TRUE)!="test.active"){
  stop("list.network.attributes.active did not return expect results for vector attributes")
}


#deactivate.vertex.attribute
####################################################################################################################################################
#testing if deactivating a spell with one value, leaving rest spell active
##########################################################################
nd <- network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=1)
activate.vertex.attribute(nd,"letters","b",onset=1,terminus=2)
activate.vertex.attribute(nd,"letters","c",onset=2,terminus=3)
deactivate.vertex.attribute(nd,prefix="letters",onset=1,terminus=2)

# was the expected structure created?
if(!all(nd$val[[1]]$letters.active[[1]][[1]] == "a",
        nd$val[[1]]$letters.active[[1]][[2]] == "c",
        all(nd$val[[1]]$letters.active[[2]] == matrix(c(0,1, 2,3),ncol=2,byrow=TRUE)))){
  stop("deactivate.vertex.attribute did not deactivate attribute with expected structure")
}



##########################################################################
#testing if deactivating a spell with one value, leaving rest spell active
##########################################################################

nd <- network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=1)
activate.vertex.attribute(nd,"letters","b",onset=1,terminus=2)
activate.vertex.attribute(nd,"letters","c",onset=2,terminus=3)
deactivate.vertex.attribute(nd,prefix="letters",onset=-Inf,terminus=Inf)

# was the expected structure created?
if(!all(nd$val[[1]]$letters.active[[1]][[1]]=="NA",
        nd$val[[1]]$letters.active[[2]]==matrix(c(Inf,Inf),ncol=2)
)){
  stop("deactivate.vertex.attribute did not deactivate attribute with expected structure")
}


##########################################################################
#testing if the deactivating period across two spells
##########################################################################

nd <- network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=2)
activate.vertex.attribute(nd,"letters","b",onset=3,terminus=5)
activate.vertex.attribute(nd,"letters","c",onset=5,terminus=7)
deactivate.vertex.attribute(nd,prefix="letters",onset=1,terminus=4)

# was the expected structure created?
if(!all(nd$val[[1]]$letters.active[[1]][[1]] == "a",
        nd$val[[1]]$letters.active[[1]][[2]] == "b",
        nd$val[[1]]$letters.active[[1]][[3]] == "c",
        all(nd$val[[1]]$letters.active[[2]] == matrix(c(0,1, 4,5, 5,7),ncol=2,byrow=TRUE)))){
  stop("deactivate.vertex.attribute did not deactivate attribute with expected structure")
}

##########################################################################
#testing if the deactivating period within one spell, and keep the spell time order
##########################################################################

nd <- network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=1)
activate.vertex.attribute(nd,"letters","b",onset=2,terminus=5)
activate.vertex.attribute(nd,"letters","c",onset=5,terminus=7)
deactivate.vertex.attribute(nd,prefix="letters",onset=3,terminus=4)

# was the expected structure created?
if(!all(nd$val[[1]]$letters.active[[1]][[1]] == "a",
        nd$val[[1]]$letters.active[[1]][[2]] == "b",
        nd$val[[1]]$letters.active[[1]][[3]] == "b",
        nd$val[[1]]$letters.active[[1]][[4]] == "c",
        all(nd$val[[1]]$letters.active[[2]] == matrix(c(0,1,2,3,4,5,5,7),ncol=2,byrow=TRUE)))){
  stop("deactivate.vertex.attribute did not deactivate attribute with expected structure")
}

##########################################################################
#testing if the deactivating the non-activate period
##########################################################################

nd <- network.initialize(5)
activate.vertex.attribute(nd,"letters","a",onset=0,terminus=1)
activate.vertex.attribute(nd,"letters","b",onset=4,terminus=5)
activate.vertex.attribute(nd,"letters","c",onset=5,terminus=7)
deactivate.vertex.attribute(nd,prefix="letters",onset=2,terminus=3)

# was the expected structure created?
if(!all(nd$val[[1]]$letters.active[[1]][[1]] == "a",
        nd$val[[1]]$letters.active[[1]][[2]] == "b",
        nd$val[[1]]$letters.active[[1]][[3]] == "c",
        all(nd$val[[1]]$letters.active[[2]] == matrix(c(0,1,4,5,5,7),ncol=2,byrow=TRUE)))){
  stop("deactivate.vertex.attribute did not deactivate attribute with expected structure")
}


##########################################################################
##########################################################################
# ------ deactivate.edge.attribute
##########################################################################
#testing if deactivating a spell with one value, leaving rest spell active
##########################################################################
nd <- network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters","a",onset=0,terminus=1)
activate.edge.attribute(nd,"letters","b",onset=1,terminus=2)
activate.edge.attribute(nd,"letters","c",onset=2,terminus=3)

deactivate.edge.attribute(x=nd, prefix="letters", onset=1, terminus=2)

if(!all(nd$mel[[1]]$atl$letters.active[[1]][[1]] == "a",
        nd$mel[[1]]$atl$letters.active[[1]][[2]] == "c",
        all(nd$mel[[1]]$atl$letters.active[[2]] == matrix(c(0,1,2,3),ncol=2,byrow=TRUE)))){
  stop("deactivate.vertex.attribute did not deactivate attribute with expected structure")
}


##########################################################################
#testing if deactivating all spells. 
##########################################################################
nd <- network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters","a",onset=0,terminus=1)
activate.edge.attribute(nd,"letters","b",onset=1,terminus=2)
activate.edge.attribute(nd,"letters","c",onset=2,terminus=3)

deactivate.edge.attribute(x=nd, prefix="letters", onset=-Inf, terminus=Inf)

if(!all(nd$mel[[1]]$atl$letters.active[[1]][[1]]=="NA",
        nd$mel[[1]]$atl$letters.active[[2]]==matrix(c(Inf,Inf),ncol=2)
        
)){
  stop("deactivate.edge.attribute did not deactivate attribute with expected structure")
}


##########################################################################
#testing if the deactivating period across two spells
##########################################################################
nd <- network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters","a",onset=0,terminus=2)
activate.edge.attribute(nd,"letters","b",onset=2,terminus=4)
activate.edge.attribute(nd,"letters","c",onset=5,terminus=6)

deactivate.edge.attribute(x=nd, prefix="letters", onset=1, terminus=3)

if(!all(nd$mel[[1]]$atl$letters.active[[1]][[1]] == "a",
        nd$mel[[1]]$atl$letters.active[[1]][[2]] == "b",
        nd$mel[[1]]$atl$letters.active[[1]][[3]] == "c",
        all(nd$mel[[1]]$atl$letters.active[[2]] == matrix(c(0,1,3,4,5,6),ncol=2,byrow=TRUE)))){
  stop("deactivate.edge.attribute did not deactivate attribute with expected structure")
}
##########################################################################


##########################################################################
#testing if the deactivating period within one spell
##########################################################################
nd <- network.initialize(5)
add.edge(nd,1,2)
add.edge(nd,2,3)
add.edge(nd,3,4)
activate.edge.attribute(nd,"letters","a",onset=0,terminus=1)
activate.edge.attribute(nd,"letters","b",onset=1,terminus=4)
activate.edge.attribute(nd,"letters","c",onset=5,terminus=6)

deactivate.edge.attribute(x=nd, prefix="letters", onset=2, terminus=3)

if(!all(nd$mel[[1]]$atl$letters.active[[1]][[1]] == "a",
        nd$mel[[1]]$atl$letters.active[[1]][[2]] == "b",
        nd$mel[[1]]$atl$letters.active[[1]][[3]] == "b",
        nd$mel[[1]]$atl$letters.active[[1]][[4]] == "c",
        all(nd$mel[[1]]$atl$letters.active[[2]] == matrix(c(0,1,1,2,3,4,5,6),ncol=2,byrow=TRUE)))){
  stop("deactivate.edge.attribute did not deactivate attribute with expected structure")
}
##########################################################################



##########################################################################
##########################################################################
# ----------------- deactivate.network.attribute
##########################################################################

nd <-network.initialize(5)
activate.network.attribute(nd,"test","a",onset=1,terminus=3)
activate.network.attribute(nd,"test","b",onset=4,terminus=5)
activate.network.attribute(nd,"test","c",onset=6,terminus=10)

deactivate.network.attribute(nd,"test",onset=2,terminus=7)

if(!all(nd$gal$test.active[[1]][[1]] == "a",
        nd$gal$test.active[[1]][[2]] == "c",
        all(nd$gal$test.active[[2]] == matrix(c(1,2,7,10),ncol=2,byrow=TRUE)))){
  stop("deactivate.network.attribute did not deactivate activity attribute with expected structure")
}



