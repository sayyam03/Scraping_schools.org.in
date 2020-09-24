rm(list=ls())
library(httr)
library(XML)
library(miscTools)
library(xml2)
library(pbapply)
library(openxlsx)
##Function for getting links to cluster, district and blocks within a state.
reqstate <- function(state= as.character()){
  raw<- readline("Enter state name: ")
  raw<- tolower(raw)
  ##Finding all the states
  a1<- GET("https://schools.org.in/schools-in-india.html")
  a2<-read_html(a1)
  a3<- xml_find_all(a2,"//td/a")
  a4<- xml_attr(a3,"href")
  a3<- xml_find_all(a2,"//td")
  a5<- xml_text(a3)
  
  no_of_schools<- c()
  for(p in 1:108){
    if(p%%3 == 0){
      a6<- a5[p]
      no_of_schools<- c(no_of_schools,a6)
    }
  }
  
  
  if(raw == "NA"){
    stop("NA initiated")
  }
  state<- paste0(unlist(strsplit(raw, " ")), collapse = "-")
  if(state %in% a4 == FALSE){
    print("You have entered a wrong state.")
    raw<- readline("Please enter a correct state: ")
    raw<- tolower(raw)
    state<- paste0(unlist(strsplit(raw, " ")), collapse = "-")
    
  }
  index<- match(state,a4)
  no.of.schools<- no_of_schools[index]
  print(paste0("The state of ",state," has ", no.of.schools, "schools"))
  
  baseURL<- "https://schools.org.in/"
  ## Getting district links in the given state
  URL<- paste0(baseURL,state )
  h<- GET(URL)
  x<- htmlTreeParse(h, useInternalNodes = T)
  rootnodes<- xmlRoot(x)
  districtlinks<- xpathSApply(rootnodes,"//td/a/@href")
  nd<- length(districtlinks)
  links<-c()
  
  print("Getting links of all zones in the state....")
  ## Getting a progress bar
  pboptions(style = 1, char = ">", type = "timer" )
  pb = startpb(0,nd) 
  
  ## Loop for extracting blocks in within the district
 
    for(i in 1:nd){
    ##setpb(pb,i)
    URL1<- paste0(baseURL,districtlinks[i])
    h1<- GET(URL1)
    x1<- htmlTreeParse(h1, useInternalNodes = T)
    rootnodes1<- xmlRoot(x1)
    blocklinks<- xpathSApply(rootnodes1,"//td/a/@href")
    
    nb<- length(blocklinks)

    ##Loop for extracting clusters within the blocks in a district
    
      for(j in 1:nb){

      URL2<- paste0(baseURL,blocklinks[j])
      h2<- GET(URL2)
      x2<- htmlTreeParse(h2, useInternalNodes = T)
      rootnodes2<- xmlRoot(x2)
      clusterlinks<- xpathSApply(rootnodes2,"//td/a/@href")
      nc<- length(clusterlinks)

      ##Final loop before school links to get inside blocks
      
        for(k in 1:nc){
       
        URL3<- paste0(baseURL,clusterlinks[k])
        h3<- GET(URL3)
        x3<- htmlTreeParse(h3, useInternalNodes = T)
        rootnodes3<- xmlRoot(x3)
        defaultlinks<- xpathSApply(rootnodes3,"//td/a/@href")
        nd<- length(defaultlinks)
        links<- c(links,URL3)
        setpb(pb,i)
        }
       
      }
      
    }
  close(pb)
All_district_links<<- links
b<- length(links)
schools<-c()
## Setting progress for schools.
print("Getting all the school links...")
ps<- startpb(0,b)

## Loop for getting links to schools within the block

for(q in 1:b){
  w<- read_html(links[q])
  e<- xml_find_all(w,"//td/a")
  r<- xml_attr(e,"href")
  rm(w,e)
  schools<-c(schools,r)
  setpb(ps,q)
}
All_school_links<<- schools
close(ps)
## rm(b,r,q,links)

##Creating the data frame to slot in the data
x<- as.matrix(data.frame("Schoolname"=as.character(),"PIN code" = as.character(),
                         "Instruction Medium"=as.character(), 
                         "Male Teachers"=as.character(), "Pre Primary Section Available"=as.character(),
                         "Board for Class 10"=as.character(), "School Type"=as.character(),
                         "Classes"=as.character(), "Female Teacher" =as.character(), 
                         "Pre Primary Teachers"=as.character(), "Board for Class 12"=as.character(),
                         "Meal"=as.character(),"Establishment"=as.character(),"School Area"=as.character(),
                         "School Shifted to New Place"=as.character(),
                         "Head Teachers"=as.character(),"Head Teacher"=as.character(),
                         "Is School Residential"=as.character(),"Residential Type"=as.character(),
                         "Total Teachers"=as.character(),"Contract Teachers"=as.character(),
                         "Management"=as.character(),"Village / Town"=as.character(),
                         "Cluster"=as.character(), "Block"=as.character(),"District"=as.character(),
                         "State"=as.character(),"UDISE Code"=as.character(),"Building"=as.character(),
                         "Class Rooms"=as.character(),
                         "Boys Toilet"=as.character(),"Girls Toilet"=as.character(),
                         "Computer Aided Learning"=as.character(),
                         "Electricity"=as.character(),
                         "Wall"=as.character(),"Library"=as.character(),"Playground"=as.character(),
                         "Books in Library"=as.character(),"Drinking Water"=as.character(),
                         "Ramps for Disable"=as.character(),"Computers"=as.character()  ))

ns<- length(schools)

## Setting progress bar for required info
print("Fetching required information...")
pf<- startpb(0,ns)
for(a in 1:ns){
  URL4<- paste0(baseURL,schools[a])
  s<- read_html(URL4)
  ## Extracting all other information for the relevant school
  d<- xml_find_all(s,"//li/b")
  reqinfo<- xml_text(d)
  
  ## Dealing with schools not having any data available
  if(length(d) == 0 ){
    na<- c(NA[1:40])
    na<- c(schools[a],na)
    x<-insertRow(x,a,na)
  }
  
  else{
    
    
    ## Extracting the school name
    f<- xml_find_all(s,"//h1/span")
    schoolname<- xml_text(f)
    
    ## Extracting Pin-code
    g<- xml_find_all(s,'//h4/center/text()')
    g1<- xml_text(g[3])
    g2<-unlist(strsplit(g1, " "))
    pincode<- g2[2]
    
    #Adjusting for meals as it is not within </b> 
    meal<- xml_find_all(s,"//li/text()")
    meal1<- xml_text(meal[18])
    reqinfo[10]<- meal1
    dat<- c(schoolname,pincode,reqinfo)
    
    x<- insertRow(x,a,dat)
    setpb(pf,a)
  }
  
}

close(pf)

ex<- readline("Would you like to save dataset as excel file?(Y/N) : ")
if(ex == "Y"){
  nam<- readline("What would you like to name the file? : ")
  nam1<- paste0(as.character(nam), ".xlsx")
  write.xlsx(as.data.frame(x), nam1)
  
  exno<- readline("Would you like to run another program? (Y/N):")
  if(exno== "Y"){
    reqstate()
  }
  if (exno== "N"){
    print("Thank you for using the program!")
  }
}
if(ex == "N"){
  reqdata<<- as.data.frame(x)
  exno<- readline("Would you like to run another program? (Y/N) :")
  if(exno== "Y"){
   reqstate()
  }
  
if(exno == "N"){
    print("Thank you for using the program!")
  }
}
}
##Initializes the function
reqstate()




