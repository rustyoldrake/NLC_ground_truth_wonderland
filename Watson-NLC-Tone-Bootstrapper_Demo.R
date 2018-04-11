######################################################
### Experimental Code.  Experimental R Interface for IBM Watson
### NLC to help with INITIAL TRIAGE of Emails that may come into a bank or insurance company
### Focus: Natural Language Classifier - R Programming Language Interface
### GROUND TRUTH IS VERY LIGHT - HANDLE WITH CARE - was authored in 20 minutes and contains NO real world data
### Could be used to BOOTSTRAP actual data - best practices are to use REAL WORLD Data for training
### HANDBOOK Natural Language Classifier (NLC) Handbook: https://ibm.box.com/s/rdlog2sue79178816s0rabkbi7ifu5vg 
### Video #1 - Training - https://www.youtube.com/watch?v=nrD37M39QnA 
### Video #2 - Testing - https://youtu.be/kBx6reEj4Gg
### Training Data - Ground truth: https://github.com/rustyoldrake/NLC_ground_truth_wonderland/blob/master/ground_truth_gt_NLC_example_email_sort.csv
### This R code lives here https://github.com/rustyoldrake/NLC_ground_truth_wonderland
#######################################################

library(RCurl) # install.packages("RCurl") # if the package is not already installed
library(httr)
library(data.table)
library(dplyr)
library(reshape2)
library(Rtts)
library(splitstackshape)
library(stringr)
library(splitstackshape)
library(tidyr)
library(XML)
library(png)

######### Housekeeping And Authentication 

setwd("/Users/ryan/Documents/Service - Natural Language Classifier (NLC)") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. Seperate R file looks sort of like below

## You will need to go to IBM Cloud / Bluemix and Create an NLC Service and GET SERVICE CREDENTIALS once it is active
username_password_NLC  # check you got this from NLC file
base_url_NLC = "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/"
getURL(base_url_NLC,userpwd = username_password_NLC )  # non essential , but checks if working /authenticated

###### FUNCTION CREATE NEW CLASSIFIER - post /v1/classifiers - Creates a classifier with CSV data ## URL below no "/" after base url
watson.nlc.createnewclassifier <- function(file,classifiername) {
  return(POST(url="https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers",
         authenticate(username_NLC,password_NLC),
         body = list(training_data = upload_file(file),
                     training_metadata = paste("{\"language\":\"en\",\"name\":",classifiername,"}",sep="") 
         )))}
###### end of function

###### FUNCTION - CHECK CLASSIFIER STATUS
watson.nlc.checkclassifierstatus <- function(classifier_id) {
  return(
    getURL(paste(base_url_NLC,classifier_id,sep=""),userpwd = username_password_NLC)
  )
}
### end of function


###### FUNCTION - DELETE CLASSIFIER - Receives name of Classifier to Kill; May not be able to do this until training complete
watson.nlc.deleteclassifier <- function(kill_classifier) {
  DELETE(url=(paste(base_url_NLC,kill_classifier,sep="")),authenticate(username_NLC,password_NLC))
}
 
### end of function

###### FUNCTION: ACCEPT QUERY & RETURN RESULT: CLASSIFIER and % FROM TEXT INPUT AND PROCESS TO LOOK GOOD
watson.nlc.processtextreturnclass <- function(classifier_id,query_text){
    query_text <- URLencode(query_text)
    data <- getURL(paste(base_url_NLC,classifier_id,"/classify","?text=", query_text,sep=""),userpwd = username_password_NLC)
    data <- as.data.frame(strsplit(as.character(data),"class_name"))
    data <- data[-c(1), ] # remove dud first row
    data <- gsub("[{}]","", data)
    data <- gsub("confidence","", data)
    data <- data.frame(matrix(data))
    setnames(data,("V1"))
    data$V1 <- gsub("\"","", data$V1)
    data$V1 <- gsub(":","", data$V1)
    data$V1 <- gsub("]","", data$V1)
    data <- cSplit(data, 'V1', sep=",", type.convert=FALSE)
    setnames(data,c("class","confidence"))
  return(data) }
### end of function
 
###### FUNCTION: LIST ALL CLASSIFIERS AND RETURN NEAT LIST
watson.nlc.listallclassifiers <- function(){ 
  data <- getURL(base_url_NLC,userpwd = username_password_NLC )
  data <- as.data.frame(strsplit(as.character(data),"classifier_id"))
  data <- data[-c(1), ] # remove dud first row
  data <- data.frame(matrix(data))
  colnames(data) <- "V1"
  data$V1 <- gsub("[{}]","", data$V1)
  data$V1 <- gsub("]","", data$V1)
  data$V1 <- gsub("\"","", data$V1)
  data$V1 <- gsub("name:","", data$V1)
  data$V1 <- gsub(":","", data$V1)
  data <- cSplit(data, 'V1', sep=",", type.convert=FALSE)
  data[,c(2,4)] <- NULL
  data <- as.data.table(data)
  setnames(data,c("classifier","name","date_created"))
  data <- data[order(date_created),] 
  return(data)
}

##### ACTION: EXECUTE FUNCTION  TO KILL (!!!) DELETE (!!!) CLASSIFIER - WARNING
watson.nlc.listallclassifiers()  # inventory - what do we want to delete - classifier id
kill <- "842c77x336-nlc-507"
watson.nlc.deleteclassifier(kill)  ## CAREFUL HERE - UNCOMMENT TO KILL CLASSIFIER
watson.nlc.listallclassifiers()  # check it's gone


######################################################### END OF FUNCTION DECLARATIONS


######################################################### OK LETS DO STUFF


###### ACTION: Create a new CLassifier!  (200 = Good outcome) - 
thename <- "\"NLC_ground_truth_open_emotion3\""   #
thefile <- "emotion_NLC_ground_truth_v3.csv" #  
watson.nlc.createnewclassifier(thefile,thename)  # calls function, passes file and name from above, starts the magic. might take 2 to 20+ minutes depending on complexityclassifier_id" : "563C46x19-nlc-377",

#   "classifier_id" : "ab2aa6x341-nlc-201",
###### ACTION: Retrieve list of classifiers (NEAT VERSION) - oldest to newest
watson.nlc.listallclassifiers()  # not happy response if no classifiers (Blank) if blank, use below

## ARE WE READY?  (might take 10-15m or more if really complex - be patient for the magic!)
classifierA <- "ab2aa6x341-nlc-201" #     ####  COPY PASTE * YOUR* CLASSIFIER HERE TO CHECK STATUS  ####
watson.nlc.checkclassifierstatus(classifierA)

### READY OR NOT?
# if new will say "not yet ready to accept classify requests" - once done in a few mintues will say
# "The classifier instance is now available and is ready to take classifier requests" - then you can submit query below


# LIGHT MANUAL TESTING
query <- "i have problems and am feeling blue"
watson.nlc.processtextreturnclass(classifierA,query)

query <- "you are the best bank on the planet!"
watson.nlc.processtextreturnclass(classifierA,query)

query <- "you rock and I think your team is great"
watson.nlc.processtextreturnclass(classifierA,query)

query <- "Dag nabbit! I was robbed and they stole my wallet"
watson.nlc.processtextreturnclass(classifierA,query)

query <- "i am bursting at the seams because I'm so delighted with life!"
watson.nlc.processtextreturnclass(classifierA,query)

query <- "i need the forms to do my taxes - when do they come?"
watson.nlc.processtextreturnclass(classifierA,query)

query <- "are you dense? You totally are missing my point"
watson.nlc.processtextreturnclass(classifierA,query)

query <- "that's creepy"
watson.nlc.processtextreturnclass(classifierA,query)

