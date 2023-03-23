#Initializing a docDB to store the files with hashtags as a folder names
root <- "docDB"

# To get the dataset of all files to create a DB.. 
#ASSUMPTION 1) All files will be under /source folder after getting current working directory getwd()..
#           2) Filenames will be of this pattern filename #tag1 #tag2 #tag3.fileextension eg. image #NEU #Boston.jpg as this code was written on Window machine.
#           3) Filenames are assumed to have only one space between file name and the tags.
folder <- paste(getwd(),"/source",sep="")
images <-c(list.files(path = folder))


## ---------------------------
## Script name:  ObjDB-Narayanan.R
## Purpose of script: To create a hierarchical database
## Author: Vasumathi Narayanan
## Date Submitted: 2023-01-22
## ---------------------------
## Notes: To create a file system based database in form of hierarchy
## ---------------------------



# Main functions to test the utility functions
main <- function()
{
  ########################### NEED TO RUN THIS FOR CREATING HIERARCHIAL DATABASE #################################
  #Config DB to create a docDB inside the project folder
  configDB(root,"")
  #Create folders for tags and copy them from source folder with VERBOSE as TRUE to print COPY message
  storeObjs(folder,root, TRUE)
  #Deleting the created folders for tags without deleting the docDB (root) folder itself
  clearDB(root)
  #################################           END        #################################
  
  
  

  #################################   TEST CASES FOR ALL FUNCTIONS ###############################
  #Test Cases for getFileName : ' ', . , _ , - , %, # in file name
  print(getFileName("image 0 #Khoury #Northeastern #Boston.jpg")) #Expected Value : "image0.jpg"
  print(getFileName("image.0 #Khoury #Northeastern #Boston.jpg")) #Expected Value : "image.0.jpg"
  print(getFileName("image_0 #Khoury #Northeastern #Boston.jpg")) #Expected Value : "image_0.jpg"
  print(getFileName("image-0 #Khoury #Northeastern #Boston.jpg")) #Expected Value : "image-0.jpg"
  print(getFileName("image#%0 #Khoury #Northeastern #Boston.jpg"))#Expected Value : "image#%0.jpg"

  #Test Cases for getTags #, space, -, _, . in tag names
  # ASSUMPTIONS: 1) Can't have two #s in front of the tags (eg ##Boston is not allowed)
  #              2) Can't have any other special characters instead of # in start (eg %Boston is not allowed)
  print(getTags("image0 #Khou#ry #Northeastern #Boston.jpg"))  #Expected Value : "Khou#ry" "Northeastern" "Boston"
  print(getTags("image0 #Khoury #North eastern #Boston.jpg"))  #Expected Value : "Khoury" "North eastern" "Boston"
  print(getTags("image0 #Khoury #Northe_astern #Boston.jpg"))  #Expected Value : "Khoury" "Northe_astern" "Boston"  
  print(getTags("image0 #Khoury #North-eastern #Boston.jpg"))  #Expected Value : "Khoury" "North-eastern" "Boston"
  print(getTags("image0 #Khoury #Northeastern #Bos.ton.jpg"))  #Expected Value : "Khoury" "Northeastern" "Bos.ton"
  # If there are no tags after file name it will return empty list to storeObjs and it will continue the loop for next iteration
  #This accounts for missing files and directories in storeobjs where it will print "There are no datasets to create the hierarchial dataset"
  print(getTags("image0.png"))

  #Test Case for storeObjs with verbose as FALSE
  storeObjs(folder,root, FALSE)
  #Test Case for storeObjs with verbose as TRUE
  storeObjs(folder,root, TRUE)
  
  #Test Case for configDB to create folder within docDB
  configDB(root,"docDB/Boston")
  #Test Case to check if the folder already exists or not- It will print "The folder already exists"
  configDB(root,"docDB/Boston")

    
  # Test Case for genObjPath to strip the # in front of the tag name
  genObjPath(root,"#NEU")
  
  
  #Test Case to check Various formats jpg, txt, mp3 inside source image folder
  print(getFileName("image-0 #Khoury #Northeastern #Boston.tiff")) #Expected Value : "image-0.tiff"
  print(getFileName("image-0 #Khoury #Northeastern #Boston.txt")) #Expected Value : "image-0.txt"
  
  # Test Case for deleting the folders inside docDB without deleting the main folder itself
  clearDB(root)
  
  ################################# END OF TEST CASES ##################################################
  
}

# Function to get the vector of hashtags
getTags <- function(fileName){
  li <- unlist(strsplit(unlist(fileName), " #"))
  for(i in 1:length(li))
    li[i] <- trimws( li [i]) 
  a<- c(li[length(li)])
  li[length(li)] <- regmatches(a, regexpr(".*(?=[.])", a, perl=TRUE))
  indices <- c(1)
  li <- li[-indices]
  return(li)
}

# Function to get the file name
getFileName <- function(fileName){
  li<- unlist(strsplit(unlist(fileName), " #"))
  filename_extension <- unlist(strsplit(li[length(li)],"\\."))[2]
  return(paste(li[1],".",filename_extension,sep=""))
}

# Function to generate the path for tag folders eg: doc/Northeastern
genObjPath <- function(root,tag){
  if(substring(tag,1,1) == "#")
    return(paste(root,"/",substring(tag,2,),sep=""))
  return(paste(root,"/",substring(tag,1,),sep=""))
}

# Function to create a directory for storing all the tag folders in the root directory
configDB <- function(root, path){
  if(file.exists(path)){
    print("The folder already exists")
  }else if(path == ""){
    dir.create(root, showWarnings = FALSE)
  }else{
    dir.create(path, showWarnings = FALSE)
    
  }
}

# Function to store the images inside tag folder by copying and renaming the images accordingly.
storeObjs<- function(folder, root, verbose){
  if(length(images) == 0){
    print("There are no datasets to create the hierarchial dataset") 
    return(-1);
  }
  for(i in 1:length(images)){
    tags <- getTags(images[i])
    # This accounts for missing files and directories
    if(length(tags) == 0){
      print(paste(images[i], " does not have any tags",sep=""))
      next;
    }
    original_image_name <- images[i]
    filename <- getFileName(images[i])
    obj_path <- 0
    for(i in 1:length(tags)){
      obj_path[i]<- genObjPath(root, tags[i])
      if(file.exists(paste(obj_path[i],"/",sep=""))){
        file.copy(paste(folder,"/",original_image_name,sep=""), paste(obj_path[i],"/",filename,sep=""))
      }else{
        configDB("",obj_path[i])
        file.copy(paste(folder,"/",original_image_name,sep=""), paste(obj_path[i],"/",filename,sep=""))
      }
    }
    if(verbose){
      hashtags=paste(tags, collapse = ' ')
      print(paste("Copying File ",filename," to ", hashtags))
    }
  }  
}

# Function to clear the docDB at the end and reinstatiate DB to blank state
clearDB <- function(root){
  dir_list <- list.dirs(root,recursive = TRUE)[-1]
  for( directory in dir_list){
   if (file.exists(directory)) {
      unlink(directory,recursive = TRUE)
    }
  }
}





