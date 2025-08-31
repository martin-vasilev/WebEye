# All Functions

trial_info<- function(file, maxtrial, data){ # extracts information for processing trials
  ### get trial names:
  ID<- which(grepl('TRIALID', file));
  trial_text<- file[ID]
  trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
  #trials<- trials[which(letter!="P" &  letter!="F")] # remove practice items and questions
  trials<- gsub(" ", "", trials)
  # sometimes there is an extra empty space that can mess up detection of duplicates
  
  ### get condition:
  I<- unlist(gregexpr(pattern ='I',trials)) # start of item info
  cond<- as.numeric(substr(trials, 2, I-1)) # extract condition number
  
  ### get item:
  D<- unlist(gregexpr(pattern ='D',trials)) # start of dependent info
  item<- as.numeric(substr(trials, I+1, D-1)) # extract condition number
  depend<- as.numeric(substr(trials, nchar(trials), nchar(trials)))
  
  ### get sequence:
  #seq<- 1:length(trials)
  
  ### get start & end times
  start<- which(grepl('DISPLAY ON', file))
  end <- which(grepl('DISPLAY OFF', file))
  
  # duplicated<- trials[duplicated(trials)]
  # 
  # if(length(duplicated)>0){ # if there were aborted trials..
  #   # message(paste(" Diplicated trial", duplicated, "for file:", data, "\n"))
  #   # message("Analysing only last attempt at the trial!")
  #   
  #   toBeRemoved<- NULL
  #   uniqueDupl<- unique(duplicated)
  #   dup_rem<- NULL
  #   
  #   for(i in 1:length(uniqueDupl)){
  #     dup_rem_T<- which(trials==uniqueDupl[i])
  #     dup_rem<- c(dup_rem, dup_rem_T[1:length(dup_rem_T)-1])
  #     
  #     
  #   } # end of i
  #   
  #   start<- start[-dup_rem]
  #   #end<- end[-dup_rem]
  #   cond<- cond[-dup_rem]
  #   item<- item[-dup_rem]
  #   # seq<- seq[-toBeRemoved]
  #   depend<- depend[-dup_rem]
  #   ID<- ID[-dup_rem]
  # } # end of aborted conditional
  
  trial_db<- data.frame(cond, item, depend, start, end, ID)
  trial_db<- subset(trial_db, depend==0 & item< maxtrial+1)
  trial_db$seq<- 1:nrow(trial_db)
  
  ###
  
  
  # trials<- trials[which(!is.element(trials, duplicated))]
  
  return(trial_db)
}


get_num <- function(string) {
  as.numeric(gsub("[^0-9]", "", string))
}

get_x<- function(string, where=2){as.numeric(unlist(strsplit(string, "\t"))[1:2])[where]}

# get type of sound that was played (STD, DEV)
get_type<- function(string){unlist(strsplit(string, ' '))[4]} 


get_files<- function(dir= "C:/Users/Martin Vasilev/Documents/Test", file_ext= ".asc"){
  
  if(dir== ""){
    dir= getwd()
  }
  # get a list of all file in dir:
  all_files<- list.files(dir)
  # remove non-asc files (if present)
  all_files<- all_files[grepl(file_ext, all_files)]
  # remove txt files (of present):
  all_files<- all_files[!grepl(".txt", all_files)]
  
  # sort files by number in string
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  num<- get_num(all_files)
  
  if(!is.na(num[1])){
    all_files<- all_files[order(num, all_files)]
  }
  # convert to directory string for each data file:
  if(length(all_files)>0){
    all_dirs<- NULL
    for(i in 1:length(all_files)){
      all_dirs[i]<- paste(dir, "/", all_files[i], sep = "")
    }
    
    message(paste("Found", toString(length(all_files)), file_ext, "files in the specified directory!", "\n"))
    return(all_dirs)
  }else{
    stop(paste("Found no", file_ext, "files in the specified directory!"))
  }
} # end of get_files()


get_x_pixel<- function(string){
  a<- data.frame(do.call( rbind, strsplit( string, '\t' ) ) )
  x<- as.numeric(as.character(unlist(a$X4)))
  
  return(x)
}


get_y_pixel<- function(string){
  a<- data.frame(do.call( rbind, strsplit( string, '\t' ) ) )
  y<- as.numeric(as.character(unlist(a$X5)))
  
  return(y)
}

get_FIX_stamp<- function(string){
  
  num_stamp<- NULL
  for(i in 1:length(string)){
    char_stamp<-  unlist(strsplit(string[i], split = '\t'))[2]
    num_stamp[i]<- as.numeric(char_stamp)
  }
  # char_stamp<-substr(string, 1, unlist(gregexpr(pattern ='\t', string))[1]-1) 
  # num_stamp <- as.numeric(char_stamp)
  return(num_stamp)
}

get_seq<- function(start, end, file, prev= F){
  
  if(!prev){
    # find start:
    lines<- grep(start, file)
    hits<- file[lines]
    loc<- grep("SFIX", hits)
    s1<- lines[loc] +1
    
    # find end:
    lines2<- grep(end, file)
    hits2<- file[lines2]
    loc2<- grep("EFIX", hits2)
    #hits2<- hits2[-loc2]
    s2<- lines2
    s2<- s2[-loc2]
    
    # extract fixation txt data:
    text<- file[s1:s2]
    out <-  as.data.frame(do.call(rbind, strsplit( text, '\t' )))
    out$V2<- as.numeric(as.character(out$V2))
    out$V3<- as.numeric(as.character(out$V3))
    out$V4<- as.numeric(as.character(out$V4))
    
  }else{
    # find start:
    s1<- grep(start, file)
    if(length(s1)>1){
      s1<- s1[1]
    }
    
    
    # find end:
    s2<- grep(end, file)
    if(length(s2)>1){
      s2<- s2[1]
    }
    
    text<- file[s1:s2]
    
    whichSSACC<- which(grepl('SSACC', text))
    whichESACC<- which(grepl('ESACC', text))
    whichSFIX<- which(grepl('SFIX', text))
    whichEFIX<- which(grepl('EFIX', text))
    whichMSG<- which(grepl('MSG', text))
    whichSBLINK<- which(grepl('SBLINK', text))
    whichEBLINK<- which(grepl('EBLINK', text))
    
    allOut<- c(whichSSACC, whichESACC, whichSFIX, whichEFIX, whichMSG,
               whichSBLINK, whichEBLINK)
    
    if(length(allOut)>0){
      text<- text[-allOut]
    }
    
    out <-  as.data.frame(do.call(rbind, strsplit( text, '\t' )))
    out$V2<- as.numeric(as.character(out$V2))
    out$V3<- as.numeric(as.character(out$V3))
    out$V4<- as.numeric(as.character(out$V4))
    
  }
  
  return(out)
  
}

