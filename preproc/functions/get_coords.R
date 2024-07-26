
get_coords<- function(string, revert= T){
  
  x_offset<- 125
  y_offset<- 112    
  ppl<- 78
  linespan<- 233
  
  lines<- unlist(strsplit(string, '@'))
  
  coords<- NULL
  
  for(i in 1:length(lines)){
    
    if(i==1){ # if first line
      
      for(j in 1:nchar(lines[i])){
        
        if(j==1){
          x1<- x_offset
          x2<- x_offset + ppl
          y1<- y_offset
          y2<- y_offset+ linespan
          char<- substr(lines[i], j, j)
          
        }else{
          x1<- coords$x2[j-1] +1
          x2<- x1+ ppl
          y1<- y_offset
          y2<- y_offset+ linespan
          char<- substr(lines[i], j, j)
        }
        t<- data.frame('x1'=x1, 'x2'=x2, 'y1'=y1, 'y2'=y2, 'char'= char)
        coords<- rbind(coords, t)
        
      }
      
    }else{
      
      for(j in 1:nchar(lines[i])){
        
        if(j==1){
          x1<- x_offset
          x2<- x1+ ppl
          y1<- coords[nrow(coords), 'y2' ]+1
          y2<- y1+ linespan
          char<- substr(lines[i], j, j)
        }else{
          x1<- coords[nrow(coords), 'x2' ]+1
          x2<- x1+ ppl
          y1<- coords[nrow(coords), 'y1' ]
          y2<- coords[nrow(coords), 'y2' ]
          char<- substr(lines[i], j, j)
        }
        
        t<- data.frame('x1'=x1, 'x2'=x2, 'y1'=y1, 'y2'=y2, 'char'= char)
        coords<- rbind(coords, t)
        
      }
      
    }
    
  }
  
  
  
  
  ## map words to dataframe:
  
  coords$wordID<- NA
  coords$char_num<- NA
  
  line_breaks<- which(diff(coords$x1)<0)
  new_coords<-NULL
  
  for(i in 1:(length(line_breaks)+1)){
    
    if(i==1){
      t<- coords[1:line_breaks[i],]
    }else{
      
      if(i== length(line_breaks)+1){
        t<- coords[(line_breaks[i-1]+1): nrow(coords),]
      }else{
        t<- coords[(line_breaks[i-1]+1):line_breaks[i],]
      }
      
    }
    
    empty_spaces<- c(which(t$char== ' '), nrow(t))
    
    for(j in 1:length(empty_spaces)){
      if(j==1){
        t[1:(empty_spaces[j]-1), 'wordID']<- paste(t[1:(empty_spaces[j]-1), 'char'], collapse = '')
        t[1:(empty_spaces[j]-1), 'char_num']<- 1:(empty_spaces[j]-1)
      }else{
        
        t[empty_spaces[j-1] : empty_spaces[j], 'wordID']<- paste(t[empty_spaces[j-1] : empty_spaces[j], 'char'], collapse='')
        t[empty_spaces[j-1] : empty_spaces[j], 'char_num']<- 0:(empty_spaces[j]-empty_spaces[j-1])
        
        
      }
    }
    
    new_coords<- rbind(new_coords, t)
    
  }
  
  if(revert){
    
    ## revert back to original frame size:
    new_coords$x1<- new_coords$x1/2.4
    new_coords$x2<- new_coords$x2/2.4
    new_coords$y1<- new_coords$y1/2.4
    new_coords$y2<- new_coords$y2/2.4
    
  }
  
  return(new_coords)
  
}
