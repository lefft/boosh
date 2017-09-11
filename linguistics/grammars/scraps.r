

for (y in c("next", "break", "switch")){
  
  print(paste0("starting y=='", y, "' part rn"))
  
  if (y=="next"){
    for (x in 1:3){
      print(paste0("befor ", x))
      if (x < 3){next("boosh")} 
      print(paste0("after ", x))
    }
  }
  
  if (y=="break"){
    for (x in 1:3){
      print(paste0("befor ", x))
      if (x < 3){break("boosh")} 
      print(paste0("after ", x))
    }
  }
  
  if (y=="switch"){
    for (x in 1:3){
      print(paste0("befor ", x))
      if (x < 3){print(switch(EXPR=as.character(x), `1`="ONE", `2`="TWO", `3`="THREE"))} 
      print(paste0("after ", x))
    }
  }
  
  print(paste0("end of y=='", y, "' part"))
}

ccc <- c("b","QQ","a","A","bb")
# note: cat() produces no output for NULL
for(ch in ccc) cat(ch,":", switch(EXPR = ch, a = 1, b = 2:3), "\n")
for(ch in ccc) cat(ch,":", switch(EXPR = ch, a =, A = 1, b = 2:3, "Otherwise: last"),"\n")






i39 <- sapply(3:9, seq) 
vapply(i39, fivenum, 
       FUN.VALUE=c("Min."=0, "1st Qu."=0, "Median"=0, "3rd Qu."=0, "Max."=0))
