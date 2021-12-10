require( "survey" )
library(survey)
options( survey.lonely.psu = "adjust" )

f <- svydesign( ~1 , strata = ~cyl , weights = ~wt , data= mtcars )

sby <- function(design , dep , ind , lvl = NULL){
  
  # if lvl is null, takes the mean, if not percent of that level
  # dep - what is the variable you want a mean of
  # ind - what is the grouping variable
  des <- get(design)
  
  ###### MEANS
  if( is.null( lvl ) ){
    
    #dep <- "mpg"
    svy1 <- eval(bquote(
      svyby(
        ~.(as.name( dep )), 
        ~ .(as.name( ind )), 	
        des, svymean, na.rm = T)))
    
    svy2 <- eval(bquote(svyby(~.(as.name(dep)), ~ .(as.name(ind)), des, svytotal, na.rm = T)))
    
    ###### Percent of a Level
  }else{
    #dep <- "am"	; lvl <-1	
    svy1 <- eval(bquote(
      svyby(
        ~(.(as.name( dep )) == .(lvl)),
        ~ .(as.name( ind )) ,
        des, svymean, na.rm = T)))
    
    svy1 <- svy1[ , c(1,grep("TRUE",colnames(svy1)))]
    
    svy2 <- eval(bquote(
      svyby(
        ~(.(as.name( dep )) == .(lvl)),
        ~ .(as.name( ind )) ,
        des, svytotal, na.rm = T)))
    
    svy2 <- svy2[ , c(1,grep("TRUE",colnames(svy2)))]
    
  }
  
  
  svy3 <- eval(bquote(
    svyby(
      ~.(as.name(dep)), 
      ~ .(as.name(ind)), 
      des, unwtd.count, na.rm = T)))
  
  svy2 <- svy2[ , 1:2 ]
  ###### Clean and Edit
  colnames( svy1 ) <- c( "Level","Estimate","SE")
  colnames( svy2 ) <- c( "Level","Weighted")
  colnames( svy3 ) <- c( "Level","Observations","SE")
  svy3$SE <- NULL
  
  svy <- Reduce(merge, list(svy1, svy2, svy3))
  svy$Variable <- ind
  
  svy[ , sapply( svy, is.numeric)] <- sapply( svy[ , sapply( svy, is.numeric)], round , 4 )
  
  svy <- svy[ , c(ncol( svy ) , (1:(ncol(svy)-1)) )]
  
  return( svy )
  
}



sby( "f" , "am" , "gear" , 1 )
sby( "f" , "mpg" , "gear"  )

do.call( rbind, 
         lapply( list( "gear" , "cyl" , "carb" ), 
                 function(X){ sby( "f" , "am" , X , 1 )}   ))
