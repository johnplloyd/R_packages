#' Rounds P-values cleanly for display in figures.
#'
#' This function takes a P-values and generates a clean, rounded version for display in plots/figures. The expression argument is included, and if set to TRUE will allow exponents to be properly displayed as a superscript in the initial plots. However, 'expression' text behaves poorly if the figure is saved in PDF format and opened in vector graphics software.
#' @param x P-values (numeric)
#' @param expression Boolean indicating whether the rounded P-value should be returned as an expression.
#' @return Rounded P-value in string format.
#' @export
#' @examples
#' p_round.display(x = 1.234e-56, expression = F)

p_round.display <- function(x, expression = FALSE){
  #x <- 9.214851e-15
  
  #0.001
  if( x == 0 ){
    p_display <- "p=0"
    p_display.expr <- str2expression("p=0")
  }else if(x < 0.001){
    #x <- 0
    #x <- 9e-21
    
    x.string <- toString(x)
    x.strsplit <- strsplit(x = x.string, split = "")[[1]]
    x.strsplit
    
    if("e" %in% x.strsplit){
      
      first_digit <- as.numeric(x.strsplit[1])+1
      e_location <- which( x.strsplit == "e" )
      exponent.split <- x.strsplit[ (e_location+1):length(x.strsplit) ]
      exponent <- as.numeric(paste(exponent.split, collapse = ""))
      
      first_digit
      e_location
      exponent.split
      exponent
      
      if(first_digit == 10){
        first_digit <- 1
        exponent <- exponent+1
      }
      
      #p_display1 <- parse(text = paste( "p<",first_digit, "x10^", exponent, sep = "" ))
      #exponent <- parse( text = paste("", "^", toString(exponent), sep = "") )
      
      p_display <- paste( "p<",first_digit, "x10", exponent, sep = "" )
      p_display.expr <- str2expression(paste( "p<",first_digit, "*x10^", exponent, sep = "" ))
      #p_display <- paste( "p<",first_digit, "x10", parse(text = exponent), sep = "" )
    }else{
      first_digit <- as.numeric(x.strsplit[6])+1
      if(first_digit < 10){
        p_display <- paste( "p<", first_digit, "x10-4", sep = "" )
        p_display.expr <- str2expression(paste( "p<", first_digit, "*x10^-4", sep = "" ))
      }else{
        p_display <- "p<0.001"
        p_display.expr <- str2expression("p<0.001")
      }
    }
    
  }else if(x < 0.01){
    # 0.009999 to 0.001
    #x <- 0.0093456
    x
    
    x.string <- toString(x)
    x.strsplit <- strsplit(x = x.string, split = "")[[1]]
    display_num <- as.numeric(x.strsplit[5])
    display_num
    
    if(display_num < 9){
      p_display <- paste("p<0.00", display_num+1, sep = "")
      p_display.expr <- str2expression(paste("p<0.00", display_num+1, sep = ""))
    }else{
      p_display <- "p<0.01"
      p_display.expr <- str2expression("p<0.01")
    }
    
  }else if(x < 0.05){
    #x <- 0.023456
    
    x.string <- toString(x)
    x.strsplit <- strsplit(x = x.string, split = "")[[1]]
    display_num <- as.numeric(x.strsplit[4])
    display_num
    p_display <- paste("p<0.0", display_num+1, sep = "")
    p_display.expr <- str2expression(text = paste("p<0.0", display_num+1, sep = ""))
  }else{
    #x <- 0.2564687695
    x.round <- round(x, 2)
    p_display <- paste("p=", x.round, sep = "" )
    p_display.expr <- str2expression(paste("p==", x.round, sep = "" ))
  }
  
  if(expression){
    return(p_display.expr)
  }else{
    return(p_display)
  }
}