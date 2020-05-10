
corner <- function(m, n = 10){
  print(m[1:n, 1:n])
}

write_df <- function (df, rowNm_header, outfile) {
  
  df.rowNm <- data.frame(val = row.names(df))
  names(df.rowNm) <- rowNm_header
  
  df.out <- cbind ( df.rowNm, df )
  
  write.table( df.out, file = outfile, quote = F, sep = "\t", row.names = F )
}

remove_outliers <- function (x){
  quart_low <- quantile (x, na.rm = T)[[2]]
  quart_high <- quantile (x, na.rm = T)[[4]]
  outlier_dist <- IQR (x, na.rm = T) * 1.5
  
  low_thresh <- quart_low - outlier_dist
  high_thresh <- quart_high + outlier_dist
  
  x <- x [ which (x > low_thresh & x < high_thresh) ]
  
  return (x)
}

ordered_dot_plot <- function (x, ylim='', xlab=NA, ylab=NA, main=NA, spread=0.3, las = 1, iqr_line=TRUE, median_line=TRUE, outlier=TRUE, ordered = TRUE, cex = 1, dot_col = "black", median_col = "red"){
  
  # read matrix as data frame
  if( !( is.null(dim(x)) ) ){
    x <- data.frame(x)
  }
  
  # if lacking, add names to x
  if( is.null( names(x) )){
    names(x) <- 1:length (x)
  }
  
  # calculate and store median and quantile values
  # calculated prior to removal of outliers, if requested
  if(median_line == TRUE){
    medn_l <- lapply(x, function(x) median(x, na.rm = T) )
  }
  
  if(iqr_line == TRUE){
    quant_l <- lapply(x, function(x) quantile(x, na.rm = T) )
  }
  
  # remove outliers
  # resulting vectors may not be of equal length: return as list of vectors
  if(outlier == FALSE){
    x.rm <- list()
    i = 1
    for (nm in names (x)){
      x.sub <- na.omit( x [[nm]] )
      x.sub.rm <- remove_outliers (x.sub)
      x.rm[[i]] <- x.sub.rm
      i <- i + 1
    }
    names(x.rm) <- names(x)
    x <- x.rm
  }
  
  # calculate y limits as min and max of all data
  if( ylim == '' ){
    y_min <- min (unlist (x), na.rm = T)
    y_max <- max (unlist (x), na.rm = T)
    ylim <- c (y_min, y_max)
  }
  
  # generate empty plot of appropriate dimensions
  # chart is effectively a scatterplot with custom x-axis
  plot (1, type = "n", xlim = c (1-spread, length(x)+spread), ylim = ylim, xaxt = 'n', xlab=xlab, ylab=ylab, main=main)
  
  # loop through x and plot data
  # y values: data distribution, sorted low to high
  # x values: even spread across subset of x axis
  for (i in 1:length (names (x))){
    print(i)
    nm <- names (x)[i]
    
    y_vals <- sort( na.omit(x[[nm]]) )
    
    if(length(y_vals) == 0){
      pass <- "PASS"
    }else{
      spread_min <- i-spread
      spread_max <- i+spread
      x_vals <- seq (from=spread_min, to = spread_max, length=length(y_vals))
      
      if(ordered == FALSE){
        if(length(x_vals) > 1){
          x_vals <- sample( x_vals )
        }
      }
      
      if (iqr_line == TRUE){
        iqr_low <- quant_l[[i]][[2]]
        iqr_high <- quant_l[[i]][[4]]
        
        segments (x0=spread_min, y0=iqr_low, x1=spread_max, y1=iqr_low, col = "gray40", lty="dotted")
        segments (x0=spread_min, y0=iqr_high, x1=spread_max, y1=iqr_high, col = "gray40", lty="dotted")
      }
      
      #points (x=x_vals, y=y_vals, pch=16, cex=0.6)
      points (x=x_vals, y=y_vals, pch=16, cex = cex, col = dot_col)
      
      if (median_line == TRUE){
        medn <- medn_l [[i]]
        segments (x0=spread_min, y0=medn, x1=spread_max, y1=medn, col = median_col, lwd=2)
      }
    }
  }
  
  # add custom x axis
  axis( 1, at = 1:length(x), labels = names(x), las = las )
}

after_first_char <- function(x, char){
  loc_first <- sapply(X = x, FUN = function(x) which(strsplit(x, "")[[1]]==char)[1] )
  after_first <- substr(x = x, start = loc_first+1, stop = 1000000000)
  return(after_first)
}

remove_nonvarying_columns <- function (df.raw, max_repeat){
  max_repeats <- apply(df.raw, 2, function(x) max(table(x)))
  max_repeats.ind <- which(max_repeats <= max_repeat)
  df <- df.raw[max_repeats.ind]
  return(df)
}

shuffle_df <- function(X, margin = 2){
  X.shuffle <- data.frame(apply( X = X, MARGIN = margin, FUN = function(x) sample(x) ))
  names(X.shuffle) <- names(X)
  row.names(X.shuffle) <- row.names(X)
  return(X.shuffle)
}

make_CV_folds <- function(names_vec, n_folds = 10){
  # https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
  folds <- cut( seq(1, length(names_vec)), breaks = n_folds, labels = FALSE)
  folds.shuffle <- sample(folds)
  return(folds.shuffle)
}

calc_MSE <- function(x1, x2){
  diff <- x1 - x2
  sqr <- diff*diff
  sm <- sum(sqr)
  MSE <- sm/length(sqr)
  return ( MSE )
}

calc_median_error <- function (x1, x2){
  diff <- x1 - x2
  abs_diff <- abs(diff)
  median_error <- median(abs_diff)
  return (median_error)
}

outersect <- function(x, y) {
  osect <- sort(c(setdiff(x, y),
         setdiff(y, x)))
  return(osect)
}

normalize_df.rank <- function(X, margin = 2, round_digits = NA){
  
  X_norm <- apply( X = X, MARGIN = margin, function(x) rank(x) / length(x) )
  X_norm <- apply( X = X_norm, MARGIN = margin, function(x) (x - min(x) ) / ( max(x) - min(x) ) )
  
  X_norm <- data.frame(X_norm)
  names(X_norm) <- names(X)
  row.names(X_norm) <- row.names(X)
  
  if( !( is.na(round_digits) )){
    X_norm <- round( X_norm, round_digits )
  }
  
  return(X_norm)
}

normalize_df.zscore <- function(X, margin = 2, round_digits = NA){
  
  X_norm <- apply( X = X, MARGIN = margin, function(x) (x - mean(x)) / sd(x) )
  
  X_norm <- data.frame(X_norm)
  names(X_norm) <- names(X)
  row.names(X_norm) <- row.names(X)
  
  if( !( is.na(round_digits) )){
    X_norm <- round( X_norm, round_digits )
  }
  
  return(X_norm)
}

normalize_df.percentile <- function(X, n_percentile = 7, margin = 2){
  
  interval <- 1/n_percentile
  percentiles <- seq(from = 0, to = 1, by = interval)
  
  X_norm <- apply( X = X, MARGIN = margin, 
                  function(x) cut(x = x, 
                                  breaks = quantile( unique(x), percentiles),
                                  include.lowest = T,
                                  labels = 1:(length(percentiles)-1) ) )
  X_norm <- apply( X = X_norm, MARGIN = margin, function(x) as.numeric(x) )
  
  
  X_norm <- data.frame(X_norm)
  names(X_norm) <- names(X)
  row.names(X_norm) <- row.names(X)
  
  return(X_norm)
}

generate_RF_model <- function (ML_df, ntree = 500){
  library(randomForest)
  
  x <- ML_df[-1]
  #y <- as.factor(sort(ML_df[,1], decreasing = TRUE))
  y <- as.factor(ML_df[,1])
  
  data <- list(cbind(x, y))
  
  rf_model <- randomForest(y ~ ., data=data[[1]], ntree = ntree)
  
  return(rf_model)
}

na.omit.vector <- function(v){
  v.omit <- v[ !(is.na(v)) ]
  return(v.omit)
}

generate_decision_tree.RF_module <- function (ML_df){
  library(randomForest)
  
  x <- ML_df[-1]
  #y <- as.factor(sort(ML_df[,1], decreasing = TRUE))
  y <- as.factor(ML_df[,1])
  
  data <- list(cbind(x, y))
  
  rf_model <- randomForest(y ~ ., data=data[[1]], ntree = 1, mtry = ncol(x))
  
  return(rf_model)
}

calc_AUCROC <- function(scores, labels){
  library(ROCR)
  roc_pred <- prediction(scores, labels) # arg1 = prediction, arg2 = labels
  auc_obj <- performance(roc_pred, "auc")
  auc_roc <- auc_obj@y.values[[1]]
  return(auc_roc)
}

calc_MCC <- function(tp, fp, fn, tn){
  numer <- (tp*tn) - (fp*fn)
  denom_unSq <- (tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)
  denom <- sqrt(denom_unSq)
  MCC <- numer/denom
  return(MCC)
}

calc_kappa <- function(tp, fp, fn, tn){
  # from: https://en.wikipedia.org/wiki/Cohen%27s_kappa#Example
  count_sum <- tp+fp+fn+tn
  
  po <- (tp+tn) / count_sum
  
  py <- ((tp+fn)/count_sum) * ((tp+fp)/count_sum)
  pn <- ((fp+tn)/count_sum) * ((fn+tn)/count_sum)
  pe <- py + pn
  
  kappa <- (po-pe) / (1-pe)
  
  return(kappa)
}

calc_FWER <- function(n_tests,alpha){
  fwer <- 1-(1-alpha)^n_tests
  return(fwer)
}

calc_adj_alpha <- function(n_tests,alpha){
  adj_alpha <- 1 - (1 - alpha)^(1/n_tests)
  return(adj_alpha)
}

calc_RSS <- function(vec1,vec2){
  rsdl <- vec1-vec2
  rsdl_sqr <- rsdl^2
  rss <- sum(rsdl_sqr)
  #print(rss)
  return(rss)
}

clean_strings <- function(strings_vector, rm_str, rp_str){
  #l <- gsub(pattern = "[.]", replacement = "_", x = labels)
  l <- strings_vector
  for(i in 1:length(rm_str) ){
    rm <- rm_str[i]
    rp = rp_str[i]
    l <- gsub(pattern = rm, replacement = rp, x = l)
  }
  return(l)
}

substring_before_character <- function(strings_vec, char = "_"){
  pre <- c()
  i <- 1
  for(lb in strings_vec){
    if(i %% 10000 == 0){
      print(c( i, length(strings_vec) ))
    }
    
    loc <- which(strsplit(lb, "")[[1]]==char)
    if( length(loc) != 0 ){
      #print( c(lb, loc) )
      lb_p <- substr(x = lb, start = 1, stop = loc-1)
    }else{
      lb_p <- lb
    }
    pre <- c(pre, lb_p)
    i <- i + 1
  }
  return(pre)
}

fit_glmnet_model <- function(Y, X, alpha, search_lambda = TRUE, lambda = NA, n_search_folds = 10){
  
  # Y = 1-column dataframe
  # X = dataframe/matrix
  
  #alpha:
  #0 = Ridge regression; 1 = LASSO; 0 < a < 1 = Elastic net
  
  library(glmnet)
  
  y = Y[,1]
  x = as.matrix(X)
  
  if(search_lambda == TRUE){
    #lambdas <- rev( c(seq(from=0.1, to=0.9, by = 0.1), 1:5 ) )
    lambdas <- rev( c(seq(from=0.1, to=1.0, by = 0.1) ) )
    cv_fit <- cv.glmnet(x, y, type = "mse", alpha = 0.5, lambda = lambdas, nfolds = n_search_folds)
    nzero_lambda.ind <- which(cv_fit$nzero != 0)
    min_nzero_cvm.val <- min(cv_fit$cvm[nzero_lambda.ind])
    min_nzero_cvm.ind <- which(cv_fit$cvm == min_nzero_cvm.val)
    lambda <- cv_fit$lambda[min_nzero_cvm.ind]
  }
  
  glmnet_model <- glmnet(x, y, alpha = alpha, lambda = lambda)
  glmnet_model.coef  <- predict(glmnet_model, type = 'coefficients', s = lambda)
  weights <- sort_LASSO_weights(glmnet_model.coef)
  
  n_sel <- length( labels(glmnet_model.coef)[[1]] [which(glmnet_model.coef != 0)] )-1
  print( paste("  Lambda:", lambda))
  print( paste("  Features selected:", n_sel) )
  
  glmnet_obj <- list( model = glmnet_model, feat = weights, lambda = lambda, alpha = alpha)
  
  return (glmnet_obj)
}

sort_glmnet_weights <- function (fit_coef){
  nonzero_ind <- which( (!fit_coef == 0)[,1] )
  if(length(nonzero_ind) == 1){
    return(NA)
  }else{
    fit_coef.weights <- fit_coef[ nonzero_ind,  ]
    intercept_ind <- which(names(fit_coef.weights) == "(Intercept)")
    fit_coef.weights <- fit_coef.weights[-intercept_ind]
    pos_weights.names <- labels(which( fit_coef.weights > 0))
    neg_weights.names <- labels(which( fit_coef.weights < 0))
    fit_coef.weights.abs.sort <- sort(abs(fit_coef.weights), decreasing = TRUE)
    
    df.weights <- data.frame(rank = 1:length(fit_coef.weights.abs.sort), weight=fit_coef.weights.abs.sort, sign=rep(NA, length(fit_coef.weights.abs.sort)), row.names = labels(fit_coef.weights.abs.sort))
    df.weights[pos_weights.names, "sign"] = "+"
    df.weights[neg_weights.names, "sign"] = "-"
    #fit_coef.weights.order_names <- names(fit_coef.weights.abs)
    return (df.weights)
  }
}

sort_logit_weights <- function(model){
  coeff <- model$coefficients
  coeff <- coeff[-1]
  coeff <- na.omit.vector(coeff)
  coeff.abs <- abs(coeff)
  
  pos_ind <- which(coeff > 0)
  neg_ind <- which(coeff < 0)
  intersect( pos_ind, neg_ind )
  sign_vec <- rep(NA, length(coeff.abs))
  sign_vec[pos_ind] <- "+"
  sign_vec[neg_ind] <- "-"
  sign_vec
  
  weights <- data.frame( weight = coeff.abs, sign = sign_vec )
  weights <- weights[ order( weights[,1], decreasing = T ), ]
  weights <- cbind( 1:nrow(weights), weights )
  names(weights)[1] <- "rank"
  head(weights)
  
  return(weights)
}

add_list_to_list <- function(main_list, to_add_list, add_name){
  add_index <- length(main_list)+1
  main_list[[add_index]] <- to_add_list
  names(main_list)[add_index] <- add_name
  return(main_list)
}

round.sci <- function(x, digits){
  if(x < 1*10^-digits){
    x.round <- formatC(x, format = "e", digits = digits)
  }else{
    x.round <- round(x = x, digits = digits)
  }
  return(x.round)
}

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

transparent_color <- function(color_name, alpha){
  col.rgb <- col2rgb(color_name)
  col.rgb2 <- col.rgb/255
  col.transparent <- rgb( col.rgb2[1], col.rgb2[2], col.rgb2[3], alpha = alpha )
  return(col.transparent)
}

intersect.multi <-function( X ){
  # X = list of vectors
  all_items <- c()
  for(i in 1:length(X)){
    vec <- X[[i]]
    all_items <- c(all_items, vec)
  }
  multi_intersect <- unique(all_items)
  #length(multi_intersect)
  
  for(i in 1:length(X)){
    for(j in 1:length(X)){
      if( j > i ){
        vec1 <- X[[i]]
        vec2 <- X[[j]]
        v12_intersect <- intersect(vec1, vec2)
        multi_intersect <- intersect(multi_intersect, v12_intersect)
      }
    }
  }
  return(multi_intersect)
}

normalize.quantiles.wrapper <- function(df){
  library(preprocessCore)
  df.qnorm <- data.frame(normalize.quantiles(x = as.matrix(df), copy = TRUE))
  colnames(df.qnorm) <- names(df)
  row.names(df.qnorm) <- row.names(df)
  return(df.qnorm)
}

find_replace.vector <- function(v, find_vec, replace_vec){
  v.rpl <- v
  for(i in 1:length(find_vec)){
    fnd <- find_vec[i]
    rpl <- replace_vec[i]
    
    fnd.i <- which(v == fnd)
    v.rpl[fnd.i] <- rpl
  }
  return(v.rpl)
}

geometric_mean <- function(x){
  geo.mean <- prod(x)^( 1/length(x) )
  return(geo.mean)
}

rm.item <- function(x, item){
  x.rm <- x[ -which(x == item) ]
  return(x.rm)
}

LogVMR.Seurat <- function (x) {
  logVMR <- log(x = var(x = exp(x = x) - 1)/mean(x = exp(x = x) - 1))
  return(logVMR)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

binarize_vector <- function(vec, thr, classes){
  
  class1_ind <- which(vec < thr)
  class2_ind <- which(vec >= thr)
  
  vec.bin <- rep(NA, length(vec))
  vec.bin[class1_ind] <- classes[1]
  vec.bin[class2_ind] <- classes[2]
  
  # vec.bin
  
  return(vec.bin)
}

binarize_vector2 <- function(vec, thr, sign){
  
  if(sign == "+"){
    class1_ind <- which(vec >= thr)
    class2_ind <- which(vec < thr)
  }else if(sign == "="){
    class1_ind <- which(vec < thr)
    class2_ind <- which(vec >= thr)
  }
  
  vec[ class1_ind ]
  vec[ class2_ind ]
  
  vec.bin <- rep(NA, length(vec))
  vec.bin[class1_ind] <- 1
  vec.bin[class2_ind] <- 0
  
  vec.bin
  
  return(vec.bin)
}

compare_binary_vectors <- function( vec1, vec2, pos_nm, neg_nm ){
  
  in1_1 <- which(vec1 == pos_nm)
  in1_0 <- which(vec1 == neg_nm)
  
  vec2[in1_1]
  
  tp.cnt <- length(which( vec2[in1_1] == pos_nm ))
  fn.cnt <- length(which( vec2[in1_1] == neg_nm ))
  
  fp.cnt <- length(which( vec2[in1_0] == pos_nm ))
  tn.cnt <- length(which( vec2[in1_0] == neg_nm ))
  
  #comparison_obj <- list( tp = tp.cnt, fp = fp.cnt, fn = fn.cnt, tn = tn.cnt )
  comparison_vec <- c( tp.cnt, fp.cnt, fn.cnt, tn.cnt )
  names( comparison_vec ) <- c("tp", "fp", "fn", "tn")
  return(comparison_vec)
}

add_PCA_variation_explained <- function(PCA_obj){
  
  eigs <- PCA_obj$sdev^2
  var_expl <- eigs / sum(eigs)
  
  vec_names <- paste( "PC", 1:length(var_expl), sep = "" )
  names(var_expl) <- vec_names
  
  length(PCA_obj)
  PCA_obj[[ length(PCA_obj)+1 ]] <- var_expl
  names(PCA_obj)[length(PCA_obj)] <- "variation_explained"
  
  return(PCA_obj)
}

prcomp.add_variation_explained <- function(PCA_obj){
  
  eigs <- PCA_obj$sdev^2
  var_expl <- eigs / sum(eigs)
  
  vec_names <- paste( "PC", 1:length(var_expl), sep = "" )
  names(var_expl) <- vec_names
  
  length(PCA_obj)
  PCA_obj[[ length(PCA_obj)+1 ]] <- var_expl
  names(PCA_obj)[length(PCA_obj)] <- "variation_explained"
  
  return(PCA_obj)
}

add_line_hist <- function( x, breaks, col = "black", type = "o", pch = 16, lwd = 1 ){
  histObj <- hist( x = x, breaks = breaks, plot = F )
  
  histObj$counts
  percent_vec <- histObj$counts/length(x)*100
  
  #histObj$density
  #percent_vec
  
  x <- histObj$mids
  y <- percent_vec
  lines( x = x, y = y, type = type, col = col, pch = pch, lwd = lwd )
}

pull_KEGG_IDs <- function(){
  #https://www.researchgate.net/post/How_i_can_get_a_list_of_KEGG_pathways_and_its_list_of_genes
  library(org.Hs.eg.db)
  mapped <- mappedkeys(org.Hs.egPATH2EG)
  L <- as.list(org.Hs.egPATH2EG[mapped])
  return(L)
}

scale_center_matrix <- function(X, MARGIN = 2){
  X.scale <- apply( X = X, MARGIN = MARGIN, FUN = function(x) ( x - min(x) )/( max(x) - min(x) )  )
  X.center <- apply( X = X.scale, MARGIN = MARGIN, FUN = function(x) x - mean(x)  )
  return(X.center)
}

p2sym <- function(p){
  #p <- 0.002
  if(p > 0.05){
    sym <- "n.s."
  }else if(p <= 0.05 && p > 0.01){
    sym <- "*"
  }else if(p <= 0.01 && p > 0.001){
    sym <- "**"
  }else if(p <= 0.001){
    sym <- "***"
  }
  sym
  return(sym)
}

calculate_MAD <- function(x){
  #x <- m.IC50.Klijn.MEK.noNA[,1]
  MAD <- median(abs(x-median(x)))
  return(MAD)
}

remove_nonAlphaNumeric <- function(x){
  
  #x <- row.names(m.IC50.GDSC)
  
  alphanumeric <- c( LETTERS, letters, 0:9 )
  alphanumeric
  
  x.split <- strsplit(x = x, split = "")
  x.split[1:5]
  
  AN_only <- c()
  for(i in 1:length(x.split)){
    splt <- x.split[[i]]
    splt
    
    rm_i <- which( !splt %in% alphanumeric )
    if(length(rm_i) > 0){
      splt2 <- splt[-rm_i]
    }else{
      splt2 <- splt
    }
    splt2
    
    AN_only <- c(AN_only, paste(splt2, collapse = ""))
  }
  
  return(AN_only)
}

scale_center_vector <- function(x){
  #x <- 1:10
  x.scale <- (x - min(x)) / (max(x) - min(x))
  #x.scale
  x.center <- x.scale - mean(x.scale)
  #x.center
  return(x.center)
}

variance_explained_from_anova <- function(anova_obj){
  sumSq <- anova_obj$`Sum Sq`
  perVar <- sumSq / sum(sumSq) * 100
  names(perVar) <- row.names(anova_obj)
  return(perVar)
}

PCA_plot.axis_labels <- function(PCA_obj, PC_dim1, PC_dim2){
  if("variation_explained" %in% names(PCA_obj)){
    var_expl <- PCA_obj$variation_explained
    var_expl1 <- round(var_expl[PC_dim1], 3)
    var_expl2 <- round(var_expl[PC_dim2], 3)
    
    xlab <- paste( "PC", PC_dim1, " (", var_expl1, ")", sep = "" )
    ylab <- paste( "PC", PC_dim2, " (", var_expl2, ")", sep = "" )
  }else{
    xlab <- paste( "PC", PC_dim1, sep = "" )
    ylab <- paste( "PC", PC_dim2, sep = "" )
  }
  return( list(xlab = xlab, ylab = ylab) )
}

plot_2dataset_PCA.common_CL <- function(PCA_obj, common_IDs, probe1, probe2, PC_dim1 = 1, PC_dim2 = 2, lwd = 1, cex = 1, main_title = NA){
  
  #PCA_obj <- pcaObj.GDSC_CCLE_TNBC.RNA
  #common_IDs <- common_CL.GDSC_CCLE
  #probe1 <- "GDSC_"
  #probe2 <- "CCLE_"
  #lwd <- 1
  #cex <- 1
  #main_title <- NA
  
  df_PC <- PCA_obj$x
  head(row.names(df_PC))
  tail(row.names(df_PC))
  
  xlim <- c( min(df_PC[,PC_dim1]), max(df_PC[,PC_dim1]) )
  ylim <- c( min(df_PC[,PC_dim2]), max(df_PC[,PC_dim2]) )
  
  ind_probe1 <- grep( pattern = probe1, x = row.names(df_PC) )
  ind_probe2 <- grep( pattern = probe2, x = row.names(df_PC) )
  
  df_PC <- df_PC[ sort( c(ind_probe1, ind_probe2) ), ]
  
  labelObj <- PCA_plot.axis_labels( PCA_obj = PCA_obj, PC_dim1 = PC_dim1, PC_dim2 = PC_dim2 )
  xlab <- labelObj$xlab
  ylab <- labelObj$ylab
  
  xlab
  ylab
  
  plot( 1, type = 'n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main_title )
  for(i in 1:length(common_IDs)){
    ID <- common_IDs[i]
    ID
    
    ID_ind <- grep( pattern = ID, x = row.names(df_PC) )
    ID_ind
    
    if( length(ID_ind) == 2 ){
      x <- c( df_PC[ ID_ind, PC_dim1 ] )
      y <- c( df_PC[ ID_ind, PC_dim2 ] )
      
      x
      y
      
      segments(x0 = x[1], y0 = y[1], x1 = x[2], y1 = y[2], lwd = lwd, col = "gray")
      points( x = x, y = y, col = c("red", "blue"), pch = 16, cex = cex )
    }
    
  }
}

plot_PCA_multidataset <- function( PCA_obj, probe_vec, col_vec, PC_dim1 = 1, PC_dim2 = 2, pch = 16, cex_vec = NA ){
  
  #PCA_obj <- pcaObj.GDSC_CCLE_TNBC.RNA
  #PC_dim1 <- 1
  #PC_dim2 <- 2
  #probe_vec <- c("GDSC_", "CCLE_", "TNBC_")
  #col_vec <- c("blue", "gray", "red")
  #cex_vec <- c(0.25, 0.25, 0.75)
  #cex_vec <- NA
  
  PC_dim1
  PC_dim2
  probe_vec
  col_vec
  
  if(is.na(cex_vec)){
    cex_vec <- rep(1, length(probe_vec))
  }
  
  df_PC <- PCA_obj$x
  head(row.names(df_PC))
  tail(row.names(df_PC))
  
  x <- df_PC[,PC_dim1]
  y <- df_PC[,PC_dim2]
  
  n_dots <- nrow(df_PC)
  
  probe_ind_obj <- list(probe_vec)
  for(i in 1:length(probe_vec)){
    probe <- probe_vec[i]
    probe_ind <- grep(pattern = probe, x = row.names(df_PC))
    
    probe_ind
    probe_ind_obj[[i]] <- probe_ind
    names(probe_ind_obj)[i] <- probe
  }
  
  probe_ind_obj
  
  color_vec <- rep(NA, n_dots)
  for(i in 1:length(probe_ind_obj)){
    probe_ind <- probe_ind_obj[[i]]
    color_vec[probe_ind] <- col_vec[i]
  }
  
  color_vec
  table(color_vec)
  
  cex_vec2 <- rep(NA, n_dots)
  for(i in 1:length(probe_ind_obj)){
    probe_ind <- probe_ind_obj[[i]]
    cex_vec2[probe_ind] <- cex_vec[i]
  }
  
  cex_vec2
  table(cex_vec2)
  
  #pch <- 16
  #cex <- 1
  
  index_shuffle <- sample( 1:n_dots )
  
  x.shuffle <- x[index_shuffle]
  y.shuffle <- y[index_shuffle]
  color.shuffle <- color_vec[index_shuffle]
  cex.shuffle <- cex_vec2[index_shuffle]
  
  if("variation_explained" %in% names(PCA_obj)){
    var_expl <- PCA_obj$variation_explained
    var_expl1 <- round(var_expl[PC_dim1], 3)
    var_expl2 <- round(var_expl[PC_dim2], 3)
    
    xlab <- paste( "PC", PC_dim1, " (", var_expl1, ")", sep = "" )
    ylab <- paste( "PC", PC_dim2, " (", var_expl2, ")", sep = "" )
  }else{
    xlab <- paste( "PC", PC_dim1, sep = "" )
    ylab <- paste( "PC", PC_dim2, sep = "" )
  }
  
  xlab
  ylab
  
  plot( x = x.shuffle, y = y.shuffle, xlab = xlab, ylab = ylab, pch = pch, cex = cex.shuffle, col = color.shuffle )
}

plot_PCA_multidataset.common_CL <- function(PCA_obj, probe_vec, col_vec, PC_dim1 = 1, PC_dim2 = 2){
  
  #PCA_obj <- pcaObj.GDSC_CCLE_TNBC.RNA
  #probe_vec <- c("GDSC_", "CCLE_", "TNBC_")
  #col_vec <- c("darkblue", "darkgray", "red")
  #PC_dim1 <- 1
  #PC_dim2 <- 2
  common_IDs
  
  
  df_PC <- PCA_obj$x
  head(row.names(df_PC))
  tail(row.names(df_PC))
  
  xlim <- c( min(df_PC[,PC_dim1]), max(df_PC[,PC_dim1]) )
  ylim <- c( min(df_PC[,PC_dim2]), max(df_PC[,PC_dim2]) )
  
  labelObj <- PCA_plot.axis_labels( PCA_obj = PCA_obj, PC_dim1 = PC_dim1, PC_dim2 = PC_dim2 )
  xlab <- labelObj$xlab
  ylab <- labelObj$ylab
  
  xlab
  ylab
  
  
  
  plot( 1, type = 'n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab )
  for(i in 1:length(common_IDs)){
    ID <- common_IDs[i]
    ID_ind <- grep( pattern = ID, x = row.names(df_PC) )
    
    if( length(ID_ind) == 2 ){
      x <- c( df_PC[ ID_ind, PC_dim1 ] )
      y <- c( df_PC[ ID_ind, PC_dim2 ] )
      
      segments(x0 = x[1], y0 = y[1], x1 = x[2], y1 = y[2], lwd = 2, col = "gray")
      points( x = x, y = y, col = c("red", "blue"), pch = 16, cex = 1 )
    }
    
  }
}

euclidean_dist.2dataset.2dim.common_CL <- function(X, common_IDs, probe1, probe2, dim1 = 1, dim2 = 2){
  
  #X <- pcaObj.GDSC_CCLE_TNBC.RNA$x
  #common_IDs <- common_CL.GDSC_CCLE
  #dim1 <- 1
  #dim2 <- 2
  #probe1 <- "GDSC_"
  #probe2 <- "CCLE_"
  
  common_IDs
  
  #df_PC <- PCA_obj$x
  head(row.names(X))
  tail(row.names(X))
  
  ind_probe1 <- grep( pattern = probe1, x = row.names(X) )
  ind_probe2 <- grep( pattern = probe2, x = row.names(X) )
  
  ind_probe1
  ind_probe2
  
  eucDist_vec <- c()
  for(i in 1:length(common_IDs)){
    ID <- common_IDs[i]
    ID
    
    ID_ind1 <- grep( pattern = ID, x = row.names(X)[ind_probe1] )
    ID_ind2 <- grep( pattern = ID, x = row.names(X)[ind_probe2] )
    
    ID_ind1
    ID_ind2
    
    ID_ind <- c(ID_ind1, ID_ind2)
    
    if( length(ID_ind) == 2 ){
      x <- c( X[ ID_ind, dim1 ] )
      y <- c( X[ ID_ind, dim2 ] )
      
      x_dist <- max(x)-min(x)
      y_dist <- max(y)-min(y)
      eucDist <- x_dist+y_dist
      
      eucDist_vec <- c(eucDist_vec, eucDist)
      names(eucDist_vec)[length(eucDist_vec)] <- ID
    }
  }
  return(eucDist_vec)
}

matrix_to_3col <- function(m){
  
  #m <- train_per
  m
  
  out_3col <- c()
  #for(i in 1:nrow(m)){
  #  
  #  row_nm <- rownames(m)[i]
  #  row_nm
  #  
  #  for(j in 1:ncol(m)){
  #    
  #    col_nm <- colnames(m)[j]
  #    col_nm
  #    
  #    value <- m[i, j]
  #    value
  #    
  #    sub_vec <- c( row_nm, col_nm, value )
  #    out_3col <- rbind( out_3col, sub_vec )
  #  }
  #}
  
  for(i in 1:ncol(m)){
    
    col_nm <- colnames(m)[i]
    col_nm
    
    for(j in 1:nrow(m)){
      
      row_nm <- rownames(m)[j]
      row_nm
      
      value <- m[i, j]
      value
      
      sub_vec <- c( col_nm, row_nm, value )
      out_3col <- rbind( out_3col, sub_vec )
    }
  }
  
  out_3col
  
  rownames(out_3col) <- NULL
  colnames(out_3col) <- c("X", "Y", "value")
  out_3col
  
  out_3col <- data.frame(out_3col)
  out_3col[,3] <- as.numeric(as.character(out_3col[,3]))
  
  train_per.3col
  train_per.3col[,3]
  
  return(out_3col)
}

intersect.iterate <- function( list_of_vectors ){
  
  # list_of_vectors <- list( LETTERS[1:15], LETTERS[3:18], LETTERS[6:23] )
  # list_of_vectors
  
  # collect all items
  all_items <- c()
  for(i in 1:length(list_of_vectors)){
    vec <- list_of_vectors[[i]]
    head(vec)
    length(vec)
    all_items <- unique( c( all_items, vec ) )
  }
  all_items
  length(all_items)
  
  # iterate intersect
  intersect.all <- all_items
  for(i in 1:length(list_of_vectors)){
    vec <- list_of_vectors[[i]]
    head(vec)
    length(vec)
    intersect.all <- intersect(intersect.all, vec)
  }
  intersect.all
  return(intersect.all)
}

check_nonAlphanumeric <- function(x){
  
  #x <- "TEST1234"
  #x <- "TEST_1234"
  
  alphanumeric <- c(LETTERS, letters, 0:9)
  #alphanumeric
  
  chars <- strsplit(x = x, split = "")[[1]]
  #chars
  
  testBool <- sapply( X = chars, FUN = function(x) x %in% alphanumeric )
  #testBool
  
  nonAlphanumeric <- FALSE %in% testBool
  #nonAlphanumeric
  
  return(nonAlphanumeric)
  
}

convert_human_geneIDs <- function( IDs, column, keytype, compIDs ){
  
  library(org.Hs.eg.db)
  
  # IDs <- colnames(m.RNA.TPM)
  # column <- "ENSEMBL"
  # keytype <- "SYMBOL"
  # compIDs <- colnames(m.RNA.TNBC)
  
  ## CONVERT IDS
  
  IDs_conv <- mapIds(x = org.Hs.eg.db, keys = IDs, column = column, keytype = keytype, multiVals = list)
  #IDs_conv[1:25]
  
  
  ## IDS WITH NO CONVERSION (NAs)
  
  NA_ind <- which(is.na(IDs_conv))
  NA_names <- names(IDs_conv)[NA_ind]
  #IDs_conv[NA_names][1:15]
  
  IDs_conv.noNA <- IDs_conv[-NA_ind]
  #which(is.na(IDs_conv.noNA))
  #IDs_conv.noNA[1:25]
  
  
  ## SINGLE OR MULTIPLE CONVERTED IDs
  
  n_IDs <- sapply( X = IDs_conv.noNA, FUN = length )
  #head(n_IDs, 25)
  #table(n_IDs)
  
  
  ## SINGLE IDs
  
  oneID_ind <- which(n_IDs == 1)
  #IDs_conv.noNA[oneID_ind][1:25]
  oneID_names <- names(IDs_conv.noNA)[oneID_ind]
  #IDs_conv.noNA[oneID_names][1:25]
  oneID_vec <- unlist(IDs_conv.noNA[oneID_ind])
  
  # head(oneID_vec)
  # tail(oneID_vec)
  # length(oneID_vec)
  # length(IDs_conv.noNA[oneID_ind])
  
  oneID_vec.inComp <- oneID_vec[which(oneID_vec %in% compIDs)]
  # length(oneID_vec)
  # length(oneID_vec.inComp)
  # head(oneID_vec.inComp)
  
  ## MULTIPLE IDs
  
  mulID_ind <- which(n_IDs >= 2)
  #IDs_conv.noNA[mulID_ind][1:25]
  
  IDs_conv.noNA.mulID <- IDs_conv.noNA[mulID_ind]
  #IDs_conv.noNA.mulID[1:25]
  
  mulID_in_comp <- lapply(X = IDs_conv.noNA.mulID, FUN = function(x) x[which(x %in% compIDs)] )
  #mulID_in_comp[1:25]
  
  n_IDs.inComp <- sapply( X = mulID_in_comp, FUN = length )
  #table(n_IDs.inComp)
  
  oneID2_ind <- which(n_IDs.inComp == 1)
  #head(oneID2_ind)
  #length(oneID2_ind)
  #mulID_in_comp[oneID2_ind][1:25]
  
  oneID2_vec <- unlist(mulID_in_comp[oneID2_ind])
  # length(oneID2_vec)
  # length(mulID_in_comp[oneID2_ind])
  # head(oneID2_vec)
  # length(oneID2_vec)
  # length(mulID_in_comp)
  
  oneToOneConv_vec <- sort(c(oneID_vec.inComp, oneID2_vec))
  #head(oneToOneConv_vec)
  #length(oneToOneConv_vec)
  
  return_obj <- list( one_to_one = oneToOneConv_vec, one_to_one.1 = oneID_vec.inComp, one_to_one.2 = oneID2_vec, noConv = NA_names, conv_list = IDs_conv )
  return(return_obj)
}
