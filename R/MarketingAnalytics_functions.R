
MarketingAnalytics <-  function(){
  cat(paste("\n",
  "####################################################\n",
  "#    R Codes and functions for Marketing Analytics #\n",
  "#    Author: Dr. Moein Khanlari                    #\n",
  "#    Course: Marketing Analytics                   #\n",
  "#    Version 1.0 : This code has not yet been      #\n",
  "#    revised for efficiency and conciseness        #\n",
  "#    Last updated: Fall 2018                       #\n",
  "####################################################\n"))
  }

#-------Functions used in CLV analysis ------------------------

#' Add comma Function
#'
#' This function allows you to group every three digits in a number using a comma.
#' @param x The number you wish to add commas to.
#' @keywords comma
#' @export
#' @examples
#' addcomma(23564715565)
#'
addcomma <- function(x){
  y <- prettyNum(x,big.mark=",",scientific=FALSE)
  return(y)
}


#--------Functions used in Segmentation Analysis-----------------

is.sig <- function(x,y){
  # This functions takes vectors x, and y; then itreturns 0 when there's NOT a significant difference between means of x and y
  temp <- t.test(x, y, alternative = "two.sided", conf.level=0.95, var.equal = FALSE)
  if (temp$p.value >= 0.05) return(0) else return(1)
}


calculate_segment_means <- function(df){
  # This function takes a data frame consisting of segmentation variables and segment memberships
  # the segment memberships is the last column of the said dataframe and is called segment.

  # We will here calculate the mean of each segmentation variable for each segment
  # But we will also take some steps to test if those means are statistically different
  # from the population mean. Population here refers to the entire sample rather than a given segment.

  # First, let's prepare to check for differences between population and segment means
  # Let's create a function that returns 0 when the means of two vectors are not
  # statistically different at the 95% confidence level
  # We will be using the Un-paired two-sample T-test.

  # How many segments do we have?
  m <- length(unique(df$segment))
  # Create an empty data frame to keep the results of significant tests
  sig_diff <- data.frame(matrix(NA, nrow=(ncol(df)-1), ncol=m))

  # For each column of the data set and each segment, do the significance test and store results
  for (i in 1:(ncol(df)-1)){
    for (j in 1:m){
      sig_diff[i,j] <- is.sig(df[,i],df[df$segment==j,i])
    }}

  # Now calculate population means
  # population refers to the "entire sample" in this script.
  df_means <- colMeans(df, na.rm = TRUE, dims = 1)
  # Now calculate segment means
  segment_means <- t(aggregate(df[,1:(ncol(df))], list(segment=df$segment), mean))
  # The next line is to simply avoid a duplicate row.name warning.
  row.names(segment_means)[1] <- paste(row.names(segment_means)[1],"__")
  segment_means <- data.frame(segment_means)
  df_means <- df_means[-length(df_means)]
  segment_means <- segment_means[-1,]
  segment_means <- segment_means[-nrow(segment_means),]
  segment_means <- data.frame(round(df_means,2),round(segment_means,2))
  mm <- ncol(segment_means)-1
  names(segment_means) <- c("Population Means",paste(rep("Segment",mm),1:mm, sep=""))
  dfdf <- segment_means
  dfdf[,2:(ncol(dfdf))] <- dfdf[,2:(ncol(dfdf))] - dfdf[,1]

  # Only show Segment Means that are significanlty different from
  # Population means at the 95% confidence level
  # Next line adds a columns of 1s to the sig_diff data frame
  sig_diff <- cbind(X0 = rep(1,nrow(sig_diff)),sig_diff)
  # We will now make insignificant semgent mean values zero
  significant_segment_means <- segment_means*sig_diff
  # and replace zeros with ---
  # I later commented out the next line of code
  # significant_segment_means[significant_segment_means==0] <- "---"
  # Display Population and Segment Means
  # segment_means
  # # Only significantly-different-from-population means
  # significant_segment_means
  #
  differences <- round(dfdf*sig_diff,2)

  # and replace zeros with ---
  differences[differences==0] <- "---"
  differences <- differences[,-1]
  bothTables <- data.frame(segment_means,differences)
  names(bothTables) <- c("Population\n Means",paste(rep("Segment",mm),1:mm, rep("\n",mm), rep("Means",mm), sep=" "), paste(rep("Segment",mm),1:mm, rep("\n",mm),rep("Difference from\n",mm),rep("Population",mm), sep=" "))
  mymsg <- "This table shows the mean values that are significantly different from the population mean. "
  mylist <- list(mymsg,bothTables)
  return(mylist)



}


Estimate_Partworth_Utilities <- function(ratings,products){
  # This function first regresses each respondent's ratings of the conjoint profiles on those product attributes,
  # and then re-scales the regression coefficients for each attribute level such that 1) for each person,
  # the  least favorite level of an attribute gets a coefficient (i.e., partworth) of zero, and 2) the partworths of most
  # preferred attribute levels within each product profile sum up to 100. The function returns a data.frame
  # of partworths for all levels of all attributes for all respondents.

  num_of_attributes <- dim(products)[2]
  mylist <- list()
  for (i in 1:ncol(ratings)){
    thislm <- lm(ratings[,i]~ ., data = products)
    mylist[[i]] <- thislm$coefficients
  }
  ncoeff=length(thislm$coefficients)

  df <- data.frame(t(matrix(unlist(mylist), nrow=ncoeff, byrow=F)))
  names(df) <- names(mylist[[1]])
  df <- df[,2:ncol(df)]


  #re-scale if all categorical attributes
  z <- apply(products,2,unique)
  nn <- attributes(z)
  AttributeCount <- length(nn$names)

  allnames=vector()
  for (i in (1:length(z))){
    temp= paste0(rep(nn$names[i],length(z[[i]])),z[[i]])
    allnames=c(allnames,temp)
  }

  namestoadd = allnames[-c(which(allnames %in% names(df)))]

  empty_df <- data.frame(matrix(rep(0,AttributeCount*nrow(df)),ncol = AttributeCount))
  names(empty_df) <- namestoadd

  df <- cbind(df,empty_df)
  df <- df[sort(names(df))]


  temp <-  lengths(apply(products,2,unique)) -1
  temp <- temp[sort(names(temp))]

  df2 <- df
  start <-  1
  for (i in 1:length(temp)){
    test= df[,start:(start + temp[i])]
    test2= test + (-1*apply(test,1,min))
    df2[,start:(start + temp[i])] = test2
    start <-  start + temp[i] +1
  }


  temp2 <- temp + 1
  SumUtils <- rep(0,nrow(df2))
  for (i in 1:nrow(df2)){
    Utils <- 0
    str <- 1
    for (j in 1:AttributeCount){
      Utils <-  Utils + max(df2[i,str:(str+temp2[j]-1)])
      str <- str + temp2[j]
    }
    SumUtils[i] <- Utils

  }
  options(digits=2)

  df3 <- 100*df2/SumUtils
  df3 <- round(100*df2/SumUtils,digits=0)
  ############### Minor bug to be fixed later ####################
  # at this point, each name(df3) has an Xi at the beginning, with i
  # representing the number of attribute. The next line eliminates that Xi.
  # if there are more than 9 attributes, this code will run into a minor
  # issue because the starting location of attribute level names would not be location 3,
  # from attribute 10 onwards, and will be location 4 instead.
  # The next line of code only removes Xi characters and keeps characters from the 3rd location onwards.
  # Fix this issue in the next version
  # Right now, a warning will be issued for the code user to fix the name of the attributes manually
  # in the function's output.
  names(df3) <-  substr(names(df3),3,nchar(names(df3)))
  # The next if statement addresses a minor bug that will be fixed in the next revision.
  if (num_of_attributes>9) {
    message(paste("\n",
                  "#######         Warning        ################\n",
                  "Starting from the tenth attribute, the names of attribute levels \n",
                  "in the produced data frame start with an extra numeric character. \n",
                  "These numeric characters need to be removed manually until this bug is fixed.\n"))
  }

  # These are our partworth utilities
  return(df3)
}


Estimate_Market_Shares_old <- function(product_alternatives,pws){

  # Ensure pws variable names and product_alternatives attributes are the same
  checkthis <- 0
  temp1 <- sort(unique(names(pws)))
  temp2 <- sort(as.character(unique(unlist(product_alternatives))))
  extras <- which(!(names(pws) %in% temp2))
  if (length(extras) > 0){
    pws <- pws[,-extras]
  }

  temp1 <- sort(unique(names(pws)))
  temp2 <- sort(as.character(unique(unlist(product_alternatives))))
  if (all(temp2 %in% temp1)) {checkthis <- 1}
  if (checkthis!=1) {
  # if (sum(sort(names(pws))!=sort(as.character(unique(unlist(product_alternatives)))))==length(names(pws))) {
  # The above condition runs into a problem if two attributes have similar values, for example yes/no
    message(paste("\n",
                  "#######         ERROR        ################\n",
                  "The function could not calculate market shares:\n",
                  "The values in the data frame for the list of products (i.e., product_alternatives)\n",
                  "Must match the column names in the partworth utilities data frame (i.e., pws).\n",
                  "Please fix this problem before running this function.\n"))
    return(0)
  }
  # Calculate_Product_Utilities
  #create matrix of 0-1 product attributes
  existence <- pws[1:ncol(product_alternatives),] # existence refers to the existence of an attribute level within a given product
  existence[,] <- 0
  for (i in 1:ncol(product_alternatives)){
    existence[i,which(names(existence) %in% as.character(product_alternatives[,i]))] <- 1
  }
  ex=existence
  tex=t(ex)
  product_utils <- data.frame(as.matrix(pws)%*%as.matrix(tex))
  names(product_utils) <- paste0(rep("product",ncol(product_utils)),c(1:ncol(product_utils)))
  pu <- product_utils
  # calculate market shares
  pu_clone <- pu
  pu_clone[,] <- 0
  for (i in 1:nrow(pu)){
    pu_clone[i,which(pu[i,]==max(pu[i,]))] <- 1
    thissum <- sum(pu_clone[i,])
    if (thissum>1) {
      pu_clone[i,which(pu_clone[i,]==1)] <- 1/thissum
    }
  }
  # Market shares using first choice rule
  MS_FirstChoice <- 100*colSums(pu_clone)/nrow(pu)
  # Market shares using Share of preference rule
  preference_share <- pu/rowSums(pu)
  MS_PreferenceShare <- 100*colSums(preference_share)/nrow(pu)
  # Market shares using the Logit rule
  MaxRatingScale <- 100                                                     #Need to change this if rating scale changes
  cp <- 100/(12*MaxRatingScale) #Define confidence parameter (cp)
  # cp=1   # Can see how results change with different values for cp
  cpUtil <- pu*cp
  exp_cpUtil <- exp(cpUtil)
  logit_share <- exp_cpUtil/rowSums(exp_cpUtil)
  MS_Logit <- 100*colSums(logit_share)/NROW(logit_share)
  tb1 <- data.frame(MS_FirstChoice,MS_PreferenceShare,MS_Logit)
  return(tb1)
}

Estimate_Market_Shares <- function(product_alternatives,pws){

  # Ensure pws variable names and product_alternatives attributes are the same
  checkthis <- 0
  temp1 <- sort(unique(names(pws)))
  temp2 <- sort(as.character(unique(unlist(product_alternatives))))
  extras <- which(!(names(pws) %in% temp2))
  if (length(extras) > 0){
    pws <- pws[,-extras]
  }

  temp1 <- sort(unique(names(pws)))
  temp2 <- sort(as.character(unique(unlist(product_alternatives))))
  if (all(temp2 %in% temp1)) {checkthis <- 1}
  if (checkthis!=1) {
    # if (sum(sort(names(pws))!=sort(as.character(unique(unlist(product_alternatives)))))==length(names(pws))) {
    # The above condition runs into a problem if two attributes have similar values, for example yes/no
    # This is a bug that needs to be fixed in later versions.
    message(paste("\n",
                  "#######         ERROR        ################\n",
                  "The function could not calculate market shares:\n",
                  "The attribute values in the data frame for the list of products (i.e., All_products)\n",
                  "Must match the column names in the partworth utilities data frame (i.e., pws), but that is not the case right now.\n",
                  "Please fix this problem before running this function.\n",
                  "Run these two functions to find and fix the differences:\n",
                  "sort(unique(names(pws)))\n",
                  "sort(as.character(unique(unlist(All_products))))"))
    return(0)
  }
  # Calculate_Product_Utilities
  #create matrix of 0-1 product attributes
  existence <- pws[1:ncol(product_alternatives),] # existence refers to the existence of an attribute level within a given product
  existence[,] <- 0
  for (i in 1:ncol(product_alternatives)){
    existence[i,which(names(existence) %in% as.character(product_alternatives[,i]))] <- 1
  }
  ex=existence
  tex=t(ex)
  product_utils <- data.frame(as.matrix(pws)%*%as.matrix(tex))
  names(product_utils) <- paste0(rep("product",ncol(product_utils)),c(1:ncol(product_utils)))
  pu <- product_utils
  # calculate market shares
  pu_clone <- pu
  pu_clone[,] <- 0
  for (i in 1:nrow(pu)){
    pu_clone[i,which(pu[i,]==max(pu[i,]))] <- 1
    thissum <- sum(pu_clone[i,])
    if (thissum>1) {
      pu_clone[i,which(pu_clone[i,]==1)] <- 1/thissum
    }
  }
  # Market shares using first choice rule
  MS_FirstChoice <- 100*colSums(pu_clone)/nrow(pu)
  # Market shares using Share of preference rule
  preference_share <- pu/rowSums(pu)
  MS_PreferenceShare <- 100*colSums(preference_share)/nrow(pu)
  # Market shares using the Logit rule
  MaxRatingScale <- 100                                                     #Need to change this if rating scale changes
  cp <- 100/(12*MaxRatingScale) #Define confidence parameter (cp)
  # cp=1   # Can see how results change with different values for cp
  cpUtil <- pu*cp
  exp_cpUtil <- exp(cpUtil)
  logit_share <- exp_cpUtil/rowSums(exp_cpUtil)
  MS_Logit <- 100*colSums(logit_share)/NROW(logit_share)
  tb1 <- data.frame(MS_FirstChoice,MS_PreferenceShare,MS_Logit)
  return(tb1)
}

Find_Optimal_Products <- function (MS,rule,n){
  #Rule can only be 1, 2, or 3
  Max_No_of_products_to_return <- n
  if ((!is.numeric(rule)) | (is.numeric(rule) & rule!=1 & rule!=2 & rule!=3)) stop("The 2nd argument of the function can be either 1, 2, or 3 \n These represent First Choice rule (1), PreferenceShare rule (2) and Logit choice rule (3)")
  tms <- data.frame(t(MS))
  mm=sort(tms[rule,], decreasing = TRUE)
  optimalp <- mm[which(mm>0)]
  howmany <- min(Max_No_of_products_to_return,length(optimalp))

  return(optimalp[1:howmany])
}

Plot_MS_rule1 <- function(MS, mytext=""){
  # pass any additional info fro plot title using the mytext argument
  if (!require("plotrix")) install.packages("plotrix")
  rule  <-  1
  n <- dim(MS)[1]
  optimals1 <- Find_Optimal_Products(MS,rule,n)
  plothis <- optimals1
  slices <- as.numeric(plothis)
  mylabels <- paste(names(plothis),"(",round(plothis,2), "%)")
  if (rule==1){ custommain <- paste("Market shares using First Choice Rule\n",mytext)}
  if (rule==2){ custommain <- paste("Market shares using Preference Share Rule\n",mytext)}
  if (rule==3){ custommain <- paste("Market shares using Logit Choice Rule\n",mytext)}
  pie(slices, labels = mylabels, main=custommain)
   # pie3D(slices,labels=mylabels,explode=0.1, main="Market Shares using First Choice rule")
}

Plot_MS_rule2 <- function(MS, mytext=""){
  # pass any additional info fro plot title using the mytext argument
  if (!require("plotrix")) install.packages("plotrix")
  rule <-  2
  n <- dim(MS)[1]
  optimals1 <- Find_Optimal_Products(MS,rule,n)
  plothis <- optimals1
  slices <- as.numeric(plothis)
  mylabels <- paste(names(plothis),"(",round(plothis,2), "%)")
  if (rule==1){ custommain <- paste("Market shares using First Choice Rule\n",mytext)}
  if (rule==2){ custommain <- paste("Market shares using Preference Share Rule\n",mytext)}
  if (rule==3){ custommain <- paste("Market shares using Logit Choice Rule\n",mytext)}
  pie(slices, labels = mylabels, main=custommain)
  # pie3D(slices,labels=mylabels,explode=0.1, main="Market Shares using First Choice rule")
}

Plot_MS_rule3 <- function(MS, mytext=""){
  # pass any additional info fro plot title using the mytext argument
  if (!require("plotrix")) install.packages("plotrix")
  rule <-  3
  n <- dim(MS)[1]
  optimals1 <- Find_Optimal_Products(MS,rule,n)
  plothis <- optimals1
  slices <- as.numeric(plothis)
  mylabels <- paste(names(plothis),"(",round(plothis,2), "%)")
  if (rule==1){ custommain <- paste("Market shares using First Choice Rule\n",mytext)}
  if (rule==2){ custommain <- paste("Market shares using Preference Share Rule\n",mytext)}
  if (rule==3){ custommain <- paste("Market shares using Logit Choice Rule\n",mytext)}
  pie(slices, labels = mylabels, main=custommain)
  # pie3D(slices,labels=mylabels,explode=0.1, main="Market Shares using First Choice rule")
}

Plot_MS <- function(MS,rule){
  if (!require("plotrix")) install.packages("plotrix")
  optimals1 <- Find_Optimal_Products(MS,rule,5)
  plothis <- optimals1
  slices <- as.numeric(plothis)
  mylabels <- paste(names(plothis),"(",round(plothis,2), "%)")
  if (rule==1){ custommain <- "Market shares using First Choice Rule"}
  if (rule==2){ custommain <- "Market shares using Preference Share Rule"}
  if (rule==3){ custommain <- "Market shares using Logit Choice Rule"}
  pie(slices, labels = mylabels, main=custommain)
  # pie3D(slices,labels=mylabels,explode=0.1, main="Market Shares using First Choice rule")
}


is.defined <- function(thisobject) {
  sym <- deparse(substitute(thisobject))
  env <- parent.frame()
  exists(sym, env)
}

detach.packages <- function(){
  #Detaches all additional packages from the environment
  lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
}
