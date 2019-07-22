#' @title Perform Two Sample T-Test and Chi-Square Test on Data
#'
#' @description The function performs two sample t-test and chi-square test and exports the output of each test in a text file.
#'
#' @param data Data frame object
#'
#' @param target_var Target variable name of data
#'
#'
#' @return NULL
#'
#' @examples stat_test(Boston)
#'
#' @export stat_test


stat_test<-function(data,target_var)
{
  if (length(unique(data[,target_var])) == 2){
    if(is.data.frame(data)){
      name = colnames(data)
      for(i in 1:ncol(data)){
        if (class(i)=="factor" | ((length(unique(data[,i])))<10 ))
        {
          sink(paste('Chi-sq_',name[i],'_&_',target_var,'.txt',sep=''))
          print(paste('CHi-Sq Test Beteen',name[i],'and',target_var))
          chisq_test = suppressWarnings(chisq.test(x = data[,i],y = data[,target_var]))
          print(chisq_test)
          sink()
        }
        else
        {

          sink(paste('T-test_',name[i],'_&_',target_var,'.txt',sep=''))
          t_test = t.test(as.formula(paste(name[i],target_var,sep="~")),data=data)
          print(t_test)
          sink()
        }
      }
    }
    else{
      print('Error! Not a data frame!')
    }
  }
  else{
    print('Error! The target variable is not binary!')
  }
}
