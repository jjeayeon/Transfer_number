#'Transfer decimal number
#'
#'This function transfer decimal number to others
#'
#'@examples
#'
#'transfer_number()

transfer_number<-function(n=0,k=2){

  if(n==0) {
    result<-0
    result
  }



  else if(n==1) {
    result<-1
    result
  }



  else if(k<=10){
    result<-c()
    result_r<-c()

    while(n>k-1){
      rest<-n%%k
      n<-n%/%k
      result<-c(rest,result)
    }

    result<-c(n,result)

    for(i in 1:length(result)){
      result_r<-paste0(result_r,result[i])
    }

    result_r
  }



  else if(k<=20){
    result<-c()
    result_r<-c()
    z<-c("A","B","C","D","E","F","G","H","I","J")

    while(n>k-1){
      rest<-n%%k
      n<-n%/%k
      result<-c(rest,result)
    }

    result<-c(n,result)

    for(i in 11:k){
      result[which(result==i)]<-z[i-10]
    }

    for(i in 1:length(result)){
      result_r<-paste0(result_r,result[i])
    }

    result_r
  }
}
