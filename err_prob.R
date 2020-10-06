order_matrix <- c(5, 50, 100, 500)
num_tests <- c(100, 1000, 10000)
Err_prob <- c(1:12,nrow=4, ncol=3);
for (i in 1:length(order_matrix))
  {
  k <- order_matrix[i]
  U = matrix(sample(0:1, k*k, TRUE), k, k)
  V = matrix(sample(0:1, k*k, TRUE), k, k)
  W = matrix(sample(0:1, k*k, TRUE), k, k)
 
   for (j in 1:length(num_tests))
  {
     for(p in 1:num_tests[j])
    t <- sum(U%*%V!=W)
   }
  Err_Prob[i ,j] = (sum(U%*%V!=W))/num_tests[j];
}
Err_prob