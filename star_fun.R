
###install.packages("ggplot2")
library(ggplot2)

####CAN REPLACE THE FOR LOOPS WITH FUNCTIONAL PROGRAMMING? ###


df1 <- data.frame(int_name = c("spaceships","submarines","aeroplanes","volcano"),
                   fixed_cost = c(100,200,300,400),
                   cost_pp = c(3,4,5,5),
                   nnt = c(2,4,6,8),
                   n_treated = c(50,40,60,80),
                   qalys_pp = c(3,5,4,9)
)

## INPUTS ####
star_fun <- function(dataframe) {
### MIGHT BE SOME COOL RISK ASJUSTMENT WE CAN DO. 
    fixed_cost <- df1[,"fixed_cost"]
    cost_pp <- df1[, "cost_pp"]
    nnt <- df1[, "nnt"]
    n_treated <- df1[,"n_treated"]
    qalys_pp <- df1[,"qalys_pp"]
    int_name <- df1[ , "int_name"]
    
    ### x_vec function that will outline the x-axes values for the geom_polygon
    ###NEED TO MAKE THIS WORK #####
    
    x_vec <- function(fixed_cost, cost_pp, n_treated) {
      m <- c()
      c <- fixed_cost[1] + (cost_pp[1] * n_treated[1])
      m <- c(0,c,c)
      a <- 2
    for (i in 2:length(fixed_cost)) {
      b <- a-1
      c <- fixed_cost[a] + (cost_pp[a] * n_treated[a])
      d <- fixed_cost[b] + (cost_pp[b] * n_treated[b])
      m <- c(m,d,c,c)
      a <- a+1
    }
    return(unlist(m))
    }
    x_vec(fixed_cost, cost_pp, n_treated)
    ### y_vec function that makes the data points on the y axis ###
    
    y_vec <- function(nnt,n_treated,qalys_pp) {
      m <- c()
      c <- (n_treated[1] * (nnt[1]/100)) * qalys_pp[1]
      m <- c(m,0,0,c)
      a <- 2
      for (i in 2:length(total_cost)) {
        b <- a-1
        c <- (n_treated[a] * (nnt[a]/100)) * qalys_pp[a]
        d <- (n_treated[b] * (nnt[b]/100)) * qalys_pp[b]
        m <- c(m,d,d,c)
        a <- a+1
      }
      return(unlist(m))
    }
    
    
    ###t_vec to group the vectors. ##### 
    
    t_vec <- function(int_name){
      a <- 1
      m <- c()
      for (i in 1:length(total_cost)){
      c <-  
      m <- c(m,int_name[a],int_name[a],int_name[a]) 
      a <- a+1
      }
      return(m)
    }
    t_vec(int_name)
    
    ###### the graph ######
    
    d = data.frame(
                  x= c(x_vec(fixed_cost, cost_pp, n_treated)),
                   y=c(y_vec(nnt, n_treated, qalys_pp)), 
                   t=c(t_vec(int_name))
                  )
    ggplot() +
      geom_polygon(data=d, mapping=aes(x=x, y=y, group=t, fill =t)) + 
      labs(title = "STAR Fun", x = "Total cost", y = "Population health benefit")+
      theme(plot.title = element_text(hjust = 0.5)) 
}


 







