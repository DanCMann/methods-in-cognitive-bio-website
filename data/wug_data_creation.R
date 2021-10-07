
set.seed(42)
rcorrnorm <- function(length = 100, r = 0.5, mu1 = 0, mu2 = 0, sig1 = 1, sig2 = 1){
        # creates a matrix of two correlated variables. User can set mean, sd, and r. 
        
        cov <- r * (sig1 * sig2)
        var1 <- sig1**2
        var2 <- sig2**2
        
        xx <- MASS::mvrnorm(
                length,
                mu = c(mu1, mu2), 
                Sigma = matrix(c(var1, cov, 
                                 cov, var2), 
                               nrow = 2), 
                empirical = T)
        return(xx)
}


mushrooms <- rcorrnorm(2500,r = 0.5, 
                       mu1 = 175, sig1 = 20, 
                       mu2 = 75, sig2 = 20)
berries <- rcorrnorm(2500,r = 0.5, 
                     mu1 = 150, sig1 = 20, 
                     mu2 = 100, sig2 = 20)
walnuts <- rcorrnorm(2500,r = 0.5, 
                     mu1 = 125, sig1 = 20, 
                     mu2 = 125, sig2 = 20)
grass <- rcorrnorm(2500,r = 0.5, 
                   mu1 = 100, sig1 = 20, 
                   mu2 = 150, sig2 = 20)

wug_forage <- data.frame(
        food_pref = rep(c('mushrooms', 'berries', 'walnuts', 'grass'), each = 2500),
        time_foraging = abs(c(mushrooms[,1], berries[,1], walnuts[,1], grass[,1])),
        food_gathered = abs(c(mushrooms[,2], berries[,2], walnuts[,2], grass[,2]))
)




index <- sample(nrow(wug_forage), 100)
sample <- wug_forage[index,]





write.csv(wug_forage, 'wug_forage.csv')
