# =============================================================================
#### Info #### 
# =============================================================================
# linear regression model
#
# Lei Zhang, UKE, Hamburg, DE
# lei.zhang@uke.de
#
# Adapted from McElreath, 2016

# =============================================================================
#### Construct Data #### 
# =============================================================================
# clear workspace
rm(list=ls(all=TRUE))
library(rstan)
library(ggplot2)

load('_data/height.RData')
d <- Howell1
d <- d[ d$age >= 18 , ]

# =============================================================================
#### basic GLM #### 
# =============================================================================
L <- lm( height ~ weight, d) # estimate model by minimizing least squares errors
summary(L)

myconfig <- theme_bw(base_size = 20) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank() )

# scatter plot
g1 <- ggplot(d, aes(weight,height))
g1 <- g1 + geom_jitter(width=0.25, height=0.25, size=3, colour='skyblue', alpha=0.9)
g1 <- g1 + myconfig + labs(x = 'weight', y = 'height')
print(g1)
ggsave(plot = g1, "_plots/scatter.png", width = 6, height = 4, type = "cairo-png", units = "in")

# add the regression line
g2 <- g1 + geom_abline(intercept = L$coefficients[[1]], slope = L$coefficients[[2]], 
                       color = "red", size = 2, alpha=0.8)
print(g2)
ggsave(plot = g2, "_plots/regressionline_lm.png", width = 6, height = 4, type = "cairo-png", units = "in")

# add interolate
x_range <- ggplot_build(g2)$panel$ranges[[1]]$x.range
y_range <- ggplot_build(g2)$panel$ranges[[1]]$y.range
y_bar_140 <- as.numeric(predict(L, data.frame(weight=40)))

g3 <- g2 + geom_segment(aes(x = 40, y = y_range[1], xend = 40, yend = y_bar_140), colour = "navyblue", size = 2, alpha=0.8)
g3 <- g3 + geom_segment(aes(x = x_range[1], y = y_bar_140, xend = 40, yend = y_bar_140), colour = "navyblue", size = 2, alpha=0.8)
print(g3)
ggsave(plot = g3, "_plots/regressionline_lm_prediction.png", width = 6, height = 4, type = "cairo-png", units = "in")


# =============================================================================
#### Running Stan #### 
# =============================================================================
rstan_options(auto_write = TRUE)
options(mc.cores = 2)

N <- length(d$height)
dataList <- list(N=N, height=d$height, weight=d$weight)

modelFile <- '_scripts/regression_height_model.stan'
nIter     <- 2000
nChains   <- 4 
nWarmup   <- floor(nIter/2)
nThin     <- 1

cat("Estimating", modelFile, "model... \n")
startTime = Sys.time(); print(startTime)
cat("Calling", nChains, "simulations in Stan... \n")

fit_reg <- stan(modelFile, 
                  data    = dataList, 
                  chains  = nChains,
                  iter    = nIter,
                  warmup  = nWarmup,
                  thin    = nThin,
                  init    = "random",
                  seed    = 1450154626)

cat("Finishing", modelFile, "model simulation ... \n")
endTime = Sys.time(); print(endTime)  
cat("It took",as.character.Date(endTime - startTime), "\n")
