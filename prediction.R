
#install.packages("parcor") # also ggplot2, reshape2
set.seed(0)
library(parcor) # for ridge.cv
library(ggplot2)
library(reshape2)
# Read, form some interactions, format, etc
data = read.csv("DemPrimaryData.csv", stringsAsFactors = T)
data$Date = as.numeric(as.Date(data$Date, format = "%m/%d/%Y"))
data$Bernie = data$Bernie/100
data$FBshare = data$InternetAccess*data$FBshare
data$Google = data$InternetAccess*data$Google
data$Independents = as.numeric(data$Independents) - 1
data$Type = as.numeric(data$Type) - 1
npcols = colnames(data) %in% c("Geography", "Bernie", "Won", "Delegates", "FBnet", "Employed")
xx = as.matrix(data[,!npcols])
xx[, 8:ncol(xx)] = xx[, 8:ncol(xx)]/100 # Percents
x = scale(xx, center=T, scale=T)/sqrt(nrow(xx)-1)
x[!rows,1] = 0
y = data$Bernie

# Subset of data for states that already voted
rows = !is.na(y) #& (data$Geography != "Vermont") #outlier?
my = mean(y[rows])
X = x[rows,]
Y = y[rows]

# Build predictive model by ridge regression with
# cross-validation. Average predictions over 100
# re-randomizations of cross-validation folds.
nsim = 100
predicted = matrix(0, nrow(x), nsim)
betas = matrix(0, nsim, ncol(x))
intercepts = numeric(nsim)
for (i in 1:nsim) {
  L = length(Y)
  cvfit = ridge.cv(X, Y, k = 4, lambda=(1:1000)/50, scale=F, plot.it = F)
  beta = cvfit$coefficients
  intercept = cvfit$intercept
  predicted[, i] = pmin(pmax(x %*% beta + intercept, 0), 1)
  betas[i, ] = beta
  intercepts[i] = intercept
}

# Form averaged predictions
colnames(betas) = colnames(x)
beta = apply(betas, 2, median)
data$mapBernie = pmin(pmax(x %*% beta + mean(intercepts), 0), 100)
data$pBernie = rowMeans(predicted)
df = cbind(data[,c("Geography", "Bernie", "pBernie", "mapBernie")],
           t(apply(predicted, 1, quantile, probs = c(.20, .80))))

# Plot predictor importance
obetas = betas[,order(abs(apply(betas, 2, median)))]
dfb = melt(data.frame(obetas))
shading <- data.frame(min = seq(from = 0.5, to = ncol(x), by = 1),
                      max = seq(from = 1.5, to = ncol(x) + 0.5, by = 1),
                      col = rep(c(0,1), 40)[1:ncol(x)])
ggplot() + geom_boxplot(dfb, mapping = aes(x = factor(variable), y = value)) +
  geom_rect(data = shading,
            aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf,
                fill = factor(col), alpha = 0.1)) +
  scale_fill_brewer() +
  geom_boxplot(dfb, mapping = aes(x = factor(variable), y = value)) +
  coord_flip() + 
  theme_bw() + 
  geom_hline(yintercept = 0) +
  ylab("Weight") + xlab("Variable") + ggtitle("Variable importance") +
  guides(fill = FALSE, alpha = FALSE)

# Interesting variable?
v = "IndustryFinance"
data[rows,c("Geography", v)][rev(order(data[rows,v])),]

# Plot predicted vs actual
with(df, plot(y[rows], pBernie[rows], xlab = "Actual", ylab = "Predicted",
              main = "States that have already voted"))
abline(a=0, b=1)

# Assess fit: predictions vs actual
df[,2:4] = 100*round(df[,2:4], 3)
DF = df[rows, 1:3]
DF$Error = DF$pBernie - DF$Bernie
DF[order(data$Date[rows]),]

# Predictions for future states
df[!rows, c(1,3)][order(data[!rows,"Date"]),]

# Total pledged delegates if predictions are accurate
# Note: some are omitted, like Democrats Abroad(9), American Samoa(2), NMI(2)
FutureDelegates = df$pBernie/100 * data$Delegates
FutureDelegates = FutureDelegates[!rows]
sum(data$Won, na.rm=T) + sum(FutureDelegates)

# Total delegates if outperforming predictions by 3%
FutureDelegates = (df$pBernie/100 + .14) * data$Delegates
FutureDelegates = FutureDelegates[!rows]
sum(data$Won, na.rm=T) + sum(FutureDelegates)
