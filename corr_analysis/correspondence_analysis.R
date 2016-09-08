## There are many functions from different packages in R, to perform correspondence analysis:

##CA is used to graphically visualize row points and column points in a low dimensional space.
##How?

##Correspondence analysis is a descriptive/exploratory technique designed 
##to analyze simple two-way and multi-way tables containing 
#some measure of correspondence between the rows and columns.
##chisq test statistic is used to test the significance 
##of the overall deviation of the actual sample from the 
##independence model it can be written as

##chisq stat used to test the significance of the overall 
##deviaiton of the actual sample from the independence model.
##it can be expressed as possible
##chisq =sum of (Actual sample size- Theoretical size)2/Theoretical sample size.

##CA decomposes the chi-square statistic associated with the 
##two-way table into orthogonal factors 
##that maximize the separation between row and column scores 
##(i.e. the frequencies computed from the table of profiles). 
##Here, you see that there is some connection with PCA but the 
##measure of variance (or the metric) retained in CA is the chiSQuared, 
##which only depends on column profiles 
##(As it tends to give more importance to modalities 
##that have large marginal values, we can also re-weight the initial data, 
##but this is another story).


##   CA [in FactoMineR package]
## ca() [in ca package]
## dudi.coa() [in ade4 package]
## corresp() [in MASS package]



install.packages("FactoMineR")
install.packages("factoextra")
# install.packages("devtools")


library(FactoMineR)
library(factoextra)

##

data(housetasks)

head(housetasks,10)
##its a contingency table

install.packages("gplots")

# install.packages("magrittr")
library("gplots")
library("magrittr")

# 1. convert the data as a table
dt <- housetasks  %>% as.matrix() %>% as.table()

# 2. Graph
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

# install.packages("graphics")
library("graphics")
## pass it a 
mosaicplot(dt, shade = TRUE, las=2,
           main = "housetasks")

##From this mosaic plot, it can be seen that the housetasks Laundry, 
##Main_meal, Dinner and breakfeast (blue color) are mainly done by the wife in our example.

# install.packages("vcd")
library("vcd")
# plot just a subset of the table
assoc(head(dt), shade = T, las=3)

## use a chi square test
##post hoc test the difference
# install.packages("fifer")

library(fifer)

chisqtest.housetasks <- chisq.test(housetasks)
print(chisqtest.housetasks)
## definitely a dependency
##run a post-hoctest 
chisq.post.hoc(dt, test = c("fisher.test"),
               popsInRows = TRUE,
               control = c( "bonferroni"),
               digits = 4)


##correspondence analysis

CA(housetasks, ncp = 5, graph = TRUE)


##how do we interpret this
##
## joint decisions are made on FInances Tidying Insurnace and holidays
## Husbands do the driving and repairs
## Alternating is how dinner is made ()
## Wife takes care of Laundry Breakfast and Main Meal and Official business (WTF is that coup attempts, military interventions? )


res.ca <- CA(housetasks, graph = FALSE)

summary(res.ca, nb.dec = 2, ncp = 2)


##

## To interpret correspondence analysis, the first step is to evaluate whether there is a significant dependency between the rows and columns.

## There are two methods to inspect the significance:
## Using the trace
## Using the Chi-square statistic
## The trace is the the total inertia of the table (i.e, the sum of the eigenvalues). The square root of the trace is interpreted as the correlation coefficient between rows and columns.
## The correlation coefficient is calculated as follow

eig <- get_eigenvalue(res.ca)
trace <- sum(eig$eigenvalue) 
cor.coef <- sqrt(trace)
cor.coef ## very strong correlation between rows and columns

chi2 <- trace*sum(as.matrix(housetasks))
chi2

eigenvalues <- get_eigenvalue(res.ca)

head(round(eigenvalues, 2))
