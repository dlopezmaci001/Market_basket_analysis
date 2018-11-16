
install.packages("arulesViz")

library(grid)
library(arules)
library(Matrix)
library(arulesViz)

groceries <- read.transactions("http://www.sci.csueastbay.edu/~esuess/classes/Statistics_6620/Presentations/ml13/groceries.csv", sep = ",")
summary(groceries)

inspect(groceries[1:5])

# Let's see what is the frquency of the total items purched

itemFrequencyPlot(groceries,support=0.05) # Those that appear at least 5% of the time

# Let's see the top 10 purchased goods 

itemFrequencyPlot(groceries, topN=10)

# Let's run an apriori rule to see what is the purchase pattern for the products sold

###### Model training #####

# Support : the percentage of records that match the antecedents. A rule with low support
# is likely to be uninteresting from a business perspective because it may not be profitable to
# promote items that customers seldom buy. 

# Confidence: is the percentage of records matching the antecedents that also match the
# consequent. Confidence measures the consistency of the interpretation made by a rule. For
# a given rule, the higher the confidence, the more likely it is for the consequent to be present
# in transactions that contain antecedents (a kind of an estimate of the conditional probability
#  of a consequent given a set of antecedents) 


groceryrules <- apriori(groceries,parameter=list(support=0.009,confidence=0.5,minlen=2))

groceryrules # set of 25 rules

##### Model performance and evaluation ####

summary(groceryrules)

# We have 1 rule for 2 items and 24 rules for 3 items

# Let's analyse the rules

inspect(groceryrules)

# Analysis:

# RUle 1:
# Baking powder is purchesed 0.9% (support) times out of all the purchases 
# similarly, 52%(confidence) of the times someone purchased baking powder also
# purchased whole milk. The lift shows that for every person who purchases baking powder
# he/she is 2.05 times more likely to buy whole milk.

## Representation of the rules 

plot(groceryrules[1:10],method="graph",control=list(type="items"))

## Interactive

arulesViz::plotly_arules(groceryrules)

##### Model enhancement ####

# Let's now order the results by Lift to see if there are any hidden patterns.

inspect(sort(groceryrules,by="lift"))

# Rule 1 Analysis:
# In this case rule 1 states that those customers who have purchased citrus fruits and root vegetables
# are 3 times as likely to buy other vegetables than any other customer. 

##### Client specific requirements ####

# The aim of the company is to reduce the use of plastic bags, therefore it wants to identify
# which bundel of products are purchased that use the most number of plastic bags.

objectiverules <- apriori(groceries,parameter=list(support=0.0006,
                                                   confidence=0.7,minlen=2),
                          appearance = list(rhs=c("shopping bags"),default="lhs"))
objectiverules

inspect(sort(objectiverules,by="lift"))

# There are the rules. 
