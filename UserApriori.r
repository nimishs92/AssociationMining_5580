library(arules)
library(plyr)
library(dplyr)
library(ggplot2)

install.packages("dplyr")
install.packages("ggplot2")

df_user= read.csv("UserData.csv")
summary(df_user)

print (df_user)


# subsetting
df_user_New <- df_user[c("user_id","milestone_name")]
print(df_user_New)

# Transpose based on the user_id using dfl function
df_user_transpose = ddply(df_user_New,c("user_id"),function(dfl)paste(dfl$milestone_name, collapse=","))

#Check the output of the transpose
head(df_user_transpose)

#Set user_id to null as it will not contribute to the apriori algorithm.
df_user_transpose$user_id = NULL

#Save the transposed data into file by skipping the columns names
write.table(df_user_transpose,"./User_Transpose.csv", quote=FALSE, row.names = FALSE, col.names = FALSE)

# For basket format, each line in the transaction dataframe represents a transaction where the items are separated by the characters specified by sep.
tr = read.transactions("./User_Transpose.csv",format="basket",sep=",")
summary(tr)

# Fetch the top 10 most repeated items
itemFrequencyPlot(tr, topN=10)

#
rules = apriori(tr,parameter = list(supp=0.3,conf=0.8))
inspect(sort(rules,by='lift'))[1:10]
itemsets=unique(generatingItemsets(rules))
itemsets
inspect(itemsets)
