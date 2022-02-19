library(arules)
library(plyr)
library(dplyr)
library(ggplot2)


install.packages("dplyr")
install.packages("ggplot2")

df_user= read.csv("Clean_UserData.csv")
summary(df_user)

print (df_user)

# # Graph- 1
# df_user_Analysis <- df_user%>%
#   dplyr::group_by(milestone_name)%>%
#   dplyr::tally()%>%
#   dplyr::mutate(percent=n/sum(n))
# 
# pl <- ggplot(data = top_n(df_user_Analysis, 5, n),aes(x= reorder(milestone_name, -n), y = n))
# pl <- pl + geom_bar(stat="identity")
# pl <- pl + geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%", " (",n,")")), position=position_stack(vjust=0.5), colour="white", size = 3)
# pl <- pl + labs(x="Milestones", y="Count", title = "Milestone visiting details")
# pl
# 
# # Graph- 2
# pl_bottom <- ggplot(data = top_n(df_user_Analysis, -5, n),aes(x= reorder(milestone_name, n), y = n))
# pl_bottom <- pl_bottom + geom_bar(stat="identity")
# pl_bottom <- pl_bottom + geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%", " (",n,")")), position=position_stack(vjust=0.5), colour="white", size = 3)
# pl_bottom <- pl_bottom + labs(x="Milestones", y="Count", title = "Milestone visiting details")
# pl_bottom


# Transpose

df_user_New <- df_user[c("user_id","milestone_name")]
print(df_user_New)

df_user_transpose = ddply(df_user_New,c("user_id"),function(dfl)paste(dfl$milestone_name, collapse=","))
head(df_user_transpose)
#print(df_user_transpose)
df_user_transpose$user_id = NULL
#head(df_user_transpose)
write.table(df_user_transpose,"./Milestones2_I.csv", quote=FALSE, row.names = FALSE, col.names = FALSE)
tr = read.transactions("./Milestones2_I.csv",format="basket",sep=",")
summary(tr)

itemFrequencyPlot(tr, topN=10)

#Try 1
rules = apriori(tr,parameter = list(supp=0.3,conf=0.8))
inspect(sort(rules,by='lift'))[1:10]
itemsets=unique(generatingItemsets(rules))
itemsets
inspect(itemsets)

