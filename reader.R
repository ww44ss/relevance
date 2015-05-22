library(readr)

# Use readr to read in the training and test data
train = read_csv("./relevance/train.csv")
test  = read_csv("./relevance/test.csv")

# Print a sample of the training data
print(head(train))

# Now it's yours to take from here!
summary(train)
train[1:4,"query"]
train[1:4, "product_title"]
train[]
hist(train$median_relevance)
hist(train$relevance_variance)

test[1,]
test[1:4,"query"]
test[1:4,"product_title"]
test[1:4,"product_description"]

