library(readr)

# Use readr to read in the training and test data
train = read_csv("../input/train.csv")
test  = read_csv("../input/test.csv")

# Print a sample of the training data
print(head(train))

# Now it's yours to take from here!
summary(train)