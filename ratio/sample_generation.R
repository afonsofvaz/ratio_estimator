source("ratio/auxs_funcs.R")

## Sample create 
## Bank dataset 
load(file = "ratio/inputs/datasets/dataset.bank.RData")
sample.bank = index.sample(dataset.bank$response)
save(sample.bank,file = "ratio/inputs/samples/samples.bank.RData")

## Block dataset
load(file = "ratio/inputs/datasets/dataset.block.RData")
sample.block = index.sample(dataset.block$response)
save(sample.block,file = "ratio/inputs/samples/samples.block.RData")

## Cancer dataset
load(file = "ratio/inputs/datasets/dataset.cancer.RData")
sample.cancer = index.sample(dataset.cancer$response)
save(sample.cancer,file = "ratio/inputs/samples/samples.cancer.RData")

# ## Candles dataset
# load(file = "ratio/inputs/datasets/dataset.candles.RData")
# sample.candles = index.sample(dataset.candles$response)
# save(sample.candles,file = "ratio/inputs/samples/samples.candles.RData")

## Spam dataset
load(file = "ratio/inputs/datasets/dataset.spam.RData")
sample.spam = index.sample(dataset.spam$response)
save(sample.spam,file = "ratio/inputs/samples/samples.spam.RData")
