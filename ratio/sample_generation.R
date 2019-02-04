source("/codes/ratio/aux_function.R")

## Sample create 
## Bank dataset 
load(file = "/codes/ratio/inputs/datasets/dataset.bank.RData")
sample.bank = index.sample(dataset.bank$response)
save(sample.bank,file = "/codes/ratio/inputs/samplessample.bank.RData")

## Block dataset
load(file = "/codes/ratio/inputs/datasets/dataset.block.RData")
sample.block = index.sample(dataset.block$response)
save(sample.block,file = "/codes/ratio/inputs/samplesdataset.block.RData")

## Cancer dataset
load(file = "/codes/ratio/inputs/datasets/dataset.cancer.RData")
sample.cancer = index.sample(dataset.cancer$response)
save(sample.cancer,file = "/codes/ratio/inputs/samplesdataset.cancer.RData")

## Candles dataset
load(file = "/codes/ratio/inputs/datasets/dataset.candles.RData")
sample.candles = index.sample(dataset.candles$response)
save(sample.candles,file = "/codes/ratio/inputs/samplesdataset.candles.RData")

## Spam dataset
load(file = "/codes/ratio/inputs/datasets/dataset.spam.RData")
sample.spam = index.sample(dataset.spam$response)
save(sample.spam,file = "/codes/ratio/inputs/samplesdataset.spam.RData")
