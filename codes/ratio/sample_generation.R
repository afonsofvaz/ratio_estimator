setwd("C:/Users/afonso/Dropbox/Artigos-Google/Paper_JMLR/simulation/")
source("simulation_codes/sample.R")

## Sample create 
## Bank dataset 
load(file = "outputs/datasets/dataset.bank.RData")
sample.bank = index.sample(dataset.bank$response)
save(sample.bank,file = "outputs/samples/sample.bank.RData")

## Block dataset
load(file = "outputs/datasets/dataset.block.RData")
sample.block = index.sample(dataset.block$response)
save(sample.block,file = "outputs/samples/dataset.block.RData")

## Cancer dataset
load(file = "outputs/datasets/dataset.cancer.RData")
sample.cancer = index.sample(dataset.cancer$response)
save(sample.cancer,file = "outputs/samples/dataset.cancer.RData")

## Candles dataset
load(file = "outputs/datasets/dataset.candles.RData")
sample.candles = index.sample(dataset.candles$response)
save(sample.candles,file = "outputs/samples/dataset.candles.RData")

## Spam dataset
load(file = "outputs/datasets/dataset.spam.RData")
sample.spam = index.sample(dataset.spam$response)
save(sample.spam,file = "outputs/samples/dataset.spam.RData")


load(file = "outputs/samples/dataset.block.RData")
load(file = "outputs/samples/sample.bank.RData")
load(file = "outputs/samples/dataset.cancer.RData")
load(file = "outputs/samples/dataset.candles.RData")
load(file = "outputs/samples/dataset.spam.RData")

dim(sample.bank[[1]]$target)
dim(sample.block[[1]]$target)
dim(sample.spam[[1]]$target)
dim(sample.candles[[1]]$target)
dim(sample.cancer[[1]]$target)
