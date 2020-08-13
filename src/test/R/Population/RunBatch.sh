#!/bin/bash

rm -r ./results
for i in $(seq "$1")
do
    mewDir=$(date +"%m-%d-%H%M")
    mkdir results/ results/logs results/logs/messages results/logs/output/ results/samples results/data
    Rscript ModelFitting.R || mkdir /history/${newDir} || mv results /history/${newDir}
done
