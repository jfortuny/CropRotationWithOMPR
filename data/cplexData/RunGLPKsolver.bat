glpsol --lp model-2021-06-08.lp --output glpkResults.txt --mipgap 0.75 --tmlim 3600
glpsol --lp model-2021-06-08.lp --nomip --output glpkLPResults.txt
glpsol --lp model-2021-06-08.lp --nomip --write glpkLPBasis.txt 