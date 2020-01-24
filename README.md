# LinearPred

```
x <- setupClass(RDS,
                betas,
                time_col="PROG_TIME",
                ind_col="PROG_IND")
x <- setupClass(exprs(RDS),
                betas,
                time=RDS$PROG_TIME,
                ind=RDS$PROG_IND)
x <- genLinearPred(x)
x <- findBestCuttoff(x,
                time=x@time_col,
                ind=x@ind_col,
                PRINT=TRUE)
```
