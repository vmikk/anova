
require(car)
# require(heplots)		# for eta-squared


# Adjusted rank transform test (to analyze interaction for heteroskedastic non-normal data)
# Vovk Mik. Apr. 23-24, 2012
	# Sawilowsky S.S. Nonparametric-Tests of Interaction in Experimental-Design // Rev Educ Res. 1990. V. 60. # 1. P. 91-126.
	# Leys C., Schumann S. A nonparametric method to analyze interactions: The adjusted rank transform test // Journal of Experimental Social Psychology. 2010. V. 46. # 4. P. 684-688.

test.interaction <- function(x, a, b){

	datt <- data.frame(x=x, a=a, b=b)

	mm <- summaryBy(x ~ a + b, datt, keep.names=T)
	m.a <- summaryBy(x ~ a, datt, keep.names=T)
	m.b <- summaryBy(x ~ b, datt, keep.names=T)

	# remove the main effects
	for(i in 1:length(interaction(mm$a, mm$b))){

	# filter falues
	subs <- datt[interaction(datt$a, datt$b) == interaction(mm$a, mm$b)[i],]

	# extract corresponding marginal means
		marg.a <- m.a[m.a$a == subs$a[1],]$x	# marginal mean 1
		marg.b <- m.b[m.b$b == subs$b[1],]$x	# marginal mean 2

	# detrend original values
	datt[interaction(datt$a, datt$b) == interaction(mm$a, mm$b)[i],]$x <- datt[interaction(datt$a, datt$b) == interaction(mm$a, mm$b)[i],]$x - marg.a - marg.b
		
	}
	# rm(i, subs, marg.a, marg.b)
	# mm2 <- summaryBy(x ~ a + b, datt, keep.names=T)

	datt$ranks <- rank(datt$x, ties.method="average")		# calculate ranks
	# mm3 <- summaryBy(ranks ~ a + b, datt, keep.names=T)

	mod <- lm(ranks ~ a*b, data=datt)
	an.mod <- Anova(mod)

	# eta <- etasq(mod)[3,]		# partial-eta-squared for interaction term

	res <- an.mod[3,]
	# res$Eta.sq <- eta
	return(res)
}

# test.interaction(Y.Value, factor1, factor2)[2:5]
# test.interaction(rnorm(100), rep(c("a", "b"), each = 50), b=rep(c("c", "d", "e", "f"), times = 25))




########################	# Color functions
########################	# based on [adegenet 1.4-1]
########################

## translate a factor into colors of a palette
## colors are randomized based on the provided seed
fac2col <- function(x, col.pal=funky, na.col="transparent", seed=NULL){
    ## get factors and levels
    x <- factor(x)
    lev <- levels(x)
    nlev <- length(lev)

    ## get colors corresponding to levels
    if(!is.null(seed)){
        set.seed(seed)
        newseed <- round(runif(1,1,1e9))
        on.exit(set.seed(newseed))
        col <- sample(col.pal(nlev))
    } else {
        col <- col.pal(nlev)
    }

    ## get output colors
    res <- rep(na.col, length(x))
    res[!is.na(x)] <- col[as.integer(x[!is.na(x)])]

    ## return
    return(res)
}


funky <- colorRampPalette(c("#A6CEE3","#1F78B4","#B2DF8A",
                            "#33A02C","#FB9A99","#E31A1C",
                            "#FDBF6F","#FF7F00","#CAB2D6",
                            "#6A3D9A","#FFFF99","#B15928"))



########################
########################	# Multiple comparisons
########################

# Prepare mult comp table
extract.glht <- function(x,								# x = summary(glht)
	digits = max(3, getOption("digits") - 3), ...) {

	res <- data.frame(
		Разница.Средних = x$test$coefficients,
		Стд.Ош = x$test$sigma,
		P = round(x$test$pvalues, 5)
		)

	printCoefmat(res, digits = digits, 
        has.Pvalue = TRUE, P.values = TRUE)
	
	cat("\nБуквы для проведенных сравнений:\n\n")
	pp <- x$test$pvalues                  		    # p-values for comparisons
	names(pp) <- names(x$test$coefficients)   		# add comparison names
	lett <- multcompLetters(pp)                     # assign letters [multcompView]
		ll <- data.frame(Группа = names(lett$Letters), Буква = lett$Letters)
		rownames(ll) <- NULL
	print( ll )
	cat("\nОдинаковые буквы обозначают отсутствие отличий.\n")

	ret <- list()
		ret$res <- res
		ret$lett <- lett
	invisible(ret)
}




extract.agricol <- function(x){		# x = result of LSD.test (agricolae)
	cat("\nБуквы для проведенных сравнений:\n\n")
	ll <- data.frame(Группа = x$groups$trt, Буква = x$groups$M)
	print( ll )
	cat("\nОдинаковые буквы обозначают отсутствие отличий.\n")
}