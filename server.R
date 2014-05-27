## server.r
# Control the server-side of the app.
# This file contains the instructions that computer will need to build the things that appear in the app 

## TO DO:
# Warning in `$.data.frame`(leveneTest(form, data = datt), Pr) :
  # Name partially matched in data frame
# Add Brown–Forsythe test




library(car)          # for hccm adjustment
library(nortest)      # for normality tests
library(multcomp)     # for Tukey & Dunnet
library(multcompView) # for multcompLetters
library(sandwich)     # for vcovHC
library(doBy)         # used in ART
library(lmPerm)       # for permutational ANOVA
library(agricolae)    # for LSD & scheffe

source("helpers_source.r")    # useful functions


shinyServer(function(input, output) {


  parseUserData <- reactive({
    loadUserData <- function() {
      sourcePath <- input$dataFile$datapath
      read.table(file = sourcePath, header=TRUE, sep="\t")
      }
    
    # try to load and parse data given by the user.
    # If an error is thrown - return a text description of what went wrong. 
    # The value of this function will therefore be either: (a) a data.frame, or
    # (b) a character vector with error information (we'll test for this below.)
    tryCatch( loadUserData(), error = function(e) { conditionMessage(e) } )
  })

# Get the data
userData <- reactive({ parseUserData() })

## Do subset checkbox
# output$do.subs <- renderUI({
#   if(is.null(input$dataFile)) { return() }
#   checkboxInput(inputId="subset", label="Анализировать фрагмент данных", value=FALSE)
#   })

# Choose one dependent variable (y)
output$y.chois <- renderUI({
  ## If missing input, return to avoid error later in function
  if(is.null(input$dataFile)) { return() }
  responseColumns <- colnames(userData())[sapply(userData(), is.numeric)]
  selectInput("responseColumn", "Зависимая переменная", responseColumns)
  })

# Choose predictors
output$fact <- renderUI({
    if(is.null(input$dataFile)) { return() }
    groupColumns <- colnames(userData())[sapply(userData(), is.factor)]
    selectInput("groupColumns", "Группирующие переменные", groupColumns, selectize = FALSE, multiple = TRUE)
  })

## Choose subset variable
# output$subs <- renderUI({
#     if(is.null(input$dataFile)) { return() }
#     if(input$subset == FALSE)  { return() }
#     if(input$subset == TRUE)  {
#       subsColumns <- colnames(userData())
#       selectInput("subsColumns", "Фильтрующая переменная", subsColumns, selectize = FALSE, multiple = TRUE)
#     }
#   })

## Choose subset options (levels of subs)
# output$subbs <- renderUI({
#     if(is.null(input$dataFile))    { return() }
#     if(is.null(input$subsColumns)) { return() }
#     datt <- userData()
#     possible.levs <- as.factor(unique(interaction(datt[, input$subsColumns])))
#     selectInput("subs.lev", "Фильтр", possible.levs, selectize = FALSE, multiple = TRUE)
#   })


## Update the data
# new.userData <- reactive({
#   if(input$subset == FALSE) { return( userData() ) }
#   if(input$subset == TRUE)  {
#     datt <- userData()
#     possible.levs <- as.factor(interaction(datt[, input$subsColumns]))
#     chosen <- which(possible.levs %in% input(subs.lev))
#     return( datt[chosen, ] )
#     }
#   })


# Choose dependent variable transformation if necessary
output$transformation <- renderUI({
  if(is.null(input$dataFile)) { return() }
  radioButtons("transf", "Трансформация зависимой переменной:",
              list("Без трансформации" = "no",
                   "Логарифм (натуральный + 1)" = "lg",
                   "Квадратный корень" = "sqr",
                   "Кубический корень" = "cbr",
                   "Преобразование Йео–Джонсона" = "YJ",
                   "Арксинус для % или частот" = "Arc",     # y'=2arcsinSQRT(p)  # Угловое фи-преобразование
                   "Обратный гиперболический синус" = "IHS",
                   "Обратная величина" = "inv"))  
  })


  #########################################
  ######################################### ## First tab with data
  #########################################

  output$dt <- renderDataTable(userData(), options = list(iDisplayLength = 8))
  output$nrow <- renderText({ 
      if(is.null(input$dataFile)) { return() }
      paste("Наблюдений - ", nrow(userData()), sep="") })
  output$ncol <- renderText({ 
      if(is.null(input$dataFile)) { return() }
      paste("Переменных - ", ncol(userData()), sep="") })
  output$nas <- renderText({ 
      if(is.null(input$dataFile)) { return() }
      paste("Пропущенных значений - ", sum(is.na(userData())), sep="") })

## Transformations
y.tr <- reactive({
	datt <- userData()
	y <- datt[,input$responseColumn]
	
	if(input$transf == "no"){
		y.tr <- y }

	if(input$transf == "lg"){
		y.tr <- log(y+1) }
	
	if(input$transf == "sqr"){
		y.tr <- sqrt(y) }
    
	if(input$transf == "sqr"){
		y.tr <- sqrt(y) }
    
	if(input$transf == "cbr"){
		y.tr <- y^(1/3) }
	
  if(input$transf == "YJ"){
    lambda <- coef(powerTransform(y, family="yjPower"), round=TRUE)  
    y.tr <- yjPower(y, lambda, jacobian.adjusted = FALSE) }

  if(input$transf == "Arc"){
    if( max(y, na.rm=T) <= 1 & min(y, na.rm=T) >= 0){		# probably y is [0;1]
		y.tr <- 2*asin(sqrt(y))
	}
	
	if( max(y, na.rm=T) <= 100 & min(y, na.rm=T) >= 0){	 # probably y is [0;100]  %
		y.tr <- 2*asin(sqrt(0.01 * y))
	}
	
	if( any(y > 100) | any(y < 0)){
		y.tr <- y
	}
   }


  if(input$transf == "IHS"){
   y.tr <- asinh(y)
 }

	if(input$transf == "inv"){
		y.tr <- 1/y }	
    
	return(y.tr)
	})
	


output$y.warning <- renderText({ 
	if(is.null(input$dataFile)) { return() }
	if(input$transf == "no") 	{ return() }

if( identical(x=userData()[,input$responseColumn], y=y.tr()) ){
return("Преобразование зависимой переменной не выполнено. Возможно, выбрана недопустимая операция.")
}
})


#########################################  
######################################### Second tab - diagnostics
#########################################

# create Q-Q plot for observed data
output$qq <- renderPlot({						    # observed data
  if(is.null(input$dataFile)) { return() }
  qqPlot(y.tr(), ylab = input$responseColumn, xlab = "Квантили нормального распределения", main = input$responseColumn)
})

# Create expected Q-Q plot if the data were normal
output$qq.rand <- renderPlot({					# random data
  if(is.null(input$dataFile)) { return() }
  datt <- userData()
  y.rand <- rnorm(nrow(datt), mean=mean(datt[,input$responseColumn]), sd=sd(datt[,input$responseColumn]))
  qqPlot(y.rand, ylab = "Нормально распределенная величина", xlab = "Квантили нормального распределения", main="Образец для нормального распределения")
})


# Add rug plot for histogram
output$rug <- renderUI({ 
  return(checkboxInput(inputId = "show.rug", label = "Показать значения вдоль оси X", value = FALSE))
})

# Add density curve
output$dens <- renderUI({ 
  return(checkboxInput(inputId = "show.dens", label = "Показать на гистограмме плотность распределения", value = FALSE))
})

# Density smooth
output$dens.adj <- renderUI({ 
  if(input$show.dens == FALSE){ return() }
  if(input$show.dens == TRUE){
  sliderInput(inputId = "dens.adjj",
        label = "Регулировка сглаживания:",
        min = 0.2, max = 2, value = 1, step = 0.2)
  }
})

# Create histogram
output$histt <- renderPlot({             # histogram
  if(input$show.dens == FALSE)  {
    hist(y.tr(), xlab="Зависимая переменная", ylab="Частота", main="", las=1)
  }
  
  if(input$show.dens == TRUE)  {
    hist(y.tr(), xlab="Зависимая переменная", ylab="Плотность вероятности", probability = TRUE, main="", las=1)
    dens <- density(y.tr(), adjust = input$dens.adjj)
    lines(dens, col = "blue")
  }

  if(input$show.rug)   { rug(y.tr()) }
})

# Color groups?
output$gr.col <- renderUI({
  return(
    checkboxInput(inputId = "gr.col",
      label = "Отсортировать и раскрасить значения в соответствии с выбранными группирующими переменными",
      value = FALSE)
    )
})


output$y.vs.tr <- renderPlot({			# transformed vs untransformed
   if(is.null(input$dataFile)) { return() }
    
   if(input$transf == "no")    { 			# just raw scatter plot
 		  if(input$gr.col == FALSE) {
       return( plot(y.tr(), ylab="Зависимая переменная", xlab="Порядковый индекс", las=1) )
      }

    if(input$gr.col == TRUE & is.null(input$groupColumns))    {
      return( plot(y.tr(), ylab="Зависимая переменная", xlab="Порядковый индекс", las=1) )
      }

    if(input$gr.col == TRUE & is.null(input$groupColumns)==FALSE) {
          xx <- interaction(userData()[, input$groupColumns])
          y.sort <- y.tr()
          y.sort <- y.sort[order(xx)]
          xx.sort <- xx[order(xx)]        
      return(plot(y.sort, ylab="Зависимая переменная", xlab="Порядок в группе значений", las=1, col=fac2col(xx.sort)))
      }
		}
    
	  if(input$transf != "no")    { 
      if(input$gr.col == FALSE) {
	      return( plot(y.tr(), userData()[,input$responseColumn],
	          ylab="Трансформированная величина", xlab="Оригинальные значения", las=1))
      }
      
   	  if(input$gr.col == TRUE) {
          xx <- interaction(userData()[, input$groupColumns])
          y.sort <- y.tr()
          y.sort <- y.sort[order(xx)]
          xx.sort <- xx[order(xx)]
          # colr <- length(levels(xx[order(xx)]))

        return( plot(y.sort, ylab="Зависимая переменная", xlab="Порядок в группе значений", las=1, col=fac2col(xx.sort))            )
	   }
   }
  })

  
# Normality tests
output$lillie <- renderText({   
	norr.l <- try(lillie.test(y.tr())$p.value)			# [nortest] Lilliefors (Kolmogorov-Smirnov) test for normality
		if(class(norr.l)[1] == 'try-error'){ norr.l <- NA }
	
	if(norr.l < 0.05) {
		lillie <- paste("Распределение данных не соответствует нормальному. Тест Колмогорова-Смирнова с поправкой Лиллиефорса: p = ", round(norr.l, 3), sep="") }
	if(norr.l >= 0.05) {
		lillie <- paste("Распределение данных соответствует нормальному. Тест Колмогорова-Смирнова с поправкой Лиллиефорса: p = ", round(norr.l, 3), sep="") }
	if(is.na(norr.l)) {
		lillie <- paste("Ошибка в рассчете теста Колмогорова-Смирнова с поправкой Лиллиефорса: p = ", norr.l, sep="") }
	return(lillie)	
	})
	
  output$shapiro <- renderText({   
    norr.sh <- try(shapiro.test(y.tr())$p.value)		# Shapiro-Wilk Normality Test
		if(class(norr.sh)[1] == 'try-error'){ norr.sh <- NA }
	
	if(norr.sh < 0.05) {
		shapiro <- paste("Распределение данных не соответствует нормальному. Тест Шапиро-Уилка: p = ",
			round(norr.sh, 3), sep="") }
	if(norr.sh >= 0.05) {
		shapiro <- paste("Распределение данных соответствует нормальному. Тест Шапиро-Уилка: p = ",
			round(norr.sh, 3), sep="") }
	if(is.na(norr.sh)) {
		shapiro <- paste("Ошибка в рассчете теста Шапиро-Уилка: p = ", norr.sh, sep="") }
	return(shapiro)	
	})	
	

	## Combined predictor values
output$choosed.fact <- renderText({
	if( is.null(input$groupColumns) == FALSE ){ 
	return(
		paste("Выбраны следующие группирующие переменные:",
		paste(input$groupColumns, collapse = ", ")
		)) }
  })
    
  formulaText <- reactive({
    paste("y ~ ", paste(input$groupColumns, collapse="*"))
  })
  
  ## Test for heteroscedasticity
output$homogeneity <- renderText({
  	if( is.null(input$groupColumns) == FALSE ){
  		y <- y.tr()
  		x <- userData()[, input$groupColumns]
  		datt <- data.frame(y = y, x)
  		
      if(length(input$groupColumns) == 1){    # only one factor choosed
        colnames(datt)[2] <- input$groupColumns
      }

      form <- as.formula( formulaText() )
		
	homog <- try( leveneTest(form, data=datt, center=mean)$Pr[1] )	# Levene's test
# homog <- try( leveneTest(form, data=datt, center=median)$Pr[1] )  # Brown–Forsythe test

	if(class(homog)[1] == 'try-error'){ homog <- NA }
	 	if(homog >= 0.05) {
  		homogeneity <- paste("Дисперсии групп однородны. Тест Ливена: p = ",
  			round(homog, 4), sep="") }
  	if(homog < 0.05) {
  		homogeneity <- paste("Дисперсии групп НЕоднородны. Тест Ливена: p = ",
  			round(homog, 4), sep="") }
  	if(is.na(homog)) {
  		homogeneity <- paste("Ошибка в рассчете теста Ливена: p = ", homog, sep="") }
  	return(homogeneity)
	}
  })


#########################################  
######################################### Tab four - ANOVA
#########################################

# Which analysis to perform
output$anov.gui <- renderUI({
    if(is.null(input$dataFile)) { return() }
    if(is.null(input$groupColumns)) { return() }
  
    selectInput("anov.type", "Анализ:",
      choices = list("Не проводить анализ" = "no",
      				       "Дисперсионный анализ" = "ANOVA",
                     "Тест Краскела-Уоллиса" = "KRUSK",
                     "ART-тест на взаимодействие" = "ART",
                     "Тест Шейрера-Рея-Хара" = "SHEIR",
                     "Пермутационный ANOVA" = "PermANOVA"
                    ),
      selected = "no",
      multiple = FALSE)
  })

# Accept the adjustment for heteroscedasticity?
output$hccm.check <- renderUI({ 
    if( is.null(input$anov.type) ){ return() }
    if(input$anov.type == "ANOVA"){
      return(
      checkboxInput(inputId = "white.adj",
        label = "Применить поправку на неоднородность дисперсий",
        value = TRUE)
      )
    }
  })

# Perform multiple comparisons?
output$multipl.compar <- renderUI({ 
    if( is.null(input$anov.type) ){ return() }
    if(input$anov.type == "ANOVA"){
      return(
      checkboxInput(inputId = "mult.comp",
        label = "Множественные сравнения",
        value = FALSE)
      )
    }
  })

# Which type of multiple comparisons to perform?
output$mult.type <- renderUI({ 
    if( is.null(input$anov.type) ){ return() }
    if( is.null(input$mult.comp) ){ return() }
    if(input$mult.comp == TRUE){
    return(
      selectInput("mult.tt", "Тип множественных сравнений:",
        choices = list("Критерий Тьюки" = "TUKEY",
        				       "Критерий Даннетта" = "DUNN",
                       "Критерий Фишера" = "LSD",
                       "Критерий Шеффе" = "SCHEFFE",
                       "Парный критерий Стьюдента с поправкой Бонферрони"="STBF"),
        selected = NULL,
        multiple = FALSE)
      )
    }
  })


# Return results of analysis
output$ress <- renderPrint({ 
  	y <- y.tr()

  if(is.null(input$groupColumns)) { return(summary(y)) }
	if(input$anov.type == "no"){ return( cat("Выберите какой анализ проводить.") ) }
  	
  	x <- userData()[, input$groupColumns]
  	datt <- data.frame(y = y, x)
  		
    if(length(input$groupColumns) == 1){    # only one factor choosed
       	  colnames(datt)[2] <- input$groupColumns
     	}

 	## Kruskal-Wallis rank sum test 
 	if(input$anov.type == "KRUSK"){
 		z <- interaction(x)			# group factors in 1 variable
 		KW <-  try(	kruskal.test(y ~ z) )

 		if(class(KW)[1] == 'try-error'){
		  # KW$statistic <- NA; KW$parameter <- NA; KW$p.value <- NA
		  return( cat("Ошибка  в рассчетах.") )
		}

		if(class(KW)[1] != 'try-error'){ return(KW) }
		# or write a parser similar to  stats:::print.htest
 	}


	if(input$anov.type == "ANOVA"){

		mod <- lm(formulaText(), data = datt)

		if(is.null(input$white.adj)) { return() }

		if(input$white.adj == TRUE){
  			AN <- try( Anova(mod, type=2, white.adjust=TRUE) )
  			if(class(AN)[1] == 'try-error') { return( cat("Ошибка  в рассчетах.") ) }
			if(class(AN)[1] != 'try-error') { return(AN) }
		}

		if(input$white.adj == FALSE){
  			return( Anova(mod, type=2, white.adjust=FALSE) )
		}
	}


	if(input$anov.type == "SHEIR"){
		
		datt$rank <- rank(datt$y,ties.method="average")
		rank.form <- paste("rank ~ ", paste(input$groupColumns, collapse="*"))
		mod <- lm( as.formula(rank.form), data = datt)
		an <- anova(mod)		# or Anova(type=3) with or without intercept 	???

		# SStotal = sum of the SS for the factors, interaction and residual
		SStotal = sum(an[, 2])

		# MStotal = SStotal divided by the degrees of freedom (sum by factors, interaction and residual)
		MStotal = SStotal/sum(an[, 1])

		# Calculate SS/MStotal for each factor and interactions
		SRH.stat <- an[, 2]/MStotal
		SRH.stat <- data.frame(
			Factor = rownames(an)[-nrow(an)],	# add factor names
			SRH = SRH.stat[-length(SRH.stat)])	# drop Residuals
		SRH.stat$Df <- an[-nrow(an), 1]			# add degrees of freedom
				
		# p-value = 1-pchisq(the SS/MStotal, the degrees of freedom)
		pp <- vector()
		for(i in 1:nrow(SRH.stat)){
			pp[i] <- 1-pchisq(SRH.stat$SRH[i], SRH.stat$Df[i])
		}

		p.stars <- symnum(pp, corr = FALSE, na = FALSE,
               cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
               symbols = c("***", "**", "*", ".", " "))
		res <- data.frame(SRH.stat, p = pp, Significant = as.vector(p.stars))
		return(
			# cat("Тест Шейрера-Рея-Хара:",
				res
				# , attr(p.stars, "legend"))
				)
	}

  # Adjusted rank transform test (to analyze interaction for heteroskedastic non-normal data)
  if(input$anov.type == "ART"){
    if(length(input$groupColumns) != 2){
      return( cat("Непараметрический анализ значимости взаимодействия факторов возможен только для двух-факторного дизайна.")) }
    if(length(input$groupColumns) == 2){
      res <- test.interaction(datt$y,                     #  [helpers_source.r]
                userData()[, input$groupColumns[1]],  # factor 1
                userData()[, input$groupColumns[2]])  # factor 2
      return( res )
    }
  }


  if(input$anov.type == "PermANOVA"){
    form <- as.formula( formulaText() )
    mod.p <- try( aovp(form, data = datt) )  # library(lmPerm)
    # return( formulaText() )
    return( summary(mod.p) )
  }




 }) # end of output$ress  (main results - ANOVA and alike)
	

	 # independent 2-group Mann-Whitney U Test 
	 # wilcox.test(y~A) 
	 # where y is numeric and A is A binary factor



# Calculate multiple comparisons
MULT <- reactive({
  y <- y.tr()
  xx <- interaction(userData()[, input$groupColumns])

  if(input$mult.tt == "TUKEY"){
      mod <- lm(y ~ xx)
      if(input$white.adj == TRUE){
        mod_glht <- glht(mod, mcp(xx = "Tukey"), vcov = vcovHC) }
      if(input$white.adj == FALSE){
        mod_glht <- glht(mod, mcp(xx = "Tukey")) }

      glht.sum <- summary(mod_glht)
      return(glht.sum)

    # confint(mod_glht)
    # plot(confint(mod_glht))
  }

  if(input$mult.tt == "DUNN"){
      mod <- lm(y ~ xx)
      if(input$white.adj == TRUE){
        mod_glht <- glht(mod, mcp(xx = "Dunnett"), vcov = vcovHC) }
      if(input$white.adj == FALSE){
        mod_glht <- glht(mod, mcp(xx = "Dunnett")) }
      
      glht.sum <- summary(mod_glht)
      return(glht.sum)
  }
  
  if(input$mult.tt == "STBF"){
    res <- pairwise.t.test(y, xx, pool.sd = FALSE, p.adj = "bonf")
    # p.adjust(p, "bonferroni")
    return(res)
  }
  

  # kruskalmc(y ~ treatments, data = experiment)    # [pgirmess]  Multiple comparison test after Kruskal-Wallis


  if(input$mult.tt == "LSD") {
    mod <- lm(y ~ xx)
    res <- LSD.test(mod, "xx", p.adj="bonf")      # [agricolae]
     # p-adj ="none" is t-student. ?????????
    return(res)
  }
  # + add correct for multiple comparisons!
  # Метод группирования выборок с наименее значимой разницей = Least Significant Difference method. 
  # (равенство  средних значений нескольких выборок и выделять группы выборок с одинаковыми средними значениями)


  if(input$mult.tt == "SCHEFFE") {
    mod <- lm(y ~ xx)
    res <- scheffe.test(mod, "xx")      # [agricolae]
    return(res)
  }

})    # END OF MULT



# Return the results of multiple comparisons
output$multcomp <- renderPrint({ 
  if(is.null(input$mult.tt)){ return(
    cat("При необходимости отметьте проведение множественных сравнений.")
    ) }
  
  if(input$mult.tt == "TUKEY"){ return( extract.glht( MULT() )) }   # helpers_source.r
  if(input$mult.tt == "DUNN") { return( extract.glht( MULT() )) }


  if(input$mult.tt == "STBF"){ return( MULT() ) }
  if(input$mult.tt == "LSD") { return( extract.agricol( MULT() )) }
  if(input$mult.tt == "SCHEFFE") { return( extract.agricol( MULT() )) }
})



# Build the boxplot
output$boxo <- renderUI({ 
  
  # if(is.null(input$mult.comp) == TRUE ){
  return(
	checkboxGroupInput("boxplot.opts", "Визуализация:",
		choices = list(
        			"Исходные данные" = "plotDot",
        			"Среднее для групп" = "groupMean",
        			"Общее среднее" = "plotMean",
              "Общая медиана" = "plotMedian",
              "Количество наблюдений" = "counts"),
		selected = NULL)
	)
  # }

  # if(is.null(input$mult.comp) == FALSE & !(input$mult.tt %in% c("TUKEY", "DUNN"))){
  # return(
  # checkboxGroupInput("boxplot.opts", "Визуализация:",
  #   choices = list(
  #             "Точки" = "plotDot",
  #             "Среднее для групп" = "groupMean",
  #             "Общее среднее" = "plotMean",
  #             "Общая медиана" = "plotMedian",
  #             "Количество наблюдений" = "counts"),
  #   selected = NULL)
  # )
  # }


  # if(is.null(input$mult.comp) == FALSE){
  # if(input$mult.tt == "TUKEY" | input$mult.tt == "DUNN"){
  # return(
  # checkboxGroupInput("boxplot.opts", "Визуализация:",
  #   choices = list(
  #             "Точки" = "plotDot",
  #             "Среднее для групп" = "groupMean",
  #             "Общее среднее" = "plotMean",
  #             "Общая медиана" = "plotMedian",
  #             "Количество наблюдений" = "counts",
  #             "Показать буквами сходство групп" = "abc"),
  #   selected = NULL)
  # )
  # }}
})


## Add letters on the boxplot
# output$letter.diff <- renderUI({ 
#     if( is.null(input$anov.type) ){ return() }
#     if( is.null(input$mult.comp) ){ return() }
#     if(input$anov.type == "ANOVA" & input$mult.comp == TRUE){
#       return(
#       checkboxInput(inputId = "diffs",
#         label = "Показать буквами на графике сходство групп",
#         value = FALSE)
#       )
#     }
#   })


output$boxp <- renderPlot({
    if(is.null(input$dataFile)) { return() }
    
    if(is.null(input$groupColumns)) { 
  		bp <- boxplot(y.tr(), main="", ylab=input$responseColumn, xlab="")
  		
  		if("plotMean" %in% input$boxplot.opts) {
  			abline(h=mean(y.tr(), na.rm=T), lty=1, lwd=1.5, col='black')	}
  		
  		if("plotMedian" %in% input$boxplot.opts){
  			abline(h=median(y.tr(), na.rm=T), lty=2, lwd=1.5, col='darkgrey') }

  		if("groupMean" %in% input$boxplot.opts){
  			points(1, mean(y.tr(), na.rm=T), pch=8, col=2, cex=1.1) }

  		if("counts" %in% input$boxplot.opts){
  			mtext(paste("n = ", bp$n, sep=""),
  				at = seq_along(bp$n), line=0, side=3) }
      if("plotDot" %in% input$boxplot.opts){
        points(jitter(rep(1, bp$n)), y.tr(), pch=16, col='lightgrey', cex=.5) }
     }

    if( is.null(input$groupColumns) == FALSE ){
  		y <- y.tr()
  		x <- userData()[, input$groupColumns]
  		datt <- data.frame(y = y, x)
  		
      if(length(input$groupColumns) == 1){    # only one factor choosed
       	colnames(datt)[2] <- input$groupColumns
     	}

    form <- as.formula( formulaText() )
		bp <- boxplot(form, data=datt,
			main="", ylab=input$responseColumn, xlab="", las=2)

		if("plotMean" %in% input$boxplot.opts) {
			abline(h=mean(datt$y, na.rm=T), lty=1, lwd=1.5, col='black')
		}
		
		if("plotMedian" %in% input$boxplot.opts){
			abline(h=median(datt$y, na.rm=T), lty=2, lwd=1.5, col='darkgrey')
		}

		if("groupMean" %in% input$boxplot.opts){
			points(1:length(bp$names), 
				tapply(datt$y, interaction(datt[,-1]), function(x) mean(x,na.rm=T)),
				pch=8, col=2, cex=1.1)
		}

		if("counts" %in% input$boxplot.opts){
			mtext(bp$n, at = seq_along(bp$n), line=0, side=3)
		}

    if("plotDot" %in% input$boxplot.opts){
     points(jitter(
      rep(1:length(bp$names),                 # jitter withing each group
          bp$n)),                             # n times = observations
      datt$y[order(interaction(datt[,-1]))],  # sort y according to groups
      pch=16, col='grey', cex=.5)
    }

    # if(exists(diffs, input)){
    # if(input$diffs == TRUE){
    #   if(input$mult.tt == "TUKEY" | input$mult.tt == "DUNN"){
    ## if("abc" %in% input$boxplot.opts){
    #     lett <- extract.glht( MULT() )$lett          # extract letters
    #     abc <- lett[charmatch(bp$names, lett[,1]), 2] # order them as on the plot
    #     mtext(abc, at = seq_along(bp$n), line=-1, side=1, col = "red")
    #   }

      
	}
})    # end of boxp







  # runRegression <- reactive({
    # lm(,data=dat)
  # })

  # output$regTab <- renderTable({
    # if(!is.null(input$independent)){
      # summary(runRegression())$coefficients
    # } else {
      # print(data.frame(Warning="Please select Model Parameters."))
    # }
  # })
  

  
  ## Dump results
	# actionButton



})

