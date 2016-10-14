## ui.r
## The user interface controls the layout and appearance of the app.


# Define UI for application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(h3("ANOVA - Дисперсионный анализ")),


  sidebarPanel(
  # helpText("Текстовый файл с разделителями табуляции."),
  fileInput("dataFile", "Загрузить файл",
    accept = c("application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
              "text/tab-separated-values", "text/plain")),
  # uiOutput("do.subs"),
  div(class="row"),
    div(class="span5", uiOutput("y.chois")),
    div(class="span5", uiOutput("subs")), 
  div(class="row"),
    div(class="span5", uiOutput("fact")),
    div(class="span5", uiOutput("subbs")), 
  div(class="row"),
  br(),
  uiOutput("transformation"),
  br(),
  uiOutput("anov.gui"),
  uiOutput("hccm.check"),
  uiOutput("multipl.compar"),
  uiOutput("mult.type")
  # , uiOutput("downl.mult.comp"),     # button to save results of multiple comparisons
  # uiOutput("downl.plot")         # button to save plot /// TO DO
  ),


  # Tabs with results
  mainPanel(
    tabsetPanel(

      tabPanel("Исходные данные",
        h5("Проверьте правильность введенных данных", style = "color:darkred"),
        h4("Описание"),
        p("Ваши данные содержат:"),
        textOutput("nrow"), textOutput("ncol"), textOutput("nas"),
        br(),
        h4("Предпросмотр"),
        dataTableOutput("dt")),

      tabPanel("Диагностика и допущения",
        h4(p("Визуальная диагностика:")),
        fluidRow(
          column(6, plotOutput("qq")),
          column(6, plotOutput("qq.rand"))
        ),

        div(class="row"),
        p("Ваши данные представлены на левом графике. Наблюдения должны укладываться в диапазон, отмеченный красными линиями. Для сравнения - на правом графике показаны искуственные данные, соответствующие нормальному распределению."),
        br(),
        h4(div(uiOutput("y.warning"), style = "color:red")),
        fluidRow(
          column(6, plotOutput("histt")),
          column(6, plotOutput("y.vs.tr"))
        ),
        fluidRow(
          column(6, uiOutput("rug")),
          column(6, uiOutput("gr.col"))
        ),
        div(class="row"),
        uiOutput("dens"), uiOutput("dens.adj"),
        br(),
        uiOutput("yeo.lamda"),
        br(),
        h4(p("Формальный тест на нормальность распределения:")),
          textOutput("shapiro"), textOutput("lillie"),
        h4(p("Тест на однородность дисперсий:")),
        ("(Зависит от выбора группирующих переменных)"),
        p(textOutput("choosed.fact")),
        textOutput("homogeneity")
      ),
      

      tabPanel("Результаты анализа",
        verbatimTextOutput("ress"),
        br(),
        fluidRow(
          column(9, plotOutput("boxp")),
          column(2, uiOutput("boxo"))
        ),
        div(class="row"),
        # uiOutput("letter.diff"),
        verbatimTextOutput("multcomp")
      ),

      tabPanel("Сохранить результаты",
      h4("Сохранить результаты анализа:"),
      p("Здесь можно выбрать результаты, которые необходимо сохранить в табличном виде."),
      br(),
      downloadButton('downloadData.anov', 'Дисперсионный анализ'),
      br(),
      downloadButton('downloadData.multcomp', 'Множественные сравнения'),
      br(),
      downloadButton('downloadData.data', 'Данные для анализа (с учетом трансформации)')
      ),


      tabPanel("Debug",
        h5("Служебная информация, предназначенная для отладки работы приложения.", style = "color:darkred"),
        verbatimTextOutput("debug"),		     # return input and stored values
        h5("Структура данных:", style = "color:darkred"),
        verbatimTextOutput("datt.str")
      ),

      tabPanel("О программе",
        h4("Цель программы:"),
        p("Основная задача программы - проведение анализа данных с умеренно неоднородными дисперсиями. Для этого применяется поправка Уайта-Хьюбера на гетероскедастичность (алгоритм HC3, см. Long & Ervin 2000)."),
        p("В качестве альтернативных подходов реализована возможность проведения пермутационного теста или 
            использование непараметрического аналога дисперсионного анализа - расширение Шейрера-Рея-Хара критерия Краскела-Уоллиса (Scheirer et al., 1976; Sokal, Rohlf, 1995)."),
        p("Также даны основные средства для проверки допущений ДА."),
        br(),

        h4("Формат данных:"),
        p("Входной файл - текстовый файл с разделителями табуляции", em("(.txt)"), "или файл Microsoft Excel", em("(.xls, .xlsx).")),
        p("Первая строка содержит названия переменных. 
            Они должны быть написаны ", span(strong("латиницей"), style = "color:red"), " и не могут начинаться с цифры, содержать пробел или тире."),
        p("Все группирующие переменные (факторы) должны быть закодированы как текстовые значения (начинаются с буквы)."), 
        p("Зависимые переменные имеют числовой вид. 
            Разделитель дробной части - ", span(strong("точка"), style = "color:red"),
            ". Пропущенные значения разрешены и должны быть закодированы как ",
             span(strong("NA"), style = "color:red"), ". 
             Однако они будут исключены из дальнейшего анализа по мере необходимости." ),
        p("В случае импорта Excel-файла импортируется только", span(strong("первый"), style = "color:red"), " лист из книги."),
        p(a("Пример текстового файла с данными.", href = "http://goo.gl/KXYU9e")),
        p(a("Пример Excel-файла.", href = "http://goo.gl/8uARBW")),
        br(),

        h4("Возможности программы:"),
        p(em("Преобразование Йео–Джонсона (Yeo-Johnson transformation)"),
          " - аналог преобразования Бокса-Кокса, но позволяет работать с отрицательными значениями. 
          Производится для всей совокупности данных и не учитывает группирующие переменные (например, для выравниявания их дисперсий)."),
        p(em("Преобразование обратного гиперболического синуса"), " - аналог логарифмического преобразования, но оно определено для отрицательных и нулевых значений."),
        p(em("Тест Шейрера-Рея-Хара (Scheirer-Ray-Hare test)"),
          " добавлен лишь в экспериментальных целях и не рекомендуется к использованию из-за крайне малой мощности метода. 
          Его реализация базируется на работе Dytham (2011)."),
        p(em("Непараметрический тест взаимодействия факторов (ART-тест на взаимодействие)"),
          " возможен пока только для двух-факторного дизайна (Leys, Schumann, 2010; Sawilowsky, 1990)."),
        p(em("Пермутационный ANOVA"), "для оценки статистической значимости использует перестановочный тест. Количество итераций опеределяется в соответствии с алгоритмом Anscome (1953).  
          При анализе факторов, представленных более чем 4-5 уровнями могут быть проблемы."),
        p(em("Множественные сравнения критерем Даннета (Dunnett)"), " следует проводить только с контрольной группой. Контрольной будет считаться та группа, которая идет первой в таблице данных."),
        br(),

        h4("Что пока не умеет, но планирует:"),
        p("Будет добавлен экпорт полученных результатов в табличном виде."),
        p("Будет добавлена возможность фильтрации данных по переменной и анализа полученного поднабора данных."),
        p(em("Тест Колмогорова-Смирнова с поправкой Лиллиефорса"),
          " возможно, следует заменить на более мощный. 
          Поэтому большее внимание следует уделить результатам теста Шапиро-Уилкса (он рассчитывается до n = 5000)."),
        p("Добавить альтернативный тест на однородность дисперсий - ",
        	em("Критерий Брауна–Форсайта (Brown–Forsythe test)"), ", который использует медиану вместо среднего, как у теста Ливена)."),
        p("При использовании трансформации зависимой переменной возвращать значения отличий между группами в исходной шкале (с помощью обратного преобразования)."),
        p("Посмотреть в сторону других непараметрических критериев: Джонкхиера-Терпстра, Данна, Q Кокрена."),
        p("Добавить другие множественные сравнения - например, Student-Newman-Keuls (SNK) менее консервативен, чем Тьюки"),  # SNK.test {agricolae}
        p("Добавить выбор контрольной группы для критерия Даннета."),
        # http://finzi.psych.upenn.edu/R/library/multtest/html/MTP.html
        br(),

        h4("Планы на далекое будущее:"),
        p("Пермутационные тесты для ANOVA - Unrestricted Permutation of observations (Manly, 2007),
            Restricted permutation of main effects (Edgington, 2007), 
            Permutation of Residuals (Still and White, 1981), ter Braak's method (1992) ... "),
        br(),
        
        h4("История версий:"),
        p(" v.0.1.0 (28.05.2014) - Первый публичный релиз."),
        p(" v.0.1.2 (20.06.2014) - Добавлен импорт данных из файлов Excel; мелкие исправления."),
        p(" v.0.1.3 (xx.xx.2015) - Добавлен экспорт результатов, отображение параметра преобразования Йео–Джонсона; мелкие исправления."),
        br()
      ),

      tabPanel("Копирайты и ссылки",
        p("Author - Vladimir Mikryukov. Email:", em("vmikryukov at gmail.com")),
        p("Данная программа была написана для внутреннего использования в ",
          a("лаборатории экотоксикологии популяций и сообществ ИЭРиЖ УрО РАН", 
            href = "http://ipae.uran.ru/"),
          " и распространяется на условиях лицензии ",
          a("Creative Commons Attribution-ShareAlike 4.0 International License", 
            href = "http://creativecommons.org/licenses/by-sa/4.0/")),
        img(src = "cc-sa.png", height = 31, width = 88),
        br(),
        br(),
        h4("Для реализации данного приложения использованы следующие компоненты:"),
        p(a(strong("R")," - язык программирования для статистической обработки данных и работы с графикой, а также свободная программная среда вычислений.", href = "http://www.R-project.org/")),
        p(a(strong("Shiny"), " - основа для построения web-приложений для R.", href = "http://shiny.rstudio.com/")),
        h4("и пакеты для R:"),
        p(a(strong("car"), href = "http://cran.r-project.org/web/packages/car/index.html"),
          ". Fox J., Weisberg S. An R Companion to Applied Regression. Thousand Oaks: SAGE Publications, 2010. 512 p."),
        p(a(strong("nortest"), href = "http://cran.r-project.org/web/packages/nortest/index.html"),
          ". Gross J., Ligges U. nortest: Tests for Normality"),
        p(a(strong("multcomp"), href = "http://cran.r-project.org/web/packages/multcomp/index.html"),
          ". Hothorn T., Bretz F., Westfall P. Simultaneous Inference in General Parametric Models // Biometrical J. 2008. V. 50. № 3. P. 346-363."),
        p(a(strong("multcompView"), href = "http://cran.r-project.org/web/packages/multcompView/index.html"),
          ". Graves S., Piepho H.-P., Selzer L., Dorai-Raj S. multcompView: Visualizations of Paired Comparisons."),
        p(a(strong("sandwich"), href = "http://cran.r-project.org/web/packages/sandwich/index.html"),
          ". Zeileis A. Econometric Computing with HC and HAC Covariance Matrix Estimators // J Stat Softw. 2004. V. 11. № 10. P. 1-17."),
        p(a(strong("lmPerm"), href = "http://cran.r-project.org/web/packages/lmPerm/index.html"),
          ". Wheeler B. lmPerm: Permutation tests for linear models."),
        p(a(strong("agricolae"), href = "http://cran.r-project.org/web/packages/agricolae/index.html"),
          ". de Mendiburu F. agricolae: Statistical Procedures for Agricultural Research."),
        p(a(strong("XLConnect"), href = "http://cran.r-project.org/web/packages/XLConnect/index.html"),
          ". Mirai Solutions GmbH. XLConnect: Excel Connector for R."),
        br(),
        h4("Список литературы"),
        p("Dytham C. Choosing and using statistics: A biologist's guide. Hoboken, NJ: Wiley-Blackwell, 2011. 298 p."),
        p("Leys C., Schumann S. A nonparametric method to analyze interactions: The adjusted rank transform test // Journal of Experimental Social Psychology. 2010. V. 46. № 4. P. 684-688."),
        p("Long J.S., Ervin L.H. Using heteroscedasticity consistent standard errors in the linear regression model // American Statistician. 2000. V. 54. № 3. P. 217-224."),
        p("Sawilowsky S.S. Nonparametric-Tests of Interaction in Experimental-Design // Rev Educ Res. 1990. V. 60. № 1. P. 91-126."),
        p("Scheirer C.J., Ray W.S., Hare N. The Analisis of Ranked Data Derived from Completely Randomized Factorial Designs // Biometrics. 1976. V. 32. № 2. P. 429-434."),
        p("Sokal R.R., Rohlf F.J. Biometry: The Principles And Practice Of Statistics In Biological Research. W.h. Freeman & Company, 1995. 880 p.")
      )

    )     # close tabsetPanel
  )       # close mainPanel
))
