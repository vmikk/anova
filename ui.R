## ui.r
## The user interface controls the layout and appearance of the app.


# Define UI for application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(h3("ANOVA - Дисперсионный анализ")),


  sidebarPanel(
  # helpText("Текстовый файл с разделителями табуляции."),
  fileInput( "dataFile", "Загрузить файл", accept="text/plain"),
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
    		div(class="row"),
    		  div(class="span5", plotOutput("qq")),
    		  div(class="span5", plotOutput("qq.rand")),
    		div(class="row"),
    		p("Ваши данные представлены на левом графике. Наблюдения должны укладываться в диапазон, отмеченный красными линиями. Для сравнения - на правом графике показаны искуственные данные, соответствующие нормальному распределению."),
    		br(),
        h4(div(uiOutput("y.warning"), style = "color:red")),
        div(class="row"),
          div(class="span5", plotOutput("histt")),
          div(class="span5", plotOutput("y.vs.tr")),
        div(class="row"),
          div(class="span5", uiOutput("rug")),
          div(class="span5", uiOutput("gr.col")),
        div(class="row"),
        uiOutput("dens"), uiOutput("dens.adj"),
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
        div(class="row"),
          div(class="span9", plotOutput("boxp")),
          div(class="span2", uiOutput("boxo")),
        div(class="row"),
        # uiOutput("letter.diff"),
        verbatimTextOutput("multcomp")
      ),



      tabPanel("О программе",
        h4("Формат данных:"),
        p("Входной файл - текстовый файл с разделителями табуляции."),
        p("Первая строка содержит названия переменных. 
            Они должны быть написаны ", span(strong("латиницей"), style = "color:red"), " и не могут начинаться с цифры, содержать пробел или тире."),
        p("Все группирующие переменные (факторы) должны быть закодированы как текстовые значения (начинаются с буквы)."), 
        p("Зависимые переменные имеют числовой вид. 
            Разделитель дробной части - ", span(strong("точка"), style = "color:red"),
            ". Пропущенные значения разрешены и должны быть закодированы как ",
             span(strong("NA"), style = "color:red"), ". 
             Однако они будут исключены из дальнейшего анализа по мере необходимости." ),
        br(),

        h4("Что умеет делать данная программа:"),
        p(em("Тест Шейрера-Рея-Хара (Scheirer-Ray-Hare test)"),
          " добавлен лишь в экспериментальных целях и не рекомендуется к использованию из-за крайне малой мощности метода. 
          Его реализация базируется на работе Dytham C. Choosing and using statistics: A biologist's guide. Hoboken, NJ: Wiley-Blackwell, 2011. 298 p."),
        p(em("Непараметрический тест взаимодействия факторов (ART-тест на взаимодействие)"),
          " возможен пока только для двух-факторного дизайна (Leys, Schumann, 2010)."),
        p(em("Пермутационный ANOVA"), "для оценки статистической значимости использует перестановочный тест. Количество итераций опеределяется в соответствии с алгоритмом Anscome (1953).  
          При анализе факторов, представленных более чем 4-5 уровнями могут быть проблемы."),
        p(em("Множественные сравнения критерем Даннета (Dunnett)"), " следует проводить только с контрольной группой. Контрольной будет считаться та группа, которая идет первой в таблице данных."),
        br(),

        h4("Что пока не умеет, но планирует:"),
        p("Будет добавлена возможность фильтрации данных по переменной и анализа полученного поднабора данных."),
        p(em("Преобразование Йео–Джонсона (Yeo-Johnson transformation)"),
          " - аналог преобразования Бокса-Кокса, но позволяет работать с отрицательными значениями. 
          Производится для всей совокупности данных и не учитывает группирующие переменные (например, для выравниявания их дисперсий)."),
        p(em("Преобразование обратного гиперболического синуса"), " - аналог логарифмического преобразования, но оно определено для значений = 0."),
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
        p("* v.0.1 - Первый публичный релиз."),
        br(),
        br(),
        br(),
        p("Author - Vladimir Mikryukov. Email:", em("vmikryukov at gmail.com")),
        p("Данная программа была написана для внутреннего использования в ",
          a("лаборатории экотоксикологии популяций и сообществ ИЭРиЖ УрО РАН", 
            href = "http://ipae.uran.ru/"),
          " и распространяется на условиях лицензии ",
          a("Creative Commons Attribution-ShareAlike 4.0 International License", 
            href = "http://creativecommons.org/licenses/by-sa/4.0/")),
        img(src = "cc-sa.png", height = 31, width = 88)
      )

    )     # close tabsetPanel
  )       # close mainPanel
))
