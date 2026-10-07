library(shiny)
library(ggplot2)
library(bslib)
library(bsicons)

# Пользовательский интерфейс
ui <- page_sidebar(
  title = "📚 Обучение математике в R Studio — Новые задачи",
  
  sidebar = sidebar(
    navset_card_tab(
      id = "tabs",
      
      # ===== ВКЛАДКА 1: ПРОИЗВОДНЫЕ (10 новых задач) =====
      nav_panel("📐 Производные", icon = bs_icon("calculator"),
                selectInput("deriv_task", "Выберите задачу:",
                            choices = c(
                              "Задача 1: f(x) = x⁴ - 3x² + 2" = "d1",
                              "Задача 2: f(x) = tan(x)" = "d2",
                              "Задача 3: f(x) = 1/x²" = "d3",
                              "Задача 4: f(x) = x·e^x" = "d4",
                              "Задача 5: f(x) = sin(x)·cos(x)" = "d5",
                              "Задача 6: f(x) = arctan(x)" = "d6",
                              "Задача 7: f(x) = x⁵" = "d7",
                              "Задача 8: f(x) = ln(x²)" = "d8",
                              "Задача 9: f(x) = e^(2x)" = "d9",
                              "Задача 10: f(x) = √(x+1)" = "d10"
                            )
                ),
                numericInput("deriv_x", "Точка вычисления (x):", value = 1, min = 0.1, max = 10, step = 0.1),
                actionButton("calc_deriv", "Вычислить производную", class = "btn-primary")
      ),
      
      # ===== ВКЛАДКА 2: СТАТИСТИКА (10 новых задач) =====
      nav_panel("📊 Статистика", icon = bs_icon("graph-up"),
                selectInput("stat_task", "Выберите задачу:",
                            choices = c(
                              "Задача 1: Описательные статистики" = "s1",
                              "Задача 2: Гистограмма с плотностью" = "s2",
                              "Задача 3: Сравнительный Boxplot" = "s3",
                              "Задача 4: Корреляция Спирмена" = "s4",
                              "Задача 5: Тест Шапиро-Уилка" = "s5",
                              "Задача 6: Доверительный интервал" = "s6",
                              "Задача 7: Частотная таблица" = "s7",
                              "Задача 8: QQ-plot" = "s8",
                              "Задача 9: Бутстрап среднего" = "s9",
                              "Задача 10: Критерий хи-квадрат" = "s10"
                            )
                ),
                textAreaInput("stat_data", "Данные через запятую:", 
                              value = "23, 25, 28, 30, 32, 35, 38, 40, 42, 45", height = "80px"),
                textAreaInput("stat_data2", "Вторая группа (если нужна):", 
                              value = "20, 22, 24, 26, 28, 30, 32, 34", height = "80px"),
                actionButton("calc_stats", "Рассчитать", class = "btn-success")
      ),
      
      # ===== ВКЛАДКА 3: ФИНАНСЫ (10 новых задач) =====
      nav_panel("💰 Финансы", icon = bs_icon("currency-dollar"),
                selectInput("fin_task", "Выберите задачу:",
                            choices = c(
                              "Задача 1: Сложный % (ежеквартально)" = "f1",
                              "Задача 2: Дисконтирование векселя" = "f2",
                              "Задача 3: Доходность облигации" = "f3",
                              "Задача 4: Аннуитет постнумерандо" = "f4",
                              "Задача 5: Аннуитет пренумерандо" = "f5",
                              "Задача 6: Бессрочный аннуитет" = "f6",
                              "Задача 7: Реальная доходность" = "f7",
                              "Задача 8: Срок удвоения капитала" = "f8",
                              "Задача 9: Дюрация облигации" = "f9",
                              "Задача 10: Рентабельность (ROI)" = "f10"
                            )
                ),
                
                # Поля для задачи 1
                conditionalPanel(condition = "input.fin_task == 'f1'",
                                 numericInput("f1_init", "Начальная сумма:", value = 50000),
                                 numericInput("f1_rate", "Годовая ставка (%):", value = 10),
                                 numericInput("f1_years", "Срок (лет):", value = 3)
                ),
                
                # Поля для задачи 2
                conditionalPanel(condition = "input.fin_task == 'f2'",
                                 numericInput("f2_nominal", "Номинал векселя:", value = 100000),
                                 numericInput("f2_rate", "Учётная ставка (%):", value = 15),
                                 numericInput("f2_days", "Дней до погашения:", value = 90)
                ),
                
                # Поля для задачи 3
                conditionalPanel(condition = "input.fin_task == 'f3'",
                                 numericInput("f3_price", "Цена облигации:", value = 950),
                                 numericInput("f3_nominal", "Номинал:", value = 1000),
                                 numericInput("f3_coupon", "Купон (%):", value = 8),
                                 numericInput("f3_years", "Лет до погашения:", value = 5)
                ),
                
                # Поля для задачи 4
                conditionalPanel(condition = "input.fin_task == 'f4'",
                                 numericInput("f4_pay", "Ежегодный платёж:", value = 10000),
                                 numericInput("f4_rate", "Ставка (%):", value = 9),
                                 numericInput("f4_years", "Срок (лет):", value = 7)
                ),
                
                # Поля для задачи 5
                conditionalPanel(condition = "input.fin_task == 'f5'",
                                 numericInput("f5_pay", "Ежегодный платёж:", value = 15000),
                                 numericInput("f5_rate", "Ставка (%):", value = 8),
                                 numericInput("f5_years", "Срок (лет):", value = 5)
                ),
                
                # Поля для задачи 6
                conditionalPanel(condition = "input.fin_task == 'f6'",
                                 numericInput("f6_pay", "Ежегодный платёж:", value = 20000),
                                 numericInput("f6_rate", "Ставка (%):", value = 10)
                ),
                
                # Поля для задачи 7
                conditionalPanel(condition = "input.fin_task == 'f7'",
                                 numericInput("f7_nominal", "Номинальная доходность (%):", value = 12),
                                 numericInput("f7_inflation", "Инфляция (%):", value = 8)
                ),
                
                # Поля для задачи 8
                conditionalPanel(condition = "input.fin_task == 'f8'",
                                 numericInput("f8_rate", "Годовая ставка (%):", value = 10)
                ),
                
                # Поля для задачи 9
                conditionalPanel(condition = "input.fin_task == 'f9'",
                                 numericInput("f9_price", "Цена облигации:", value = 980),
                                 numericInput("f9_coupon", "Купон (%):", value = 7),
                                 numericInput("f9_years", "Лет до погашения:", value = 4)
                ),
                
                # Поля для задачи 10
                conditionalPanel(condition = "input.fin_task == 'f10'",
                                 numericInput("f10_invest", "Инвестиции:", value = 100000),
                                 numericInput("f10_profit", "Прибыль:", value = 35000)
                ),
                
                actionButton("calc_finance", "Рассчитать", class = "btn-warning")
      )
    )
  ),
  
  card(
    card_header(h3("📋 Результаты")),
    uiOutput("result_output")
  )
)

# Серверная логика
server <- function(input, output, session) {
  
  output$result_output <- renderUI({
    HTML("<div style='padding:15px; background:#f5f5f5; border-radius:8px; text-align:center;'>
          👈 Выберите задачу и нажмите кнопку расчёта</div>")
  })
  
  # ===== ПРОИЗВОДНЫЕ =====
  observeEvent(input$calc_deriv, {
    x <- input$deriv_x
    result <- switch(input$deriv_task,
                     "d1" = paste0("f(x) = x⁴ - 3x² + 2<br>f'(x) = 4x³ - 6x<br>f'(", x, ") = <b>", 4*x^3 - 6*x, "</b>"),
                     "d2" = paste0("f(x) = tan(x)<br>f'(x) = 1/cos²(x)<br>f'(", round(x,2), ") = <b>", round(1/cos(x)^2, 4), "</b>"),
                     "d3" = paste0("f(x) = 1/x²<br>f'(x) = -2/x³<br>f'(", round(x,2), ") = <b>", round(-2/x^3, 4), "</b>"),
                     "d4" = paste0("f(x) = x·e^x<br>f'(x) = e^x·(x+1)<br>f'(", round(x,2), ") = <b>", round(exp(x)*(x+1), 4), "</b>"),
                     "d5" = paste0("f(x) = sin(x)·cos(x)<br>f'(x) = cos²(x) - sin²(x) = cos(2x)<br>f'(", round(x,2), ") = <b>", round(cos(2*x), 4), "</b>"),
                     "d6" = paste0("f(x) = arctan(x)<br>f'(x) = 1/(1+x²)<br>f'(", round(x,2), ") = <b>", round(1/(1+x^2), 4), "</b>"),
                     "d7" = paste0("f(x) = x⁵<br>f'(x) = 5x⁴<br>f'(", x, ") = <b>", 5*x^4, "</b>"),
                     "d8" = paste0("f(x) = ln(x²)<br>f'(x) = 2/x<br>f'(", round(x,2), ") = <b>", round(2/x, 4), "</b>"),
                     "d9" = paste0("f(x) = e^(2x)<br>f'(x) = 2·e^(2x)<br>f'(", round(x,2), ") = <b>", round(2*exp(2*x), 4), "</b>"),
                     "d10" = paste0("f(x) = √(x+1)<br>f'(x) = 1/(2√(x+1))<br>f'(", round(x,2), ") = <b>", round(1/(2*sqrt(x+1)), 4), "</b>")
    )
    output$result_output <- renderUI({
      HTML(paste0("<div style='padding:15px; background:#e3f2fd; border-radius:8px;'><h4>📐 Производная</h4>", result, "</div>"))
    })
  })
  
  # ===== СТАТИСТИКА =====
  observeEvent(input$calc_stats, {
    d1 <- na.omit(as.numeric(unlist(strsplit(input$stat_data, ","))))
    d2 <- na.omit(as.numeric(unlist(strsplit(input$stat_data2, ","))))
    
    if (length(d1) < 2) {
      output$result_output <- renderUI({
        HTML("<div style='padding:15px; background:#ffebee; color:red;'>❌ Минимум 2 числа</div>")
      })
      return()
    }
    
    result <- switch(input$stat_task,
                     "s1" = paste0("<b>Описательные статистики:</b><br>",
                                   "Среднее: ", round(mean(d1),2), "<br>Медиана: ", round(median(d1),2),
                                   "<br>Мода: ", names(sort(table(d1), decreasing=TRUE)[1]),
                                   "<br>Ст.откл: ", round(sd(d1),2), "<br>Асимметрия: ", round(mean((d1-mean(d1))^3)/sd(d1)^3, 4),
                                   "<br>Эксцесс: ", round(mean((d1-mean(d1))^4)/sd(d1)^4 - 3, 4)),
                     
                     "s2" = "Смотрите гистограмму с линией плотности 👇",
                     "s3" = "Смотрите сравнительный boxplot 👇",
                     
                     "s4" = {
                       cor_val <- cor(d1[1:min(length(d1),length(d2))], d2[1:min(length(d1),length(d2))], method="spearman")
                       paste0("<b>Корреляция Спирмена:</b> ", round(cor_val, 4), "<br>",
                              if(abs(cor_val)>0.7) "🔥 Сильная монотонная связь" else if(abs(cor_val)>0.4) "🔶 Средняя связь" else "🔹 Слабая связь")
                     },
                     
                     "s5" = {
                       sw <- shapiro.test(d1)
                       paste0("<b>Тест Шапиро-Уилка на нормальность:</b><br>",
                              "W = ", round(sw$statistic, 4), "<br>p-value = ", round(sw$p.value, 4), "<br>",
                              if(sw$p.value > 0.05) "✅ Данные нормальны (p > 0.05)" else "❌ Данные не нормальны (p ≤ 0.05)")
                     },
                     
                     "s6" = {
                       n <- length(d1); se <- sd(d1)/sqrt(n)
                       ci_low <- mean(d1) - 1.96*se; ci_high <- mean(d1) + 1.96*se
                       paste0("<b>95% доверительный интервал для среднего:</b><br>",
                              "Среднее: ", round(mean(d1), 2), "<br>",
                              "Границы: [", round(ci_low, 2), "; ", round(ci_high, 2), "]<br>",
                              "Ст. ошибка: ", round(se, 4))
                     },
                     
                     "s7" = {
                       tbl <- sort(table(d1), decreasing = TRUE)
                       paste0("<b>Частотная таблица:</b><br>",
                              paste(names(tbl), "→", tbl, collapse = "<br>"))
                     },
                     
                     "s8" = "Смотрите QQ-plot 👇",
                     
                     "s9" = {
                       set.seed(42)
                       boots <- replicate(1000, mean(sample(d1, replace=TRUE)))
                       paste0("<b>Бутстрап среднего (1000 итераций):</b><br>",
                              "Среднее бутстрапа: ", round(mean(boots), 2), "<br>",
                              "Ст. ошибка: ", round(sd(boots), 4), "<br>",
                              "95% ДИ: [", round(quantile(boots, 0.025), 2), "; ", round(quantile(boots, 0.975), 2), "]")
                     },
                     
                     "s10" = {
                       tbl <- table(d1)
                       chi <- chisq.test(tbl)
                       paste0("<b>Критерий хи-квадрат:</b><br>",
                              "χ² = ", round(chi$statistic, 4), "<br>p-value = ", round(chi$p.value, 4), "<br>",
                              if(chi$p.value < 0.05) "✅ Распределение неравномерно" else "⚠️ Распределение равномерно")
                     }
    )
    
    # График
    df <- data.frame(values = d1)
    p <- switch(input$stat_task,
                "s1" = ggplot(df, aes(values)) + geom_histogram(fill="#667eea", bins=10, color="white") + theme_minimal() + labs(title="Распределение"),
                "s2" = ggplot(df, aes(values)) + geom_histogram(aes(y=after_stat(density)), fill="#764ba2", bins=15, color="white") + geom_density(color="red", size=1) + theme_minimal() + labs(title="Гистограмма с плотностью"),
                "s3" = {df_c <- data.frame(v=c(d1,d2), g=rep(c("Группа 1","Группа 2"), c(length(d1),length(d2)))); ggplot(df_c, aes(g,v,fill=g)) + geom_boxplot() + theme_minimal() + labs(title="Сравнение групп")},
                "s4" = {df2 <- data.frame(x=d1[1:min(length(d1),length(d2))], y=d2[1:min(length(d1),length(d2))]); ggplot(df2, aes(x,y)) + geom_point(color="#764ba2", size=3) + geom_smooth(method="lm", se=FALSE, color="red") + theme_minimal() + labs(title="Корреляция Спирмена")},
                "s5" = ggplot(df, aes(values)) + geom_histogram(fill="#667eea", bins=10) + theme_minimal() + labs(title="Распределение (проверка нормальности)"),
                "s6" = ggplot(df, aes(values)) + geom_histogram(fill="#764ba2", bins=10) + geom_vline(xintercept=mean(d1), color="red", size=1) + theme_minimal() + labs(title="Среднее и ДИ"),
                "s7" = {tbl <- as.data.frame(table(d1)); ggplot(tbl, aes(x=Var1, y=Freq)) + geom_col(fill="#667eea") + theme_minimal() + labs(title="Частоты")},
                "s8" = ggplot(df, aes(sample=values)) + stat_qq() + stat_qq_line(color="red") + theme_minimal() + labs(title="QQ-plot"),
                "s9" = {set.seed(42); boots <- replicate(1000, mean(sample(d1, replace=TRUE))); df_b <- data.frame(mean=boots); ggplot(df_b, aes(mean)) + geom_histogram(fill="#764ba2", bins=30) + theme_minimal() + labs(title="Бутстрап-распределение")},
                "s10" = {tbl <- as.data.frame(table(d1)); ggplot(tbl, aes(x=Var1, y=Freq)) + geom_col(fill="#667eea") + theme_minimal() + labs(title="Для хи-квадрат")}
    )
    
    output$result_output <- renderUI({
      tagList(
        div(style="padding:15px; background:#e8f5e9; border-radius:8px;", HTML(result)),
        div(style="margin-top:20px;", plotOutput("stat_plot", height="400px"))
      )
    })
    output$stat_plot <- renderPlot({ print(p) })
  })
  
  # ===== ФИНАНСЫ =====
  observeEvent(input$calc_finance, {
    result <- switch(input$fin_task,
                     "f1" = {
                       fv <- input$f1_init * (1 + input$f1_rate/100/4)^(input$f1_years*4)
                       paste0("<b>Сложный % (ежеквартально):</b><br>Будущая стоимость: <b style='color:green;'>", format(round(fv,2), big.mark=" "), " руб.</b><br>Прибыль: ", format(round(fv-input$f1_init,2), big.mark=" "), " руб.")
                     },
                     "f2" = {
                       discount <- input$f2_nominal * (input$f2_rate/100) * (input$f2_days/360)
                       pv <- input$f2_nominal - discount
                       paste0("<b>Дисконтирование векселя:</b><br>Дисконт: ", format(round(discount,2), big.mark=" "), " руб.<br>Текущая стоимость: <b style='color:blue;'>", format(round(pv,2), big.mark=" "), " руб.</b>")
                     },
                     "f3" = {
                       coupon <- input$f3_nominal * input$f3_coupon/100
                       ytm <- (coupon + (input$f3_nominal - input$f3_price)/input$f3_years) / ((input$f3_nominal + input$f3_price)/2) * 100
                       paste0("<b>Доходность облигации (YTM):</b><br>Купон: ", format(round(coupon,2), big.mark=" "), " руб.<br>Доходность к погашению: <b style='color:green;'>", round(ytm, 2), "%</b>")
                     },
                     "f4" = {
                       r <- input$f4_rate/100
                       fv <- input$f4_pay * (((1+r)^input$f4_years - 1)/r)
                       paste0("<b>Аннуитет постнумерандо:</b><br>Будущая стоимость: <b style='color:green;'>", format(round(fv,2), big.mark=" "), " руб.</b><br>Внесено: ", format(round(input$f4_pay*input$f4_years,2), big.mark=" "), " руб.")
                     },
                     "f5" = {
                       r <- input$f5_rate/100
                       fv <- input$f5_pay * (((1+r)^input$f5_years - 1)/r) * (1+r)
                       paste0("<b>Аннуитет пренумерандо:</b><br>Будущая стоимость: <b style='color:green;'>", format(round(fv,2), big.mark=" "), " руб.</b>")
                     },
                     "f6" = {
                       pv <- input$f6_pay / (input$f6_rate/100)
                       paste0("<b>Бессрочный аннуитет (перпетуитет):</b><br>Приведённая стоимость: <b style='color:blue;'>", format(round(pv,2), big.mark=" "), " руб.</b>")
                     },
                     "f7" = {
                       real <- ((1 + input$f7_nominal/100) / (1 + input$f7_inflation/100) - 1) * 100
                       paste0("<b>Реальная доходность (формула Фишера):</b><br>Номинальная: ", input$f7_nominal, "%<br>Инфляция: ", input$f7_inflation, "%<br>Реальная: <b style='color:green;'>", round(real, 2), "%</b>")
                     },
                     "f8" = {
                       years <- log(2) / log(1 + input$f8_rate/100)
                       rule72 <- 72 / input$f8_rate
                       paste0("<b>Срок удвоения капитала:</b><br>Точный расчёт: <b style='color:green;'>", round(years, 2), " лет</b><br>Правило 72: ≈ ", round(rule72, 2), " лет")
                     },
                     "f9" = {
                       coupon <- input$f9_coupon/100
                       y <- input$f9_years
                       mac_dur <- 0
                       for(t in 1:y) { mac_dur <- mac_dur + t * coupon / (1+coupon)^t }
                       mac_dur <- mac_dur + y * 1 / (1+coupon)^y
                       mac_dur <- mac_dur / (input$f9_price/1000)
                       mod_dur <- mac_dur / (1 + coupon)
                       paste0("<b>Дюрация облигации:</b><br>Дюрация Маколея: <b>", round(mac_dur, 2), " лет</b><br>Модифицированная дюрация: <b style='color:blue;'>", round(mod_dur, 2), "</b>")
                     },
                     "f10" = {
                       roi <- (input$f10_profit / input$f10_invest) * 100
                       paste0("<b>Рентабельность инвестиций (ROI):</b><br>Инвестиции: ", format(input$f10_invest, big.mark=" "), " руб.<br>Прибыль: ", format(input$f10_profit, big.mark=" "), " руб.<br>ROI: <b style='color:green; font-size:1.2em;'>", round(roi, 2), "%</b>")
                     }
    )
    output$result_output <- renderUI({
      HTML(paste0("<div style='padding:15px; background:#fff3e0; border-radius:8px;'><h4>💰 Финансы</h4>", result, "</div>"))
    })
  })
}

