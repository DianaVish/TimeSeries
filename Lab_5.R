library(ggplot2)
library(astsa)
library(forecast)
library(TTR)
library(smooth)
library(Mcomp)
data = USAccDeaths
# 1.Зобразити отриманий часовий ряд (з підписами).
plot(data, xlab="Рік", ylab="Кількість смертей", las=1, main="Часовий ряд")
# 2. Провести згладжування ряду методом рухомого середнього з різним кроком.
plot.ts(sma(data, order=3, silent=F))
plot.ts(sma(data, order=5, silent=F))
plot.ts(sma(data, order=7, silent=F))
# 3. Розбити вихідний часовий ряд на систематичну, періодичну та хаотичну
#   складові.
plot(decompose(data))
# 4. Побудувати корелограму та частинну корелограму ЧР.
data %>% ggtsdisplay()
data1 = diff(data, lag=4)
plot(data1, xlab = "Час", ylab = "Кількість смертей", main = "1-е застосування диференціювання")
data2 = diff(data1)
plot(data2, xlab = "Час", ylab = "Кількість смертей", main = "2-е застосування диференціювання")
data2 %>% ggtsdisplay()
# 5. Вибрати модель, яка адекватно прогнозуватиме даний ЧР. Побудувати прогноз
#    відповідним до моделі ЧР методом експоненційного згладжування (звичайним,
#    подвійним або потрійним – залежно від моделі) та відповідним до ряду методом з
#    групи ARIMA.
hw <- HoltWinters(data)
p = predict(hw, 50, prediction.interval=F)
plot(hw, p, xlab = "Рік", ylab = "Кількість смертей", main = "Прогноз за методом Холта-Вінтерса")
data %>% Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay()
data %>% Arima(order=c(0,1,2), seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay()
data %>% Arima(order=c(0,1,3), seasonal=c(0,1,1)) %>% residuals() %>% ggtsdisplay()
fit <- Arima(data, order=c(0,1,3), seasonal=c(0,1,1))
predict(fit,12)

# 6. Побудувати корелограми залишків та інші діаграми, що характеризують
#    розподіл залишків моделі.
sarima(data,0,1,3,0,1,1,4)
