library("tidyverse")
library("plotly")

df <- read.csv("superstore_orders.csv")
df$Country <- NULL # Remove this column because it only contains the USA

# Calculate the final price
totalPrice <- df$Sales * df$Quantity
df$FinalPrice <- round(totalPrice - (totalPrice * df$Discount), 2)

# Calculate the shipping days
df$Order.Date <- as.Date(df$Order.Date, format = "%m/%d/%Y")
df$Ship.Date <- as.Date(df$Ship.Date, format = "%m/%d/%Y")
df$Ship.Days <- ifelse(df$Ship.Date - df$Order.Date < 0, 365 + (df$Ship.Date - df$Order.Date) + 1, df$Ship.Date - df$Order.Date)

style.bgcolor <- "#FAFAFA"

# Summary data
orders.summary <- data.frame(Data = c("Общо продадено количество", "Продукти", "Категории", "Подкатегории", "Клиенти", "Градове", "Щати"),
                             Labels = c(sum(df$Quantity), length(unique(df$Product.Name)), length(unique(df$Category)), length(unique(df$Sub.Category)),
                                        length(unique(df$Customer.ID)), length(unique(df$City)), "Не са включени Аляска и Хаваи"))

# Years distribution
orders.years <- aggregate(cbind(df$Quantity), by = list(Year = substring(df$Order.Date, 1, 4)), FUN = sum)
names(orders.years)[2] <- "Freq"
plot_ly(orders.years, labels = ~Year, values = ~Freq, type = "pie", hole = 0.5) %>%
  layout(paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor)

# Segments distribution
orders.segments <- aggregate(cbind(df$Quantity), by = list(Segment = df$Segment), FUN = sum)
names(orders.segments)[2] <- "Freq"
plot_ly(orders.segments, labels = ~Segment, values = ~Freq, type = "pie", hole = 0.5) %>%
  layout(paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor)

# Shippings
orders.SameDay <- filter(df, Ship.Mode == "Same Day")
orders.SameDay.delays <- orders.SameDay %>% filter(Ship.Days > 0)
orders.SameDay <- orders.SameDay$Ship.Days
orders.FirstClass <- filter(df, Ship.Mode == "First Class")$Ship.Days
orders.SecondClass <- filter(df, Ship.Mode == "Second Class")$Ship.Days
orders.StandardClass <- filter(df, Ship.Mode == "Standard Class")$Ship.Days

# Shipping modes and days
shippingModes <- c("Same Day", "First Class", "Second Class", "Standard Class")
orders.shipping <- data.frame(Mode = shippingModes,
                              Days = c(mean(orders.SameDay), mean(orders.FirstClass), mean(orders.SecondClass), mean(orders.StandardClass)))
orders.shipping$Mode <- factor(orders.shipping$Mode, levels = orders.shipping$Mode)

plot_ly(data = orders.shipping, x = ~Mode, y = ~Days, type = "scatter", mode = "lines") %>%
  layout(paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor,
         xaxis = list(title = "Тип доставка"), yaxis = list(title = "Брой продажби"))

orders.shipping.days <- data.frame(Mode = shippingModes,
                                   Min = c(min(orders.SameDay), min(orders.FirstClass), min(orders.SecondClass), min(orders.StandardClass)),
                                   Max = c(max(orders.SameDay), max(orders.FirstClass), max(orders.SecondClass), max(orders.StandardClass)))
names(orders.shipping.days) <- c("Тип", "Най-бърза доставка (дни)", "Най-бавна доставка (дни)")

# Map of the US which represents the count of sales
orders.states <- aggregate(cbind(df$Quantity), by = list(State = df$State), FUN = sum)
names(orders.states)[2] <- "Freq"
states <- read.csv("usa_state_codes.csv")
orders.states$code <- states$code
orders.states$state.bg <- states$state.bg

orders.states$hover <- with(orders.states, paste(state.bg, "\n", "Количество: ", Freq))
l <- list(color = toRGB("white"), width = 2)
g <- list(scope = "usa", projection = list(type = "albers usa"), showlakes = TRUE, lakecolor = toRGB("white"))

plot_geo(orders.states, locationmode = "USA-states") %>%
  add_trace(z = ~Freq, text = ~hover, locations = ~code, color = ~Freq, colors = "Blues") %>%
  colorbar(title = "Продажби") %>%
  layout(geo = g, paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor)

# (Sales) TOP 5 and least 5 states
orders.states.top5 <- select(head(orders.states[order(-orders.states$Freq, orders.states$state ),], 5), "state.bg", "Freq")
orders.states.least5 <- select(tail(orders.states[order(-orders.states$Freq, orders.states$state ),], 5), "state.bg", "Freq")
orders.states.top5$state.bg <- factor(orders.states.top5$state.bg, levels = unique(orders.states.top5$state.bg)[order(orders.states.top5$Freq, decreasing = TRUE)])
orders.states.least5$state.bg <- factor(orders.states.least5$state.bg, levels = unique(orders.states.least5$state.bg)[order(orders.states.least5$Freq, decreasing = TRUE)])

plot_ly(data = orders.states.top5, x = ~orders.states.top5$state.bg, y = ~orders.states.top5$Freq, type = "bar") %>%
  layout(paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor,
         xaxis = list(title = "Щат"), yaxis = list(title = "Брой продажби"))

plot_ly(data = orders.states.least5, x = ~state.bg, y = ~Freq, type = "bar", marker = list(color = "#FF7F0E")) %>%
  layout(paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor,
         xaxis = list(title = "Щат"), yaxis = list(title = "Брой продажби"))

# TOP 10 cities
orders.cities <- aggregate(cbind(df$Quantity), by = list(City = df$City), FUN = sum)
names(orders.cities)[2] <- "Freq"
orders.cities.top10 <- select(head(orders.cities[order(-orders.cities$Freq, orders.cities$City ),], 10), "City", "Freq")
orders.cities.top10$City <- factor(orders.cities.top10$City, levels = unique(orders.cities.top10$City)[order(orders.cities.top10$Freq, decreasing = TRUE)])

plot_ly(data = orders.cities.top10, x = ~City, y = ~Freq, type = "bar") %>%
  layout(paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor,
         xaxis = list(title = "Град"), yaxis = list(title = "Брой продажби"))

# The biggest sales
orders.products.top10 <- select(head(df[order(-df$FinalPrice),]), "Order.ID", "Product.Name", "Category", "Sales", "Quantity", "Discount", "FinalPrice")
orders.products.top10$Discount <- paste0(orders.products.top10$Discount * 100, "%")
orders.products.top10$FinalPrice <- paste0("$", orders.products.top10$FinalPrice)
names(orders.products.top10) <- c("№ поръчка", "Продукт", "Категория", "Единична цена", "Количество", "Отстъпка", "Крайна цена")
rownames(orders.products.top10) <- orders.products.top10$Order.ID

orders.products.top25.profit <- aggregate(cbind(df$Profit), by = list(Product.Name = df$Product.Name), FUN = sum)
orders.products.top25.profit <- head(orders.products.top25.profit[order(-orders.products.top25.profit$V1),], 25)
orders.products.top25.profit$Product.Name <- factor(orders.products.top25.profit$Product.Name, levels = unique(orders.products.top25.profit$Product.Name)[order(orders.products.top25.profit$V1, decreasing = TRUE)])

plot_ly(orders.products.top25.profit, x = ~Product.Name, y = ~V1, type = "bar") %>%
  layout(paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor,
         xaxis = list(title = "Продукт"), yaxis = list(title = "Печалба"))

orders.products.top25.count <- aggregate(cbind(df$Quantity), by = list(Product.Name = df$Product.Name), FUN = sum)
names(orders.products.top25.count)[2] <- "Freq"
orders.products.top25.count <- head(orders.products.top25.count[order(-orders.products.top25.count$Freq),], 25)
orders.products.top25.count$Product.Name <- factor(orders.products.top25.count$Product.Name, levels = unique(orders.products.top25.count$Product.Name)[order(orders.products.top25.count$Freq, decreasing = TRUE)])

plot_ly(orders.products.top25.count, x = ~Product.Name, y = ~Freq, type = "bar") %>%
  layout(paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor,
         xaxis = list(title = "Продукт"), yaxis = list(title = "Брой"))

# Final price and profit
orders.top50 <- aggregate(cbind(df$FinalPrice, df$Profit), by = list(Order.ID = df$Order.ID), FUN = sum)
orders.top50 <- head(orders.top50[order(-orders.top50$V1),], 50)
round(mean(orders.top50$V2), 2)

plot_ly(orders.top50, x = ~1:50, y = ~V1, name = "Крайна сума", type = "scatter", mode = "lines+markers") %>%
  add_trace(y = ~V2, name = "Печалба", mode = "lines+markers") %>%
  layout(paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor,
         xaxis = list(title = "Поръчка"), yaxis = list(title = "Крайна цена"))

# Top 10 customers
orders.customers.top10 <- aggregate(cbind(df$FinalPrice, df$Profit), by = list(Customer.ID = df$Customer.ID), FUN = sum)
orders.customers.top10 <- head(orders.customers.top10[order(-orders.customers.top10$V1),], 10)
orders.customers.top10$Customer.ID <- factor(orders.customers.top10$Customer.ID, levels = unique(orders.customers.top10$Customer.ID)[order(orders.customers.top10$V1, decreasing = TRUE)])

orders.customers.top10 %>% plot_ly() %>%
  add_trace(x = ~orders.customers.top10$Customer.ID, y = ~orders.customers.top10$V1, name = "Крайна сума", type = "bar") %>%
  add_trace(x = ~orders.customers.top10$Customer.ID, y = ~orders.customers.top10$V2, name = "Печалба", type = "bar") %>%
  layout(paper_bgcolor = style.bgcolor, plot_bgcolor = style.bgcolor,
         xaxis = list(title = "Клиент"), yaxis = list(title = "Сума"))
