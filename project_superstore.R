# install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load the dataset and key packages.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv("Sample - Superstore.csv")

# print(head(df))
##############################################################################
## Question 1: How does sales performance vary over time?
# Identify which months consistently show sales peaks

# Convert the Order.Date column to a Date data type.
df <- df %>%
    mutate(Order.Date = as.Date(Order.Date, "%m/%d/%Y"))

# make sure the format was consistent and correctly assumed
num_na_dates <- sum(is.na(df$Order.Date))
cat("Number of NA values in Order.Date after conversion:", num_na_dates, "\n")

# Add month_num column
df <- df %>%
    mutate(
        month_num = month(Order.Date)
    )
# sum total for each month over all years
total_by_month <- df %>%
    filter(!is.na(Order.Date)) %>% 
    group_by(month_num) %>%
    summarise(
        total_sales = sum(Sales, na.rm = TRUE)
    )
print(total_by_month)
# Identify which months consistently show sales peaks
    #    month_num total_sales
    #        <dbl>       <dbl>
    #  1         1      94925.
    #  2         2      59751.
    #  3         3     205005.
    #  4         4     137762.
    #  5         5     155029.
    #  6         6     152719.
    #  7         7     147238.
    #  8         8     159044.
    #  9         9     307650.
    # 10        10     200323.
    # 11        11     352461.
    # 12        12     325294.

    # the data contiuously rises throughout the year (except a dip in October)
    # maxvalues are November and December -> Probably Christmas purchases

    # plot data to visualize 
p_monthly_sales_Total = 
    ggplot(total_by_month, aes(x = month_num, y = total_sales)) +
        geom_line(color = "blue") +
        geom_point() +
        labs(title = "Monthly Sales Trend", y = "Sales", x = "Month")
ggsave("p_monthly_sales_total.png", plot = p_monthly_sales_Total)

# Find any unusual dips in the trend line
    # There is a significant dip in October as shown in "p_monthly_sales_total.png"
    # Also January and February show a significant Dip in sales compared to December


# (Optional) Calculate YOY growth rate for the most recent complete year
# Determine latest date in dataset:
max_date <- max(df$Order.Date, na.rm = TRUE)
cat("Latest date in dataset:", as.character(max_date), "\n")

# function to get the last full year from the maxdate
get_last_full_year <- function(dt) {
  # Convert input to Date class to ensure correct date handling
  dt <- as.Date(dt)  
  # Extract the year part from the date
  yr <- year(dt)  
  # Check if the date is December 31 (end of the year)
  if (month(dt) == 12 && day(dt) == 31) {
    # If the date is exactly the last day of the year,
    # then the last full year is the current year itself
    return(yr)
  } else {
    # Otherwise, if the date is before the end of the year,
    # the last full completed year is the previous year
    return(yr - 1)
  }
}
last_full_year <- get_last_full_year(max_date)
cat("Last full year in the dataset: ", last_full_year, "\n")

yearly_summary <- df %>%
  mutate(Year = year(Order.Date)) %>%
  group_by(Year) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  )
# Extract sales for last full year and previous year
sales_current <- yearly_summary %>% filter(Year == last_full_year) %>% pull(Total_Sales)
sales_prev <- yearly_summary %>% filter(Year == (last_full_year - 1)) %>% pull(Total_Sales)
# Calculate YOY growth rate
yoy_growth <- (sales_current - sales_prev) / sales_prev
# Output:
cat("Sales in ", last_full_year, ":", sales_current, "\n")
cat("Sales in ", last_full_year - 1, ":", sales_prev, "\n")
cat("YOY Growth Rate for", last_full_year, ":", round(yoy_growth * 100, 2), "%\n")
    # Sales in  2016 : 609205.6
    # Sales in  2015 : 470532.5
    # YOY Growth Rate for 2016 : 29.47 %
# Suggest potential reasons for seasonal patterns
    # Christmas sale spikes sales in winter



##############################################################################
## Question 2 Which product categories have the best/worst profit margins?
# Identify which category has the thinnest margins

# Add profit margin 
df <- df %>%
    mutate(Profit_Margin= Profit / Sales)

# Calculate average margin for each category
category_margins <- df %>%
    group_by(Category) %>%
    summarise(
        avg_margin = mean(Profit_Margin, na.rm=FALSE)
    )
# Output
print(category_margins)
# Furniture has the thinnes margin

#   Category        avg_margin
#   <chr>                <dbl>
# 1 Furniture           0.0388
# 2 Office Supplies     0.138
# 3 Technology          0.156

# Calculate the profit margin difference between top and bottom categories
# Calculate top and bottom margins
top_margin <- max(category_margins$avg_margin)
bottom_margin <- min(category_margins$avg_margin)
margin_difference <- top_margin - bottom_margin

# Get corresponding categories
top_category <- category_margins %>%
  filter(avg_margin == top_margin) %>%
  pull(Category)
bottom_category <- category_margins %>%
  filter(avg_margin == bottom_margin) %>%
  pull(Category) 

# Output
cat("Top Category:", top_category, "(", round(top_margin * 100, 2), "% )\n")
cat("Bottom Category:", bottom_category, "(", round(bottom_margin * 100, 2), "% )\n")
cat("Difference:", round(margin_difference * 100, 2), "%\n")
    # Top Category: Technology ( 15.61 % )
    # Bottom Category: Furniture ( 3.88 % )
    # Difference: 11.74 %

# Suggest strategies to improve low-performing categories
    # combine the Top performing category (Technology) with the lowest performing category (Furniture) and design smart furniture :)
    # genereally speaking decreasing purchase prices could and less discounts could also improve profitability


##############################################################################
## Question 3: How do regional performances compare?
# Identify which region has both high sales and high profitability
regions <- df %>%
    group_by(Region) %>%
    summarise(
        avg_sales = mean(Sales),
        sum_sales = sum(Sales),
        avg_profit = mean(Profit),
        sum_profit = sum(Profit)
    ) %>%
    arrange(desc(sum_profit))
print(regions)
    #   Region  avg_sales sum_sales avg_profit sum_profit
    #   <chr>       <dbl>     <dbl>      <dbl>      <dbl>
    # 1 West         226.   725458.       33.8    108418.
    # 2 East         238.   678781.       32.1     91523.
    # 3 South        242.   391722.       28.9     46749.
    # 4 Central      216.   501240.       17.1     39706.

    # West has high sales numbers and highest profitability


# Find any regions with negative profits
    # There are no Regions with negative Profits. 
    # The central Region has the lowest profitability, even though South has more sales
    # Cities and States have negative profits

# Analyze if high sales always correlate with high profits  
    # There are states and cities with negative profitability:
    # Some of these states and cities have high sales numbers.
    # Therefore high sales can also result in negative profit
    # Also, the Central Region has higher sales than the South and a lower profitability

# Propose regional-specific strategies based on findings
    # cities and states with negative Profitability should raise prices and/or give less discounts
    # Souring prices could be lowered by purchasing all regions together and implementing scaling opportunities

states <- df %>%
    group_by(State) %>%
    summarise(
        avg_sales = mean(Sales),
        sum_sales = sum(Sales),
        avg_profit = mean(Profit),
        sum_profit = sum(Profit)
    ) %>%
    filter(sum_profit < 0) %>%
    arrange(sum_profit)
# print(states)
#    State          avg_sales sum_sales avg_profit sum_profit
#    <chr>              <dbl>     <dbl>      <dbl>      <dbl>
#  1 Texas               173.   170188.     -26.1     -25729.
#  2 Ohio                167.    78258.     -36.2     -16971.
#  3 Pennsylvania        198.   116512.     -26.5     -15560.
#  4 Illinois            163.    80166.     -25.6     -12608.
#  5 North Carolina      223.    55603.     -30.1      -7491.
#  6 Colorado            176.    32108.     -35.9      -6528.
#  7 Tennessee           168.    30662.     -29.2      -5342.
#  8 Arizona             158.    35282.     -15.3      -3428.
#  9 Florida             234.    89474.      -8.88     -3399.
# 10 Oregon              141.    17431.      -9.60     -1190.

citites <- df %>%
    group_by(City) %>%
    summarise(
        avg_sales = mean(Sales),
        sum_sales = sum(Sales),
        avg_profit = mean(Profit),
        sum_profit = sum(Profit)
    ) %>%
    filter(sum_profit < 0) %>%
    arrange(sum_profit)
# print(citites)
#    City         avg_sales sum_sales avg_profit sum_profit
#    <chr>            <dbl>     <dbl>      <dbl>      <dbl>
#  1 Philadelphia      203.   109077.      -25.8    -13838.
#  2 Houston           171.    64505.      -26.9    -10154.
#  3 San Antonio       370.    21844.     -124.      -7299.
#  4 Lancaster         215.     9891.     -157.      -7239.
#  5 Chicago           155.    48540.      -21.2     -6655.
#  6 Burlington        867.    21668.     -145.      -3623.
#  7 Dallas            128.    20132.      -18.1     -2847.
#  8 Phoenix           175.    11000.      -44.3     -2791.
#  9 Aurora            171.    11656.      -39.6     -2692.
# 10 Jacksonville      358.    44713.      -18.6     -2324.


##############################################################################
## Question 4: What does customer segmentation reveal?
# Calculate percentage of customers in each segment
segments <- df %>%
  group_by(Segment) %>%
  summarize(customer_count = n(), .groups = "drop") %>%
  mutate(
    percentage_of_total = (customer_count / sum(customer_count)) * 100
  )
print(head(df))
    #   Segment     customer_count percentage_of_total
    #   <chr>                <int>               <dbl>
    # 1 Consumer              5191                51.9
    # 2 Corporate             3020                30.2
    # 3 Home Office           1783                17.8

# Identify which segment generates the most revenue
total_revenue <- df %>%
    group_by(Segment) %>%
    summarize(
        total_revenue = sum(Sales)
    )
    #   Segment     total_revenue
    #   <chr>               <dbl>
    # 1 Consumer         1161401.
    # 2 Corporate         706146.
    # 3 Home Office       429653.
    # The highest revenue is generated in the consumer Segment

print(total_revenue)

# Develop retention strategies for "At Risk" customers
# My Attemt to identify "at Risk" Customers aims to calculate the average time between purchases 
# Those customers, that haven't bought anything for longer than their twice their normal/average gap in between purchases 
# are at risk to buy their products elsewhere. These customer IDs should be targeted with discounts.

# Calculate gaps between purchases per customer
customer_purchase_gaps <- df %>%
  arrange(Customer.ID, Order.Date) %>%
  group_by(Customer.ID) %>%
  summarise(
    purchases = n(),
    avg_gap = ifelse(n() > 1,
                     mean(diff(Order.Date)),
                     NA),  # avg gap in days
    last_purchase = max(Order.Date),
    .groups = "drop"
  ) %>%
  mutate(
    # days_since_last = as.numeric(Sys.Date() - last_purchase) -> if the dataset was current I would use this line
    days_since_last = as.numeric(max_date - last_purchase)
    
  )
# print(customer_purchase_gaps)

# Identify customers with at least 2 purchases and who are overdue
at_risk_customers <- customer_purchase_gaps %>%
  filter(
    purchases > 2,
    !is.na(avg_gap),
    2 * days_since_last > avg_gap
  )%>%
   arrange(desc(days_since_last))

# print(at_risk_customers)
# These Customers could be targeted with a discount:

    #    Customer.ID purchases avg_gap last_purchase days_since_last
    #    <chr>           <int>   <dbl> <date>                  <dbl>
    #  1 GR-14560            5     0.5 2014-11-21               1135
    #  2 CM-12715           13    29.9 2015-03-01               1035
    #  3 VT-21700            9    37.9 2015-04-05               1000
    #  4 PC-19000            3   240   2015-08-01                882
    #  5 AG-10525            9    49.2 2015-09-07                845
    #  6 PF-19120           19    27.8 2015-09-17                835
    #  7 DP-13165            4   104   2015-10-10                812
    #  8 CC-12685           12    52.7 2015-10-23                799
    #  9 SC-20020           17    38.6 2015-11-10                781
    # 10 LS-17230            6   135.  2015-11-14                777


# Suggest marketing approaches for "High Spenders"
#   - Offer early access to new products
#   - Free premium shipping
#   - Use purchase history to recommend high-end or complementary products.
#   - Send exclusive discounts or personal thank-you notes.
#   - Promote higher-margin or premium versions of what they already buy





##############################################################################
## Question 5: How does shipping mode affect profitability?
# Compare profit margins across shipping modes
shipping_mode_margins <- df %>%
    group_by(Ship.Mode) %>%
    summarize(
        avg_margin = mean(Profit_Margin, na.rm=FALSE)
    ) %>%
   arrange(desc(avg_margin))
print(shipping_mode_margins)
    #   Ship.Mode      avg_margin
    #   <chr>               <dbl>
    # 1 Second Class        0.150
    # 2 Same Day            0.138
    # 3 First Class         0.116
    # 4 Standard Class      0.110

profit_per_order <- df %>%
  group_by(Ship.Mode) %>%
  summarize(
    total_profit = sum(Profit, na.rm = TRUE),
    total_orders = n(),  # counts rows (orders)
    profit_per_order = total_profit / total_orders,
    .groups = "drop"
  )
print(profit_per_order)
    #   Ship.Mode      total_profit total_orders profit_per_order
    #   <chr>                 <dbl>        <int>            <dbl>
    # 1 First Class          48970.         1538             31.8
    # 2 Same Day             15892.          543             29.3
    # 3 Second Class         57447.         1945             29.5
    # 4 Standard Class      164089.         5968             27.5

# Suggest optimal shipping strategy based on findings
# The Fact that profit_per_order is higher in FIrst Class and Same day delivery indicates 
# that the absolute sales price is higher. The average margin is highest in Second Class delivery, 
# but Items delivered in this class tend to have lower Prices to begin with.
# I would recommend focussing on the most profitable items
