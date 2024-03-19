# My variables: order priority,Shipping mode, City/Postal Code,Customer Segment,	Product Category
#Price, Quantity, Sales, Profit 

#Loading Important Packages
#install.packages("shadowtext")
#install.packages("patchwork")
#install.packages("gtsummary")
library(grid)
library(tidyverse)
library(shadowtext)
library(ggplot2)

#Reading the data
df=read.csv("dataclean.csv")
#Understand the data and the types of the variables
str(df)
summary(df)
glimpse(df)
sapply(df, class)

#Quantitative variables 

#Variable 1 Price

p1=ggplot(data = df, aes(x = Unit.Price)) +
  geom_histogram(binwidth = 70, fill = "#98bece" , color = "#336699") +
  ggtitle("Unit Price Histogram") +
  xlab("Unit Price") +
  ylab("Frequency") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  xlim(0, 1000)+ ylim(0,350)

# Boxplot
p2= ggplot(data = df, aes(x = "", y = Unit.Price)) +
  geom_boxplot(fill = "#98bece", color = "#336699") +
  ggtitle("Boxplot of Unit Price") +
  ylab("Unit Price") +
  theme_bw()+ylim(0,500)+theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )


# Density Plot
p3=ggplot(data = df, aes(x = Unit.Price)) +
  geom_density(fill = "#98bece", color = "#336699") +
  ggtitle("Density Plot of Unit Price") +
  xlab("Unit Price") +
  ylab("Density") +
  theme_bw()  +xlim(0, 1000) + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )


# Load the required library
library(cowplot)

# Assuming p1 and p3 are individual ggplot objects

# Combine p1 and p3 using cowplot's plot_grid function
combined_plots <- plot_grid(p1, p3,p2, nrow = 2)

# View the combined plot
print(combined_plots)

summary(df$Unit.Price)


#Variable 2: Shipping cost 

p1=ggplot(data = df, aes(x = Shipping.Cost)) +
  geom_histogram(binwidth = 5, fill = "#98bece" , color = "#336699") +
  ggtitle("Shipping Cost Histogram") +
  xlab("Shipping Cost") +
  ylab("Frequency") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) 
p1

# Boxplot
p2= ggplot(data = df, aes(x = "", y = Shipping.Cost)) +
  geom_boxplot(fill = "#98bece", color = "#336699") +
  ggtitle("Boxplot of Shipping Cost") +
  ylab("Unit Price") +
  theme_bw()+theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

p2

# Density Plot
p3=ggplot(data = df, aes(x = Shipping.Cost)) +
  geom_density(fill = "#98bece", color = "#336699") +
  ggtitle("Density Plot of Shipping Cost") +
  xlab("Shipping Cost") +
  ylab("Density") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )
p3

# Load the required library
library(cowplot)

# Assuming p1 and p3 are individual ggplot objects

# Combine p1 and p3 using cowplot's plot_grid function
combined_plots <- plot_grid(p1, p3,p2, nrow = 2)

# View the combined plot
print(combined_plots)

summary(df$Shipping.Cost)

#Variable 3: Sales
p1=ggplot(data = df, aes(x = Sales)) +
  geom_histogram(binwidth = 45, fill = "#98bece" , color = "#336699") +
  ggtitle("Sales Histogram") +
  xlab("Sales") +
  ylab("Frequency") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )+xlim(0,5000)
p1

# Boxplot

p2= ggplot(data = df, aes(x = "", y = Sales)) +
  geom_boxplot(fill = "#98bece", color = "#336699") +
  ggtitle("Boxplot of Sales") +
  ylab("Sales") +ylim(0,10000)+
  theme_bw()+theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

p2

# Density Plot
p3=ggplot(data = df, aes(x = Sales)) +
  geom_density(fill = "#98bece", color = "#336699") +
  ggtitle("Density Plot of Sales") +
  xlab("Sales") +
  ylab("Density") +
  theme_bw() + theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )+xlim(0,20000)
p3

# Load the required library
library(cowplot)

# Assuming p1 and p3 are individual ggplot objects

# Combine p1 and p3 using cowplot's plot_grid function
combined_plots <- plot_grid(p1, p3,p2, nrow = 2)

# View the combined plot
print(combined_plots)

summary(df$Sales)

##################################################################################33

#Variable 4: Ordered Priority 
library(ggplot2)

# Create a summary of Order.Priority
order_summary <- table(df$Order.Priority)

# Convert the table to a dataframe
order_summary_df <- as.data.frame(order_summary)
colnames(order_summary_df) <- c("Order.Priority", "Frequency")

#Frequency table 
library(formattable)
formattable(order_summary_df)

#Histogram for order priority 

# Calculate percentage
order_summary_df$Percentage <- (order_summary_df$Frequency / sum(order_summary_df$Frequency)) * 100

# Plotting a pie chart with blue shades
blue_palette <- c("#3182bd", "#6baed6", "#9ecae1", "#c6dbef","#c6dbe9")  # Blue shades

p1 <- ggplot(order_summary_df, aes(x = "", y = Frequency, fill = `Order.Priority`)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5), size = 4) +
  coord_polar("y", start = 0) +
  ggtitle("Order Priority Distribution") +
  theme_void() +  # Remove background and gridlines
  theme(legend.position = "right") +
  scale_fill_manual(values = blue_palette)  # Blue color palette

p1


# Grouped Quantity ordered . Customer Segment , order priority 


p2= ggplot(df, aes(fill = Customer.Segment, y = Quantity.ordered.new, x = Order.Priority)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Quantity Ordered Relations") +
  xlab("Order Priority") + ylab("Quantity ordered") + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white")  # Change plot background to white
  ) +
  scale_fill_manual(values = c("#4e79a7", "#336699", "#98bece", "#76b7b2"))  # Adjust fill colors to blue tones

p2
##############################################
library(kableExtra)
# create contingency tables of 2-way table
table_1=table(df$Customer.Segment,df$Order.Priority)
contingency_table_with_totals_1 <- addmargins(table_1, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_1, "html", align = "c", caption = "Table (1): Two-Way Contingency Table for Consumer Segement & Order Priority") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_1), background = ifelse(row(contingency_table_with_totals_1) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table


###############################################
p3= ggplot(df, aes(fill = Customer.Segment, x = Order.Priority)) + 
  geom_bar(position = "dodge") +
  ggtitle("Customer segment & Ordered priorirty Relations") +
  xlab("Order Priority")  + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white")  # Change plot background to white
  ) +
  scale_fill_manual(values = c("#4e79a7", "#336699", "#98bece", "#76b7b2"))  # Adjust fill colors to blue tones

p3
############################################################
#Relationship between order priority and shipping mode

p3= ggplot(df, aes(fill = Ship.Mode, x = Order.Priority)) + 
  geom_bar(position = "dodge") +
  ggtitle("Shipping Mode & Ordered priorirty Relations") +
  xlab("Order Priority")  + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white")  # Change plot background to white
  ) +
  scale_fill_manual(values = c("#4e79a7", "#336699", "#98bece", "#76b7b2"))  # Adjust fill colors to blue tones

p3

#Two way table
library(kableExtra)
# create contingency tables of 2-way table
table_1=table(df$Ship.Mode,df$Order.Priority)
contingency_table_with_totals_1 <- addmargins(table_1, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_1, "html", align = "c", caption = "Table (2): Two-Way Contingency Table for Ship Mode & Order Priority") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_1), background = ifelse(row(contingency_table_with_totals_1) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table


# Pie char for Ship Mode
# Create a summary of  ship mode
order_summary <- table(df$Ship.Mode)

# Convert the table to a dataframe_
order_summary_df <- as.data.frame(order_summary)
colnames(order_summary_df) <- c("Ship.Mode", "Frequency")

#Frequency table 
library(formattable)
formattable(order_summary_df)


# Calculate percentage
order_summary_df$Percentage <- (order_summary_df$Frequency / sum(order_summary_df$Frequency)) * 100

# Plotting a pie chart with blue shades
blue_palette <- c("#3182bd", "#6baed6", "#9ecae1", "#c6dbef","#c6dbe9")  # Blue shades

p5 <- ggplot(order_summary_df, aes(x = "", y = Frequency, fill = Ship.Mode)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5), size = 4) +
  coord_polar("y", start = 0) +
  ggtitle("Ship Mode Distribution") +
  theme_void() +  # Remove background and gridlines
  theme(legend.position = "right") +
  scale_fill_manual(values = blue_palette)  # Blue color palette

p5

#Ship mode and customer segment 
p3= ggplot(df, aes(fill =  Customer.Segment, x =Ship.Mode)) + 
  geom_bar(position = "dodge") +
  ggtitle("Shipping Mode & Customer Segment Relations") +
  xlab("Customer Segment")  + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white")  # Change plot background to white
  ) +
  scale_fill_manual(values = c("#4e79a7", "#336699", "#98bece", "#76b7b2"))  # Adjust fill colors to blue tones

p3

#Two way table
library(kableExtra)
# create contingency tables of 2-way table
table_1=table(df$Ship.Mode,df$Customer.Segment)
contingency_table_with_totals_1 <- addmargins(table_1, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_1, "html", align = "c", caption = "Table (3): Two-Way Contingency Table for Ship Mode & Customer Segment") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_1), background = ifelse(row(contingency_table_with_totals_1) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

################################
#Barcharts for categorical variables
#Ship mode and customer segment 

p3 <- ggplot(df, aes(x = Ship.Mode, fill = "Specific Color")) + 
  geom_bar(position = "dodge") +
  ggtitle("Shipping Mode Bar Charts") +
  xlab("ShipMode")  + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white")  # Change plot background to white
  ) +
  scale_fill_manual(values = c("#4e79a7"), guide = FALSE)  # Adjust fill colors to blue tones

p3

p3 <- ggplot(df, aes(x = Customer.Segment, fill = "Specific Color")) + 
  geom_bar(position = "dodge") +
  ggtitle("Customer Segment  Bar Charts") +
  xlab("Customer Segment")  + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white")  # Change plot background to white
  ) +
  scale_fill_manual(values = c("#4e79a7"), guide = FALSE)  # Adjust fill colors to blue tones

p3



p3 <- ggplot(df, aes(x = Order.Priority, fill = "Specific Color")) + 
  geom_bar(position = "dodge") +
  ggtitle("Order Priority  Bar Charts") +
  xlab("Order Priority")  + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.background = element_rect(fill = "white")  # Change plot background to white
  ) +
  scale_fill_manual(values = c("#4e79a7"), guide = FALSE)  # Adjust fill colors to blue tones

p3




###########################################
#Pie chart for customer segment 
# Create a summary of  Customer.Segment
order_summary <- table(df$ Customer.Segment)

# Convert the table to a dataframe
order_summary_df <- as.data.frame(order_summary)
colnames(order_summary_df) <- c(" Customer.Segment", "Frequency")

# Calculate percentage
order_summary_df$Percentage <- (order_summary_df$Frequency / sum(order_summary_df$Frequency)) * 100

# Plotting a pie chart with blue shades
blue_palette <- c("#3182bd", "#6baed6", "#9ecae1", "#c6dbef","#c6dbe9")  # Blue shades

p5 <- ggplot(order_summary_df, aes(x = "", y = Frequency, fill = ` Customer.Segment`)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5), size = 4) +
  coord_polar("y", start = 0) +
  ggtitle("Customer.Segment Distribution") +
  theme_void() +  # Remove background and gridlines
  theme(legend.position = "right") +
  scale_fill_manual(values = blue_palette)  # Blue color palette

p5

#############################################################
#CORRELATION

# All combinations for all quantitative variables
#install.packages("GGally")
library(corrplot)
library(GGally)

quant= df %>% select(Unit.Price,Sales, Shipping.Cost)

cor=cor(quant)


corrplot.mixed(cor,lower.col="#6baed6",tl.col = "#336699")

plot(quant$Unit.Price,quant$Sales, 
     cex=1, 
     col="#6baed6",
     xlab="Unit Price", ylab="Sales",
     main="A simple scatterplot")

pairs(quant,
      upper.panel = NULL,
      pch = 21,
      bg = c("turquoise"))


########################################################
#Saving r script as pdf
rmarkdown::render("ass4.2.R")

