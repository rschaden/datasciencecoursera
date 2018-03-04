table = read.csv(file="~/code/data_science/datasciencecoursera/r_programming/week1/hw1_data.csv", header = T)

# table
# head(table)
# tail(table)

names(table)

table[1:2,]

# dim(table)
nrow(table)

# table[152:153,]
tail(table, n = 2)

# table[47,]
table[47, "Ozone"]

sum(is.na(table["Ozone"]))
sum(is.na(table$Ozone))

colMeans(table, na.rm = T)
mean(table$Ozone, na.rm = T)

filteredByOzoneAndTemp <- subset(table, Ozone > 31 & Temp > 90)
mean(filteredByOzoneAndTemp$Solar.R)

filteredByMonthJune = subset(table, Month == 6)
mean(filteredByMonthJune$Temp)

filteredByMonthMay = subset(table, Month == 5)
max(filteredByMonthMay$Ozone, na.rm = T)