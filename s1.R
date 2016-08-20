library(stringr)
library(XML)
library(maps)
library(RCurl)

fileURL <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"
xData <- getURL(fileURL)
heritage_parsed <- htmlParse(xData, encoding = "UTF-8")
tables <- readHTMLTable(heritage_parsed, stringsAsFactors = FALSE)
danger_table <- tables[[2]]
names(danger_table)
danger_table <- danger_table[, c(1,3,4,6,7)]
colnames(danger_table) <- c("name", "locn", "crit", "yins", "yend")
danger_table$name[1:3]
danger_table$crit[1:3]
danger_table$yins <- as.numeric(danger_table$yins)
danger_table$yend
yend_clean <- unlist(str_extract_all(danger_table$yend, "[0-9]{4}", simplify = FALSE))
danger_table$yend <- as.numeric(yend_clean[1:54])
danger_table$yend[1:3]
reg_y <- "[/][ -]*[0-9]*[.]*[0-9]*[;]"
reg_x <- "[;][ -]*[0-9]*[.]*[0-9]+"
y_coords <- str_extract(danger_table$locn, reg_y)
y_coords <- as.numeric(str_sub(y_coords, 3, -2))
danger_table$y_coords <- y_coords
x_coords <- str_extract(danger_table$locn, reg_x)
x_coords <- as.numeric(str_sub(x_coords, 3, -1))
danger_table$x_coords <- x_coords
danger_table$locn <- NULL
danger_table$crit <- str_extract(danger_table$crit, "[A-z]+")
pch <- ifelse(danger_table$crit == "nat", 19, 2)
map("world", col = "darkgrey", lwd = 0.5, mar = c(0.1, 0.1, 0.1, 0.1))
pch <- ifelse(danger_table$crit == "Natural", 19, 2)
points(danger_table$x_coords, danger_table$y_coords, pch = pch)
table(danger_table$crit)
hist(danger_table$yend, freq = TRUE, xlab = "Year when site was put on the list of endagered sites", main = "")
