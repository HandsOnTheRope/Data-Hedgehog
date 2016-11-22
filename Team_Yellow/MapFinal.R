# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
midwest_map <- function(var, color, legend.title, min = 640, max = 830) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("state", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("north dakota", "south dakota", "nebraska", "kansas", "missouri", "iowa", "minnesota", "wisconsin", "illinois", "indiana", "ohio", "michigan"))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("north dakota", "south dakota", "nebraska", "kansas", "missouri", "iowa", "minnesota", "wisconsin", "illinois", "indiana", "ohio", "michigan"))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min),
                   paste0(min + inc),
                   paste0(min + 2 * inc),
                   paste0(min + 3 * inc),
                   paste0(max))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title,cex = 0.75)
}

northeast_map <- function(var, color, legend.title, min = 640, max = 830) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("state", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("district of columbia", "maryland", "delaware", "penn", "new jersey", "new york", "connecticut", "mass", "rhode island", "vermont", "new hampshire", "maine"))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("district of columbia", "maryland", "delaware", "penn", "new jersey", "new york", "connecticut", "mass", "rhode island", "vermont", "new hampshire", "maine"))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min),
                   paste0(min + inc),
                   paste0(min + 2 * inc),
                   paste0(min + 3 * inc),
                   paste0(max))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title, cex = 0.75)
}

west_map <- function(var, color, legend.title, min = 640, max = 830) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("state", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("california", "oregon", "washington", "alaska", "hawaii"))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("california", "oregon", "washington", "alaska", "hawaii"))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min),
                   paste0(min + inc),
                   paste0(min + 2 * inc),
                   paste0(min + 3 * inc),
                   paste0(max))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title, cex = 0.75)
}

southeast_map <- function(var, color, legend.title, min = 640, max = 830) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("state", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("florida", "georgia", "alabama", "mississippi", "tennessee", "kentucky", "west virginia", "north carolina", "virginia", "south carolina"))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("florida", "georgia", "alabama", "mississippi", "tennessee", "kentucky", "west virginia", "north carolina", "virginia", "south carolina"))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min),
                   paste0(min + inc),
                   paste0(min + 2 * inc),
                   paste0(min + 3 * inc),
                   paste0(max))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title, cex = 0.75)
}

mountainwest_map <- function(var, color, legend.title, min = 640, max = 830) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("state", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("nevada", "utah", "colorado", "wyoming", "idaho", "montana"))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("nevada", "utah", "colorado", "wyoming", "idaho", "montana"))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min),
                   paste0(min + inc),
                   paste0(min + 2 * inc),
                   paste0(min + 3 * inc),
                   paste0(max))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title, cex = 0.75)
}

southwest_map <- function(var, color, legend.title, min = 640, max = 830) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("state", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("arizona", "texas", "new mexico", "louisiana", "oklahoma", "arkansas"))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0),region = c("arizona", "texas", "new mexico", "louisiana", "oklahoma", "arkansas"))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0(min),
                   paste0(min + inc),
                   paste0(min + 2 * inc),
                   paste0(min + 3 * inc),
                   paste0(max))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title, cex = 0.75)
}