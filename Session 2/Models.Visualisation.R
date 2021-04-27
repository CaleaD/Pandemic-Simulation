
###################
### Vizualizare ###
###################


### A.) Interfete Interactive
# TODO: urmatoarele ore;

# A.1.) Shiny app:
# - links & gallery:
#   https://shiny.rstudio.com/
#   https://shiny.rstudio.com/gallery/

# A.2.) Dashboards:
# - de evaluat: shinydashboard, flexdashboard;

library("shiny")
library("shinyjs")
library("shinyBS") # Buttons & Components
library("shinydashboard") # diverse dashboard-uri
library("flexdashboard")


###########

### Colours

### Function colors():
# - displays the names of all available colors;
# - find.col(): helper function (see below);

### Function heat.colors():



####################
### Helper Functions

find.col = function(name="red", start=1, max=30, bottom.mrg=8, ...) {
	is.col = grepl(name, colors());
	n.max = min(sum(is.col), start + max - 1);
	id = seq(start, n.max);
	name.col = colors()[is.col][id]
	x = rep(1, length(id)); names(x) = name.col;
	# set bottom margin
	old.par = par(mar=c(bottom.mrg,1,2,1) + 0.1)
		barplot(x, col=name.col, las=3, ...)
	par(old.par)
	invisible(name.col)
}
plot.col = function(col, bottom.mrg=8, ...) {
	x = rep(1, length(col)); names(x) = names(col);
	# set bottom margin
	old.par = par(mar=c(bottom.mrg,1,2,1) + 0.1)
		barplot(x, col=col, las=3, ...)
	par(old.par)
	invisible()
}


######################

### Examples

find.col()
find.col("green")
find.col("pale")

plot.col(heat.colors(30))


