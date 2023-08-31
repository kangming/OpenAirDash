#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
multiplot <- function(ls, rows=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(ls)

  numPlots = length(plots)

  print(numPlots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, rows * ceiling(numPlots/rows)),
                     ncol = rows, nrow = ceiling(numPlots/rows))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    # grid.newpage()
    # pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], split = c(matchidx$row, matchidx$col,nrow(layout),ncol(layout)),more = TRUE)
    }
  }
}
