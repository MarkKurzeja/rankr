#' For this file, the following is useful: library(roxygen2) library(devtools) 
#' From the package parent directory: Create a package with:
#' devtools::create("packagename") You can document it with
#' devtools::document("packagename") and documenting it allows the function to
#' be visible Install it with devtools::install("packagename") which will make
#' it usable to all. 
#' Download from github with: devtools::install_github("username/projectName")

#' @import magrittr
#' @import dplyr
#' @import plyr
#' @import ggplot2
NULL

#' Google Color Pallette Setter
#'
#' This function allows you to use all of the colors that are common in a google
#' charts implementation and can be used with the palette command to set the
#' colors
#' @keywords colors Google
#' @param n The number of colors to select for the palette
#' @export
#' @examples
#' scales::show_col(Google_Color_Palette())
Google_Color_Palette <- function(n = 20){
  return(c("#3366CC", "#DC3912", "#FF9900", "#109618", "#990099",
           "#3B3EAC", "#0099C6", "#DD4477", "#66AA00", "#B82E2E",
           "#316395", "#994499", "#22AA99", "#AAAA11", "#6633CC",
           "#E67300", "#8B0707", "#329262", "#5574A6", "#3B3EAC")[1:n])
}


#' Google Logo Palette
#'
#' This function allows you to use all of the colors that are common in a google
#' logo charts implementation and can be used with the palette command to set
#' the colors
#' @keywords colors Google
#' @param none none
#' @export
#' @examples
#' scales::show_col(Google_Logo_Palette())

Google_Logo_Palette <- function() {
  c("#0057E7","#008744","#D62D20","#FFA700")
}

#' ggplot2 Color Defaults
#'
#' The colors from the great ggplot2 package by Hadley Wickhim which are
#' evenly spaced hues aroudn the hcl colour wheel
#' @keywords colors ggplot2
#' @param n number of colors
#' @export
#' @examples
#' scales::show_col(gg_colors(4))

gg_colors <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

#' Huber Loss Function
#' 
#' The huber loss function is quadratic for x < delta 
#' and then linear past this
#' @keywords huber loss
#' @param x residual value
#' @param delta cutoff point for the loss function
#' @export
#' @examples
#' huber_loss(1)

huber_loss <- function(x, delta = 1) {
  sapply(x, function(i) {
    if (abs(i) < delta) {
      return(1/2 * i * i)
    } else {
      return(delta * (abs(i) - 1 / 2 * delta))
    }
  })
}

#' Modified Huber Loss Function
#' 
#' The modified huber loss function acts like the traditional huber loss
#' function but is analytic and has all derivatives cont.
#' @keywords huber loss
#' @param x residual value
#' @param delta cutoff point for the loss function
#' @export
#' @examples
#' mod_huber_loss(1)

mod_huber_loss <- function(x, delta = 1) { # This one is analytic smooth
  delta^2 * (sqrt(1 + (x / delta)^2)-1)
}

#' Assistance function for ranking
#' 
#' p0 is defined to be a constant for which the raking program builds off of.
#' sum_(i = 1)^(n) p_0 * (mu)^i = 1
#' @keywords p0 rankr
#' @param mu Multiplicative constant
#' @param n Number of items to rank
#' @export
#' @examples
#' determine_p0(.65,10)
#' 
determine_p0 <- function(mu, n) {
  return((1 - mu) / (1 - mu^n))
}

#' Visualization of the Rank Space
#' 
#' A plotting function which tries to give an intuitive view of how things are
#' ranked with different parameters
#' @keywords plot rankr
#' @param mu Multiplicative constant
#' @param n Number of items to rank
#' @param t Power that controls degree of curvature of the Goodness of Fit lines
#' @export
#' @examples
#' plot_rank_2d_contour(mu = 0.25, n = 15)

plot_rank_2d_contour <- function(mu, n = 20, t = 2, weights = 1, plotContour = TRUE) {
  # First plot the rank 2d
  plotx = determine_p0(mu,n) * mu ^ (0:(n - 1))
  plotobj = expand.grid(plotx, plotx)
  plot(
    plotobj,
    pch = 21,
    bg = "red",
    col = "white",
    main = paste("Contour Plot with mu = ", mu, ", n = ", n, ", t = ", t, sep = ""),
    xlab = "x",
    ylab = "y"
  )
  
  # Now add the contour lines for equidistant points across the top
  if (plotContour) {
    p_0 = determine_p0(mu, n)
    
    m = seq(from = 0, to = sqrt(2 * (p_0 - p_0 * mu ^ (n - 1)) ^ 2), length = 15)
    for (i in m) {
      plotx = seq(from = 0, to = i, length = 100)
      ploty = i * (1 - (plotx / i) ^ t) ^ (1 / t)
      lines(-plotx + p_0,-ploty + p_0, lty = 1, col = "blue")
    }
  }
  
}

#' Multifactor Ranking
#' 
#' A plotting function which tries to give an intuitive view of how things are
#' ranked with different parameters
#' @keywords plot rankr multifactor ranking
#' @param data Data frame to rank
#' @param highlow vector of "H", "L", "T" that indicates how to rank columns
#' @param targets vector of NA's and targets to hit, if applicable
#' @param mu is the geometric decreasing parameter
#' @param t is the norm calculation power parameter
#' @export
#' @examples
#' # Find the flowers in iris with the Highest Sepal Length and Lowest width
#' mydata = iris[1:2]
#' ranks <- rankr(mydata, highlow = c("H", "L"), targets = rep(NA,2))
#' plot(mydata[ranks, ], type = "n", xlim = c(4,8), ylim = c(1,5))
#' text(mydata[ranks, ], labels = seq_along(ranks))

rankr <- function(data, highlow = NULL, targets = NULL, mu = 0.99, t = 2, return_weights = F,
                  weights = NULL, display_chart = F, bar_n_display = 10, text_n_display = 30) {
  # ===========================================================================
  # Housekeeping
  # ===========================================================================
  
  # Ensure that we are dealing with only numeric values
  stopifnot(sapply(data, function(x) {
    is.numeric(x)
  }) %>% all)
  
  # Transform the class of data into a matrix for super fast operations
  data <- as.matrix(data)
  
  # Get the parameters for the function
  n_cols = ncol(data)
  n_rows = nrow(data)
  data_input = data
  
  # ===========================================================================
  # First set up each column to be ranked
  # Default is that lower is better for each one if highlow is not specified
  # We are trying to get each column such that the minimum value is the best and
  #      the max is the worst
  # "H" indicates that we take the negative of the column
  # "L" indicates we leave as is,
  # "Target" indicates we take the absolute value of the target minus the value
  #      in the column
  # ===========================================================================
  
  # Ensure that the highlow vector is valid
  if (!is.null(highlow)) {
    # Loop through each of the columns adjusting the values
    for (i in 1:n_cols) {
      if (highlow[i] == "H") {
        # We Take the negative of the column
        data[,i] = -1 * data[,i]
      } else if (highlow[i] == "L") {
        # We do nothing - the column is already ordered
      } else if (highlow[i] == "T") {
        stopifnot(!is.na(targets[i]))
        # Take the absolute value of the difference of the target and values
        data[,i] = abs(targets[i] - data[,i])
      } else {
        stop("There was an invalid character in the highlow input vector")
      }
    }
  }
  
  # ===========================================================================
  # All columns are now adjusted - we rank now based on the maximums of each vector
  # Replace all values with their ranks
  # The default is that equal values are stored with their ranks of the minimum one
  # This is the standard competition ranking: Tie in 2:3 -> 1224
  # ===========================================================================
  
  # Apply the ranking function to each of the columns and subtract one
  # data <- lapply(data, function(x) {rank(x, ties.method = "min") - 1}) %>% as.data.frame
  data <-
    apply(data, 2, function(x) {
      rank(x, ties.method = "min") - 1
    })
  # Replace each line with the formula p_0 * mu^i where i is the rank of that cell
  p_0 = (1 - mu) / (1 - mu ^ n_rows)
  data <- apply(data, 2, function(x) {
    p_0 * mu ^ x
  })
  
  # ===========================================================================
  # We now need to calculate each items norm from the best value possible:
  # (p_0, p_0, ... , p_0)
  # ===========================================================================
  
  # First, determine if the weights are set to the default of all ones or not
  if (is.null(weights)) {
    # Use the default weights
    weights = rep(1, ncol(data))
  }
  
  # Get the ideal point -> one that is first in every category
  ideal_val = rep(p_0, n_cols)
  
  # Get the distance of each row from this ideal point, using the general norm formula
  # and letting the weights scale each of the variables -> Notice how higher weights
  # decrease that variables distance to the "ideal candidate" thus making them closer to
  # ideal than others thus "weighting" that variable higher than another
  data <- apply(data, 1, function(x) {
    sum(((ideal_val - x) / weights) ^ t) ^ (1 / t)
  }) %>% as.numeric
  
  # ===========================================================================
  # Display a customized barchart if the user requests, and then return the
  # ranking vector
  # ===========================================================================
  
  # Display the weight bar chart if that is requested
  if (display_chart) {
    bar_n_display = min(bar_n_display, length(data))
    top_n = sort(data)[bar_n_display:1]
    barplot(
      top_n,
      col = "red",
      horiz = T,
      names.arg = order(data)[bar_n_display:1],
      border = F,
      axes = T,
      xlab = "Weight",
      ylab = "Data",
      main = "Relative Weight Plot of Rankings"
    )
    
    text_n_display = min(length(data), text_n_display)
    df <- data.frame(data = data, n = 1:length(data)) %>%
      slice(1:text_n_display) %>%
      arrange(data)
    
    # Plot the text-position plot to visually see the difference between points
    # This chart is, in my opinion, far better than the barchart, but, alas,
    # the bar chart is standard in identification
    
    ggobj <- ggplot(data = df, aes(x = 1:length(data), y = data)) +
      geom_label(aes(label = n), size = 6) +
      labs(title = "Rank Order Weights Plotted", x = "Rank Order", y = "Weights") +
      theme_gray(base_size = 18)
    print(ggobj)
  }
  # Return data to the user - if they chose weights then return the raw weights
  # and if they do not then return the order
  if (return_weights){
    return(data)
  } else {
    return(order(data))
  }
}





