#
# Copyright (C) 2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#' @export
missingnessInspection3 <- function(jaspResults, dataset, options) {

  # check if ready
  ready <- (options$dependent != "")

  # read in the dataset
  if (ready) {
    if (is.null(dataset)){
      dataset = (.readDataSetToEnd(columns=c(options$variables)))
    } else {
      return(dataset)
    }
  }

  # Create plot
  .missingDataPatternPlot(jaspResults, dataset, options, ready)
}

# Function to create the missing data pattern plot
.missingDataPatternPlot <- function(jaspResults, dataset, options, ready) {
  # Set up the plot parameters
  missing_Plot <- createJaspPlot(title = "Missing Data Pattern", width = 600, height = 450)
  missing_Plot$dependOn(c("variables")) # GV: change if variables input change -
  # GV: other depends may be needed if the top-level function would govern
  # multiple inputs for missingness inspection
  # GV: citation below omitted - ggmice citation more in order?
  # missing_Plot$addCitation("van Buuren, S., & Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67.")

  # setup the JASP object
  jaspResults[["missing_Plot"]] <- missing_Plot

  # Check if the function is ready to run
  if (!ready){
    return()
  }

  # Prepare the data for the missing data pattern plot
  if (is.null(options$variables) || length(options$variables) == 0) {
    missing_Plot$setError("Please select at least one variable.")
    return()
  }

  # Extract selected variables
  # GV: Currently done in the .readDataSetToEnd function in missingnessInspection3.R
  #selected_vars <- unlist(options$variables)
  #selected_data <- dataset[, selected_vars, drop = FALSE]

  # Check if there are any missing values in the selected variables
  if (all(!is.na(selected_data))) {
    missing_Plot$setError("No missing data in the selected variables.")
    return()
  }

  # Create the missing data pattern plot
  # pattern <- mice::md.pattern(selected_data, plot = FALSE) # see 55-58
  # pattern <- ggmice::plot_pattern(data = dataset, rotate = TRUE)

  # Add the plot to the JASP results
  missing_Plot$plotObject <- mice::boys %>% ggplot(aes(x = age, y = hgt)) + geom_point()

  # return the plot to the main function
  return()
}
