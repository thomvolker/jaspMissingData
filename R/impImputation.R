#
# Copyright (C) 2024 Utrecht University
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

# This is a generated file. Don't change it

ImputationInternal <- function(jaspResults, dataset, options) {

  ready <- (length(options$variables) > 0)

  # variables <- unlist(options$variables)
  # splitName <- options$splitBy
  # makeSplit <- splitName != ""
  # numberMissingSplitBy <- 0
  #
  # if (is.null(dataset)) {
  #   if (makeSplit) {
  #     dataset         <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = splitName)
  #     dataset.factors <- .readDataSetToEnd(columns = variables, columns.as.factor = splitName)
  #   } else {
  #     dataset         <- .readDataSetToEnd(columns.as.numeric = variables)
  #     dataset.factors <- .readDataSetToEnd(columns = variables)
  #   }
  # }
}
