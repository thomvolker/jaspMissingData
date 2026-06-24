//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls


Section
	{
		title: qsTr("Basic Plots")
		columns: 2

		Group
		{
			Row
			{
				spacing: jaspTheme.columnGroupSpacing
				CheckBox
				{
					name: "distributionPlots";	label: qsTr("Distribution plots");	id:	distributionPlots
					info: qsTr("For continuous variables, displays a histogram and the fit of a nonparametric density estimator. For nominal and ordinal variables, displays a frequency distribution.")
				}
				CheckBox
				{
					name: "correlationPlots";	label: qsTr("Correlation plots");	id:	correlationPlots
					info: qsTr("Displays a matrix of plots between continuous variables, with scatterplots between the variables in the off-diagonal entries, and histograms and density plots in the diagonal entries. The line represents the fit of a 1st, 2nd, 3rd, or 4th order polynomial (the selection is based on the Bayesian information criterion; Schwarz, 1978).")
				}
			}

			Group
			{
				enabled: distributionPlots.checked || correlationPlots.checked

				indent:		true
				CheckBox {	name: "distributionAndCorrelationPlotDensity";		label: qsTr("Display density")						}
				CheckBox {	name: "distributionAndCorrelationPlotRugMarks";		label: qsTr("Display rug marks")					}
				DropDown {
					name: "distributionAndCorrelationPlotHistogramBinWidthType"
					label: qsTr("Bin width type")
					indexDefaultValue: 0
					values:
						[
						{label: qsTr("Sturges"),				value: "sturges"},
						{label: qsTr("Scott"),					value: "scott"},
						{label: qsTr("Doane"),					value: "doane"},
						{label: qsTr("Freedman-Diaconis"),		value: "fd"	},
						{label: qsTr("Manual"),					value: "manual"	}
					]
					id: binWidthType
				}
				DoubleField
				{
					name:			"distributionAndCorrelationPlotHistogramManualNumberOfBins"
					label:			qsTr("Number of bins")
					defaultValue:	30
					min:			3;
					max:			10000;
					enabled:		binWidthType.currentValue === "manual"
				}
			}
		}

		Group
		{
			CheckBox {				name: "intervalPlot";	label: qsTr("Interval plots")					}
			CheckBox {				name: "pieChart";		label: qsTr("Pie charts")						}
			CheckBox {				name: "dotPlot";		label: qsTr("Dot plots")						}
			CheckBox 
			{				
				name: "qqPlot";			label: qsTr("Q-Q plots")
				CheckBox
				{
					name:               "qqPlotCi"
					label:              qsTr("Confidence interval")
					childrenOnSameRow:  true
					CIField{ name: "qqPlotCiLevel" }
				}						
			}
		}

		Group
		{
			title: qsTr("Categorical Plots")

			CheckBox
			{
				name: 	"paretoPlot"
				label: 	qsTr("Pareto plots")
				info:	qsTr("Displays the counts of each factor/level within the variable in a descending order. The y-axis represents the frequency (counts as grey bars) of each factor/level, the x-axis represents the factors/levels of the variable in an ordered sequence.") + "<br>"
						+ qsTr("By default, a cumulative line is drawn indicating the proportional contribution of each factor. A second vertical axis to the right side of the graph scales with this cumulative line and represents percentages to enable the description of the cumulative line.") + "<br>"
						+ qsTr("If \"Pareto rule\" is enabled, the two new lines enable a more precise assessment of factor/level contribution to the overall contribution (in percent) by using different input numbers.")

				CheckBox
				{
					name: 				"paretoPlotRule"
					label: 				qsTr("Pareto rule")
					childrenOnSameRow: 	true

					CIField { name: 	"paretoPlotRuleCi"; defaultValue: 80 }
				}
				CheckBox
				{
					name: "paretoShiftAccumulationLine"
					label: qsTr("Shift accumulation line")
					info: qsTr("Shifts the cumulative line so that it starts at the top-right corner of the first bar instead of in the middle. This corresponds to the traditional Pareto chart layout.")
				}

				CheckBox
				{
					name: "paretoPlotTiltXAxisLabels"
					label: qsTr("Tilt x-axis labels")
					info: qsTr("Tilts the x-axis labels by 45 degrees to improve readability when there are many long levels.")
				}

				DropDown
				{
					visible: paretoAddCountVariable.checked
					name: "paretoAddCountVariable"
					label: qsTr("Add count variable")
					showVariableTypeIcon: true
					addEmptyValue: true
					allowedColumns: ["ordinal", "scale"]
					info: qsTr("Allows you to specify an additional variable that contains the counts for each category instead of letting the analysis compute frequencies automatically. "
										+ "This is useful when the data are already aggregated (e.g., each row represents a category with an associated frequency). "
										+ "If left empty, counts are calculated directly from the selected variable.")
				}

				
			}

			CheckBox
			{
				name: 	"likertPlot"
				label: 	qsTr("Likert plots")
				info:	qsTr("Displays a horizontally stacked bar chart showing the contribution of levels within a variable in percent. Order of levels depends on defined order in the JASP data table. A legend below the graph provides an overview of levels and their respective colors in the graph.")
					+ "<ul>"
					+	"<li>" + qsTr("The y-axis represents the variables used, the x-axis represents the percentages. Percentage contribution of all lower-order (below the middle level) and higher-order (above the middle level) levels are displayed on their respective side of the graph.") + "</li>"
					+	"<li>" + qsTr("The graph displays percentages on the x-axis as positive in both directions. Reason for the chosen display (in two directions) is the graphs usefulness in survey research where levels often follow a Likert based order (e.g., high - low, likely - unlikely, agreement - disagreement). Therefore, the graph contains a split between levels at their median.") + "</li>"
					+	"<li>" + qsTr("The number of variable levels determines the number of layers displayed. Layers represent the percentage distribution of the levels of the variable under investigation.") + "</li>"
					+	"<li>" + qsTr("If the variables contain an uneven amount of levels, the middle level is displayed as a grey block in the middle of the stacked bar with its percentage contribution on top.") + "</li>"
					+ "</ul>"

				CheckBox
				{
					name: 				"likertPlotAssumeVariablesSameLevel"
					label: 				qsTr("Assume all variables share the same levels")
					childrenOnSameRow: 	true
				}

				DropDown
				{
					id: 				likertPlotAdjustableFontSize
					name: 				"likertPlotAdjustableFontSize"
					label: 				qsTr("Adjustable font size for vertical axis")
					indexDefaultValue: 	0
					values:
					[
						{label: qsTr("Normal"), 	value: "normal"},
						{label: qsTr("Small"),		value: "small"},
						{label: qsTr("Medium"),		value: "medium"},
						{label: qsTr("Large"),		value: "large"}
					]
				}
			}
		}
	}
