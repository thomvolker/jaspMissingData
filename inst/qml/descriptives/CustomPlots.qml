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
// import "./common"		as Common


Section
	{
		title: qsTr("Customizable Plots")
		columns: 1

        property var candidateVariables
		property int splitByCount: 0

		ColorPalette{}

		CheckBox
		{
			name: "boxPlot";
			label: qsTr("Boxplots")
			info: qsTr("For continuous variables, displays a boxplot. Optionally, the outliers are labelled. Outliers are based on the interquartile range (IQR), i.e., [25th percentile] - 1.5 × IQR and [75th percentile] + 1.5 × IQR. Can also display in color, and has selectable boxplot, violin, and jitter elements for displaying the distribution of the data. This can be split by a categorical variable such as experimental condition.")
			Group {
				columns: 2
				Group {
					CheckBox {	name: "boxPlotBoxPlot";			label: qsTr("Boxplot element"); checked: true	}
					CheckBox {	name: "boxPlotViolin";			label: qsTr("Violin element")					}
					CheckBox {	name: "boxPlotJitter";			label: qsTr("Jitter element")					}
				}
				Group {
					CheckBox {  name: "boxPlotColourPalette";		label: qsTr("Use color palette")				}
					CheckBox {	name: "boxPlotOutlierLabel";		label: qsTr("Label outliers")					}
				}
			}
		}


		CheckBox
		{
			name: "scatterPlot";	label: qsTr("Scatter plots")
			columns: 2
			RadioButtonGroup
			{
				name:	"scatterPlotGraphTypeAbove";
				title:	qsTr("Graph above scatter plot")
				RadioButton { value: "density";		label: qsTr("Density");		checked: true	}
				RadioButton { value: "histogram";	label: qsTr("Histogram")					}
				RadioButton { value: "none";		label: qsTr("None")							}
			}
			RadioButtonGroup
			{
				name:	"scatterPlotGraphTypeRight";
				title:	qsTr("Graph right of scatter plot")
				RadioButton { value: "density";		label: qsTr("Density");		checked: true	}
				RadioButton { value: "histogram";	label: qsTr("Histogram")					}
				RadioButton { value: "none";		label: qsTr("None")							}
			}
			CheckBox
			{
				name: "scatterPlotRegressionLine"
				label: qsTr("Add regression line")
				checked: true
				RadioButtonGroup
				{
					name:	"scatterPlotRegressionLineType";
					RadioButton { value: "linear";	label: qsTr("Linear");	checked: true}
					RadioButton { value: "smooth";	label: qsTr("Smooth")}
				}

				CheckBox
				{
					name: "scatterPlotRegressionLineCi"
					label: qsTr("Show confidence interval")
					checked: true
					childrenOnSameRow: true
					CIField {	name: "scatterPlotRegressionLineCiLevel" }
				}
			}
			CheckBox
			{
				enabled: splitByCount > 0
				name: "scatterPlotLegend"
				label: qsTr("Show legend")
				checked: true
			}
		}

		CheckBox
		{

			name: 		"densityPlot"
			label: 		qsTr("Frequency plots")
			columns: 2
			info: qsTr("visualizes data distributions, adapting to the type of variable selected. For scale variables, it generates histograms or density plots to show continuous data distributions. For categorical variables, it creates bar plots displaying counts or proportions of each category.")

			VariablesForm
			{
				preferredHeight: 100 * preferencesModel.uiScale

				AvailableVariablesList
				{
					name: 				"densityPlotVariables"
					source: 			candidateVariables
				}

				AssignedVariablesList
				{
					name: 				"densityPlotSeparate"
					id: 				densityPlotSeparate
					singleVariable: 	true
					title: 				qsTr("Separate frequencies:")
					allowedColumns: 	["nominal"]
				}
			}

			RadioButtonGroup
			{
				name:	"densityPlotType"
				id: 	densityPlotType
				title:	qsTr("Type for scale variables:")
				info:  qsTr("Whether to display a density plot or histogram.")
				RadioButton { value: "density";		label: qsTr("Density");		checked: true	}
				RadioButton
				{
					value: "histogram"
					label: qsTr("Histogram")
					RadioButtonGroup
					{
						name:	"customHistogramPosition";
						id: 	customHistogramPosition
						title:	qsTr("How to combine separate frequencies")
						info: qsTr("Options for separate bins of the histogram")
						RadioButton { value: "stack";		label: qsTr("Stack");		info: qsTr("Stack: Bars are stacked vertically, combining counts across categories within each bin."); checked: true			}
						RadioButton { value: "identity";	label: qsTr("Identity");	info: qsTr("Identity: Bars are layered on top of each other, with transparency often used to distinguish overlapping data.")	}
						RadioButton { value: "dodge";		label: qsTr("Dodge");		info: qsTr("Dodge: Bars are placed side-by-side, allowing for easy comparison of different categories within each bin.")		}
					}
				}
			}
			RadioButtonGroup
			{
				name:	"densityPlotCategoricalType";
				id: 	densityPlotCategoricalType
				title:	qsTr("Type for categorical variables:")
				RadioButton { value: "count";		label: qsTr("Counts");		checked: true	}
				RadioButton { value: "prop";		label: qsTr("Proportions")					}
				RadioButton { value: "condProp";	label: qsTr("Conditional proportions")							}
				info: qsTr("Display counts, proportions, or conditional proportions (conditional for each category on the x-axis).")

			}
			DoubleField
			{
				name:			"densityPlotTransparency"
				label:			qsTr("Transparency")
				fieldWidth:		32
				defaultValue:	20
				min:			0
				max:			100
				enabled: densityPlotSeparate.count > 0 && ((densityPlotType.value === "density") || (densityPlotType.value === "histogram" && customHistogramPosition.value === "identity"))
			}

		}
		CheckBox
		{

				name: 		"heatmapPlot"
				label: 		qsTr("Tile heatmaps for selected variables")
				columns: 1

			VariablesForm
			{
				preferredHeight: 100 * preferencesModel.uiScale
				AvailableVariablesList
				{
					name: "heatmapVariables"
					source: candidateVariables
				}
				AssignedVariablesList
				{
					name: "heatmapHorizontalAxis"
					label: qsTr("Horizontal axis:")
					singleVariable: true
					allowedColumns: ["nominal"]
					minLevels: 2
				}
				AssignedVariablesList
				{
					name: "heatmapVerticalAxis"
					label: qsTr("Vertical axis:")
					singleVariable: true
					allowedColumns: ["nominal"]
					minLevels: 2
				}
			}

			DoubleField { name: "heatmapTileWidthHeightRatio"; label: qsTr("Width to height ratio of tiles"); negativeValues: false; defaultValue: 1}

			CheckBox
			{
				columns: 2
				name: "heatmapDisplayValue"
				label: qsTr("Display value")
				RadioButtonGroup
				{
					name: "heatmapStatisticContinuous"
					title: qsTr("For scale variables")
					RadioButton { value: "mean";		label: qsTr("Mean");	checked: true }
					RadioButton { value: "median";		label: qsTr("Median") }
					RadioButton { value: "identity";	label: qsTr("Value itself") }
					RadioButton { value: "length";		label: qsTr("Number of observations") }
				}

				RadioButtonGroup
				{
					name: "heatmapStatisticDiscrete"
					title: qsTr("For nominal and ordinal variables")
					RadioButton { value: "mode";		label: qsTr("Mode");	checked: true }
					RadioButton { value: "identity";	label: qsTr("Value itself") }
					RadioButton { value: "length";		label: qsTr("Number of observations") }
				}
				DoubleField { name: "heatmapDisplayValueRelativeTextSize"; label: qsTr("Relative text size"); negativeValues: false; defaultValue: 1 }

			}
			CheckBox { name: "heatmapLegend"; label: qsTr("Display legend")	}

		}
	}