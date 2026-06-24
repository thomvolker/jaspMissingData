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
		title: qsTr("Statistics")

		Group
		{
			title: qsTr("Sample size")
			CheckBox { name: "valid";			label: qsTr("Valid");	checked: true
						 info: qsTr("Number of valid observations")}
			CheckBox { name: "missing";			label: qsTr("Missing");	checked: true	}
		}

		Group
		{
			title:	qsTr("Quantiles")
			info:	qsTr("Percentile Values")

			DropDown
			{
				name: "quantilesType"
				label: qsTr("Type")
				id: quantilesType	
				indexDefaultValue: 6 // Type 7, the default in R 
				info: qsTr("Method used to compute quantiles (see Hyndman & Fan, 1996; Langford, 2006, for more information). The selection carries over to the inter-quartile range (IQR) and box-plot calculations.  For ordinal variables, only types 1 and 3 are supported; other types fall back to type 3.\n")
				values: [
						{label: qsTr("1"),	                value: 1},
						{label: qsTr("2 (SAS)"),	        value: 2},
						{label: qsTr("3"),	                value: 3},
						{label: qsTr("4"),	                value: 4},
						{label: qsTr("5"),	                value: 5},
						{label: qsTr("6 (Minitab, SPSS)"),	value: 6},
						{label: qsTr("7 (R)"),	            value: 7},
						{label: qsTr("8"),	                value: 8},
						{label: qsTr("9"),	                value: 9}
					]
			}

			CheckBox { name: "quartiles";	label: qsTr("Quartiles"); info: qsTr("Displays the 25th, 50th, and 75th percentiles of the data points.") }
			CheckBox
			{
				name:				"quantilesForEqualGroups"; label: qsTr("Cut points for: ")
				infoLabel:			qsTr("Cut points for x equal groups")
				info:				qsTr("Displays the cut points that divide the data into x equal groups; default is 4 equal groups.")
				childrenOnSameRow:	true

				IntegerField
				{
					name:			"quantilesForEqualGroupsNumber"
					min:			2
					max:			1000
					defaultValue:	4
					afterLabel:		qsTr(" equal groups")
				}
			}

			CheckBox
			{
				name:				"percentiles"
				label:				qsTr("Percentiles:")
				info:				qsTr("Displays the xth percentile; percentile values must be separated by comma.")
				childrenOnSameRow:	true

				TextField
				{
					inputType:	"doubleArray"
					name:		"percentileValues"
					fieldWidth: 60
				}
			}
		}

		Group
		{
			title: qsTr("Central tendency")
			infoLabel: qsTr("Central Tendency (only for continuous variables)")

			CheckBox { name: "mode";			label: qsTr("Mode");				info: qsTr("Mode of the data points; if more than one mode exists, only the first is reported. For nominal and ordinal data, the mode is the most frequent observed value. For continuous data, the mode is the value with highest density estimate (see 'Distribution Plots' -> 'Display density'). If a footnote about multimodality for continuous variables is reported, we recommend visualizing the data to check for multimodality.")	}
			CheckBox { name: "median";			label: qsTr("Median");				info: qsTr("Median of the data points.")					}
			CheckBox { name: "meanArithmetic";	label: qsTr("Mean (arithmetic)");	info: qsTr("Arithmetic mean of the data points; otherwise also colloquially called 'the mean' or 'the average'. Calculated as the sum of all data points divided by the number of data points.") ;	checked: true		}
			CheckBox { name: "meanGeometric";	label: qsTr("Mean (geometric)");	info: qsTr("Geometric mean of the data points; defined only for strictly positive variables. Calculated as the product of all data points, raised to the power of 1 divided by the number of points (the n-th root of the product).");	checked: false	}
			CheckBox { name: "meanHarmonic";	label: qsTr("Mean (harmonic)");		info: qsTr("Harmonic mean of the data points; defined only for strictly positive variables. Calculated as the number of data points divided by the sum of the reciprocals of the data points.");	checked: false	}

		}

		Group
		{
			title:	qsTr("Distribution")

			CheckBox { name: "skewness";		label: qsTr("Skewness");			info: qsTr("Skewness of the distribution of the data points.") }
			CheckBox { name: "kurtosis";		label: qsTr("Kurtosis");			info: qsTr("Kurtosis of the distribution of the data points.") }
			CheckBox { name: "shapiroWilkTest";	label: qsTr("Shapiro-Wilk test");	info: qsTr("Shapiro-Wilk test") }
			CheckBox { name: "sum";				label: qsTr("Sum");					info: qsTr("Sum of the data points.") }
		}

		Group
		{
			title:				qsTr("Dispersion")
			infoLabel:			qsTr("Dispersion (only for continuous variables)")
			columns:			2
			Layout.columnSpan:	parent.columns

			CheckBox { name: "sd";							label: qsTr("Std. deviation");				info: qsTr("Standard deviation of the data points."); checked: true	}
			CheckBox { name: "coefficientOfVariation";		label: qsTr("Coefficient of variation");	info: qsTr("The Coefficient of variation gives us the relative dispersion of the data, in contrast to the standard deviation, which gives the absolute dispersion. For this purpose, the standard deviation is divided by the mean value, so that the unit is truncated away.") }
			CheckBox { name: "mad";							label: qsTr("MAD");							info: qsTr("Median absolute deviation of the data points.") }
			CheckBox { name: "madRobust";					label: qsTr("MAD robust");					info: qsTr("Median absolute deviation of the data points, adjusted by a factor for asymptotically normal consistency.") }
			CheckBox { name: "iqr";							label: qsTr("IQR");							info: qsTr("Interquartile range of the data points; 75th percentile - 25th percentile.") }
			CheckBox { name: "variance";					label: qsTr("Variance");					info: qsTr("Variance of the data points.") }
			CheckBox { name: "range";						label: qsTr("Range");						info: qsTr("Range of the data points; maximum - minimum.") }
			CheckBox { name: "minimum";						label: qsTr("Minimum");						info: qsTr("Minimum value of the data points."); checked: true	}
			CheckBox { name: "maximum";						label: qsTr("Maximum");						info: qsTr("Maximum value of the data points."); checked: true	}
		}


		Group
		{
			title:		qsTr("Inference")

			CheckBox { name: "seMean";	label: qsTr("S.E. mean"); info: qsTr("Standard error of the arithmetic mean.") }

			CheckBox
			{
				name: "meanCi"
				label: qsTr("Confidence interval for the arithmetic mean")

				CIField
				{
					name: "meanCiLevel"
					label: qsTr("Width")
					info: qsTr("width of the confidence interval.")
				}

				DropDown
				{
					name: "meanCiMethod"
					label: qsTr("Method")
					id: meanCiMethod
					indexDefaultValue: 0
					info: qsTr("How should the confidence interval be computed? By default, we use a `T model`, which yields results identical to a one-sample t-test. Alternative options are a normal model (%1), or `Bootstrap`.").arg("$\\bar{x} \\pm z_{95} \\times SE$")
					values:
					[
						{label: qsTr("T model"),	value: "oneSampleTTest"},
						{label: qsTr("Normal model"),	value: "normalModel"},
						{label: qsTr("Bootstrap"),		value: "bootstrap"}
					]
				}
			}

			CheckBox
			{
				name: "sdCi"
				label: qsTr("Confidence interval for std. deviation")
				info: qsTr("a confidence interval for the standard deviation based on bootstrap samples.")

				CIField
				{
					name: "sdCiLevel"
					label: qsTr("Width")
				}

				DropDown
				{
					name: "sdCiMethod"
					label: qsTr("Method")
					id: sdCiMethod
					indexDefaultValue: 0
					info: qsTr("How should the confidence interval be computed? By default, we use an analytical approach (chi-square). The alternative option is `Bootstrap`")
					values:
					[
						{label: qsTr("Analytical (chi-square)"),	value: "chiSquaredModel"},
						{label: qsTr("Bootstrap"),		value: "bootstrap"}
					]
				}
			}

			CheckBox
			{
				name: "varianceCi"
				label: qsTr("Confidence interval for variance")
				info: qsTr("a confidence interval for the variance based on bootstrap samples.")

				CIField
				{
					name: "varianceCiLevel"
					label: qsTr("Width")
				}

				DropDown
				{
					name: "varianceCiMethod"
					label: qsTr("Method")
					id: varianceCiMethod
					indexDefaultValue: 0
					info: qsTr("How should the confidence interval be computed? By default, we use a analytical approach (chi-square). The alternative option is `Bootstrap`")
					values:
					[
						{label: qsTr("Analytical (chi-square)"),	value: "chiSquaredModel"},
						{label: qsTr("Bootstrap"),		value: "bootstrap"}
					]
				}
			}

			Group
			{
				title: qsTr("Bootstrap confidence interval options")
				visible:		meanCiMethod.currentValue == "bootstrap" | sdCiMethod.currentValue == "bootstrap" | varianceCiMethod.currentValue == "bootstrap"

				IntegerField
				{
					name:			"ciBootstrapSamples"
					label:			qsTr("Bootstrap samples")
					info: 			qsTr("the number of bootstrap samples to be used.")
					defaultValue:	1000
					min:			1;
					max:			50000;
				}
			}
		}
		Group
		{
			title:	qsTr("Association matrix")

			CheckBox { name: "covariance";		label: qsTr("Covariance");	info: qsTr("Covariance value."); id:covariance}
			CheckBox { name: "correlation";		label: qsTr("Correlation");	info: qsTr("Pearson's correlation coefficient."); id:correlation}

			DropDown
			{
				name: "associationMatrixUse"
				id : associationMatrixUse
				label: qsTr("Use")
				info: qsTr("How to deal with missing values?")
				enabled:  covariance.checked || correlation.checked
				indexDefaultValue: 0
				values:
				[
					{label: qsTr("Everything"),							value: "everything",			info: qsTr("use all observations, resulting in NA when there are missing values.") },
					{label: qsTr("Complete observations"),				value: "complete.obs",			info: qsTr("missing values are handled by casewise deletion (i.e., only use rows of the data set that are complete).") },
					{label: qsTr("Pairwise compelete observations"),	value: "pairwise.complete.obs",	info: qsTr("use all complete pairs of observations on those variables. This can result in covariance or correlation matrices which are not positive semi-definite.") }
				]
			}
		}

		CheckBox { name: "statisticsValuesAreGroupMidpoints"; label: qsTr("Values are group midpoints"); debug: true }
	}