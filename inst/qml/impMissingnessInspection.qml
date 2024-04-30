//
// Copyright (C) 2024 Utrecht University
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

// All Analysis forms must be built with the From QML item
Form
{
    columns: 1

    Formula
    {
        rhs: "variables"
    }

    VariablesForm
    {
        AvailableVariablesList	{ name: "allVariablesList"								}
        AssignedVariablesList	{ name: "variables";		title: qsTr("Variables")	}
        AssignedVariablesList	{ name: "splitBy";			title: qsTr("Split");		singleVariable: true; suggestedColumns: ["ordinal", "nominal"];	id: splitBy }
    }

    Section
    {
        title: qsTr("Missingness Data Pattern")
        columns: 1

        Group
        {
            Group
            {
                columns: 2
                CheckBox {				name: "patternPlots";			label: qsTr("Pattern plots");	id:	patternPlots					}
            }
        }
    }

    Section
    {
        title: qsTr("Influx / Outflux")
        columns: 1

        Group
        {
            Group
            {
                columns: 2
                CheckBox {				name: "observedPlots";	label: qsTr("Observed data correlation plots");	id:	observedPlots			}
                CheckBox {				name: "fluxPlots";			label: qsTr("Influx/Outflux plot");	id:	fluxPlots					}
                CheckBox {				name: "missingPlots";	label: qsTr("Missing data correlation plots");	id:	missingPlots			}

            }
            Group
            {
                enabled: observedPlots.checked || missingPlots.checked

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
    }
}
