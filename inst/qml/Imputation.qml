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
import "./regression"	as	Regression

// All Analysis forms must be built with the From QML item
Form
{

	VariablesForm
	{

		AvailableVariablesList { name:	"allVariablesList" }
		AssignedVariablesList
		{
			name: "variables"
			title: qsTr("Variables")
		}

	}

	Group
	{

		title:	qsTr("Parameterization")

		IntegerField
		{
			name:			"nImp"
			defaultValue:	5
			label:			qsTr("Number of Imputation")
			min:			1
		}

		IntegerField
		{
			name:			"nIter"
			defaultValue:	10
			label:			qsTr("Number of Iterations")
			min:			1
		}

		IntegerField
		{
			name:			"seed"
			label:			qsTr("Random Number Seed")
			defaultValue:	235711
		}
	}

	Group
	{

		title:	qsTr("Convergence")

		CheckBox
		{
			name:		"tracePlot"
			label:		qsTr("Trace Plots")
			id:			tracePlot
			checked:	false
		}

		CheckBox
		{
			name:		"densityPlot"
			label:		qsTr("Density Plots")
			id:			densityPlot
			checked:	false
		}

	}

	Regression.RegressionLinear { id: regressionLinear;	visible: true } //runRegression.checked }

		// VariablesForm
		// {

		// 	AvailableVariablesList { name:	"potentialDependent" }
		// 	AssignedVariablesList
		// 	{
		// 		name:			"dependent"
		// 		title:			qsTr("Dependent Variable")
		// 		singleVariable:	true
		// 	}

		// }

		// VariablesForm
		// {

		// 	AvailableVariablesList { name:	"potentialIndependent" }
		// 	AssignedVariablesList
		// 	{
		// 		name:	"independents"
		// 		title:	qsTr("Independent Variables")
		// 	}

		// }

}
