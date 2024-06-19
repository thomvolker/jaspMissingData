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

	VariablesForm
	{

		AvailableVariablesList { name:	"allVariablesList" }
		AssignedVariablesList
		{
			name: "variables"
			title: qsTr("Variables")
		}

		AssignedVariablesList
		{
			name:			"groupVar"
			title:			qsTr("Split Variable")
			singleVariable:	true
			allowedColumns:	"nominal"
			id:				groupVar
			toolTip:		qsTr("I'm ignoring this, for now. Sorry :(")
		}

	}

	Group
	{
		title:	qsTr("Analyses")

		CheckBox
		{
			name:		"patternPlot"
			label:		qsTr("Response Pattern Plot")
			id:			patternPlot
			checked:	true
		}

		CheckBox
		{
			name:		"fluxTable"
			label:		qsTr("In/Out-Flux Statistics")
			id:			fluxTable
			checked:	false
		}

	}

}
