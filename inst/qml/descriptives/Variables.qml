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

VariablesForm
{
    property var candidateVariables
    property alias variablesControl: variablesControl
    property alias splitByControl: splitByControl

    infoLabel: qsTr("Input")

    AvailableVariablesList {
        name: "descVariablesList"
        source: candidateVariables
    }

    AssignedVariablesList {
        name: "variables"
        id: variablesControl
        title: qsTr("Variables")
        info: qsTr("All variables of interest.")
        allowTypeChange: true
    }

    AssignedVariablesList {
        name: "splitBy"
        id: splitByControl
        title: qsTr("Split")
        info: qsTr("Can be split by a categorical variable such as experimental condition.")
        singleVariable: true
        allowedColumns: ["nominal"]
        minLevels: 2
        maxLevels: 256
    }
}