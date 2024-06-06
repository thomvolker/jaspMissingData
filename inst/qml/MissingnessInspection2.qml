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
        AvailableVariablesList
        {
            name: "allVariablesList"
        }

        AssignedVariablesList
        {
            name: "variables"
            title: qsTr("Variables")
        }
    }

    Group
    {
      Group
       {
                columns: 4
                CheckBox {				name: "patternPlots";			label: qsTr("Pattern plots");	id:	patternPlots				}
                CheckBox {				name: "fluxPlots";			label: qsTr("Influx/Outflux plot");	id:	fluxPlots					}
                CheckBox {				name: "observedPlots";	label: qsTr("Observed data correlation plots");	id:	observedPlots		}
                CheckBox {				name: "missingPlots";	label: qsTr("Missing data correlation plots");	id:	missingPlots		}
        }
    }
}
