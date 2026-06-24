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
		title: qsTr("Tables")

		CheckBox
		{
			name:			"frequencyTables"
			label:			qsTr("Frequency tables")
			infoLabel:		qsTr("Frequency Tables (nominal and ordinal variables)")
			info:			qsTr("Displays a frequency table for each variable.")
			IntegerField
			{
				name:			"frequencyTablesMaximumDistinctValues"
				label:			qsTr("Maximum distinct values")
				min:			1
				defaultValue:	10
				fieldWidth:		50
				max:			2e2
			}
		}
		CheckBox
		{
			name	: "stemAndLeaf";
			label	: qsTr("Stem and leaf tables")
			info	: qsTr("Displays the spread of a variable.")
					+ "<ul>"
					  +	"<li>" + qsTr("Stem: the first digit(s).") + "</li>"
					  +	"<li>" + qsTr("Leaf: the first digit after the stem.") + "</li>"
					+ "</ul>"

			DoubleField
			{
				name: "stemAndLeafScale";	label: qsTr("scale");	negativeValues: false;	inclusive: JASP.MaxOnly;	max: 200;	defaultValue: 1.0;
				info: qsTr("The scale parameter controls how much the table is expanded. For example, scale = 2 will cause the table to be roughly twice as long as the default (scale = 1).")
			}
		}
	}