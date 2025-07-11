import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name:			"jaspMissingData"
	title:			qsTr("Missing Data")
	description:	qsTr("Inspect, impute, and analyze incomplete data.")
	version:		"0.1.0"
	author:			"UU JASP Team and MICE Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"https://jasp-stats.org"
	license:		"GPL (>= 2)"
	icon:			"impMice.svg"
	hasWrappers:	false
	preloadData:	true

	GroupTitle
	{
		title:	qsTr("Inspection")
		icon:	"impPattern.svg"
	}

	Analysis
	{
		menu:	qsTr("Missing Data Inspection")
		title:	qsTr("Missing Data Inspection")
		func:	"MissingDataInspection"
	}

	Separator {}

	GroupTitle
	{
		title:	qsTr("Imputation")
		icon:	"impImpute.svg"
	}

	Analysis
	{
		menu:	qsTr("Multiple Imputation")
		title:	qsTr("Missing Data Imputation with MICE")
		func:	"MissingDataImputation"
	}
}
