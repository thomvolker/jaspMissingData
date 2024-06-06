import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name : "jaspImputation"
	title : qsTr("Imputation")
	description	: qsTr("Impute missing data by means of Multivariate Imputation by Chained Equations (MICE).")
	version : "0.1"
	author : "JASP Team and MICE Team"
	maintainer : "JASP Team <info@jasp-stats.org>"
	website : "https://jasp-stats.org"
	license : "GPL (>= 2)"
	icon : "impMice.svg"
    hasWrappers: true

	GroupTitle
	{
    title:	qsTr("Missingness")
    icon: 	"impPattern.svg"
	}
	Analysis
	{
    menu:	qsTr("Missingness Inspection")
		title:	qsTr("Inspecting the missing data pattern")
        func:	"MissingnessInspection"
    }
    Analysis
    {
    menu:	qsTr("Test")
        title:	qsTr("Test")
        func:	"MissingnessInspection2"
    }
    Analysis
    {
    menu:	qsTr("Test2")
        title:	qsTr("Test2")
        qml:    "MissingnessInspection3.qml"
        func:	"MissingnessInspection3"
    }
    Analysis
    {
        title: 	qsTr("Flexplot")
        qml:   	"Flexplot.qml"
        func:	"flexplot"
        hasWrapper: true
    }

  Separator {}

  GroupTitle
	{
    title:	qsTr("Imputation")
    icon: 	"impImpute.svg"
	}
	Analysis
	{
    menu:	qsTr("Multiple Imputation")
		title:	qsTr("Impute the missing data")
        func:	"Imputation"
  }
}
