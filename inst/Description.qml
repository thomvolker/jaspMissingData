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
	hasWrappers: false

	GroupTitle
	{
    title:	qsTr("Missingness")
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
        func:	"Test"
    }
    Analysis
    {
    menu:	qsTr("Correlation")
        title:	qsTr("Correlation")
        func:	"Correlation"
    }
  Separator {}

  GroupTitle
	{
    title:	qsTr("Imputation")
	}
	Analysis
	{
    menu:	qsTr("Multiple Imputation")
		title:	qsTr("Impute the missing data")
        func:	"Imputation"
  }
}
