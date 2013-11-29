library(shiny)

shinyUI(pageWithSidebar(

	#Appl title
	headerPanel("A/B Test Analysis"),
	
	sidebarPanel(
	selectInput("variable", "Significance at Confidence",
	list("95%" = "95",
	"99%" = "99")), 
	
	numericInput("Avisitors", "Number of Control Visitors:", 1000),
	numericInput("Bvisitors", "Number of Variation Visitors:", 1000),
	numericInput("Aconversions", "Conversions in Contol Case:", 200),
	numericInput("Bconversions", "Conversions in Variation Case:", 200),
	
	submitButton("Compute Significance")

	),
	
	mainPanel(
	h3(textOutput("caption")),
		verbatimTextOutput("Confidence_intervalA"),
		verbatimTextOutput("Confidence_intervalB"),
		verbatimTextOutput("Zscore"),
		verbatimTextOutput("pvalue")
	)
	
))
