library(shinydashboard)
library(readr)
#library(tidyverse)
#library(rsconnect)

myData  <- read_csv("sampleData")
myError <- read_csv("sampleError")
mySize  <- read_csv("sampleSize")

ui <- dashboardPage(
	skin = "red",
	dashboardHeader(title = "General Education",
			dropdownMenu(type = "messages",
				   messageItem(
				   	from = "Dr. Franklund",
				     	message = "These data are fictitious.",
				     	icon = icon("exclamation-triangle")
				     )
			)),
	dashboardSidebar(
		titlePanel("  Performance by"),
		selectInput("semester", "Semester:",
			    c("All" = 5,
			      "Fall 2016" = 1,
			      "Spring 2017" = 2,
			      "Fall 2017" = 3,
			      "Spring 2018" = 4))
	),
	dashboardBody(
		# Setting the CSS of the dashboard to look prettier
		tags$head(
			tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
		),

		img(src = 'boxflame.png', align = "left"),
		titlePanel("  Ferris State University"),
		p("This is a demonstration dashboard for the revised General Education program at Ferris State University. It has been created using Shiny and the R programming language. A full dashboard would contain several additional filters in the sidebar and operational instructions with advice on interpretation. This tool is meant to provide a clean and clear overview of the program's status."),
		p(class="status unsatisfactory", "Unsatisfactory"),
		p(class="status beginning", "Beginning"),
		p(class="status progressing", "Progressing"),
		p(class="status proficient", "Proficient"),
		p(class="status advanced", "Advanced"),
		fluidRow(
			valueBoxOutput("overallBox", width = 12)
		),
		fluidRow(
			valueBoxOutput("collBox", width = 6),
			valueBoxOutput("commBox", width = 6),
			valueBoxOutput("cultBox", width = 6),
			valueBoxOutput("divrBox", width = 6),
			valueBoxOutput("nsciBox", width = 6),
			valueBoxOutput("probBox", width = 6),
			valueBoxOutput("qualBox", width = 6),
			valueBoxOutput("ssocBox", width = 6)
		),
			div(id="disqus_thread",
			    HTML(
			    	"(function() { // DON'T EDIT BELOW THIS LINE
var d = document, s = d.createElement('script');
s.src = 'https://dashboard-3.disqus.com/embed.js';
s.setAttribute('data-timestamp', +new Date());
(d.head || d.body).appendChild(s);
})();
</script>
<noscript>Please enable JavaScript to view the <a href="https://disqus.com/?ref_noscript">comments powered by Disqus.</a></noscript>"
			    )
			),
		div(HTML('<script id="dsq-count-scr" src="//dashboard-3.disqus.com/count.js" async></script>'))




	)
)

server <- function(input, output) {
	myColor <- function(score) {
		if(score >= 3.4) {
			return("blue")
		}
		else if(score>=2.6) {
			return("green")
		}
		else if(score >= 1.8) {
			return("orange")
		}
		else if(score >= 1.0) {
			return("red")
		}
		else {
			return("purple")
		}
	}

	output$overallBox <- renderValueBox({
		valueBox(
			paste(sprintf("%0.2f", myData[as.integer(input$semester),10]),
			      "±",
			      sprintf("%0.2f", myError[as.integer(input$semester),10]),sep = " "),
			paste("OVERALL   (n = ",prettyNum(mySize[as.integer(input$semester),10],big.mark = ","),")",sep = ""), icon = icon("list"),
			color = myColor(myData[as.integer(input$semester),10])
		)
	})
	output$collBox <- renderValueBox({
		valueBox(
			paste(sprintf("%0.2f", myData[as.integer(input$semester),2]),
			      "±",
			      sprintf("%0.2f", myError[as.integer(input$semester),2]),sep = " "),
			paste("Collaboration   (n = ",prettyNum(mySize[as.integer(input$semester),2],big.mark = ","),")",sep = ""), icon = icon("handshake-o"),
			color = myColor(myData[as.integer(input$semester),2])
		)
	})
	output$commBox <- renderValueBox({
		valueBox(
			paste(sprintf("%0.2f", myData[as.integer(input$semester),3]),
			      "±",
			      sprintf("%0.2f", myError[as.integer(input$semester),3]),sep = " "),
			paste("Communication   (n = ",prettyNum(mySize[as.integer(input$semester),3],big.mark = ","),")",sep = ""), icon = icon("comments"),
			color = myColor(myData[as.integer(input$semester),3])
		)
	})
	output$cultBox <- renderValueBox({
		valueBox(
			paste(sprintf("%0.2f", myData[as.integer(input$semester),4]),
			      "±",
			      sprintf("%0.2f", myError[as.integer(input$semester),4]),sep = " "),
			paste("Culture   (n = ",prettyNum(mySize[as.integer(input$semester),4],big.mark = ","),")",sep = ""), icon = icon("book", lib = "glyphicon"),
			color = myColor(myData[as.integer(input$semester),4])
		)
	})
	output$divrBox <- renderValueBox({
		valueBox(
			paste(sprintf("%0.2f", myData[as.integer(input$semester),5]),
			      "±",
			      sprintf("%0.2f", myError[as.integer(input$semester),5]),sep = " "),
			paste("Diversity   (n = ",prettyNum(mySize[as.integer(input$semester),5],big.mark = ","),")",sep = ""), icon = icon("globe", lib = "glyphicon"),
			color = myColor(myData[as.integer(input$semester),5])
		)
	})
	output$nsciBox <- renderValueBox({
		valueBox(
			paste(sprintf("%0.2f", myData[as.integer(input$semester),6]),
			      "±",
			      sprintf("%0.2f", myError[as.integer(input$semester),6]),sep = " "),
			paste("Natural Sciences   (n = ",prettyNum(mySize[as.integer(input$semester),6],big.mark = ","),")",sep = ""), icon = icon("leaf", lib = "glyphicon"),
			color = myColor(myData[as.integer(input$semester),6])
		)
	})
	output$probBox <- renderValueBox({
		valueBox(
			paste(sprintf("%0.2f", myData[as.integer(input$semester),7]),
			      "±",
			      sprintf("%0.2f", myError[as.integer(input$semester),7]),sep = " "),
			paste("Problem Solving   (n = ",prettyNum(mySize[as.integer(input$semester),7],big.mark = ","),")",sep = ""), icon = icon("wrench", lib = "glyphicon"),
			color = myColor(myData[as.integer(input$semester),7])
		)
	})
	output$qualBox <- renderValueBox({
		valueBox(
			paste(sprintf("%0.2f", myData[as.integer(input$semester),8]),
			      "±",
			      sprintf("%0.2f", myError[as.integer(input$semester),8]),sep = " "),
			paste("Quantitative Literacy   (n = ",prettyNum(mySize[as.integer(input$semester),8],big.mark = ","),")",sep = ""), icon = icon("signal", lib = "glyphicon"),
			color = myColor(myData[as.integer(input$semester),8])
		)
	})
	output$ssocBox <- renderValueBox({
		valueBox(
			paste(sprintf("%0.2f", myData[as.integer(input$semester),9]),
			      "±",
			      sprintf("%0.2f", myError[as.integer(input$semester),9]),sep = " "),
			paste("Self and Society   (n = ",prettyNum(mySize[as.integer(input$semester),9],big.mark = ","),")",sep = ""), icon = icon("group"),
			color = myColor(myData[as.integer(input$semester),9])
		)
	})



}

shinyApp(ui, server)
