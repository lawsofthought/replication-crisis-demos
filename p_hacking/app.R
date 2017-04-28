library(ggplot2)
library(dplyr)

# I set this to a value that will easily illustrate how a significant result is easy to obtain.
# But almost any seed will allow you to see how the p-value can be lowered by p-hacking.
set.seed(1002)

N <- 250

p.value <- function(Df) anova(lm(chocolate~IQ,data=Df))$`Pr(>F)`[1]
r.sq <- function(Df) summary(lm(chocolate~IQ,data=Df))$r.sq

Df <- data.frame(chocolate = rnorm(N),
                 IQ = rnorm(N, mean=100, sd=15),
                 subject = seq(N),
                 sex = sample(c('male', 'female'), size=N, replace=T),
                 age = sample(c('young', 'old', 'middle.age'), size=N, replace=T))

############################################################################

ui <- fluidPage(

  withMathJax(),
  titlePanel("P-hacking to achieve significant results"),
  fluidRow(
    column(3,
           helpText("Select which subgroups to include in an analysis, and watch the p-value change.",
                    "This is one of many Questional Research Practices (QRPs) that are known as p-hacking.",
                    "You can read more about p-hacking in the Simmons, Nelson & Simonsohn (2011) paper",
		                a("False Positive Psychology: Undisclosed Flexibility in Data Collection and Analysis Allows Presenting Anything as Significant.",
		                  href="http://journals.sagepub.com/doi/abs/10.1177/0956797611417632",
		                  target="_blank"),
		                "This demo is with artificial data, but a more extensive demo, using real world data, is provided by fivethirtyeight.com: ",
		                a("Hack Your Way To Scientific Glory.",
		                  href="https://fivethirtyeight.com/features/science-isnt-broken/",
		                  target="_blank"),
		                "All code for this demo can be found on",
		                a("GitHub.",
		                  href="https://github.com/lawsofthought/replication-crisis-demos",
		                  target='_blank')
		    ),
           checkboxGroupInput('variable', 'Select subgroups to include:',
                              c('Males' = 'male',
                                'Females' = 'female',
                                'Young' = 'young',
                                'Middle aged' = 'middle',
                                'Old' = 'old'),
                                selected = c('male', 'female', 'young', 'old', 'middle')),
           htmlOutput("text")
    ),
    column(4,
           mainPanel(plotOutput(outputId="barPlot")))
  )
)

server <- function(input, output) {

  output$barPlot <- renderPlot({

    # This is a bit of mess, but still.
    if ( !('male' %in% input$variable)) {
      Df.tmp <- filter(Df, sex != 'male')
    } else {
      Df.tmp <- Df
    }

    if ( !('female' %in% input$variable)) {
      Df.tmp <- filter(Df.tmp, sex != 'female')
    }

    if ( !('old' %in% input$variable)) {
      Df.tmp <- filter(Df.tmp, age != 'old')
    }

    if ( !('middle' %in% input$variable)) {
      Df.tmp <- filter(Df.tmp, age != 'middle.age')
    }

    if ( !('young' %in% input$variable)) {
      Df.tmp <- filter(Df.tmp, age != 'young')
    }

    output$text <- renderUI({

      opening.stmt.1 <- "We hypothesize that chocolate lovers have higher IQs. So we collect some data."
      if (dim(Df.tmp)[1] > 0){
        stmt.1 <- sprintf('N=%d. The p-value is p=%2.2f. R<sup>2</sup> is %2.3f.',
                          dim(Df.tmp)[1],
                          p.value(Df.tmp),
                          r.sq(Df.tmp))
        if (p.value(Df.tmp) < 0.05){
          stmt.2 <- sprintf('Your result is <b>significant</b>.')
          stmt <- paste(stmt.1, stmt.2, sep = '<br/>')
        } else {
          stmt <- stmt.1
        }
      } else {
        stmt <- sprintf('N=%d', dim(Df.tmp)[1])
      }
      HTML(paste(opening.stmt.1, stmt, sep = '<br/>'))
    })

    if (dim(Df.tmp)[1] > 0) {

      ggplot(Df.tmp, mapping=aes(x=chocolate, y=IQ)) +
        geom_point(aes(col=sex, shape=age)) +
        geom_smooth(method='lm') +
        xlab("Average chocolate consumption (standard units)") +
        theme_classic()

    } else {

      ggplot(Df.tmp) +
        geom_point() +
        xlim(-2, 2) + ylim(70, 130) +
        xlab("Average chocolate consumption (standard units)") +
        ylab('IQ') +
        theme_classic()

    }


  }, height=500, width=600)
}


shinyApp(ui = ui, server = server)

