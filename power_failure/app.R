library(ggplot2)
library(dplyr)
library(pwr)

sample.effects <- function(N, true.effect, alpha=0.05, samples=10000){
  z.critical <- qnorm(1-alpha/2.0)
  x <- replicate(samples, mean(rnorm(N, true.effect)))
  z <- sqrt(N) * abs(x)

  data.frame(effects = x[z > z.critical]) %>%
    mutate(sign.error = sign(effects) != sign(true.effect))
}

z.test.power <- function(true.effect, sample.size, alpha=0.05){
  pwr.norm.test(d=true.effect, n=sample.size, sig.level=alpha)$power
}

############################################################################

ui <- fluidPage(

  withMathJax(),
  titlePanel("How low power inflates effect size"),
  fluidRow(
    column(3,
           helpText("Here, we demonstrate that in low powered studies, even if there is a true effect, its estimated value is often inflated.",
		    "You can learn more about the perils of small sample sizes, and low powered studies generally, in Button et al (2013):",
		    a("Power failure: why small sample size undermines the reliability of neuroscience.",
		      href="https://www.nature.com/nrn/journal/v14/n5/pdf/nrn3475.pdf",
		      target="_blank"),
		    "All code for this demo can be found on",
		    a("GitHub.",
		      href="https://github.com/lawsofthought/replication-crisis-demos",
		      target='_blank')
		    ),
           sliderInput(inputId = "true.effect",
                       label = "True effect size:",
                       min = 0.0,
                       max = 0.8,
                       step = 0.05,
                       value = 0.25),
           sliderInput(inputId = "sample.size",
                       label = "Sample size:",
                       min = 5,
                       max = 50,
                       step = 5,
                       value = 25),
           sliderInput(inputId = "alpha",
                       label = "Type I error rate:",
                       min = 0.01,
                       max = 0.1,
                       step = 0.01,
                       value = 0.05),
           htmlOutput("text")
    ),
    column(4,
           mainPanel(plotOutput(outputId="barPlot")))
  )
)

server <- function(input, output) {

  output$barPlot <- renderPlot({

    sampled.effects <- sample.effects(N = input$sample.size,
                                      true.effect = input$true.effect,
                                      alpha=input$alpha)

    output$text <- renderUI({

      power <- z.test.power(true.effect = input$true.effect,
                            sample.size = input$sample.size,
                            alpha = input$alpha)

      sign.error.rate <- mean(sampled.effects$sign.error,
                              na.rm=T)

      correct.sign.effects.mean <- mean(filter(sampled.effects,
                                               sign.error == FALSE)$effects,
                                        na.rm=T)

      stmt.1 <- sprintf("With a true effect size of %2.2f, a sample size of %d,
                      and a Type 1 error rate of %2.2f, the statistical power is %2.2f",
                        input$true.effect,
                        input$sample.size,
                        input$alpha,
                        power)

      stmt.2 <- sprintf('Of all the effects (in the correct direction), their average value is %2.2f.
                        This is an effect inflation rate of %d%%.',
                        correct.sign.effects.mean,
                        as.integer(100 * (correct.sign.effects.mean/input$true.effect)) - 100)


      HTML(paste(stmt.1, stmt.2, sep = '<br/>'))
    })

    ggplot(sampled.effects,
           aes(effects)) + geom_histogram(color="black", fill="white", bins=50) + theme_classic()

  }, height=500, width=500)



}


shinyApp(ui = ui, server = server)

