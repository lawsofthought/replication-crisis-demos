library(dplyr)
library(ggplot2)

z.test <- function(x){
  n <- length(x)
  z <- mean(x) * sqrt(n)
  pnorm(abs(z), lower.tail=F) * 2.0
}

is.significant <- function(x, alpha){
  ifelse(z.test(x) <= alpha, TRUE, FALSE)
}

sample.until.significance <- function(initial.sample.size = 10,
                                      alpha=0.05,
                                      max.sample.size = 100,
                                      sample.incrementation=10){

  # Draw initial sample
  x <- rnorm(initial.sample.size)

  # Is the initial sample significant?
  significant <- is.significant(x, alpha)

  # Repeat until significant (or maximum sample size exceeded)
  while (!significant & length(x) < max.sample.size){

    x <- c(x, rnorm(sample.incrementation))
    significant <- is.significant(x, alpha)
  }

  return(significant)

}


error.rates.with.optional.stopping <- function(max.sample.size = 100,
                                               initial.sample.size = 10,
                                               sample.size.increment = 10,
                                               alpha = 0.05,
                                               iterations = 10000){

  I <- seq(initial.sample.size, max.sample.size, by=sample.size.increment)

  error.rates <- apply(
    replicate(iterations, {
      # Draw max-size sample first
      x <- rnorm(max.sample.size)

      # Calculate the Z statistic after each additional data point
      # This is (the abs value of) the cumulative mean times the
      # sqrt of the cumulative sample size
      z.stats <- abs(cummean(x) * sqrt(seq(length(x))))

      # Calculate the significance after each addtional data point
      significant <- z.stats >= qnorm(1-alpha/2.0)

      # Examine significance at initial.sample.size and every sample.size.increment
      # steps until we reach max.sample.size
      s <- significant[I]

      # A cumulative sum at each value of s tells us how many
      # significant values we've seen until that point.
      # If there is more than 0, then that means we would have
      # stopped at declared a significant result at that stage.
      cumsum(significant[I]) > 0
    }), 1, mean)

  data.frame(sample.size = I,
             error.rate = error.rates)

}

############################################################################

ui <- fluidPage(

  withMathJax(),
  titlePanel("Optional stopping and Type I error rates"),
  fluidRow(
    column(3,
           helpText("If we collect data and test a hypothesis, obtain a non-significant result, but then collect more data and test again, and do so repeatedly,
                    we will increase our Type I error rate.",
                    "In this demo, the null hypothesis is always true. We start with some fixed sample data, and then test the null hypothesis.",
                    "If it is not significant, we then iteratively increment the sample with new data and re-test the hypothesis.",
                    "We do so until we obtain a significant result or until some maximum sample size is reached.",
                    "We plot how the Type I error is inflated by this act of repeated sampling.",
                    "One of the earliest papers on this topic is by Anscombe (1954)",
                    a("Fixed-Sample-Size Analysis of Sequential Observations.",
                      href="https://sci-hub.cc/10.2307/3001665",
                      target='_blank'),
                    "All code for this demo can be found on",
                    a("GitHub.",
                      href="https://github.com/lawsofthought/replication-crisis-demos",
                      target='_blank')
           ),
           sliderInput(inputId = "initial.sample.size",
                       label = "Initial sample size:",
                       min = 5,
                       max = 25,
                       step = 1,
                       value = 10),
           sliderInput(inputId = "sample.size.increment",
                       label = "Sample size increment:",
                       min = 1,
                       max = 10,
                       value = 5),
           sliderInput(inputId = "max.sample.size",
                       label = "Maximum sample size:",
                       min = 50,
                       max = 150,
                       value = 60),
           sliderInput(inputId = "alpha",
                       label = "Nominal Type I error rate:",
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
    s <- error.rates.with.optional.stopping(initial.sample.size = input$initial.sample.size,
                                            max.sample.size = input$max.sample.size,
                                            sample.size.increment = input$sample.size.increment,
                                            alpha = input$alpha)

    g0 <- ggplot(s, mapping=aes(x = sample.size, y=error.rate)) + theme_classic() + xlab('Sample size') + ylab('Probability of significant result')

    if (nrow(s) > 25){
      g0 + geom_point() +
        geom_segment(aes(xend=sample.size, yend=0))
    } else {
      g0 + geom_bar(stat='identity')
    }



  }, height=500, width=500)



  output$text <- renderUI({

    stmt.1 <- sprintf("The nominal Type I error rate is %2.2f.", input$alpha)
    stmt.2 <- sprintf("The initial sample size is %d.", input$initial.sample.size)
    stmt.3 <- sprintf("The maximum sample size we use is %d.", input$max.sample.size)
    stmt.4 <- sprintf("We increment our sample by %d at each step.", input$sample.size.increment)

    HTML(paste(stmt.1, stmt.2, stmt.3, stmt.4, sep = '<br/>'))
  })

}


shinyApp(ui = ui, server = server)

