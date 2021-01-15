pacman::p_load(shiny, bayesAB, ggplot2, dplyr, tidyr)

ui <- fluidPage(
    titlePanel("Bayes A/B Testing"),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("part_a",
                         h4("# Participants Group A"),
                         value=15000),
            numericInput("conv_a",
                         h4("# Conversions Group A"),
                         value=100),
            numericInput("part_b",
                         h4("# Participants Group B"),
                         value=15000),
            numericInput("conv_b",
                         h4("# Conversions Froup B"),
                         value=150)
        ),


        mainPanel(
           plotOutput("plot")
        )
))

server <- function(input, output) {
    output$plot <- renderPlot({
        a <- c(rep(1,input$conv_a),rep(0,input$part_a-input$conv_a))
        b <- c(rep(1,input$conv_b),rep(0,input$part_b-input$conv_b))

        AB1 <- bayesAB::bayesTest (a, b,
                                   priors = c ('alpha' = 1, 'beta' = 1),
                                   distribution = 'bernoulli')
        data <- data.frame("Group A" = AB1$posteriors$Probability$A,
                           "Group B" = AB1$posteriors$Probability$B)
        rm(a,b,AB1)
        
        # Posteriors
        
        data %>%
            tidyr::pivot_longer(everything()) %>%
            ggplot2::ggplot(ggplot2::aes(x = value, fill=name)) +
            ggplot2::geom_density(alpha=.4) +
            ggplot2::xlab("") + ggplot2::ylab("") + ggplot2::theme_light() +
            ggplot2::labs(fill = "Groups") + ggplot2::scale_fill_manual(labels = c("A","B"), values = c("red", "blue"))
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
