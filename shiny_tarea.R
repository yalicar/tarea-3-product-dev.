library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
ui <- fluidPage(
    titlePanel("Intereracciones del usuario con graficas"),
    tabsetPanel(
        tabPanel(
            "Graficas Shiny",
            h1(
                "Graficas en Shiny"
            ),
            plotOutput(
                "grafica_base_r"
            ),
            plotOutput(
                "grafica_ggplot2"
            )
        ),
        tabPanel(
            "Interacciones con graficas",
            plotOutput(
                "plot_click_option",
                click = "clk",
                dblclick = "dblclk",
                hover = "hvr",
                brush = "brsh",
            ),
            verbatimTextOutput(
                "click_data"
            ),
            
            DT::dataTableOutput(
                "mtcars_tbl"
            ),
        )
    )
)

server <- function(input, output) {
# ---- Graficas del primer panel
    output$grafica_base_r <- renderPlot({
        plot(
            mtcars$wt,
            mtcars$mpg,
            xlab = "wt",
            ylab = "mpg"
        )
    })
    output$grafica_ggplot2 <- renderPlot({
        diamonds %>%
        ggplot(
            aes(
                x = carat,
                y = price,
                color = color
            )
        ) +
        geom_point() +
        xlab("Precio") +
        ylab("Kilates") +
        ggtitle("Precio de diamantes por kilates")
    })
# ---- Graficas del segundo panel ----
    selected <- reactiveVal(
        rep(
            0,
            nrow(mtcars)
        )
    )
    selected2 <- reactiveVal(
        rep(
            0,
            nrow(mtcars)
        )
    )
    ax <- reactiveVal(
        rep(
            0,
            nrow(mtcars)
        )
    )
    df_clk <- reactiveVal(
        data.frame(0,0)
    )
    updater <- function(l1, l2) {
        changes <- ifelse(l1 == 0 | l2 != 2, F, T)
        l1[changes] <- l2[changes]
        changes <- ifelse(l1 == 0 | l2 != 0, T, F)
        l1[changes] <- l2[changes]
        return(l1)
}
    # ---- Seleccion de puntos + click para cambiar el color de los puntos ----
    observeEvent(
        input$brsh, {
            brushed <- brushedPoints(
                mtcars,
                input$brsh,
                allRows = TRUE
            )$selected_
            selected(replace((brushed | selected()),
            (brushed | selected()) == TRUE, 1))
            ax(updater(selected2(), selected()))
        }
    )
    observeEvent(
        input$clk, {
            cliked <- nearPoints(
                mtcars,
                input$clk,
                allRows = TRUE
            )$selected_
            selected((cliked | selected()) * 1)
            ax(updater(selected2(), selected()))
        }
    )
    observeEvent(
        input$hvr, {
            hovered <- nearPoints(
                mtcars,
                input$hvr,
                allRows = TRUE
            )$selected_
            selected2(replace((hovered | selected2()),
            (hovered | selected2()) == TRUE, 2))
            ax(updater(selected(), selected2()))
        }
    )


    # ---- Doble click para quitar el color ----
    observeEvent(
        input$dblclk, {
            selected(rep(0, nrow(mtcars)))
            selected2(rep(0, nrow(mtcars)))
            ax(updater(selected2(), selected()))
        }
    )
    output$plot_click_option <- renderPlot({
        mtcars$sel <- ax()
        ggplot(mtcars,
        aes(wt, mpg)) +
        geom_point(
            aes(
                color = as.factor(sel)
            )
        ) +
        scale_color_manual(
            values = c("0" = "blue", "1" = "green", "2" = "gray")
        )
    }, res = 96
    )
# ---- Tabla ----

    output$click_data <- renderPrint({
        list(
            click_xy = c(
                input$clk$x,
                input$clk$y
            ),
            doble_click_xy = c(
                input$dblclk$x,
                input$dblclk$y
            ),
            hover_xy = c(
                input$hvr$x,
                input$hvr$y
            ),
            brush_xy = c(
                input$brsh$xmin,
                input$brsh$ymin,
                input$brus$xmax,
                input$brsh$ymax,
                ncol = 2,
                byrow = TRUE
            )
        )
    })

    output$mtcars_tbl <- DT::renderDataTable(
        
        datatable(
            df_clk(),
            options = list(
                pageLength = 5
            )

        )
    )

       

}

shinyApp(ui, server)