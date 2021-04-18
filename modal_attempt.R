


LayoutModuleUI <- function(id){
  
}

LayoutModel <- function(layout) {
  dataModal <- modalDialog(
    
  )
}

LayoutModuleServer <- function(id, Layout, start) {
  moduleServer(
    id,
    function(input, output, session){
      
      dataModal <- function() {
        modalDialog(
          p("The form layout is made up of 'columns' with a width of 1 through 12. ",
            "Multiple columns may be in one row privded there aggregate width is less",
            "than or equal to 12. If a new column pushes the total width to greater than 12,",
            " it will wrap to a new line. Elements added to these columns will be placed next",
            " to one another privided that there is enough space."),
          br(),
          ,
          actionButton("rmcolumn", NULL, icon = icon("minus")),
          actionButton("editcolumn", NULL, icon = icon("edit")),
          br(),
          dataTableOutput("layout_summary"),
          footer = tagList(
            modalButton("Cancel")
          )
        )
      }
      
      observeEvent(input$Layout, {
        showModal(dataModal())
      })
      
      observeEvent(input$ok, {
        
      })
      
      
    }
  )
}


