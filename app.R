# Load two packages: shiny (for using Shiny) + shinythemes (for overall layout)
library(shiny)
library(shinythemes)

# R usually presets the working directory to wherever this app code resides
# If this doesn't work (or you wish to change the working directory for another reason),
# uncomment and update the following line to point to where the Douglas/Sarpy venue list is stored
# and make sure its filename is Douglas-Sarpy.csv (setwd stands for set working directory)
# setwd('C:\\Users\\Irina\\Documents\\R\\ComplyChek')

## The Shiny Web app contains 2 basic components: UI and Server.


## UI = User Interface.
# This is where we set the initial layout of the web app, which is what will display as soon as the app loads.
# Other UI components will be inserted later in the program, when we're ready for them.

ui <- fluidPage(
  # Here is where we use shinythemes pkg (from the library loading step)
  # which comes with pre-built CSS/layout code that controls the app's appearance
  theme = shinytheme("cerulean"),
  
  navbarPage("ComplyChek", # this is navigation bar at the top of the app, with the name of our tool at the top left
             
             tabPanel("Main Tool", # this is the name of the tab where our ComplyChek tool lives
                      
                      sidebarPanel(  # this is where we indicate what will appear in the left nav bar in the app
                        
                        # Display multiple UI widgets to allow users to load a CSV file into the web app.
                        # A "div" is an html element that defines a section of the code.
                        # Wrapping the file options in div makes it easy to delete later in the loadCriteriaSidePanel function.
                        tags$div(id='csvFileOptionPanel',
                                 actionButton("existingFile", "Use the existing venue data"),
                                 
                                 hr(), # this is a horizonal rule dividing the two options (though its barely visible on the app)
                                 
                                 # Here is where we create the option for users to upload their own Douglas/Sarpy data
                                 fileInput("file1", "Or upload a new CSV file",
                                           # ensure we get an actual csv file
                                           accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"
                                           )),
                                 
                                 # Here is where we create an input for users to indicate the type of separators in their .csv file
                                 # Most frequently used separator (comma) is already pre-selected for convenience
                                 radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
                                 
                                 # Here is where we create an input for users to indicate the type of quotes in their .csv file
                                 # The most frequently selected quote type (double quote) is already pre-selected for convenience
                                 radioButtons("quote", "Quote", choices = c(None = "", "Single Quote" = "'", "Double Quote" = '"'), selected = '"'),
                        )
                      ),
                      
                      mainPanel( # this is where we'll be displaying output data in the web app (right side of app)
                        htmlOutput("topText"), # this is where our various helper texts will display (at the top of main panel)
                        
                        DT::dataTableOutput("dataTable") #this is where our Douglas/Sarpy data will eventually display
                      )
             ), # Navbar 1, apart from the ComplyChek tool, there will be 4 additional tabs, ie.:
             
             tabPanel("Venue List", 
              # sidebarPanel(),
               mainPanel( # this is where we'll be displaying output data in the web app (right side of app)
                 htmlOutput("venueListTopText"), # this is where our various helper texts will display (at the top of main panel)
                 
                 DT::dataTableOutput("venueList") #this is where our Douglas/Sarpy data will eventually display
               )
               
             ),
              tabPanel("Log",
                sidebarPanel(
                  selectInput(inputId = "logFileList", label = "Please select a log entry you would like to view.", choices = "Empty List"),
                ),
                mainPanel(
                  htmlOutput("logTopText"),
                  DT::dataTableOutput("logResults"),
                  htmlOutput("logMiddleText"),
                  DT::dataTableOutput("logUsedDataSet")
                )
              ),
              tabPanel("Documentation", "This tab will contain the documentation for the tool."),
              tabPanel("About", "This tab will have information about the project and authors.")
  ) # closes navbarPage block
) # closes fluidPage block


##
## The SERVER component contains all logic that's run in the backend by the app.
##

# The server function has 3 arguments:
# input (inputs provided by user)
# output (outputs that we defined in the UI)
# session (which distinguishes between different users)
server <- function(input, output, session) {
  print("Server function executed") # This text goes into the Console just for troubleshooting purposes

  # Create a variable to eventually hold the contents of the .csv file selected by the user.
  # Set to NULL until we have something to put into it
  dataFrame <- NULL

  # The filename of the stored csv file
  storedCSVFile <- './Douglas-Sarpy.csv'

  # The "Venue List" tab just shows the contents of the currently stored .csv file
  output$venueList <- DT::renderDataTable({
    output$venueListTopText <- renderText("<h4>This is the data currently stored in the app.")

    read.csv(storedCSVFile,
              header = TRUE,
              sep = ",", # value of the "sep" widget (delimiter type)
              quote = '"', # value for the "quote" widget (quote type)
              strip.white = TRUE) # removes any extraneous whitespace
   })

  # Handles initial loading of the CSV file
  # Either uploaded via the user interface or from the copy stored on the server
  output$dataTable <- DT::renderDataTable({
    print("output$dataTable <- DT::renderDataTable() running")

    # We can proceed only if the existingFile button has been pressed or a file has been uploaded,
    if (input$existingFile == 1 || !is.null(input$file1)) {
      if (is.null(input$file1)) {
        # If the existingFile button was pressed, then just load ./Douglas-Sarpy.csv
        filepath <- storedCSVFile
      }
      else
      {
        # Require that the uploaded file must exist
        req(input$file1)
        
        # This is where the file is on the server 
        filepath <- input$file1$datapath
        
        # Check to make sure the file being uploaded is really a .csv file, otherwise display an error message
        validate(need(tools::file_ext(filepath) == "csv", "Please upload a valid csv file"))
      }

      # This is just for troubleshooting purposes
      print(paste0("File option chosen. Filenpath is ", filepath)) 
      
      # Load the content in the CSV file into the dataFrame (variable) created earlier.
      # <<- is used because the dataFrame variable exists one "level" up
      dataFrame <<- read.csv(filepath,
                             header = TRUE,
                             sep = input$sep, # value of the "sep" widget (delimiter type)
                             quote = input$quote, # value for the "quote" widget (quote type)
                             strip.white = TRUE) # removes any extraneous whitespace

      # This replaces the file uploading sidepanel options (seen on the "file upload" page)
      # with ones related to working with the loaded data (seen on the "data output" page)
      loadCriteriaSidePanel(dataFrame)
    }
    else
    {
      print("No .csv file selected yet. Waiting for file input selection")
    }

    # Display some useful usage instructions to the user
    output$topText <- renderText("<h4>Welcome to the ComplyChek app.<br /><br />In order to assist you with random venue selection, the app first needs to know your data source. Here, you have the option to either use the venue data already stored in the app or upload your own .csv file. If you'd like to view the existing venue data, simply navigate to the Venue List tab.<br /><br />If you choose to upload your own .csv file, please confirm that your file contains the following case-sensitive headers:<br />
<ul>
  <li>Class</li>
  <li>License</li>
  <li>City</li>
  <li>Zip</li>
  <li>Limits</li>
</ul>
The order of your columns does not matter and your file can contain as many other columns as you like. If needed, you have the option to indicate how your data fields are separated (commas, semicolons, or tabs) and the type of qualifier (none, single quotes, double quotes). The most common configuration has already been pre-selected for you.</h4>")

    # Now is also a good time to load the Log tab dropdown
    reloadLogTab(session)

    # As this function runs before a CSV file is loaded, there is not yet
    # any data to display in output$dataTable
    return (NULL)
  })

  # Handle the reset button
  observeEvent(input$resetApp, {session$reload()})

  # Displays the appropriate log entry (as selected from the dropdown)
  # Initially shows the most recent one, as it's the first one in the dropdown
  observeEvent(input$logFileList, {
    print(paste0("Log dropdown set to: ", input$logFileList))

    # Only attempt to display a log entry after they have been loaded
    if (input$logFileList != "Empty List")
    {
      # Each log entry consists of three files: (1) a .csv file of the set of venues
      # (2) a .csv file of the selection results, and (3) a text file with details

      # Get the contents of the desired log's details file
      filename <- paste0('./log/details/', input$logFileList)
      logDetails <- readChar(filename, file.info(filename)$size)

      output$logTopText <- renderText(paste0("<h3>This is a log of a random selection run on:<br />", input$logFileList, "</h3><br />", logDetails, "<h4>These are the venues that were selected:</h4><br />"))

      # Get the contents of the results .csv for the desired log
      logResults <- read.csv(paste0("./log/results/", input$logFileList, ".csv"))

      output$logResults <- DT::renderDataTable({
        DT::datatable(logResults, rownames = FALSE)
      })

      output$logMiddleText <- renderText(paste0("<br /><h4>This is the full set of venues from which the above results were selected.</h4><br /><br />"))

      # Get the contents of the full venue set .csv for the desired log  
      logDataFrame <- read.csv(paste0('./log/dataframes/', input$logFileList, ".csv"))

      output$logUsedDataSet <- DT::renderDataTable({
        DT::datatable(logDataFrame, rownames = FALSE)
      })
    }
  })

  # Whenever choices are made in any of the class/cities/zips/etc. dropdowns, the matching venue
  # list needs to be completely regenerated. These observe events detect any changes to the
  # dropdowns, update the venue list, and then update the table with the new venue list.
  
  # Observe classes dropdown entry change event
  observeEvent(input$classes, {
    print(paste0("Class dropdown: ", toString(input$classes)))
    output$dataTable <- DT::renderDataTable({
      DT::datatable(selectDesiredEntries(dataFrame, input, output, session), rownames = FALSE)
    })
  })
  
  # Observe cities dropdown entry change event
  observeEvent(input$cities, {
    print(paste0("Cities dropdown contains: ", toString(input$cities)))
    output$dataTable <- DT::renderDataTable({
      DT::datatable(selectDesiredEntries(dataFrame, input, output, session), rownames = FALSE)
    })
  })
  
  # Observe zip code dropdown entry change event
  observeEvent(input$zips, {
    print(paste0("Zips dropdown contains: ", toString(input$zips)))
    output$dataTable <- DT::renderDataTable({
      DT::datatable(selectDesiredEntries(dataFrame, input, output, session), rownames = FALSE)
    })
  })
  
  # Observe limits dropdown entry change event
  observeEvent(input$limits, {
    print(paste0("City Limits dropdown contains: ", input$limits))
    output$dataTable <- DT::renderDataTable({
      DT::datatable(selectDesiredEntries(dataFrame, input, output, session), rownames = FALSE)
    })
  })

  # Observe exclude venues by dropdown entry change event
  observeEvent(input$excludeVenuesDropdown, {
    print(paste0("Exclude Venues dropdown contains: ", input$excludeVenuesDropdown))
    output$dataTable <- DT::renderDataTable({
      DT::datatable(selectDesiredEntries(dataFrame, input, output, session), rownames = FALSE)
    })
  })

  # Observe howManyDropdown event
  observeEvent(input$howManyDropdown, {
    print(paste0("howManyDropdown dropdown contains: ", input$howManyDropdown))
    
    # When a number is selected from the howManyDropdown, a submit button magically appears.
    # The values in howManyDropdown change based on the search criteria in the zip/cities/etc.
    # dropdowns, since the highest value in howManyDropdown should never exceed the total number
    # of matching venues. If howManyDropdown changes, the Submit button should be removed. This
    # situation only happens if someone chooses a value from howManyDropdown, and then goes back
    # and makes a changes to any of the choices in the zip/cities/etc. dropdowns.
    
    # The submit button gets inserted in the code below. This removes the old one (if there was one)
    # to avoid accidentally creating multiple submit buttons.
    removeUI(selector = '#sacrificalLamb')
    
    # Check whether the Submit button should be displayed, and then display if so.
    if (input$howManyDropdown != "Select One" && input$howManyDropdown != "Your search criteria doesn't match any venues") {
      insertUI(
        selector = "#howManyDropdownLocation",
        where = "afterEnd",
        ui = tags$div(id='sacrificalLamb', 
              list(
                checkboxInput("saveToLog", "Save search to log", FALSE),
                actionButton("confirmSelectVenues", paste0("Submit")))
              )
      )
    }
  })
  
  # Observe confirmSelectVenues button click
  observeEvent(input$confirmSelectVenues, {
    print(paste0("confirmSelectVenues button clicked"))

    # Log the values in the dropdowns, but only if something was selected
    logOptions <- NULL

    # Check if anything was selected in the Classes dropdown
    if (length(input$classes) > 0) {
      logOptions <- paste0(logOptions, "The Classes dropdown contained: ", 
                           toString(input$classes), "<br />")
    }

    # Check if anything was selected in the Cities dropdown
    if (length(input$cities) > 0) {
      logOptions <- paste0(logOptions, "The Cities dropdown contained: ", 
                           toString(input$cities), "<br />")
    }

    # Check if anything was selected in the Zip Code dropdown
    if (length(input$zips) > 0) {
      logOptions <- paste0(logOptions, "The Zip Code dropdown contained: ", 
                           toString(input$zips), "<br />")
    }

    # Check if anything was selected in the City Limits dropdown
    if (input$limits != "Any") {
      logOptions <- paste0(logOptions, "The City Limits dropdown contained: ", 
                           toString(input$limits), "<br />")
    }

    # Check if anything was selected in the Exclude Venues dropdown
    if (length(input$excludeVenuesDropdown) > 0) {
      logOptions <- paste0(logOptions, "The Exclude Venues dropdown contained: ", 
                           toString(input$excludeVenuesDropdown), "<br />")
    }

    # Get all the venues that match the selections made in the dropdowns
    selectedVenueSet <- selectDesiredEntries(dataFrame, input, output, session, FALSE)

    selectedVenueSetSize <- nrow(selectedVenueSet)
    
    # Generate a big integer to be used as the seed.
    randomSeed <- as.integer(runif(1, 100000000, 999999999))
    
    # Tell the system to use the generated seed
    set.seed(randomSeed)
    
# Take a random sample, of the size requested in howManyDropdown.
# The matches() function will use the seed set above.
matches <- selectedVenueSet[sample(selectedVenueSetSize, input$howManyDropdown), ]
    
    # Calculate the odds of any one venue being randomly selected
    probabilityOfBeingSelected <- round(as.integer(input$howManyDropdown) / selectedVenueSetSize * 100, 2)
    
    # Display all relevant information, including the seed that was used for the sampling.
    outputMessage <- paste0("<h4>A selection of ", input$howManyDropdown, 
                         " venue(s) was requested from a total set of ", selectedVenueSetSize, 
                         " venue(s).<br />Every venue had a " , probabilityOfBeingSelected, 
                         "% probability of being selected.<br />The seed for this sampling is: ",
                         randomSeed, "</h4><br />")

    output$topText <- renderText({outputMessage})

    # Display the results of the sampling.
    output$dataTable <- DT::renderDataTable({
      DT::datatable(matches, extensions = 'Buttons',
                    rownames = FALSE,
                    options = list(
                      # Display buttons to allow the data to be copied/downloaded/printed.
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'print')
                    ))
    }, server=FALSE)

    # Check whether saving this search to the log was requested
    if (input$saveToLog == TRUE) {
      # Set timezone to US/Central (Omaha)
      Sys.setenv(TZ = "US/Central")

      # The date/time is used for the filenames
      filename <- format(Sys.time(), "%Y-%m-%d_%I-%M-%p-%Ss")
      write.csv(selectedVenueSet, paste0("./log/dataframes/", filename, ".csv"))
      write.csv(matches, paste0("./log/results/", filename, ".csv"))

      logMessage <- paste0(outputMessage, "<h4>", logOptions, "</h4><br />")
      write(logMessage, paste0("./log/details/", filename))

      reloadLogTab(session)
    }
  })
}

##
## FUNCTIONS
##

# Reads the contents of the log folder and adds all found log entries to the dropdown 
reloadLogTab <- function(session) {
  # Sort logs to be in reverse chronological order
  logFiles <- sort(list.files("./log/details/"), decreasing = TRUE)

  if (length(logFiles) > 0) {
    updateSelectInput(session, "logFileList", choices = logFiles, selected = NULL)
  }
}

# Replaces the initial .csv-file-uploading UI with the selection criteria dropdowns
loadCriteriaSidePanel <- function(dataFrame) {
  # For each dropdown, determine the unique values that appear in the respective CSV column
  zipcodesDropdownChoices <- sort(unique(dataFrame[["Zip"]]))
  classesDropdownChoices <- sort(unique(dataFrame[["Class"]]))
  citiesDropdownChoices <- sort(unique(dataFrame[["City"]]))

  # The "Any" option is added so it can be the default value
  limitsDropdownChoices <- c("Any", as.vector(sort(unique(dataFrame[["Limits"]]))))
  licenseDropdownChoices <- sort(unique(dataFrame[["License"]]))
  
  # Create the new UI, which includes a reset button, dropdowns, etc.
  insertUI(
    selector = "#csvFileOptionPanel",
    where = "afterEnd",
    ui = list(
      actionButton("resetApp", "Reset App"),
      hr(),
      selectInput(inputId = "classes", label = "[Optional] Narrow By Class", choices = classesDropdownChoices, multiple = TRUE),
      selectInput(inputId = "cities", label = "[Optional] Narrow By Cities", choices = citiesDropdownChoices, multiple = TRUE),
      selectInput(inputId = "zips", label = "[Optional] Narrow By Zip Code", choices = zipcodesDropdownChoices, multiple = TRUE),
      selectInput(inputId = "limits", label = "[Optional] Narrow By Limits", choices = limitsDropdownChoices),
      selectInput(inputId = "excludeVenuesDropdown", label = "[Optional] Exclude venues by License #", choices = licenseDropdownChoices, multiple = TRUE),
#      textInput("seed", "[Optional] Previously generated seed"),
      selectInput(inputId = "howManyDropdown", label = "How many venues would you like to randomly select?", choices = "Select One"),
      tags$div(id='howManyDropdownLocation'),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
    )
  )
  
  # Delete the old UI (for uploading a .csv file) that's no longer needed
  removeUI(selector = '#csvFileOptionPanel')
}

# This function takes the entire dataframe (from the .csv file) and narrows the rows
# down to just the ones requested, according to the search criteria (dropdowns).
selectDesiredEntries <- function(matches, input, output, session, resetSelectDropdown = TRUE) {
  print ("Entered selectDesiredEntries()")

  # Verify that the expected header are present, and display a warning if not.
  # Assume there are no errors
  headerError <- NULL

  # Check that all the expected headers exist in the .csv file. 
  # If any don't exist, add them to the error list.
  for (column in list("Class", "License", "City", "Zip", "Limits")) {
    if(!column %in% colnames(matches)) {
      headerError <- append(headerError, column)
    }
  }

  # If there's anything in the error list, then an expected header was missing
  if (length(headerError) > 0) {
    # Build a helpful error message
    headerError <- paste0('<h4 style="color: red;">WARNING - the following expected headers were not found in the .csv file: ', toString(headerError), "</h4><br />")
  }

  # The original dataset needs to be narrowed based on the choices made in the dropdowns.
  # Dropdowns without any selections can be skipped.
  if (length(input$classes) > 0) {
    matches <- matches[matches$Class %in% input$classes,]
  }
  
  if (length(input$cities) > 0) {
    matches <- matches[matches$City %in% input$cities,]
  }
  
  if (length(input$zips) > 0) {
    matches <- matches[matches$Zip %in% input$zips,]
  }
  
  if (input$limits != "Any") {
    matches <- matches[matches$Limits %in% input$limits,]
  }

  if (length(input$excludeVenuesDropdown) > 0) {
    matches <- subset(matches, !(License %in% input$excludeVenuesDropdown))
  }

  # Count how many rows matched all of the selected criteria
  numberOfMatches = nrow(matches)

  output$topText <- renderText({
    paste0(headerError, "<h4>Your search parameters match ", numberOfMatches, " entries in the .csv file.</h4><br /><br />")
  })
  
  # When this function is run, numberOfMatches may change, and that means howManyDropdown must be
  # updated to ensure its highest value doesn't exceed the total number of available venues.
  # However, this function is also used when the "Submit" button is pressed to run the
  # random sampling, in which case no change to numberOfMatches has taken place, and 
  # howManyDropdown shouldn't be reset.
  if (resetSelectDropdown) {
    if (numberOfMatches < 1) {
      # Handle the case for when no venues matched the search criteria
      howManyDropdownChoices <- "Your search criteria doesn't match any venues"
    }
    else {
      # Otherwise, generate a list of all the integers from 1 to the numberOfMatches
      howManyDropdownChoices <- c("Select One", seq(1, numberOfMatches))
    }

    # Replace the options in howManyDropdown with the new ones.
    updateSelectInput(session, "howManyDropdown", choices = howManyDropdownChoices, selected = NULL)
  }

  return (matches)
}

# This actually runs the app
shinyApp(ui, server)
