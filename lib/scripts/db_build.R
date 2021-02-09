DashboardRender <- function(data.display = "both", scenarios) {
  # Builds CMAP dashboard to the appropriate location within the scenarios
  # folder of the model file structure.
  #
  # Args:
  #   scenarios: vector of strings, with each string being the name of a
  #                scenario folder within the scenarios folder of the rFreight 
  #                model file structure.
  #   data.display: string, must be one of "model", "reference", or "both". 
  #                  If "model" is chosen, then only model run data will be displayed.
  #                  If "reference" is chosen, then only FAF and mode-specific 
  #                    reference data will be displayed. 
  #                  If "both" is chosen, then both model and reference data will be 
  #                    displayed together. 
  #                  Some tables and charts may not work, depending on the
  #                    display setting you choose.
  # Returns:
  #   None, but creates a .html file of the dashboard. If length(scenarios) == 1,
  #     the .html file is placed in the outputs file for that specific scenario.
  #     If length(scenarios) > 1, then the file is placed in the scenarios folder
  #     on the same level as the folders for each specific scenario.
  
  if (missing(scenarios) & data.display != "reference") {
    stop("No data to graph. Please specify scenarios or set data.display to 'reference'.")
  } else if (data.display == "reference") {
    if (!missing(scenarios)) {
      warning("Ignoring specified scenarios because data.display = 'reference'.")
    }
    rmarkdown::render(input = "./lib/Dashboard/CMAPDashboard.Rmd",
                      output_file = paste0("CMAPDashboard_Reference_Only.html"),
                      output_dir = paste0("./scenarios"),
                      params = list(model.run.names = c(),
                                    display = data.display))
  } else if (length(scenarios) == 1) {
    rmarkdown::render(input = "./lib/Dashboard/CMAPDashboard.Rmd",
                      output_file = paste0("CMAPDashboard_", scenarios[1], ".html"),
                      output_dir = paste0("./scenarios/", scenarios[1], "/outputs"),
                      params = list(model.run.names = scenarios,
                                    display = data.display))
  } else if (length(scenarios) == 2) {
    rmarkdown::render(input = "./lib/Dashboard/CMAPDashboard.Rmd",
                      output_file = paste0("CMAPDashboard_", scenarios[1], "_", scenarios[2], ".html"),
                      output_dir = paste0("./scenarios"),
                      params = list(model.run.names = scenarios,
                                    display = data.display))
  } else if (length(scenarios) == 3) {
    rmarkdown::render(input = "./lib/Dashboard/CMAPDashboard.Rmd",
                      output_file = paste0("CMAPDashboard_", scenarios[1], "_", scenarios[2], "_", scenarios[3], ".html"),
                      output_dir = paste0("./scenarios"),
                      params = list(model.run.names = scenarios,
                                    display = data.display))
  } else if (length(scenarios) > 3) {
    # More than 3 scenarios would make dashboard graphs very cluttered
    stop("The dashboard is best viewed with 1, 2, or 3 model runs. More than three model runs will make the charts very cluttered. Please reduce the number of model runs given.")
  }
}

