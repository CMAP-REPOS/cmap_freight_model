# Freight truck touring model summary
ft_sim_summary_render <- function(shipments) {

  # Render the dashboard
  rmarkdown::render(input = "./lib/Dashboard/CMAP_FTTM_Summary.Rmd",
                    output_file = paste0("CMAP_FTTM_Summary.html"),
                    output_dir = SCENARIO_OUTPUT_PATH)

  return(shipments)

}
