# NCAA_XC_Simulator
In this project, I scraped all NCAA XC races since 2010 and used the data to normalize times. With the adjusted times, I then create meet simulations to predict placing for each runner. The process for this is as follows:

1. Use the scrape script to scrape all data from 2010 to now (This takes several hours to run)
2. Use the meet_simulation script to create the model and run the MAC simulation for the men
3. Use the NCAA Regionals script to run the simulation for regionals
4. Use the NCAA Nationals script to run the simulation for nationals
5. Repeat steps 2-4 with the women
6. Run the create app data script
7. Create and run the shiny app that populates the website at https://bkmiller96.shinyapps.io/ncaa-xc-simulator/
