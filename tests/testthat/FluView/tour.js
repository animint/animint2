 
        document.addEventListener("DOMContentLoaded", function() {
            
            document.getElementById("startTour").onclick = function() {
              const driver = window.driver.js.driver;
              
              const pointPlot = document.querySelector(" #plot_pointPlot");
              const vlinePlot = document.querySelector(" #plot_vlinePlot");
      
              // Define the steps for the driver tour using the selected SVG elements
              const driverObj = driver({
                  showProgress: true,
                  steps: [
                      {
                          element: pointPlot,  // Target SVG element for the point plot
                          popover: {
                              title: "Point Plot",
                              description: "This shows individual data points."
                          }
                      },
                      {
                          element: vlinePlot,  // Target SVG element for the vertical line plot
                          popover: {
                              title: "Vertical Line Plot",
                              description: "This displays vertical lines to indicate trends."
                          }
                      },
                  ]
              });
      
              // Start the driver tour
              driverObj.drive();  
            };
        }); 