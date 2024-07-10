# Author: Roman Tremmel
# Code for www.Pharmfreq.com
# PLease cite XXXX when you are using code or data

tag_head <- shiny::tags$head(tags$script('var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                Shiny.addCustomMessageHandler("update-tabs", function(message) {
                 // console.log( message );
                      // hide and inactivate all not selected tabs
                $(".active.show").removeClass("active show");
                $(".tab-pane.active.show").removeClass("active show");
                
                // add active class to the current selected tab and show its content
                $("#tab-" + message).addClass("active show");
                $("#shiny-tab-" + message).addClass("active show");
                $("#tabset_wrld-Worldmap-tab").addClass("active show");
                $("#tabset_wrld-Worldmap").addClass("active show");
               });'),
tags$style(".dropdown-header{color: #000000}"))

