### About Page

header <- HTML('<div class="jumbotron" style="height: 70vh; display: flex; justify-content: center; align-items: center; 
background-image:linear-gradient( rgba(197,219,235, 0.65), rgba(197,219,235, 0.2)),
url(image.jpg); background-size: cover; background-position: center top; letter-spacing: 0.7em;">
                           <h1 class="display-3" style="text-align: center; font-size: 5em; font-weight: 550;">Siting &nbsp;Multiple &nbsp;Benefit &nbsp;Groundwater &nbsp;Recharge &nbsp;Projects</h1>
                           </div>')

project <- HTML('<div style="height: 90vh; overflow: auto" class="jumbotron">
                      <h1 class="display-3" >The Project</h1>
                      <div style="display: table-cell; vertical-align: middle;">
  <p style="text-align: justify;" class="lead">This website explores the results of a decision support tool for assessing site suitability for multiple benefit groundwater recharge projects in California’s San Joaquin Valley. Sites are identified as relatively suitable or unsuitable based on the following: an assessment of surface and subsurface characteristics, such as soil infiltration capacity, depth to groundwater, the presence or absence of confining layers, and soil coarseness; an assessment of nitrogen loading at the ground surface, largely a result of historical land use. More suitable areas are those that allow for water to move from the surface into aquifer storage without degrading water quality. </p>
  <p class="lead">From the land areas identified as most suitable, the decision support tool then allows for a user to consider additional benefits of groundwater recharge projects to groundwater dependent ecosystems or domestic wells that have run dry. Feasibility considerations, including a site’s proximity to existing water conveyance and distance from contaminated sites, are also included. A user can assign unique weights to the benefit and feasibility considerations based on a basin’s needs and generate a multi-benefit recommended output.</p>
  <p class="lead">This app provides a qualitative view of some of the decision support tool results and considerations. The full decision support tool is free and publically available. To downlaod a copy of the tool, or for more information on the project, please visit our website.</p>
  <p style="display: inline-block; vertical-align: bottom" class="lead">
    <a class="btn btn-primary btn-lg" href=https://waterresilience.wixsite.com/waterresilienceca target="_blank" role="button">Learn more</a>
  </p>
  </div>
</div>'
)

creators <- HTML('<div style="height: 75vh; overflow: auto" class="jumbotron">
  <h1 class="display-3"> Meet Our Team</h1>
  <div>
  <img style="width: 50vh; height: 40vh; object-fit: cover;  margin: 0px 0px 1em 1.5em; padding: 0.5em 0px 0px 0px; float: right; vertical-align: middle" src="team.jpg" alt="Team image">
  <p style="text-align: justify;" class="lead">The Bren School of Environmental Science and Management at UC Santa Barbara is an interdisciplinary graudate program that includes a yearlong master’s thesis project. The decision support tool was created by four master’s students: Anna Perez Welter, Claire Madden, Jenny Balmagia, and Bridget Gibbons. The project team was advised by Dr. Scott Jasechko, and the work was completed in partnership with the Environmental Defense Fund.  </p>
  <p class="lead">The visualizations contained in this application were developed by Bridget Gibbons, Claire Madden, and Lydia Bleifuss.</p>

  </p>
  </div>
</div>')