sidebar <- argonDash::argonDashSidebar(
  vertical = FALSE,
  id = 'sidebar',
  size = "md",
  brand_logo = "Logo_PharmFreq.svg",
  argonSidebarMenu(
    style = "display:-webkit-inline-box;",
    argonSidebarItem(tabName = 'map', 'Map & Data', icon = icon("far fa-map")),
    argonSidebarItem(tabName = 'tools', 'Tools', icon = icon("gears"))
    )
)
