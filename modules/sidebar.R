sidebar <- argonDash::argonDashSidebar(
  vertical = FALSE,
  id = 'sidebar',
  size = "md",
  brand_logo = "https://upload.wikimedia.org/wikipedia/commons/c/c4/Globe_icon.svg",
  argonSidebarMenu(
    style = "display:-webkit-inline-box;",
    argonSidebarItem(tabName = 'home', 'Home', icon = icon("house-user")),
    argonSidebarItem(tabName = 'map', 'Map & Data', icon = icon("far fa-map")),
    argonSidebarItem(tabName = 'tools', 'Tools', icon = icon("gears"))
    )
)
