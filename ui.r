header = dashboardHeader(title='Optimizr')
sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem('Polynomial Regression', tabName='polyreg', icon=icon('dashboard')),
    menuItem('Quadratic Spline', tabName='qspline', icon=icon('dashboard')),
    menuItem('Optimization', tabName='simplex', icon=icon('dashboard'))
  )
)

body = dashboardBody(
  tabItems(
    tabItem(
      tabName='polyreg',
      polyregInput,
      polyregData
    ),
    tabItem(
      tabName='qspline',
      qsplineInput,
      qsplineData
    ),
    tabItem(
      tabName='simplex',
      simplexData,
      simplexResults,
      tableu
    )
  )
)

client <- dashboardPage(title='Optimizr', header, sidebar, body, skin='black')
