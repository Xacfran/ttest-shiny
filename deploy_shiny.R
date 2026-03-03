# Install pcakge for shiny deployment
install.packages("rsconnect")

# Set up account info
rsconnect::setAccountInfo(
  name   = "yourusername",
  token  = "ABCDEF123456",
  secret = "your-secret-here"
)

# Deploy
rsconnect::deployApp(
  appDir  = ".",          # current folder
  appName = "dart-ttest-explorer"
)
