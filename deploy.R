library(rsconnect)

deployApp(
    appDir = getwd(),
    appFiles = NULL,
    appFileManifest = ".manifest",
    appPrimaryDoc = NULL,
    appSourceDoc = NULL,
    appName = "Omicron",
    appTitle = "Spatio-temporal view of the Omicron wave in metropolitan France",
    appId = NULL,
    contentCategory = NULL,
    account = NULL,
    server = NULL,
    upload = TRUE,
    recordDir = NULL,
    launch.browser = getOption("rsconnect.launch.browser", interactive()),
    logLevel = c("normal", "quiet", "verbose"),
    lint = TRUE,
    metadata = list(),
    forceUpdate = getOption("rsconnect.force.update.apps", FALSE),
    python = NULL,
    on.failure = NULL,
    forceGeneratePythonEnvironment = FALSE
)

