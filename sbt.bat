set SCRIPT_DIR=%~dp0
java -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M -Xmx512M -Xss2M -jar "%SCRIPT_DIR%\sbt-launcher.jar" %*
