set FROM_DIR=c:\users\roc\lackwit\ajax
set TO_DIR=c:\users\roc\ajaxsrv

copy %FROM_DIR%\*.gif %TO_DIR%
copy %FROM_DIR%\*.html %TO_DIR%
cd %FROM_DIR%
jar cf %TO_DIR%\ajaxclient.jar ajax\tools\protocol\*.class ajax\tools\client\*.class ajax\util\*.class ajax\*.class ajax\tools\misc\ServerData.class
jar cf %TO_DIR%\ajaxlauncher.jar ajax\tools\protocol\*.class ajax\util\*.class ajax\*.class ajax\tools\misc\LaunchData.class ajax\tools\misc\ServerLauncher.class
