set TOOL_OPTIONS=-timelimit 7200 -ndvirtuals
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% ajax.tools.benchmarks.DowncastChecker -cp .;D:\programs\jdk117\Lib\classes.zip > out.nd.ajax
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% sun.tools.javap.JavaP -cp D:\programs\jdk117\Lib\classes.zip > out.nd.javap
\usr\bin\date
