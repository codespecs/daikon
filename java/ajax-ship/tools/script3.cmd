set TOOL_OPTIONS=-ndvirtuals
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% ajax.tools.benchmarks.DowncastChecker -cp .;D:\programs\jdk117\Lib\classes.zip > out.nd.ajax
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% sun.tools.javap.JavaP -cp D:\programs\jdk117\Lib\classes.zip > out.nd.javap
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% sun.tools.jar.Main -cp D:\programs\jdk117\Lib\classes.zip > out.nd.jar
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% COM.sun.labs.javacc.Main -cp javacc\javacc.zip;D:\programs\jdk117\Lib\classes.zip > out.nd.javacc
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% jess.Main -cp  examples\jess44;D:\programs\jdk117\Lib\classes.zip > out.nd.jess
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% sun.tools.javac.Main -cp D:\programs\jdk117\Lib\classes.zip > out.nd.javac
\usr\bin\date

set TOOL_OPTIONS=-timelimit 7200
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% ajax.tools.benchmarks.DowncastChecker -cp .;D:\programs\jdk117\Lib\classes.zip > out.ajax
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% sun.tools.javap.JavaP -cp D:\programs\jdk117\Lib\classes.zip > out.javap
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% sun.tools.jar.Main -cp D:\programs\jdk117\Lib\classes.zip > out.jar
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% COM.sun.labs.javacc.Main -cp javacc\javacc.zip;D:\programs\jdk117\Lib\classes.zip > out.javacc
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% jess.Main -cp  examples\jess44;D:\programs\jdk117\Lib\classes.zip > out.jess
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% sun.tools.javac.Main -cp D:\programs\jdk117\Lib\classes.zip -hindleymilner > out.javac2
\usr\bin\date
start /b /wait /low java -Xmx200m -cp . ajax.tools.benchmarks.DowncastChecker %TOOL_OPTIONS% sun.tools.javac.Main -cp D:\programs\jdk117\Lib\classes.zip > out.javac
\usr\bin\date
