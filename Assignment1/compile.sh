scalac KnapSack.scala
jar cfm app.jar Manifest.txt *.class scala
java -Xmx512m -jar app.jar data/ks_4_0
