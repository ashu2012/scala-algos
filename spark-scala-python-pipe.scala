import org.apache.spark.SparkFiles;

val conf = new SparkConf().setAppName("Pipe")
val sc = new SparkContext(conf)
val distScript = "wasbs://sent-desbx@salsbx01sparkdata.blob.core.windows.net/user/h217119/print-line.sh"
val distScriptName = "print-line.sh"
sc.addFile(distScript , true)
val ipData = sc.parallelize(List("asd","xyz","zxcz","sdfsfd","Ssdfd","Sdfsf"))
val opData = ipData.pipe(SparkFiles.get(distScriptName))
opData.foreach(println)

val pipeRDD = ipData.pipe("./print-line.sh")
pipeRDD.collect()