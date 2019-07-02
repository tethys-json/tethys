dir=`pwd`/modules/benchmarks

sbt 'benchmarks/jmh:run -rf json -rff $dir/jmh-reader.json -o $dir/jmh-reader.log .*JmhReaderBench.*'
sbt 'benchmarks/jmh:run -rf json -rff $dir/jmh-writer.json -o $dir/jmh-writer.log .*JmhWriterBench.*'
sbt 'benchmarks/run-main json.bench.BenchMarkdown $dir'