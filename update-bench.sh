dir=`pwd`/$(dirname "$0")/modules/benchmarks
forks=${1:-1}

echo "Benchmark dir: $dir"
echo "Forks count: $forks"


sbt "benchmarks/jmh:run -f $forks -rf json -rff $dir/jmh-reader.json -o $dir/jmh-reader.log .*JmhReaderBench.*"
sbt "benchmarks/jmh:run -f $forks -rf json -rff $dir/jmh-writer.json -o $dir/jmh-writer.log .*JmhWriterBench.*"
sbt "benchmarks/runMain json.bench.BenchMarkdown $dir"
