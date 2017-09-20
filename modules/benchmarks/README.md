# benchmarks

# read

```text
Benchmark                                              (arraySize)  (processorName)   Mode  Cnt            Score            Error   Units
JmhReaderBench.bench                                            10   tethys-jackson  thrpt    8        21303.493 ±        233.432   ops/s
JmhReaderBench.bench                                            10       circe-jawn  thrpt    8        13375.937 ±        182.539   ops/s
JmhReaderBench.bench                                            10     pure-jackson  thrpt    8        25437.778 ±        572.707   ops/s
JmhReaderBench.bench                                            10   json4s-jackson  thrpt    8         4206.049 ±        162.484   ops/s
JmhReaderBench.bench                                            10    json4s-native  thrpt    8         4604.009 ±         73.922   ops/s
JmhReaderBench.bench                                            10        play-json  thrpt    8         7064.693 ±         97.385   ops/s
JmhReaderBench.bench                                            10       spray-json  thrpt    8        10990.755 ±        324.898   ops/s
JmhReaderBench.bench                                           100   tethys-jackson  thrpt    8         2414.252 ±         50.675   ops/s
JmhReaderBench.bench                                           100       circe-jawn  thrpt    8         1586.145 ±         26.513   ops/s
JmhReaderBench.bench                                           100     pure-jackson  thrpt    8         2692.034 ±         38.457   ops/s
JmhReaderBench.bench                                           100   json4s-jackson  thrpt    8          517.942 ±          2.963   ops/s
JmhReaderBench.bench                                           100    json4s-native  thrpt    8          501.529 ±         22.473   ops/s
JmhReaderBench.bench                                           100        play-json  thrpt    8          770.131 ±          1.187   ops/s
JmhReaderBench.bench                                           100       spray-json  thrpt    8         1219.752 ±         93.009   ops/s
JmhReaderBench.bench                                          1000   tethys-jackson  thrpt    8          236.927 ±          9.205   ops/s
JmhReaderBench.bench                                          1000       circe-jawn  thrpt    8          149.804 ±          1.761   ops/s
JmhReaderBench.bench                                          1000     pure-jackson  thrpt    8          264.297 ±          4.779   ops/s
JmhReaderBench.bench                                          1000   json4s-jackson  thrpt    8           48.666 ±          3.072   ops/s
JmhReaderBench.bench                                          1000    json4s-native  thrpt    8           42.920 ±          1.290   ops/s
JmhReaderBench.bench                                          1000        play-json  thrpt    8           70.975 ±          1.833   ops/s
JmhReaderBench.bench                                          1000       spray-json  thrpt    8          107.871 ±          4.684   ops/s
JmhReaderBench.bench                                          5000   tethys-jackson  thrpt    8           41.417 ±          0.157   ops/s
JmhReaderBench.bench                                          5000       circe-jawn  thrpt    8           29.474 ±          0.485   ops/s
JmhReaderBench.bench                                          5000     pure-jackson  thrpt    8           52.536 ±          2.237   ops/s
JmhReaderBench.bench                                          5000   json4s-jackson  thrpt    8            8.619 ±          0.202   ops/s
JmhReaderBench.bench                                          5000    json4s-native  thrpt    8            6.959 ±          0.269   ops/s
JmhReaderBench.bench                                          5000        play-json  thrpt    8           12.354 ±          0.160   ops/s
JmhReaderBench.bench                                          5000       spray-json  thrpt    8           22.230 ±          2.932   ops/s
JmhReaderBench.bench                                         10000   tethys-jackson  thrpt    8           19.784 ±          0.261   ops/s
JmhReaderBench.bench                                         10000       circe-jawn  thrpt    8           13.993 ±          1.692   ops/s
JmhReaderBench.bench                                         10000     pure-jackson  thrpt    8           24.367 ±          0.361   ops/s
JmhReaderBench.bench                                         10000   json4s-jackson  thrpt    8            4.432 ±          0.134   ops/s
JmhReaderBench.bench                                         10000    json4s-native  thrpt    8            2.727 ±          0.049   ops/s
JmhReaderBench.bench                                         10000        play-json  thrpt    8            5.909 ±          0.047   ops/s
JmhReaderBench.bench                                         10000       spray-json  thrpt    8           10.815 ±          0.720   ops/s
JmhReaderBench.bench                                         50000   tethys-jackson  thrpt    8            4.481 ±          0.163   ops/s
JmhReaderBench.bench                                         50000       circe-jawn  thrpt    8            2.833 ±          0.078   ops/s
JmhReaderBench.bench                                         50000     pure-jackson  thrpt    8            5.491 ±          0.046   ops/s
JmhReaderBench.bench                                         50000   json4s-jackson  thrpt    8            0.908 ±          0.038   ops/s
JmhReaderBench.bench                                         50000    json4s-native  thrpt    8            0.193 ±          0.004   ops/s
JmhReaderBench.bench                                         50000        play-json  thrpt    8            1.224 ±          0.016   ops/s
JmhReaderBench.bench                                         50000       spray-json  thrpt    8            1.961 ±          0.046   ops/s
JmhReaderBench.bench                                        100000   tethys-jackson  thrpt    8            2.245 ±          0.072   ops/s
JmhReaderBench.bench                                        100000       circe-jawn  thrpt    8            1.244 ±          0.019   ops/s
JmhReaderBench.bench                                        100000     pure-jackson  thrpt    8            2.663 ±          0.113   ops/s
JmhReaderBench.bench                                        100000   json4s-jackson  thrpt    8            0.432 ±          0.031   ops/s
JmhReaderBench.bench                                        100000    json4s-native  thrpt    8            0.057 ±          0.001   ops/s
JmhReaderBench.bench                                        100000        play-json  thrpt    8            0.540 ±          0.015   ops/s
JmhReaderBench.bench                                        100000       spray-json  thrpt    8            0.910 ±          0.074   ops/s
```

# write

```text
Benchmark                                              (arraySize)  (processorName)   Mode  Cnt            Score            Error   Units
JmhWriterBench.bench                                            10   tethys-jackson  thrpt    8        58726.322 ±       3387.821   ops/s
JmhWriterBench.bench                                            10       circe-jawn  thrpt    8        19304.978 ±        199.893   ops/s
JmhWriterBench.bench                                            10    StringBuilder  thrpt    8        45838.890 ±        248.894   ops/s
JmhWriterBench.bench                                            10     pure-jackson  thrpt    8        43536.608 ±        115.022   ops/s
JmhWriterBench.bench                                            10   json4s-jackson  thrpt    8         5129.888 ±        110.653   ops/s
JmhWriterBench.bench                                            10    json4s-native  thrpt    8        11511.215 ±        159.982   ops/s
JmhWriterBench.bench                                            10        play-json  thrpt    8         3295.809 ±        125.626   ops/s
JmhWriterBench.bench                                            10       spray-json  thrpt    8        14724.930 ±        330.543   ops/s
JmhWriterBench.bench                                            10           pushka  thrpt    8        14143.540 ±        350.526   ops/s
JmhWriterBench.bench                                           100   tethys-jackson  thrpt    8         6016.780 ±        326.293   ops/s
JmhWriterBench.bench                                           100       circe-jawn  thrpt    8         2033.939 ±         77.782   ops/s
JmhWriterBench.bench                                           100    StringBuilder  thrpt    8         5199.731 ±        109.446   ops/s
JmhWriterBench.bench                                           100     pure-jackson  thrpt    8         6236.041 ±         62.721   ops/s
JmhWriterBench.bench                                           100   json4s-jackson  thrpt    8          605.616 ±          5.184   ops/s
JmhWriterBench.bench                                           100    json4s-native  thrpt    8         1252.696 ±         27.990   ops/s
JmhWriterBench.bench                                           100        play-json  thrpt    8          371.122 ±          7.248   ops/s
JmhWriterBench.bench                                           100       spray-json  thrpt    8         1523.961 ±         71.879   ops/s
JmhWriterBench.bench                                           100           pushka  thrpt    8         1570.292 ±          5.752   ops/s
JmhWriterBench.bench                                          1000   tethys-jackson  thrpt    8          520.132 ±         14.182   ops/s
JmhWriterBench.bench                                          1000       circe-jawn  thrpt    8          170.821 ±          1.426   ops/s
JmhWriterBench.bench                                          1000    StringBuilder  thrpt    8          435.241 ±          4.721   ops/s
JmhWriterBench.bench                                          1000     pure-jackson  thrpt    8          555.361 ±          5.305   ops/s
JmhWriterBench.bench                                          1000   json4s-jackson  thrpt    8           57.352 ±          2.025   ops/s
JmhWriterBench.bench                                          1000    json4s-native  thrpt    8          132.510 ±          2.552   ops/s
JmhWriterBench.bench                                          1000        play-json  thrpt    8           35.469 ±          2.525   ops/s
JmhWriterBench.bench                                          1000       spray-json  thrpt    8          128.216 ±          8.660   ops/s
JmhWriterBench.bench                                          1000           pushka  thrpt    8          131.264 ±          0.940   ops/s
JmhWriterBench.bench                                         10000   tethys-jackson  thrpt    8           50.724 ±          4.632   ops/s
JmhWriterBench.bench                                         10000       circe-jawn  thrpt    8           17.148 ±          0.396   ops/s
JmhWriterBench.bench                                         10000    StringBuilder  thrpt    8           42.100 ±          0.343   ops/s
JmhWriterBench.bench                                         10000     pure-jackson  thrpt    8           47.841 ±          0.665   ops/s
JmhWriterBench.bench                                         10000   json4s-jackson  thrpt    8            5.735 ±          0.061   ops/s
JmhWriterBench.bench                                         10000    json4s-native  thrpt    8           12.405 ±          0.716   ops/s
JmhWriterBench.bench                                         10000        play-json  thrpt    8            3.691 ±          0.113   ops/s
JmhWriterBench.bench                                         10000       spray-json  thrpt    8           13.208 ±          0.661   ops/s
JmhWriterBench.bench                                         10000           pushka  thrpt    8           14.130 ±          0.113   ops/s
JmhWriterBench.bench                                        100000   tethys-jackson  thrpt    8            4.957 ±          0.488   ops/s
JmhWriterBench.bench                                        100000       circe-jawn  thrpt    8            1.689 ±          0.036   ops/s
JmhWriterBench.bench                                        100000    StringBuilder  thrpt    8            4.623 ±          0.069   ops/s
JmhWriterBench.bench                                        100000     pure-jackson  thrpt    8            4.971 ±          0.037   ops/s
JmhWriterBench.bench                                        100000   json4s-jackson  thrpt    8            0.565 ±          0.008   ops/s
JmhWriterBench.bench                                        100000    json4s-native  thrpt    8            1.341 ±          0.128   ops/s
JmhWriterBench.bench                                        100000        play-json  thrpt    8            0.347 ±          0.014   ops/s
JmhWriterBench.bench                                        100000       spray-json  thrpt    8            1.221 ±          0.085   ops/s
JmhWriterBench.bench                                        100000           pushka  thrpt    8            1.291 ±          0.025   ops/s
```