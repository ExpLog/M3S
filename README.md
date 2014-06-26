M3S
===

Multi-State System Simulation

This project is built on top of [sbt](http://www.scala-sbt.org/).

To build this project using sbt, simply run the compile command.

After running the tests, individual logs for each test suite can be found in the directory ".target\streams\test\test" (all the logs are files named out).
It is recommended that you use the flag "-Dsbt.log.noformat=true" when running sbt before running tests, so the logs won't contain console coloring.
