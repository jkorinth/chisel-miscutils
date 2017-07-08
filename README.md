# Miscellaneous Chisel IP

Some very basic IP for everyday use, written in Chisel 3.0. Currently contains:

  * [DataWidthConverter][1]  
    Converts `UInt` of arbitrary bit width to `UInt` of different bit width.
    Zero delay, handshaked channels using Chisels `Decoupled` interface.

  * [DecoupledDataSource][2]  
    Generic data provider with fixed (compile-time) data; uses handshakes via
    Chisels `Decoupled` interface.

  * [SignalGenerator][3]  
    Primitive 1-bit signal generator: Specify via change list, can cycle.

These were basically warm-up exercises with Chisel, but can be useful now and
then. For usage examples see the [unit test suites][4].

[1]: src/main/scala/DataWidthConverter.scala
[2]: src/main/scala/DecoupledDataSource.scala
[3]: src/main/scala/SignalGenerator.scala
[4]: src/test/scala/
