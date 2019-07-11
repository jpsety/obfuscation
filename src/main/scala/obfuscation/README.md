# Obfuscation overview

This branch has added obfuscation to the GPS system's AcquisitionChannel. The obfuscation used is XOR logic locking in which randomly selected signals have been keyed with XOR gates. The signals either must be inverted with a 1 applied to its corresponding key input, or buffered with a 0 applied. 
Modifications:
- src/main/scala/obfuscation/KL_Transform.scala : obfuscation code, a firrtl pass that inserts the logic locking
- src/main/scala/gps/acquisition/AcquisitionChannel.scala : a new port, obfKey, is added to the AcquisitionChannelIO, port is hooked up in wrapper. 
- src/test/scala/gps/acquisition/AcquisitionChannelSpec.scala : imported the obfuscation pass, set the correct key value durring the test, added custom transorm in tester.


TODO: 
- the firrtl pass will apply logic locking to the module named in the pass's code. In this case it is the acquisition channel, but this will need to be changed to target the duplicated module name (we need to coordinate on how the module will be duplicated).
- the key and desired length must be set in both the tester as well as the pass


