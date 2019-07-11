# Obfuscation
A chisel3/FIRRTL package that enables obfuscation of circuits

## Overview
Currently implemented are two techniques: Xor Locking and FSM Locking. 
Xor locking will randomly add Xor gates to nets in the circuit. The Xor is inserted such that the net is broken by the Xor gate. The original net driver feeds into one Xor input and the output is connected to the net's original loads. The other input of the Xor is connected to a key input. With a 50% probability these Xors are accompanied by an inverter; thus in order to restore proper functionality, the key input must either be set to 1, inverting the net, or 0 buffering it. 
The other technique is FSM Locking, which incorperates an activating reset sequence into a circuit. If the incorrect reset sequence is given, the circuit's functionality will be corrupted. This is implemented with an auxillary FSM which is added to the circut. The correct final state of the FSM is all 0s, and the FSM remains in this state until reset again. The FSM's outputs are connected to Xor gates in the same fashion as Xor Locking. Thus when the final state is reached the correct
functionality is restored. The FSM's inputs are connected to circuit inputs so no additional io is needed. 
 
## Usage
Examples of each the usage of each technique can be found in src/main/scala/obfuscation/GCD.scala

