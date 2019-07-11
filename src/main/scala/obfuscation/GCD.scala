// See README.md for license details.

package obfuscation

import chisel3._

/**
 * Compute GCD using subtraction method.
 * Subtracts the smaller from the larger until register y is zero.
 * value in register x is then the GCD
 */
class GCD_FSMLock extends Module {
	val io = IO(new Bundle {
		val value1        = Input(UInt(16.W))
		val value2        = Input(UInt(16.W))
		val loadingValues = Input(Bool())
		val outputGCD     = Output(UInt(16.W))
		val outputValid   = Output(Bool())
	})

	val x  = Reg(UInt())
	val y  = Reg(UInt())

	// FSM locking
	FSMLock(this) //annotate
	//val auxFSM_out = Wire(UInt(4.W))
	val auxFSM = Module(new AuxFSM(Seq(31,4,2), 16)) //add fsm
	auxFSM.io.in := io.value1 //hookup FSM input
	//auxFSM_out := auxFSM.io.out

	when(x > y) { x := x - y }
	.otherwise { y := y - x }

	when(io.loadingValues) {
		x := io.value1
		y := io.value2
	}

	io.outputGCD := x
	io.outputValid := y === 0.U
}

class GCD_XorLock extends Module {
	val io = IO(new Bundle {
		val value1        = Input(UInt(16.W))
		val value2        = Input(UInt(16.W))
		val loadingValues = Input(Bool())
		val outputGCD     = Output(UInt(16.W))
		val outputValid   = Output(Bool())
		val obfKey = Input(UInt(4.W)) //add an additional key input
	})

	val x  = Reg(UInt())
	val y  = Reg(UInt())

	// Xor locking
	xorLock(this, "0101") //annotate
	val key = 5 //set key for testbench

	when(x > y) { x := x - y }
	.otherwise { y := y - x }

	when(io.loadingValues) {
		x := io.value1
		y := io.value2
	}

	io.outputGCD := x
	io.outputValid := y === 0.U
}
