package obfuscation

import chisel3._
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester

//test class for FSM Locking
class GCD_FSMLockTester(c: GCD_FSMLock) extends PeekPokeTester(c) {
	val seqLen = c.auxFSM.inputSeq.length
	private val gcd = c

	def computeGcd(a: Int, b: Int): (Int, Int) = {
		var x = a
		var y = b
		var depth = 1
		while(y > 0 ) {
			if (x > y) {
				x -= y
			}
			else {
				y -= x
			}
			depth += 1
		}
		(x, depth)
	}

	reset(1)
	//provide input sequence
	println("input sequence:")
	for (i <- 0 until seqLen) {
		poke(c.io.value1, c.auxFSM.inputSeq(i))		
		step(1)
	}

	for(i <- 1 to 40 by 3) {
		for (j <- 1 to 40 by 7) {
			poke(gcd.io.value1, i)
			poke(gcd.io.value2, j)
			poke(gcd.io.loadingValues, 1)
			step(1)
			poke(gcd.io.loadingValues, 0)

			val (expected_gcd, steps) = computeGcd(i, j)

			step(steps - 1) // -1 is because we step(1) already to toggle the enable
			expect(gcd.io.outputGCD, expected_gcd)
			expect(gcd.io.outputValid, 1)
		}
	}
}

object FSMLockingMain extends App {
	iotesters.Driver.execute(Array("-tbn","verilator","--no-dce"), () => new GCD_FSMLock) {
		c => new GCD_FSMLockTester(c)
	}
}


//test class for Xor Locking
class GCD_XorLockTester(c: GCD_XorLock) extends PeekPokeTester(c) {
	private val gcd = c

	def computeGcd(a: Int, b: Int): (Int, Int) = {
		var x = a
		var y = b
		var depth = 1
		while(y > 0 ) {
			if (x > y) {
				x -= y
			}
			else {
				y -= x
			}
			depth += 1
		}
		(x, depth)
	}

	reset(1)
	//set the key
	poke(gcd.io.obfKey, gcd.key) 
	for(i <- 1 to 40 by 3) {
		for (j <- 1 to 40 by 7) {
			poke(gcd.io.value1, i)
			poke(gcd.io.value2, j)
			poke(gcd.io.loadingValues, 1)
			step(1)
			poke(gcd.io.loadingValues, 0)

			val (expected_gcd, steps) = computeGcd(i, j)

			step(steps - 1) // -1 is because we step(1) already to toggle the enable
			expect(gcd.io.outputGCD, expected_gcd)
			expect(gcd.io.outputValid, 1)
		}
	}
}

object XorLockingMain extends App {
	iotesters.Driver.execute(Array(), () => new GCD_XorLock) {
		c => new GCD_XorLockTester(c)
	}
}

