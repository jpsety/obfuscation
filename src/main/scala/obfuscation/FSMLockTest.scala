
package obfuscation

import scala.util.Random
import chisel3._
import chisel3.iotesters.PeekPokeTester

//test class for axillary FSM
class AuxFSMTester(c: AuxFSM) extends PeekPokeTester(c) {
	val seqLen = c.inputSeq.length
	reset(1)
	println("input sequence:")
	for (i <- 0 until seqLen) {
		poke(c.io.in, c.inputSeq(i))		
		println("in: " + c.inputSeq(i).toString + " out:" + peek(c.io.out).toString)
		expect(peek(c.io.out)!=0, "output not 0")
		step(1)
	}
	for (i <- 0 until 100) {
		expect(c.io.out,0)
		step(1)
	}
	reset(1)
	for (i <- 0 until 100) {
		poke(c.io.in, Random.nextInt)		
		expect(peek(c.io.out)!=0, "output not 0")
		step(1)
	}
}

object AuxFSMMain extends App {
	iotesters.Driver.execute(args, () => new AuxFSM(Seq(31,3,2),32)) {
		c => new AuxFSMTester(c)
	}
}

