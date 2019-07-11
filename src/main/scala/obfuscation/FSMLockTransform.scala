// See README.md for license details.

package obfuscation

import firrtl.PrimOps.{Xor,Bits,Cat}
import firrtl.{CircuitState, LowForm, FEMALE, MALE, NodeKind, PortKind, InstanceKind, WRef, WSubField}
import firrtl.ir._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.util.Random
import chisel3._
import chisel3.util._
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform, annotate}
import chisel3.internal.InstanceId
import firrtl.{CircuitState, LowForm}
import firrtl.annotations.{SingleTargetAnnotation, ModuleName, Named}


/** FIRRTL annotation */
case class FSMLockAnnotation(target: Named) extends SingleTargetAnnotation[Named] {
	  def duplicate(n: Named): FSMLockAnnotation = this.copy(target = n)
}

/** ChiselAnnotation that corresponds to the above FIRRTL annotation */
case class FSMLockChiselAnnotation(target: InstanceId) extends ChiselAnnotation with RunFirrtlTransform {
	def toFirrtl: FSMLockAnnotation = FSMLockAnnotation(target.toNamed)
	def transformClass: Class[FSMLockTransform] = classOf[FSMLockTransform]
}

object FSMLock { // scalastyle:ignore object.name
	def apply(component: InstanceId): Unit = {
  		val anno = FSMLockChiselAnnotation(component)
		annotate(anno)
	}
}

/**
  * Adds Keyed logic to a low FIRRTL circuit
  */
class FSMLockTransform extends firrtl.Transform {

  	def inputForm: LowForm.type = LowForm
  	def outputForm: LowForm.type = LowForm

	//fix random seed
  	val rand = new Random(155)

	//variables passed through ast
	val stmtInds = ListBuffer[Int]()
	var stmtCount = 0
	var keyCount = 0
	var key = ""
	var keyLen = 0
	var modules = ListBuffer[String]()
	var inWidth = 0
	var outWidth = 0

  	def execute(state: CircuitState): CircuitState = {
   		val circuit = state.circuit

		//get annotations, contains input sequences and module InstanceId
		state.annotations.map {
			case FSMLockAnnotation(ModuleName(name,_)) =>
				//map module name to key length
				modules += name
			case other => other
		}

		//insert keys and connect FSMs
		val modifiedModules = circuit.modules map keyModules

		//return keyed circuit
		state.copy(circuit=circuit.copy(modules = modifiedModules))
	}

	//count all stmts in the design
	def keyModules(m: DefModule): DefModule = {
		m match {
			case mod @ firrtl.ir.Module(_,_,_,_) =>
				if (modules contains mod.name) {
					//get module key
					keyLen = 4
					mod mapStmt findWidth
					println(inWidth, outWidth)

					//count statements in module
					stmtCount = 0
					mod mapStmt countStatements

					// randomly pick the statements to replace
					stmtInds.clear()
					stmtInds ++= rand.shuffle(List.range(0,stmtCount)).take(keyLen).sorted

					//return modified module
					stmtCount = 0
					keyCount = 0
					mod mapStmt replaceStatements
				} else {mod}
			case other => other
		}
	}

  	//count stmts
    def countStatements(s: Statement): Statement = {
		s mapStmt countStatements
		s match {
		  case DefNode(_,_,_) =>
			stmtCount += 1
			s
		  case other => other
		}
  	}

  	//find width
    def findWidth(s: Statement): Statement = {
		s mapStmt findWidth
		s match {
			case stmt @ Connect(_,WSubField(WRef("auxFSM",BundleType(ArrayBuffer(Field("clock",Flip,ClockType), Field("reset",Flip,_), Field("io_in",Flip,UIntType(IntWidth(in_width))), Field("io_out",Default,UIntType(IntWidth(out_width))))),InstanceKind,MALE),clock,ClockType,FEMALE),WRef("clock",ClockType,PortKind,MALE)) => 
				inWidth = in_width.toInt
				outWidth = out_width.toInt
				stmt
			case other => other
		}
  	}

  	//replace selected stmts
    def replaceStatements(s: Statement): Statement = {
    	val s_mod = s mapStmt replaceStatements
        s_mod match {
        	case node @ DefNode(_,_,_) =>
            	if (stmtInds.contains(stmtCount)) {

					val out_node = DefNode(NoInfo,"auxFSM_out",WSubField(WRef("auxFSM",BundleType(ArrayBuffer(Field("clock",Flip,ClockType), Field("reset",Flip,UIntType(IntWidth(1))), Field("io_in",Flip,UIntType(IntWidth(inWidth))), Field("io_out",Default,UIntType(IntWidth(outWidth))))),InstanceKind,MALE),"io_out",UIntType(IntWidth(outWidth)),MALE))

				  	// select key input to connect
				  	val key_io_ref = WRef("auxFSM_out",UIntType(IntWidth(keyLen)),NodeKind,MALE)
				  	val key_bit_op = DoPrim(Bits,Seq(key_io_ref),Seq(keyCount,keyCount),UIntType(IntWidth(1)))
				  	val key_bit_node = DefNode(NoInfo,s"KEY_BIT_NODE_${keyCount}",key_bit_op)
				  	val key_bit_ref = WRef(s"KEY_BIT_NODE_${keyCount}",UIntType(IntWidth(1)),NodeKind,MALE)

				  	// change the name of the original node
				  	val orig_node = DefNode(node.info,node.name + "_mod",node.value)
				  	val orig_ref = WRef(node.name + "_mod",node.value.tpe,NodeKind,MALE)

				  	node.value.tpe match {
					  	case GroundType(IntWidth(width)) if width > 1 => 
							var cat_width = 2
							val cat_nodes = ListBuffer[DefNode]()
							var key_bit_cat_ref = key_bit_ref
							while (cat_width<=width) {
								val key_bit_cat_op = DoPrim(Cat,Seq(key_bit_ref, key_bit_cat_ref),Seq(),UIntType(IntWidth(cat_width)))
								val key_bit_cat_node = DefNode(NoInfo,s"KEY_BIT_CAT_NODE_${keyCount}_${cat_width}",key_bit_cat_op)
								cat_nodes += key_bit_cat_node
								key_bit_cat_ref = WRef(s"KEY_BIT_CAT_NODE_${keyCount}_${cat_width}",UIntType(IntWidth(cat_width)),NodeKind,MALE)
								cat_width += 1
							}
					  
							// connect xor
							val xor_op = DoPrim(Xor,Seq(key_bit_cat_ref,orig_ref),Seq(),node.value.tpe)
							val xor_node = DefNode(NoInfo,node.name,xor_op)
						
							//increment inds and return new block statement
							stmtCount += 1
							keyCount += 1
							if (keyCount==1) {	
								Block(Seq(out_node,key_bit_node) ++ cat_nodes ++ Seq(orig_node,xor_node))
							} else {
								Block(Seq(key_bit_node) ++ cat_nodes ++ Seq(orig_node,xor_node))
							}

					  	case GroundType(IntWidth(width)) if width == 1 => 
							// connect xor
							val xor_op = DoPrim(Xor,Seq(key_bit_ref,orig_ref),Seq(),node.value.tpe)
							val xor_node = DefNode(NoInfo,node.name,xor_op)
						
							//increment inds and return new block statement
							stmtCount += 1
							keyCount += 1
							if (keyCount==1) {	
								Block(Seq(out_node,key_bit_node,orig_node,xor_node))
							} else {
								Block(Seq(key_bit_node,orig_node,xor_node))
							}
						  	

				  		case _ => s_mod
					}

            	} else {
              		stmtCount += 1
              		s_mod
            	}
          	case other => other
        }
	}

}

class AuxFSM(
	val inputSeq: Seq[Int],
	val inputWidth: Int,
	nObfStates: Int = 10,
	maxFanout: Int = 4
) extends chisel3.Module {
	
	//get input, output, and state widths
	val nKeyStates = inputSeq.length+1
	val nStates = nKeyStates + nObfStates
	val stateWidth = scala.math.ceil(scala.math.log(nStates)/scala.math.log(2)).toInt

	//io
	val io = IO(new Bundle {
		val in = chisel3.Input(UInt(inputWidth.W))
		val out = chisel3.Output(UInt(stateWidth.W))
	})

	//generate random state indicies
	val stateInds = (Random.shuffle(1 to nStates-1)++Seq(0)).map(x=>x.U(stateWidth.W))
	val inputInds = inputSeq.map(_.U(inputWidth.W))
	val obfStateInds = stateInds.take(nObfStates)
	val keyStateInds = stateInds.drop(nObfStates)
	val nextKeyStateInds = keyStateInds.drop(1)++keyStateInds.take(1)
	val nInputStates = scala.math.pow(2,inputWidth).toInt

	//FSM state
	val state = RegInit(keyStateInds(0))

	//randomly connect obfuscated states
	def GenObfStateMux(stateInd: chisel3.core.UInt) = {
		//determine fanout for this state
		val n_fanout = 1+Random.nextInt(maxFanout-1)
		//select input states
		val inputs = List.fill(n_fanout)(Random.nextInt(nInputStates)).map(x=>x.U(inputWidth.W))
		//select their respective next states
		val next_states = Vector.fill(n_fanout)(obfStateInds(Random.nextInt(obfStateInds.length)))
		//return tuple of state and mux
		(stateInd, MuxLookup(io.in, stateInd, (inputs zip next_states)))
	}
	val obfStateMap = obfStateInds.map(GenObfStateMux)

	//connect key states in chain
	def GenKeyStateMux(t: (chisel3.core.UInt, chisel3.core.UInt, chisel3.core.UInt)) = {
		val inputInd = t._3
		val nextStateInd = t._2
		val stateInd = t._1
		//determine fanout for this state
		val n_fanout = 1+Random.nextInt(maxFanout-1)
		//select input states
		val inputs = List.fill(n_fanout-1)(Random.nextInt(nInputStates)).map(x=>x.U(inputWidth.W)).filterNot(_==inputInd)++Seq(inputInd)
		//select their respective next states
		val next_states = List.fill(inputs.length-1)(obfStateInds(Random.nextInt(obfStateInds.length)))++Seq(nextStateInd)
		//return tuple of state and mux
		(stateInd, MuxLookup(io.in, stateInd, inputs zip next_states))
	}
	val keyStateMap = (keyStateInds, nextKeyStateInds, inputInds).zipped.toList.map(GenKeyStateMux)

	val nextState = MuxLookup(state, state, obfStateMap ++ keyStateMap)
	state := nextState
	io.out := state

}


